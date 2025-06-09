"""
PLEASE DO NOT MOVE FROM FOLDER, EVERYTHING IS DYNAMIC

This Code is not ideal as it is:
* Requires multiple runs as the total time to datascrape could take multiple weeks of constat running
* The storage space can max-out and therefore you would need to restart

The solution to this issue is having the partial saving throughout the process
"""

import os
import sys
import subprocess
from pathlib import Path

# Function to ensure pip is installed
def ensure_pip():
    try:
        import pip
    except ImportError:
        print("pip not found. Installing pip...")
        subprocess.check_call([sys.executable, "-m", "ensurepip", "--default-pip"])

# Function to install missing packages
def install_if_missing(packages):
    for package in packages:
        try:
            __import__(package)
        except ImportError:
            print(f"Installing {package}...")
            subprocess.check_call([sys.executable, "-m", "pip", "install", package])

# Ensure pip is available
ensure_pip()

# List of required packages
required_packages = ["pandas", "requests", "lxml"]

# Install missing packages
install_if_missing(required_packages)

# Import necessary libraries
import pandas as pd
import requests
import time
import random
from concurrent.futures import ThreadPoolExecutor, as_completed
from lxml import html
import gc

# Dynamically set the base path to the current working directory
base_path = Path.cwd()

# Define paths for the input file and output directory
tconst_location_path = os.path.join(base_path, "tconst.csv")
output_dir = base_path
filming_dates_save_path = os.path.join(output_dir, 'location.csv')
partial_filming_dates_save_path = os.path.join(output_dir, 'location_partial.csv')

# Check if the main input file exists
if not os.path.exists(tconst_location_path):
    print(f"Error: File {tconst_location_path} not found. Please check the path and try again.")
    quit()

# Load tconst IDs from the CSV file
try:
    tconst_df = pd.read_csv(tconst_location_path)
    tconst_ids = tconst_df['tconst'].tolist()
    print(f"Loaded {len(tconst_ids)} tconst IDs from {tconst_location_path}.")
except Exception as e:
    print(f"Error loading input data: {e}")
    quit()

# Load previously saved filming dates if they exist
if os.path.exists(filming_dates_save_path):
    try:
        filming_dates_data = pd.read_csv(filming_dates_save_path)
        filming_dates_list = filming_dates_data.to_dict('records')
        scraped_ids = filming_dates_data['tconst'].tolist()
    except Exception as e:
        print(f"Error loading filming dates: {e}. Starting from scratch.")
        filming_dates_list = []
        scraped_ids = []
else:
    filming_dates_list = []
    scraped_ids = []

# Load previously saved partial filming dates if they exist
if os.path.exists(partial_filming_dates_save_path):
    try:
        partial_filming_dates_data = pd.read_csv(partial_filming_dates_save_path)
        partial_filming_dates_list = partial_filming_dates_data.to_dict('records')
        scraped_ids += partial_filming_dates_data['tconst'].tolist()
        print(
            f"Loaded {len(partial_filming_dates_list)} previously scraped partial filming dates from {partial_filming_dates_save_path}.")
    except Exception as e:
        print(f"Error loading partial filming dates: {e}. Starting from scratch.")
        partial_filming_dates_list = []
else:
    print("No partial filming dates found. Starting fresh.")
    partial_filming_dates_list = []

# Filter out already processed tconst IDs
tconst_remaining_ids = [tconst for tconst in tconst_ids if tconst not in scraped_ids]
print(f"Remaining tconst IDs to scrape: {len(tconst_remaining_ids)}")

# Set the batch size and thread count
batch_size = 50  # Adjust as needed to manage memory usage
max_threads = 26

# Split remaining tconst IDs into manageable batch sizes
tconst_batches = [tconst_remaining_ids[i:i + batch_size] for i in range(0, len(tconst_remaining_ids), batch_size)]

# Save filming dates
def save_filming_dates(filming_dates_data, new_dates, save_path):
    if new_dates:
        filming_dates_data.extend(new_dates)
        pd.DataFrame(filming_dates_data).to_csv(save_path, index=False)
        print(f"Total saved filming dates: {len(filming_dates_data)}. Saved to {save_path}.")
    return filming_dates_data

# Save partial filming dates
def save_partial_filming_dates(partial_data, new_dates, save_path):
    if new_dates:
        partial_data.extend(new_dates)
        pd.DataFrame(partial_data).to_csv(save_path, index=False)
        print(f"Total saved partial filming dates: {len(partial_data)}. Saved to {save_path}.")
    return partial_data

# List of user-agents specifically for Chrome, Microsoft Edge, and Firefox
user_agents = [
    # Chrome on Windows
    'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36',
    # Edge on Windows
    'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Edg/91.0.864.59 Safari/537.36',
    # Firefox on Windows
    'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:91.0) Gecko/20100101 Firefox/91.0',
    # Chrome on macOS
    'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.114 Safari/537.36',
    # Safari on macOS
    'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/14.0.3 Safari/605.1.15',
    # Chrome on Android
    'Mozilla/5.0 (Linux; Android 11; SM-G991B) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.105 Mobile Safari/537.36',
    # Safari on iOS
    'Mozilla/5.0 (iPhone; CPU iPhone OS 14_6 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/14.0 Mobile/15E148 Safari/604.1',
    # Edge on macOS
    'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.114 Safari/537.36 Edg/91.0.864.59'
]

referers = [
    'https://www.google.com/',
    'https://www.bing.com/',
    'https://www.yahoo.com/',
    'https://www.duckduckgo.com/',
    'https://www.ask.com/',
    'https://www.ecosia.org/'
]

accept_languages = [
    'en-US,en;q=0.9',
    'en-GB,en;q=0.9'
]

# Function to scrape filming locations and dates for a single tconst ID
def scrape_filming_data(tconst):
    base_url = "https://www.imdb.com"
    filming_locations_url = f"{base_url}/title/{tconst}/locations/"
    headers = {
        'User-Agent': random.choice(user_agents),
        'Accept-Language': random.choice(accept_languages),
        'Referer': random.choice(referers),
        'Connection': 'keep-alive'
    }
    max_retries = 5

    for attempt in range(max_retries):
        try:
            response = requests.get(filming_locations_url, headers=headers, timeout=10)
            if response.status_code == 404:
                print(f"Invalid URL for {tconst}. Setting filming locations and dates as NA.")
                return [{'tconst': tconst, 'filming_location': 'NA', 'filming_dates': 'NA'}]
            response.raise_for_status()
            tree = html.fromstring(response.content)

            # Extract filming locations
            filming_locations = tree.xpath(
                "//a[contains(@href, '/search/title/?locations=')]/text()")
            filming_locations = filming_locations if filming_locations else ['NA']

            # Extract filming dates
            filming_dates = tree.xpath(
                "//div[@data-testid='sub-section-flmg_dates']//div[@class='ipc-html-content-inner-div']/text()")
            filming_dates = [date.strip() for date in filming_dates] if filming_dates else ['NA']

            # Create a separate entry for each combination of filming location and date
            result = [{'tconst': tconst,
                       'filming_location': loc,
                       'filming_dates': date}
                      for loc in filming_locations for date in filming_dates]

            # Simulate delay to avoid rate limiting
            time.sleep(random.uniform(3, 8))

            return result

        except requests.exceptions.HTTPError as e:
            if response.status_code == 429:  # Block detected
                print(f"Error 429: Too many requests for {tconst}. Saving progress and exiting...")
                save_partial_filming_dates(partial_filming_dates_list, filming_dates_batch, partial_filming_dates_save_path)
                quit()
            else:
                print(f"HTTP error for {tconst}: {e}. Retrying in {2 ** attempt} seconds...")
                time.sleep(2 ** attempt)
        except requests.exceptions.RequestException as e:
            print(f"Error scraping {tconst}: {e}. Retrying in {2 ** attempt} seconds...")
            time.sleep(2 ** attempt)

    print(f"Failed to retrieve filming locations for {tconst} after {max_retries} retries.")
    return [{'tconst': tconst, 'filming_location': 'NA', 'filming_dates': 'NA'}]

# Function to handle a batch of tconst IDs
def process_batch(ids, max_threads=max_threads):
    filming_dates_batch = []
    with ThreadPoolExecutor(max_workers=max_threads) as executor:
        future_to_id = {executor.submit(scrape_filming_data, _id): _id for _id in ids}
        for future in as_completed(future_to_id):
            filming_dates = future.result()
            if filming_dates:
                filming_dates_batch.extend(filming_dates)
    return filming_dates_batch

# Initialize list to store new results
filming_dates_batch = []

# Scrape filming dates in batches with error handling
try:
    for i, batch in enumerate(tconst_batches, start=1):
        filming_dates = process_batch(batch, max_threads=max_threads)
        filming_dates_batch.extend(filming_dates)
        print(
            f"Processed batch {i} of {len(tconst_batches)} ({len(batch)} IDs in this batch). Batches left: {len(tconst_batches) - i}")

        # Save partial accumulated results incrementally and clear memory
        partial_filming_dates_list = save_partial_filming_dates(partial_filming_dates_list, filming_dates_batch, partial_filming_dates_save_path)
        filming_dates_batch.clear()  # Clear results to free memory
        gc.collect()  # Manually trigger garbage collection

# Catch any exceptions and save the data collected so far
except Exception as e:
    print(f"An error occurred: {e}. Saving current progress.")
    partial_filming_dates_list = save_partial_filming_dates(partial_filming_dates_list, filming_dates_batch, partial_filming_dates_save_path)
    quit()

# Final save of filming locations and dates
filming_dates_list = save_filming_dates(filming_dates_list, partial_filming_dates_list, filming_dates_save_path)
print(f"Saved final filming locations and dates to {filming_dates_save_path}")
quit()
