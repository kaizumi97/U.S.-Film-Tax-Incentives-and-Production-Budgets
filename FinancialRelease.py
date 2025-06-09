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
tconst_path = os.path.join(base_path, "tconst.csv")
output_dir = base_path
partial_save_path = os.path.join(output_dir, 'imdb_partial.csv')

# Check if the main input file exists
if not os.path.exists(tconst_path):
    print(f"Error: File {tconst_path} not found. Please check the path and try again.")
    quit()

# Load tconst IDs and titles from the CSV file
try:
    df = pd.read_csv(tconst_path)
    tconst_ids = df['tconst'].tolist()
    original_titles = df['originalTitle'].tolist()
    print(f"Loaded {len(tconst_ids)} tconst IDs and titles from {tconst_path}.")
except Exception as e:
    print(f"Error loading tconst data: {e}")
    quit()

# Load previously saved partial results if they exist
if os.path.exists(partial_save_path):
    try:
        scraped_data = pd.read_csv(partial_save_path)
        scraped_ids = scraped_data['tconst'].tolist()
        print(f"Loaded {len(scraped_ids)} previously scraped IDs from {partial_save_path}.")
    except Exception as e:
        print(f"Error loading partial results: {e}. Starting from scratch.")
        scraped_ids = []
        scraped_data = pd.DataFrame(
            columns=['tconst', 'release_date', 'country', 'distributor', 'budget', 'domesticBO','internationalBO'])
else:
    print("No partial results found. Starting fresh.")
    scraped_ids = []
    scraped_data = pd.DataFrame(
        columns=['tconst', 'release_date', 'country', 'distributor', 'budget', 'domesticBO','internationalBO'])

# Filter out already processed IDs
remaining_ids = [tconst for tconst in tconst_ids if tconst not in scraped_ids]
remaining_titles = [original_titles[tconst_ids.index(tconst)] for tconst in remaining_ids]
print(f"Remaining IDs to scrape: {len(remaining_ids)}")

# Set the batch size and thread count
batch_size = 50  # Adjust as needed to manage memory usage
max_threads = 26

# Split remaining tconst IDs and titles into manageable batch sizes
tconst_batches = [remaining_ids[i:i + batch_size] for i in range(0, len(remaining_ids), batch_size)]
title_batches = [remaining_titles[i:i + batch_size] for i in range(0, len(remaining_titles), batch_size)]

# Save the entire accumulated results
def save_all_results(scraped_data, tconst_results, output_dir):
    if tconst_results:
        # Add the new results to the accumulated scraped data
        scraped_data = pd.concat([scraped_data, pd.DataFrame(tconst_results)], ignore_index=True)
        save_path = os.path.join(output_dir, 'imdb_partial.csv')
        scraped_data.to_csv(save_path, index=False)
        print(f"Total saved results: {len(scraped_data)}. Saved to {save_path}.")
    return scraped_data

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

# Create a session object for persistent connections
session = requests.Session()

# Function to scrape BoxOffice Mojo data based on tconst
def scrape_boxofficemojo(tconst):
    bom_url = f"https://www.boxofficemojo.com/title/{tconst}"
    headers = {'User-Agent': random.choice(user_agents)}
    try:
        response = session.get(bom_url, headers=headers)
        tree = html.fromstring(response.content)

        # Extract Distributor
        distributor = tree.xpath(
            '//span[contains(text(),"Domestic Distributor")]/following-sibling::span[1]/text()')
        distributor = distributor[0] if distributor else 'NA'

        return distributor
    except Exception as e:
        print(f"Error scraping BoxOffice Mojo for {tconst}: {e}")
        return 'NA'

# Function to scrape data for a single tconst ID
def scrape_imdb_tconst(tconst, original_title):
    base_url = "https://www.imdb.com"
    url = f"{base_url}/title/{tconst}"
    headers = {
        'User-Agent': random.choice(user_agents),
        'Referer': random.choice(referers),
        'Accept-Language': random.choice(accept_languages),
        'Connection': 'keep-alive'
    }
    max_retries = 7  # Increased retries for more resilience

    for attempt in range(max_retries):
        try:
            response = session.get(url, headers=headers, timeout=10)
            response.raise_for_status()
            tree = html.fromstring(response.content)

            # Extract country of origin
            country = tree.xpath(
                "//a[contains(@href, '/search/title/?country_of_origin=')]/text()")
            country = country[0] if country else 'NA'

            # Extract release date
            release_date = tree.xpath(
                "//div[@class='ipc-metadata-list-item__content-container']//a[contains(@href, '/releaseinfo')]/text()")
            release_date = release_date[0] if release_date else 'NA'

            # Extract budget
            budget = tree.xpath(
                "//li[@data-testid='title-boxoffice-budget']//span[contains(text(), '$')]/text()")
            budget = budget[0] if budget else 'NA'

            # Extract domestic box office
            domestic_box_office = tree.xpath("//li[@data-testid='title-boxoffice-grossdomestic']//span[@class='ipc-metadata-list-item__list-content-item' and contains(text(), '$')]/text()")
            domestic_box_office = domestic_box_office[0] if domestic_box_office else 'NA'

            # Extract box office
            international_box_office = tree.xpath(
                "//li[@data-testid='title-boxoffice-cumulativeworldwidegross']//span[contains(text(), '$')]/text()")
            international_box_office = international_box_office[0] if international_box_office else 'NA'

            # Get BoxOffice Mojo data
            distributor = scrape_boxofficemojo(tconst)

            time.sleep(random.uniform(3, 8))  # Random delay between 3 to 8 seconds
            return {
                'tconst': tconst,
                'releaseDate': release_date,
                'country': country,
                'distributor': distributor,
                'budget': budget,
                'domesticBO': domestic_box_office,
                'internationalBO': international_box_office,
            }

        except requests.exceptions.HTTPError as e:
            if response.status_code == 429:  # Block detected
                print(f"Error 429: Too many requests for {tconst}. Pausing for 5 minutes...")
                save_all_results(scraped_data, tconst_results, output_dir)
                time.sleep(300)  # Pause for 5 minutes
            elif response.status_code == 503:
                print(f"Error 503: Service unavailable for {tconst}. Pausing for 5 minutes...")
                time.sleep(300)  # Pause for 5 minutes
            else:
                print(f"HTTP error for {tconst}: {e}. Retrying in {2 ** attempt + random.uniform(1, 5)} seconds...")
        except requests.exceptions.RequestException as e:
            print(
                f"Error scraping {tconst}: {e}. Retrying in {2 ** attempt + random.uniform(1, 5)} seconds...")
            time.sleep(2 ** attempt + random.uniform(1, 5))

    print(f"Failed to retrieve data for {tconst} after {max_retries} retries.")
    return {'tconst': tconst,
            'release_date': 'NA',
            'country': 'NA',
            'distributor': 'NA',
            'budget': 'NA',
            'domesticBO': 'NA',
            'internationalBO': 'NA'}

# Function to handle a batch of tconst IDs
def process_tconst_batch(tconsts, titles, max_threads=max_threads):
    results = []
    with ThreadPoolExecutor(max_workers=max_threads) as executor:
        future_to_tconst = {
            executor.submit(scrape_imdb_tconst, tconst, title): tconst for tconst, title in zip(tconsts, titles)}
        for future in as_completed(future_to_tconst):
            result = future.result()
            if result:
                results.append(result)
    return results

# Initialize list to store new results
tconst_results = []

# Scrape remaining tconst data in batches with error handling
try:
    for i, (batch, title_batch) in enumerate(zip(tconst_batches, title_batches), start=1):
        results = process_tconst_batch(batch, title_batch, max_threads=max_threads)
        tconst_results.extend(results)
        print(
            f"Processed batch {i} of {len(tconst_batches)} ({len(batch)} IDs in this batch). Batches left: {len(tconst_batches) - i}")

        # Save all accumulated results incrementally and clear memory
        scraped_data = save_all_results(scraped_data, tconst_results, output_dir)
        tconst_results.clear()  # Clear results to free memory
        gc.collect()  # Manually trigger garbage collection

# Catch any exceptions and save the data collected so far
except Exception as e:
    print(f"An error occurred: {e}. Saving current progress.")
    scraped_data = save_all_results(scraped_data, tconst_results, output_dir)
    quit()

# Final save of results
scraped_data = save_all_results(scraped_data, tconst_results, output_dir)
scraped_data.to_csv(os.path.join(output_dir, 'imdb_final.csv'), index=False)
print(f"Saved final tconst results to {os.path.join(output_dir, 'imdb_final.csv')}")
quit()
