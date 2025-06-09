##### Setting Work Directory & Loading Packages ####
main_dir <- getwd()

required_packages <- c(
  # Core Data Manipulation and Processing
  "dplyr", "tidyr", "purrr",
  
  # String Manipulation
  "stringr",
  
  # Date and Time Handling
  "lubridate",
  
  # Data Visualization
  "ggplot2", "viridis",
  
  # Data Import/Export
  "readr",
  
  # Statistical and Modeling Tools
  "plm", "broom", "synthdid", "scales", 
  "kableExtra", "knitr", "fixest", "tibble",
  "did", "doParallel", "forcats", "reshape2",
  
  # Additional Libraries
  "patchwork", "curl", "countrycode"
)

# Loop to check for and install missing packages
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    cat(paste("Installing missing package:", pkg, "\n"))
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)  # Load the package
}

rm(pkg,required_packages)

#### Gathering Data  ####
# It is important that everything is in the same folder as this will determine whether your data is there
# Update the work directory is needed, bu insure that the folder is correct

# Initialize a list to store datasets
imdb_data_list <- list()

# Define IMDb file identifiers (base names)
file_identifiers <- c("title.basics", "title.ratings", "name.basics","title.principals","title.crew", "title.episode")
base_url <- "https://datasets.imdbws.com/"

# Loop to dynamically download, extract, and process files
for (identifier in file_identifiers) {
  
  # Define URL and destination file name
  url <- paste0(base_url, identifier, ".tsv.gz")
  destfile <- file.path(paste0(identifier, ".tsv.gz"))
  
  # Check if file exists before downloading
  if (!file.exists(destfile)) {
    cat(paste("Downloading:", destfile, "\n"))
    curl_download(url, destfile)
  } else {
    cat(paste("File already exists:", destfile, "\n"))
  }
  
  # Extract and process the file
  cat(paste("Extracting and processing:", destfile, "\n"))
  data <- read_tsv(destfile, col_types = cols())
  data[data == "\\N"] <- NA  # Replace "\\N" with NA
  
  # Add processed data to the list with identifier as the name
  imdb_data_list[[identifier]] <- data
  
  rm(data)
}

# Warning message can be ignored:
# Files were already present, extracted, and processed successfully,  with minor parsing warnings that can be investigated if necessary, but they are fine.

# Assign extracted tibbles to variables
title_basics <- imdb_data_list[["title.basics"]]
title_ratings <- imdb_data_list[["title.ratings"]]
name.basics <- imdb_data_list[["name.basics"]]
title.principals <- imdb_data_list[["title.principals"]]
title.crew <- imdb_data_list[["title.crew"]]
title.episode <- imdb_data_list[["title.episode"]]


summary(title_basics)

print(title_basics %>%
  count(titleType))

print(title_basics %>%
  count(startYear), n = 200)

# Merge into one data frame and filter for only movies
film <- full_join(title_basics, title_ratings, by = "tconst") %>% 
  filter(titleType == "movie") %>% # Filter for those that are categorised at movies
  select (-titleType, -primaryTitle, -isAdult, -endYear)  %>% # Remove information that would not be relevant here
  mutate(across(c(3, 4, 6, 7), as.numeric)) %>% # Convert years, runtime, ratings and votes to numerics
  # We are interested in a 40-year time frame so from 1989 (10 years before Missouris implementation in 1999) to the 2023 (as 2024 is not over yet as of the current date 17th of December 2024).
  filter(startYear < 2025 & startYear > 1987)  # Notice we expanded the range by 1 year in order to make sure that the data scrapes the correct information. Films could have pre-premieres of debuts

head(film)

# Create the CSV file containing the tconsnt and origintalTitle for Datascraping
write.csv(film %>% distinct(tconst, originalTitle),"tconst.csv", row.names = FALSE)

rm(imdb_data_list, base_url,destfile, file_identifiers, identifier, url, title_basics, title_ratings)

gc()

save.image("Film_Data.RData")

#### Cleaning Data ####
load("Film_Data.RData")

locationDates <- read_csv("location.csv")
financeRelease <- read_csv("imdb_final.csv")

financeRelease %>% filter(!is.na(budget)) # Films with budgets
financeRelease %>% filter(!is.na(releaseDate)) # Films with Release Dates
locationDates %>% filter(!is.na(filming_location)) %>% distinct(tconst) # Films with Filming Locations
locationDates %>% filter(!is.na(filming_dates)) %>% distinct(tconst) # Films with Filming Dates


str(tconst_data)
str(tconst_location)


financeRelease <-  financeRelease %>%
  rename (countryRelease = country) %>% # Change to Country
  
  # Convert release dates to date fornat
  mutate(extractedDate = str_extract(releaseDate, "[A-Za-z]+ \\d{1,2}, \\d{4}")) %>% # Convert to date
  mutate(release_date_clean = mdy(extractedDate)) %>%   # Convert to date format month date year
  select(-extractedDate, -releaseDate) %>% # remove extractedDate and releaseDate
  rename (releaseDate = release_date_clean) %>% # rename column to releaseData 
  mutate(releaseYear = year(releaseDate)) %>% # extract actual release year 
  
  # Convert Box office to numeric
  mutate(domesticBO = as.numeric(str_replace_all(domesticBO, "[\\$,]", ""))) %>% 
  mutate(internationalBO = as.numeric(str_replace_all(internationalBO, "[\\$,]", ""))) %>%
  
  #Extract film budgets and covner to numerical
  mutate(budget = str_extract(budget, "\\$[0-9,]+"),
         budget = as.numeric(str_replace_all(budget, "[\\$,]", "")))

# Clean-up the location and filming_date data
locationDates <-  locationDates %>% 
  # Clean up he filming location to seperate State and Country
  mutate(location_split = str_split(filming_location, ",\\s*")) %>% # Split into states and country of filming
  mutate(
    filmingState = map_chr(location_split, ~ if (length(.x) > 1) .x[[length(.x) - 1]] else NA_character_),
    filmingCountry = map_chr(location_split, ~ .x[[length(.x)]]),
  ) %>%
  distinct(tconst, filming_dates, filming_location, filmingState, filmingCountry) %>%
  
  # Notice that some years have varying filming dates. Some have all four digits (e.g., 1984), while another one could could have just 84.
  mutate(
    filmYear = str_extract(filming_dates, "\\d{4}|\\d{2}$"), #extract filming years
    filmYear = if_else(str_length(filmYear) == 2 & filmYear > 24, paste0("19", filmYear), filmYear), #if the film year is greater that 24, then its likely from 1900s
    filmYear = if_else(str_length(filmYear) == 2 & filmYear < 24, paste0("20", filmYear), filmYear) # if the film year is less than 24
  ) %>%
  mutate(
    years = str_extract_all(filming_dates, "\\d{4}"),
    filmYear = map_dbl(years, ~ if (length(.x) > 1) as.numeric(.x[length(.x)]) else as.numeric(.x[1])),
  ) %>%
  select(-years)


film <- film %>%
  full_join(financeRelease, by = "tconst") %>%
  full_join(locationDates, by ="tconst") %>%
  mutate(
    releaseYear = ifelse(is.na(releaseYear), startYear, releaseYear) # Ensure that the some form of releaseYear is put inplace incase there is an NA
  ) %>%
  select(-originalTitle, -startYear, -genres, -releaseDate) # Remove columns that we no longer have use for from IMDB Commercial Database

film %>% filter(!is.na(filming_location) & !is.na(filming_dates)) %>% distinct(tconst)
film %>% filter(!is.na(budget) &!is.na(filming_location) & !is.na(filming_dates)) %>% distinct(tconst)

##### Extracted US Relevant Films ####
film %>%
  count(filmingCountry, sort = TRUE)

data <- film %>%
  mutate(filmingCountry = ifelse(filmingCountry %in% c("United States", "Unites States of America", "The United States of America","United States of America", "America", "US"), "USA", filmingCountry)) %>%
  mutate(filmingCountry = ifelse(filmingCountry == "U.S. Virgin Islands","US Virgin Islands", filmingCountry)) %>% # Special cases where U.S. 

  # The case of Georgia and other major filming countries
  filter(countryRelease != "Georgia" & filmingCountry != "Georgia") %>% # If the film is released in Georgia AND filmed in Georgia, then it is likely a Georgian Film.
  filter(!filmingCountry %in% c("Republic of Georgia","South Georgia","South Georgia and the South Sandwich Islands", "Antarctica"))

# Use countrycode to isolate countries that of in the list and certainly not USA
all_countries <- countrycode::codelist$country.name.en
all_countries <- all_countries[!all_countries %in% c("United States", "American Samoa", "Guam", "Georgia", 
                                                  "Northern Mariana Islands", "Puerto Rico", 
                                                  "U.S. Virgin Islands", "United States Minor Outlying Islands (the)", "Unknown")] # Remove United States, and other possible regional territoriesfrom the list of countrie that will be eliminated

all_countries

data <- data[!(data$filmingCountry %in% all_countries), ]


"
We remove countries that we were sure are not US based
Now we that we have removed all countries that we know are not USA and we know that a film was made in the country Georige, we can take a closer look at extracting the states.

"

# Create a table for the States as mentioned in the NCSL
us_states <- c("Alabama", "Alaska", "American Samoa","Arizona", "Arkansas", "California", "Colorado", 
               "Connecticut", "Delaware", "District of Columbia","Florida", "Georgia", "Guam", "Hawaii", "Idaho", 
               "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
               "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
               "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
               "New Hampshire", "New Jersey", "New Mexico", "New York", 
               "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", 
               "Pennsylvania", "Puerto Rico","Rhode Island", "South Carolina", "South Dakota", 
               "Tennessee", "Texas", "US Virgin Islands","Utah", "Vermont", "Virginia", 
               "Washington", "West Virginia", "Wisconsin", "Wyoming")

state_abbreviations <- c("AL", "AK", "AS", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "GU",
                         "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
                         "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
                         "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "PR", "RI", "SC", 
                         "SD", "TN", "TX", "USVI","UT", "VT", "VA", "WA", "WV", "WI", "WY")

state_lookup <- setNames(us_states, state_abbreviations)

extract_state <- function(location) {
  if (is.na(location)) { # Check if the location is NA
    return(NA)
  }
  for (state in us_states) {
    if (str_detect(location, fixed(state, ignore_case = TRUE))) {
      return(state)
    }
  }
  for (abbr in names(state_lookup)) {
    if (str_detect(location, paste0("\\b", abbr, "\\b"))) {
      return(state_lookup[abbr])
    }
  }
  return(NA)
}

# Apply State Extraction
data_state <- data %>%
  mutate(
    # Extract state from 'filming_location'
    extractedState = sapply(filming_location, extract_state),
    
    # If still NA, extract state from 'filmingState'
    extractedState = ifelse(is.na(extractedState), 
                            sapply(filmingState, extract_state), extractedState),
    
    # If still NA, extract state from 'filmingCountry'
    extractedState = ifelse(is.na(extractedState), 
                            sapply(filmingCountry, extract_state), extractedState),
    
    # Update columns conditionally
    filmingState = ifelse(!is.na(extractedState), extractedState, filmingState),
    filmingCountry = ifelse(!is.na(extractedState), "USA", filmingCountry)
  ) %>%
  # Drop the temporary 'extractedState' column
  select(-extractedState) %>%
  filter(!str_trim(filming_location) %in% c("United States of America", "USA", "America")) # If this the only thing in the filming_location, then there is now way to figure out the state

# Observations that are already good
data_US <- data_state %>%
  filter(filmingCountry == "USA" & filmingState %in% us_states) %>%
  select(-filmingCountry) %>%
  distinct() 

# US observations that need close look
data_unsure <- data_state %>%
  filter(!filmingState %in% us_states)

location_ranking_unsure <- data_unsure %>%
  count(filmingCountry, sort = TRUE)

location_ranking_unsure

"
Attempted to use AI based tools or other forms of R packages, but due inconsistent
formatting of addresses, it became impossible to predict and so it would be serve
to review it by eye, remove the locations that are not necessary and then clean 
from there.
"

# First 500 rows
location_ranking_unsure <- location_ranking_unsure %>%
  slice(-1:-2, -4:-38, -40:-41, -43:-47,-49:-76, -78:-180,
        -182:-196,-198:-216,-218:-238,-239:-244,-246:-262,-263:-270,
        -273:-339,-341:-344,-347:-356,-358:-371,-375:-376,-378:-388,
        -390:-396,-399:-413,-415:-421,-423:-437,-439:-456,-458:-497,
        -499:-511,-513:-525,-527:-540,-542:-543,-545:-550,-553:-567,
        -569:-595,-597:-600,-604:-624,-626:-643,-645:-675,-678:-681,
        -684:-700,-702:-704,-706,-708:-725,-727:-737,-739:-755)

# Remove those that we deleted from the table
data_unsure_clean <- full_join(data_unsure, location_ranking_unsure, by = "filmingCountry") %>%
  filter (!is.na(n)) %>%
  select (-n, -filmingCountry) %>%
  distinct()

data_state_clean <- data_unsure_clean %>%
  mutate(
    filmingState = case_when(
      str_detect(filming_location, "West 83rd street and Columbus|32 Beach Ln, Westhampton Beach|104 W 83rd St.|Suffolk County Sheriff's Office Riverhead Correctional Facility|Kings Theater|The Gateway Playhouse|Tompkins Cortland Community|The Coorporation of Yaddo Facilities|Guggenheim|Manhattan|Eleanor Roosevelt High School|NYC|Brooklyn|Rumpus Room 239 eldridge st.") 
      ~ "New York",
      str_detect(filming_location, "Minn|Minneapolis") 
      ~ "Minnesota",
      str_detect(filming_location, "North, Sourth Carolina|1034 W Dekalb St, Camden|519 Whitehead Rd, Lugoff") 
      ~ "South Carolina",
      str_detect(filming_location, "Museum of the Bible|Columbia")
      ~ "District of Columbia",
      str_detect(filming_location, "Rocky Mountain National Park|Red Rock Canyon Open Space|Paint Mines Interpretive Park|Local Relic|Fountain Creek Regional Park|Bishops Castle")
      ~ "Colorado",
      str_detect(filming_location, "Seattle|Greater Glory Ministries|Lake Stevens|Bliss Small Batch Creamery")
      ~ "Washington",
      str_detect(filming_location, "Copper Hills High School")
      ~ "Utah",
      str_detect(filming_location, "Hampton Convocation Center")
      ~ "Virginia",
      str_detect(filming_location, "Bruneau Sand Dunes")
      ~ "Idaho",
      str_detect(filming_location, "Alaksa")
      ~ "Alaska",
      str_detect(filming_location, "Knoxville|Tennesse|Optimum Studios|Memphis") 
      ~ "Tennessee",
      str_detect(filming_location, "Avondale|LPT Studios LLC") 
      ~ "Alabama",
      str_detect(filming_location, "Miami|Fl|Orlando") 
      ~ "Florida",
      str_detect(filming_location, "The Ranch Studios|Fishpot Studios|Creative Film Connections|Louiaiana|Luisiana|New Orleans|Lousiana|Louisisana|Louisianna") 
      ~ "Louisiana",
      str_detect(filming_location, "Old Cowtown Museum|Flint Hills National Prairie Reserve") 
      ~ "Kansas",
      str_detect(filming_location, "Black Sea Hall|Rockridge Forest Preserve and Wildlife Refuge|Historic Forth Ward, Skate Park, USA|The Earl and Rachel Smith Strand Theatre|Atlanta|Decatur & Grady Head Start|Decatur County School District") 
      ~ "Georgia",
      str_detect(filming_location, "Roxy Theater|Cortes Bank|Gothic Avenue / San Fernando Mission Boulevard|Encinitas|Mercantile|Kimball's East|San Fransisco|Waldo Point|San Francisco|Yosemite Park|Malibu|Ace Theater|C.A|Callifornia|Califorina|Cinema Town Studios|Californa|Los Angeles|Hollywood|Jacumba Resort & Spa|Defiant Muay Thai|Yacoubian Muay Thai Academy|Beverly Hills|The Ivar Theatre|Unified Pictures|Fort Rosecrans National Cemetary") 
      ~ "California",
      str_detect(filming_location, "Sheraton Old San Juan") 
      ~ "Puerto Rico",
      str_detect(filming_location, "Plymouth Meeting|Pennsylvaina|924 N 19th St|Philadelphia|Pittsburgh|Pennyslvania") 
      ~ "Pennsylvania",
      str_detect(filming_location, "Winslow Diner|The Perfecting Church|19 Victor, United States") 
      ~ "New Jersey",
      str_detect(filming_location, "189 Belleville Ave, New Bedford|Fenway Park|Lowell|Boston|Massachuetts|Massachusettes|Massachussets|Massachussetts") 
      ~ "Massachusetts",
      str_detect(filming_location, "U.S. Virgin Islands") 
      ~ "US Virgin Islands",
      str_detect(filming_location, "Houston|Capernaum Studio|18216 Weiss Ln, Pflugerville|Tx.|Dallas|J. Lorraine Ghost Town, United States|Double Creek Ranch|Brookstreet Barbeque") 
      ~ "Texas",
      str_detect(filming_location, "EJs Warrior Karate Academy|1309 Roselawn Avenue, Belpre, USA|1207 Wyatt Ln Apt C Belpre|Iron Horse Studios|Cleveland|Square Seven Coffee House") 
      ~ "Ohio",
      str_detect(filming_location, "Ilinois|Chicago") 
      ~ "Illinois",
      str_detect(filming_location, "Carson and William Streets|Carson Tahoe Hospital|Stateline, South Lake Tahoe|Cork n Thorn|Tap and Ash|Pinto Muay Thai|Boulder City Pkwy|Necada|Vegas") 
      ~ "Nevada",
      str_detect(filming_location, "Wisconson|UW Milwaukee Panther Arena") 
      ~ "Wisconsin",
      str_detect(filming_location, "Yuba State Park|Utta") 
      ~ "Utah",
      str_detect(filming_location, "Ozark National Forest") 
      ~ "Arkansas",
      str_detect(filming_location, "Native Biodata Consortium") 
      ~ "South Dakota",
      str_detect(filming_location, "The Blalock House|Raleigh") 
      ~ "North Carolina",
      str_detect(filming_location, "Mchigan") 
      ~ "Michigan",
      str_detect(filming_location, "Louisville")
      ~ "Kentucky",
      str_detect(filming_location, "Saint Studios Films")
      ~ "Mississippi",
      str_detect(filming_location, "Hawai'i") 
      ~ "Hawaii",
      str_detect(filming_location, "Waldo County General Hospital") 
      ~ "Maine",
      TRUE ~ filmingState # Keep the existing value if no matches
    )
  ) %>%
  filter(!filming_location %in% c("The Atlantic Ocean","Roughshod Studio","Hummel Park","Pacific Coast Highway","North Wales","Harvey","Mar del Plata","Pacific Northwest","South Pacific","Plage de Benerville-sur-Mer, 14910","off Newfoundland, Atlantic Ocean","USS Enterprise, Pacific Ocean","USS Constellation, Pacific Ocean","USS Carl Vinson, Pacific Ocean","The Lake District","St. Catharines, USA","South Pacific, Pacific Ocean","Claybank Brick Factory Historic Site","Providence Island","Lima, Peru, [Studios Granada Films]","Gulf of Thailand, Pacific Ocean", "Kallaur Manson, USA","Escuela Proceres de la Independencia - Mexico, USA","Canyon Ranch","Titanic wreck, Titanic Canyon, North Atlantic, Atlantic Ocean","The United States of America","Atlantic Ocean","Pacific Ocean", "Irish Sea, North Atlantic, Atlantic Ocean",
    "Tryout Field, USA","Norwood Park, USA","Norwood Junction Skate Park, USA","Holiday Market, USA", "Jefferson Hospital, USA","Henry Chang design, USA","Game Field, USA","Cavella home, USA","Cascade Range, USA","USCGC Vigilant, USA","FANEX Film Convention, USA","Apple Harvest, USA","Agricultural Automation and Robotics Lab, USA","Standing Rock, USA","Old Mexico, USA","Appalachian Trail, USA","Appalachian Mountains, USA","Death Valley National Park, USA","Gulf of Mexico, USA","Ipiales, Columbia, USA","Medellin, Columbia, USA","Turbo, Columbia, USA","Mexico City, Mexico, USA","Navajo Nation, USA","Springfield, Stinkysockville, USA","Coast, New England, USA","Death Valley National Park, US","New England, USA","São Raimundo Nonato, PI", "America", "United State", "Rte 66, United States",
    "London, England, UK, 288 Uxbridge Rd, W12 7JA", "Yellowstone National Park",
    "Rock Experience", "United States", "US", "Bethlehem, West Bank", "North Stuart Street.","Papahahaumokuakea Marine, National Monument, United States"
  ))

data_US_clean <- rbind(data_US, data_state_clean) %>%
  filter(!is.na(filmingState)) %>%
  distinct()

rm(data_US,data_unsure, all_countries,financeRelease, locationDates, data_unsure_clean, data_state,data_state_clean, 
   location_ranking_unsure, state_abbreviations, us_states, extract_state)

gc()

save.image("Data_Cleaned.RData")

#### Data ####
# Load the dataset with complete information on U.S. film productions.
load("Data_Cleaned.RData")

data_US_clean %>% distinct(tconst)

##### Incentive Year Assignment ####
# Define the dataframe for states with implementation year, tax percentage, end year, and reinstatement year
incentive_state_year <- data.frame(
  filmingState = c(
    "Alabama", "Alaska", "Arizona", "Arkansas", "California", "Connecticut", 
    "Florida", "Georgia", "Hawaii", "Illinois", "Indiana", "Iowa", "Kentucky", 
    "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", 
    "Mississippi", "Missouri", "Montana", "Nevada", "New Jersey", "New Mexico", 
    "New York", "North Carolina", "Ohio", "Oregon", "Pennsylvania", "Rhode Island", 
    "South Carolina", "Tennessee", "Utah", "Virginia", "West Virginia", 
    "Wisconsin", "Puerto Rico", "U.S. Virgin Islands"
  )
  ,
  incentive_year = c(
    2009, 2008, 2006, 2009, 2009, 2006, 2004, 2005, 2006, 2004, 2022, 2007, 2009, 
    2002, 2008, 2011, 2006, 2008, 2004, 1999, 2019, 2014, 2006, 2002, 2004, 
    2005, 2009, 2005, 2004, 2005, 2005, 2021, 2005, 2011, 2007, 2008, 1999, 2013
  )
  ,
  tax = c(
    "25", "30", "20", "15", "20", "30", "15", "9", "4", "25", "5", 
    "25", "20", "10", "10", "2", "25", "40", "20", "50", "20", 
    "15", "20", "15", "10", "15", "25", "10", "20", "25", "20", "13", 
    "10", "15", "27", "25", "40", "10"
  ),
  end_year = c(
    NA, 2015, 2010, NA, NA, NA, 2012, NA, NA, NA, NA, 2009, NA, NA, NA, NA, 
    NA, 2015, NA, 2013, NA, 2017, 2010, NA, NA, 2014, NA, NA, NA, NA, 
    NA, NA, NA, 2018, 2013, NA, NA, NA
  ),
  reinstated_year = c(
    NA, NA, 2023, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA, NA, 2023, NA, NA, 2018, NA, NA, 2015, NA, NA, NA, NA, 
    NA, NA, NA, NA, NA, NA, NA, NA
  )
)

##### Production Year Assignment ####
# Assign production years to films based on availability of filmYear and releaseYear.
state_year_data <- data_US_clean %>%
  left_join(incentive_state_year, by = "filmingState") %>%
  filter(filmingState != "Minnesota") %>% # Eliminate Minnesota oberservations due to inconsistency in tax incentive program
  mutate(
    filmingTreatmentYear = ifelse(!is.na(incentive_year),incentive_year, NA),
    usedYear = case_when(
      !is.na(filmYear) ~ filmYear,
      is.na(filmYear) & (releaseYear < incentive_year) ~ releaseYear,
      is.na(filmYear) & !is.na(end_year) & releaseYear > end_year & is.na(reinstated_year) ~ NA_real_, # Exclude invalid
      is.na(filmYear) & !is.na(end_year) & !is.na(reinstated_year) & releaseYear > end_year & releaseYear < reinstated_year ~ releaseYear,
      TRUE ~ releaseYear
        )) %>%
  filter(!is.na(budget)) %>%
  filter(usedYear < reinstated_year | is.na(reinstated_year)) # Remove filme that can out AFTER a FTI was reinstated

##### Adjust Budgets for Inflation ####
# CPI Data based on https://www.minneapolisfed.org/about-us/monetary-policy/inflation-calculator/consumer-price-index-1913-
cpi_data <- data.frame(
  Year = 1984:2024,
  CPI = c(
    103.9, 107.6, 109.6, 113.6, 118.3, 124.0, 130.7, 136.2, 
    140.3, 144.5, 148.2, 152.4, 156.9, 160.5, 163.0, 166.6, 172.2, 177.1, 
    179.9, 184.0, 188.9, 195.3, 201.6, 207.3, 215.3, 214.5, 218.1, 224.9, 
    229.6, 233.0, 236.7, 237.0, 240.0, 245.1, 251.1, 255.7, 258.8, 271.0, 
    292.7, 304.7, 314.4
  )
)

# Function to adjust monetary values for inflation
adjust_for_inflation <- function(original_value, original_year, reference_year, cpi_data) {
  cpi_original <- cpi_data$CPI[cpi_data$Year == original_year]
  cpi_reference <- cpi_data$CPI[cpi_data$Year == reference_year]
  if (length(cpi_original) == 0 || length(cpi_reference) == 0) {
    stop("Invalid year(s) provided.")
  }
  adjusted_value <- original_value * (cpi_reference / cpi_original)
  return(adjusted_value)
}

# Adjust budgets, domestic box office, and international box office for inflation
state_year_data <- state_year_data %>%
  filter(usedYear >= 1984) %>%
  rowwise() %>%
  mutate(
    adjusted_budget = ifelse(
      !is.na(budget) & !is.na(usedYear),
      adjust_for_inflation(budget, usedYear, 2024, cpi_data),
      NA
    ),
    adjusted_domesticBO = ifelse(
      !is.na(domesticBO) & !is.na(usedYear),
      adjust_for_inflation(domesticBO, usedYear, 2024, cpi_data),
      NA
    ),
    adjusted_internationalBO = ifelse(
      !is.na(internationalBO) & !is.na(usedYear),
      adjust_for_inflation(internationalBO, usedYear, 2024, cpi_data),
      NA
    )
  ) %>%
  ungroup() %>%
  mutate(
    budget = adjusted_budget,
    domesticBO = adjusted_domesticBO,
    internationalBO = adjusted_internationalBO
  ) %>%
  select(-adjusted_budget, -adjusted_domesticBO, -adjusted_internationalBO)

##### Restrict Budget ####
state_year_data <- state_year_data %>%
  mutate(budget_category = case_when(
    budget <= 5000000 ~ "Low-Budget",
    budget <= 30000000 ~ "Mid-Budget",
    TRUE ~ "High-Budget"
  ))

# Summarize the budget category distribution
budget_summary <- state_year_data %>%
  group_by(budget_category) %>%
  summarise(FilmCount = n())

budget_summary
summary(state_year_data$budget)

state_year_data <- state_year_data %>%
  filter(budget >= 250000 & budget <= 532000000)

state_year_data %>%
  group_by(budget_category) %>%
  summarise(FilmCount = n())

summary(state_year_data$budget)

##### Incentive Variable ####
state_year_data <- state_year_data %>%
  mutate(
    FTI_st = ifelse(!is.na(filmingTreatmentYear), 1, 0),
    role = case_when(
      FTI_st == "1" & usedYear >= filmingTreatmentYear ~ "Post-treatment",
      FTI_st == "1" & usedYear < filmingTreatmentYear ~ "Pre-treatment",
      FTI_st == "0" ~ "Control",
      FTI_st == "1" & usedYear >= end_year & usedYear < reinstated_year & !is.na(reinstated_year) ~ "Control",
      TRUE ~ NA_character_
    ),
    Post_t = ifelse(role == "Post-treatment", 1, 0),
    earlyAdopter = ifelse(!is.na(filmingTreatmentYear) & filmingTreatmentYear <= 2005, 1, 0)
  )

expanded_data <- state_year_data %>%
  crossing(analyzedState = ifelse(FTI_st == "1", filmingState, NA)) %>%
  filter(!is.na(analyzedState)) %>%
  filter(analyzedState == filmingState | role == "Control") %>%
  select(26,everything())

##### Assign one primary state and filming date ####
# Count state mentions for each movie
state_counts <- state_year_data %>%
  group_by(tconst, filmingState) %>%
  summarize(
    state_mentions = n(),  # Count number of mentions
    treated_flag = any(FTI_st == "1", na.rm = TRUE),  # Flag if the movie has any treated states
    .groups = "drop"
  )

# Assign primary location
primary_state <- state_counts %>%
  group_by(tconst, filmingState) %>%
  arrange(desc(state_mentions), desc(treated_flag), filmingState) %>% # Prioritize mentions, treated state, then alphabetical
  slice(1) %>%  # Keep the top-ranked state for each movie
  ungroup() %>%
  select(tconst, primaryState = filmingState, primaryTreated = treated_flag)

# Merge back to the original dataset
expanded_data_primary <- expanded_data %>%
  full_join(primary_state, "tconst", relationship = "many-to-many")

# Handle pre-treatment and control overlap
# Identify movies with mentions in both treated and control states
pre_control_overlap <- expanded_data_primary %>%
  group_by(tconst) %>%
  summarize(has_treated = any(FTI_st == "1" & Post_t == "0", na.rm = TRUE),
            has_control = any(role == "Control", na.rm = TRUE)) %>%
  mutate(overlap_flag = has_treated & has_control)  # Flag overlaps where both exist

# Handle overlaps - prioritize "control" state 
cleaned_data <- expanded_data_primary %>%
  full_join(pre_control_overlap, by = "tconst") %>%
  group_by(tconst) %>%
  filter(if_else(overlap_flag, is.na(filmingTreatmentYear), TRUE)) %>%  # Choose control state if overlap
  ungroup()

# Remove duplicates - keep one row per movie (primary state)
incentive_state_year <- incentive_state_year %>%
  rename("analyzedState" = "filmingState")

final_data <- cleaned_data %>%
  filter(filmingState == primaryState) %>%
  distinct(analyzedState,tconst,filmingState,budget,domesticBO,internationalBO,releaseYear,
           usedYear,filmingTreatmentYear,budget_category,FTI_st,Post_t,role,earlyAdopter,
           runtimeMinutes,averageRating,numVotes)%>%
  group_by(tconst) %>% # Group by unique film identifier
  filter(usedYear == min(usedYear, na.rm = TRUE)) %>% # Keep rows with the lowest usedYear
  ungroup() %>% # Remove grouping for the final output 
  left_join(incentive_state_year, by = "analyzedState") %>%
  mutate(
    control_flag = ifelse(role == "Control", 1, 0),
    relativeYear = usedYear - incentive_year,
    Post_t = ifelse(relativeYear >= 0 & role == "Control", 1, Post_t)
  ) 

final_data %>% filter(FTI_st == "0") %>% distinct(tconst)

##### Summary Statistics by Budget Tier ####
# Group the data by budget category and calculate summary statistics.

summary_by_budget <- final_data %>%
  distinct(tconst, budget, filmingState, releaseYear, .keep_all = TRUE) %>%
  group_by(budget_category) %>%
  summarise(
    Count = n_distinct(tconst, na.rm = TRUE),
    Budget_Mean = mean(budget, na.rm = TRUE),
    Budget_SD = sd(budget, na.rm = TRUE),
    DomesticBO_Mean = mean(domesticBO, na.rm = TRUE),
    DomesticBO_SD = sd(domesticBO, na.rm = TRUE),
    InternationalBO_Mean = mean(internationalBO, na.rm = TRUE),
    InternationalBO_SD = sd(internationalBO, na.rm = TRUE),
    Runtime_Mean = mean(runtimeMinutes, na.rm = TRUE),
    Runtime_SD = sd(runtimeMinutes, na.rm = TRUE),
    Votes_Mean = mean(numVotes, na.rm = TRUE),
    Votes_SD = sd(numVotes, na.rm = TRUE),
    IMDbRating_Mean = mean(averageRating, na.rm = TRUE),
    IMDbRating_SD = sd(averageRating, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -budget_category, names_to = c("Variable", "Metric"), names_sep = "_") %>%
  pivot_wider(names_from = c("budget_category", "Metric"), values_from = value)

options(max.print = 1e6)
print(summary_by_budget, n = Inf, width = Inf)

##### Summary Statistics by Treatment Status ####
# Group the data by treatment status and calculate summary statistics.
summary_by_treatment <- final_data %>%
  distinct(tconst, budget, filmingState, releaseYear, .keep_all = TRUE) %>%
  mutate(treatment_status = if_else(FTI_st == "1", "Treated", "Untreated")) %>%
  group_by(treatment_status) %>%
  summarise(
    Count = n_distinct(tconst, na.rm = TRUE),
    Budget_Mean = mean(budget, na.rm = TRUE),
    Budget_SD = sd(budget, na.rm = TRUE),
    DomesticBO_Mean = mean(domesticBO, na.rm = TRUE),
    DomesticBO_SD = sd(domesticBO, na.rm = TRUE),
    InternationalBO_Mean = mean(internationalBO, na.rm = TRUE),
    InternationalBO_SD = sd(internationalBO, na.rm = TRUE),
    Runtime_Mean = mean(runtimeMinutes, na.rm = TRUE),
    Runtime_SD = sd(runtimeMinutes, na.rm = TRUE),
    Votes_Mean = mean(numVotes, na.rm = TRUE),
    Votes_SD = sd(numVotes, na.rm = TRUE),
    IMDbRating_Mean = mean(averageRating, na.rm = TRUE),
    IMDbRating_SD = sd(averageRating, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = -treatment_status, names_to = c("Variable", "Metric"), names_sep = "_") %>%
  pivot_wider(names_from = c("treatment_status", "Metric"), values_from = value)

options(max.print = 1e6)
print(summary_by_treatment, n = Inf, width = Inf)

##### Visualize Data #####
# Create a cleaned summary for each release year
yearly_summary_data <- film %>%
  distinct(tconst, releaseYear, budget, filming_dates, filming_location) %>%
  group_by(releaseYear) %>%
  summarize(
    Total = n(), # Count the total number of films per year
    Budgets = sum(!is.na(budget)), # Count films with non-missing budgets
    Dates = sum(!is.na(filming_dates)), # Count films with non-missing filming dates
    Locations = sum(!is.na(filming_location)), # Count films with non-missing filming locations
    All = sum(!is.na(budget) & !is.na(filming_dates) & !is.na(filming_location)), # Count films with all info
    .groups = "drop" # Drop grouping for a clean summary
  )

yearly_summary_Ours <- final_data %>%
  group_by(releaseYear) %>%
  summarize(
    Cleaned = n()  # Count the number of unique films per year
  )

yearly_summary_data <- yearly_summary_data %>%
  left_join(yearly_summary_Ours, by = "releaseYear")

# Reshape the data into a long format for plotting
yearly_summary_long <- yearly_summary_data %>%
  pivot_longer(cols = c(Total, Locations, Budgets, Dates, All, Cleaned), 
               names_to = "Category", 
               values_to = "Count") %>%
  mutate(
    Category = recode(Category,
                      "Total" = "Total in IMDB",
                      "Locations" = "Locations Only",
                      "Dates" = "Dates Only",
                      "Budgets" = "Budgets Only",
                      "All" = "Full IMDB Information",
                      "Cleaned" = "US-Based Location & Budget")
  )

yearly_summary_long <- yearly_summary_long %>%
  mutate(Category = factor(Category, levels = yearly_summary_long %>%
                             group_by(Category) %>%
                             summarize(TotalCount = sum(Count, na.rm = TRUE)) %>%
                             arrange(desc(TotalCount)) %>%
                             pull(Category)))

# Plot yearly trends (1994-2024) for black-and-white prints with reordered legend
yearlySummary <- ggplot(yearly_summary_long, aes(x = releaseYear, y = Count, linetype = Category, shape = Category)) +
  geom_line(size = 1.2) +  # Lines with varied linetypes for distinction
  geom_point(size = 3) +  # Points to highlight data
  labs(

    title = "Trends in Film Data Availability by Information Type (1989–2023)",
    x = "Year of Release",
    y = "Average Budget (Natural Log Scale)",
    linetype = "State Category",
    fill = "State Category",
  ) + 
  scale_x_continuous(
    breaks = seq(min(average_budget_trends$releaseYear, na.rm = TRUE), 
                 max(average_budget_trends$releaseYear, na.rm = TRUE), by = 5),
    labels = function(x) substr(as.character(x), 3, 4)  # Show only last two digits of the year
  ) +
  scale_fill_manual(values = c("gray80", "gray50")) +  # Black-and-white shades
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    axis.title.x = element_text(size = 14),  # Increase font size for x-axis
    axis.title.y = element_text(size = 14),  # Increase font size for y-axis
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Calculate total counts for treated vs. untreated states over time
average_n_trends <- final_data %>%
  distinct(tconst, releaseYear, .keep_all = TRUE) %>%
  group_by(releaseYear, FTI_st) %>%
  summarize(
    total = n(),  # Count total films for each group
    .groups = "drop"
  ) %>%
  mutate(
    treatedState = ifelse(FTI_st == "1", "Treated States", "Untreated States")  # Relabel for clarity
  )


# Plot trends in avg budget
average_budget_trends <- final_data %>%
  distinct(tconst, releaseYear, .keep_all = TRUE) %>%
  group_by(releaseYear, FTI_st) %>%
  summarize(
    avg_budget = mean(log(budget)),  # Count total films for each group
    se = sd(log(budget[budget > 0]), na.rm = TRUE) / sqrt(n()),  # Calculate SE
    .groups = "drop"
  ) %>%
  mutate(
    treatedState = ifelse(FTI_st == "1", "Treated States", "Untreated States")  # Relabel for clarity
  )

# Plot trends in total counts over time
USTotal <- ggplot(average_n_trends, aes(x = releaseYear, y = total, linetype = treatedState)) +
  geom_line(size = 1) +
  labs(
    title = "Trends in Total Film Counts Over Time",
    x = "Year of Release",
    y = "Total Films",
    linetype = "State Category"
  ) +
  scale_x_continuous(
    breaks = seq(min(average_n_trends$releaseYear, na.rm = TRUE), 
                 max(average_n_trends$releaseYear, na.rm = TRUE), by = 5),
    labels = function(x) substr(as.character(x), 3, 4)  # Show only last two digits of the year
  ) +
  scale_linetype_manual(values = c("solid", "dashed")) +  # Differentiate treated and untreated
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 12)
  )

budget_trends <- ggplot(average_budget_trends, aes(x = releaseYear, y = avg_budget, linetype = treatedState)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = avg_budget - 1.96 * se, ymax = avg_budget + 1.96 * se), alpha = 0.2) +  # Example C
  labs(
    title = "Budget Trends Over Time",
    x = "Year of Release",
    y = "Average Budget (Log Scale)",
    linetype = "State Category"
  ) +
  scale_x_continuous(
    breaks = seq(min(average_n_trends$releaseYear, na.rm = TRUE), 
                 max(average_n_trends$releaseYear, na.rm = TRUE), by = 5),
    labels = function(x) substr(as.character(x), 3, 4)  # Show only last two digits of the year
  ) +
  scale_linetype_manual(values = c("solid", "dashed")) +  # Differentiate treated and untreated
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 12)
  )


USTotal
budget_trends

# Summarize filming activity by state and identify the top 20 states
# Calculate average number of movies per state
average_films_per_state <- final_data %>%
  distinct(tconst, filmingState) %>%
  count(filmingState) %>%
  summarize(Average = mean(n, na.rm = TRUE)) %>%
  pull(Average)

# Summarize filming activity by state and identify the top 20 states
state_distribution <- final_data %>%
  distinct(tconst, budget, filmingState, releaseYear, .keep_all = TRUE) %>%
  count(filmingState, name = "FilmCount") %>%
  arrange(desc(FilmCount)) %>%
  slice(1:20) %>%
  mutate(filmingState = names(state_lookup)[match(filmingState, state_lookup)]) # Replace state names with abbreviations

# Plot the top 20 states by filming activity with average line
top20<- ggplot(state_distribution, aes(x = reorder(filmingState, -FilmCount), y = FilmCount, fill = filmingState)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = average_films_per_state, linetype = "dashed", color = "red", size = 1) +  # Add average line
  labs(
    title = "Top 20 States by Filming Activity",
    x = "State",
    y = "Number of Films"
  ) +
  scale_fill_grey() +  # Black-and-white color scheme
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Angled labels for readability
  )

top20
average_films_per_state

"
Above the average film per state line:
  California
  New York
  Louisiana
  Georgia
  Florida
  Illinois
  Pennsylvania
  New Jersey
  Nevada
  Massacheusetts
  Arizona
"

# Bubble chart for average budget vs. number of films
state_summary <- final_data %>%
  distinct(tconst, budget, releaseYear, .keep_all = TRUE) %>%
  group_by(filmingState) %>%
  reframe(
    FTI_st = unique(FTI_st),  # Ensure unique or consistent values
    Avg_Budget = mean(log(budget), na.rm = TRUE),  # Natural log transformation
    Film_Count = n_distinct(tconst)  # Number of unique films
  ) %>%
  filter(!is.na(filmingState)) %>%
  mutate(
    Is_Incentive_State = ifelse(FTI_st == "1", "Incentive State", "Non-Incentive State"),
    filmingState = names(state_lookup)[match(filmingState, state_lookup)] # Replace state names with abbreviations
  )

print(state_summary %>% select(filmingState, Is_Incentive_State, Avg_Budget, Film_Count)%>% arrange(desc(Film_Count)), n = 53)

# Plot bubble chart with increased text size
bubble <- ggplot(state_summary, aes(x = Avg_Budget, y = Film_Count, size = Film_Count, shape = Is_Incentive_State, label = filmingState)) +
  geom_point(alpha = 0.7, color = "black") +  # Black-and-white points
  geom_smooth(method = "lm", aes(group = 1), color = "black", linetype = "dashed", size = 0.8, se = FALSE) +
  geom_text(vjust = -1.2, hjust = 0.5, size = 4, check_overlap = TRUE) +  # Increase text size for labels
  scale_size_area(max_size = 12) +  # Adjust bubble size scaling
  scale_shape_manual(values = c("Incentive State" = 16, "Non-Incentive State" = 17)) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Log of Average Budget vs. Number of Films",
    x = "Log of Average Budget (USD)",
    y = "Number of Films (Log Scale)",
    size = "Film Count",
    shape = "State Type"
  ) +
  theme_minimal(base_size = 16) +  # Increase base font size
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Larger and bold title
    axis.title.x = element_text(size = 16),  # Larger axis titles
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),  # Larger and angled x-axis text
    axis.text.y = element_text(size = 14),  # Larger y-axis text
    legend.title = element_text(size = 14),  # Larger legend titles
    legend.text = element_text(size = 12),  # Larger legend text
    legend.position = "top"
  )

# Display the plot
print(bubble)

# Remove unnecessary variables and clean memory.
rm(average_budget_trends,average_n_trends, budget_summary, cleaned_data, cpi_data, data, data_US_clean,expanded_data,expanded_data_primary, 
   pre_control_overlap,primary_state, relative_year_trends,state_counts,state_year_data,summary_by_budget, summary_by_treatment,yearly_summary_data,yearly_summary_long,
   yearly_summary_Ours,average_films_per_state,adjust_for_inflation)

gc()
save.image("Final_Data.RData")

#### Methodology & Analysis ####
load("Final_Data.RData")

##### DiD Analysis #####

###### Aggregate and State-Specific Models ######
# Aggregate data to calculate weighted and unweighted averages, and other group-level statistics.
state_year_data <- final_data %>%
  filter(relativeYear >= -5 & relativeYear <= 5) %>%
  group_by(filmingState, incentive_year, relativeYear, Post_t, FTI_st, earlyAdopter) %>%
  summarise(
    avg_budget = mean(log(budget), na.rm = TRUE),
    n_films = n(),
    weighted_avg_budget = sum(log(budget) * n_films, na.rm = TRUE) / sum(n_films, na.rm = TRUE),
    avg_controls = mean(control_flag, na.rm = TRUE),
    .groups = "drop"
  )

# Fit the aggregate model with weights and clustering.
aggregate_model <- feols(
  log(avg_budget) ~ Post_t * FTI_st,
  data = state_year_data,
  weights = state_year_data$n_films,  # Weight by the number of films per group.
  vcov = ~ filmingState               # Cluster by filming state.
)

# Print summary of aggregate model.
summary(aggregate_model)

# Extract aggregate model results for reporting.
aggregate_row <- data.frame(
  State = "Aggregate",
  Effect = coef(aggregate_model)["Post_t:FTI_st"],
  StdError = sqrt(vcov(aggregate_model)["Post_t:FTI_st", "Post_t:FTI_st"]),
  LowerCI = coef(aggregate_model)["Post_t:FTI_st"] - 1.96 * sqrt(vcov(aggregate_model)["Post_t:FTI_st", "Post_t:FTI_st"]),
  UpperCI = coef(aggregate_model)["Post_t:FTI_st"] + 1.96 * sqrt(vcov(aggregate_model)["Post_t:FTI_st", "Post_t:FTI_st"]),
  Significance = ifelse(abs(coef(aggregate_model)["Post_t:FTI_st"] / sqrt(vcov(aggregate_model)["Post_t:FTI_st", "Post_t:FTI_st"])) > 1.96, "Significant", "Not Significant")
)

###### Normalised Parallel Trends #####
combined_trends_data <- final_data %>%
  mutate(FTI_st = ifelse(FTI_st == "1", 1, 0)) %>%
  group_by(FTI_st, relativeYear) %>%
  summarise(
    avg_budget = mean(log(budget), na.rm = TRUE),
    se = sd(log(budget), na.rm = TRUE) / sqrt(sum(!is.na(budget))),
    .groups = "drop"
  )

# Calculate differences and normalize to zero at t=0.
difference_trends_data <- combined_trends_data %>%
  pivot_wider(
    names_from = FTI_st,
    values_from = c(avg_budget, se),
    names_prefix = "FTI_"
  ) %>%
  mutate(
    avg_difference = avg_budget_FTI_1 - avg_budget_FTI_0,
    combined_sd = sqrt(se_FTI_1^2 + se_FTI_0^2),
    avg_difference = avg_difference - avg_difference[relativeYear == 0]
  ) %>%
  filter(relativeYear >= -5 & relativeYear <= 5)

### Plot Parallel Trends ###
parallel_trends_plot <- ggplot(difference_trends_data, aes(x = relativeYear, y = avg_difference)) +
  geom_line(size = 1, color = "black") +
  geom_errorbar(aes(ymin = avg_difference - 1.96 * combined_sd, ymax = avg_difference + 1.96 * combined_sd),
                width = 0.2, color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "blue") +
  labs(
    title = "Normalized Differences in Average Log Budgets",
    x = "Relative Year",
    y = "Normalized Difference in Average Log Budget"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),  # Larger title font
    axis.title.x = element_text(size = 16),  # X-axis label font size
    axis.title.y = element_text(size = 16),  # Y-axis label font size
    axis.text.x = element_text(size = 14),   # X-axis ticks font size
    axis.text.y = element_text(size = 14)    # Y-axis ticks font size
  ) +
  coord_cartesian(ylim = c(-1, 1))  # Adjust vertical limits for clarity

difference_trends_data
print(parallel_trends_plot)

###### State-Specific Models ######
state_specific_results <- list()

for (state in unique(final_data$analyzedState)) {
  state_data <- final_data %>% filter(analyzedState == state, relativeYear >= -5 & relativeYear <= 5)
  
  # Run state-specific model.
  model <- tryCatch({
    feols(log(budget) ~ Post_t * FTI_st, 
          data = state_data, 
          vcov = ~ filmingState)
  }, error = function(e) {
    message("Error in state: ", state, " - ", e$message)
    return(NULL)
  })
  
  if (!is.null(model) && "Post_t:FTI_st" %in% names(coef(model))) {
    state_specific_results[[state]] <- summary(model)
  }
}

# Combine state-specific results.
state_results <- do.call(rbind, lapply(names(state_specific_results), function(state) {
  model <- state_specific_results[[state]]
  if (!is.null(model)) {
    effect <- coef(model)["Post_t:FTI_st"]
    std_error <- model$se["Post_t:FTI_st"]
    data.frame(
      State = state,
      Effect = effect,
      StdError = std_error,
      LowerCI = effect - 1.96 * std_error,
      UpperCI = effect + 1.96 * std_error,
      Significance = ifelse(abs(effect / std_error) > 1.96, "Significant", "Not Significant")
    )
  } else {
    NULL
  }
}))

# Add aggregate results to state-specific results.
state_results <- rbind(state_results, aggregate_row)
state_results

# Merge Incentive Years with State Results
state_results_merged <- state_results %>%
  left_join(incentive_state_year, by = c("State" = "analyzedState")) %>%
  mutate(
    IncentiveLabel = ifelse(!is.na(incentive_year),
                            paste0(State,"(",incentive_year,")"),
                            State) # Combine state name and incentive year
  )

# Plot State-Specific Effects with Incentive Years
state_agg <- ggplot(state_results_merged, aes(x = reorder(IncentiveLabel, Effect), y = Effect)) +
  # Add point estimates
  geom_point(size = 3, aes(shape = Significance, color = Significance)) +
  
  # Add error bars for confidence intervals
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2) +
  
  # Customize shapes and color for significance
  scale_shape_manual(values = c("Significant" = 15, "Not Significant" = 17)) +
  scale_color_manual(values = c("Significant" = "black", "Not Significant" = "gray")) +
  
  # Add labels and themes
  labs(
    title = "State-Specific Effects of FTIs on Production Budgets",
    x = "State (Incentive Year)",
    y = "Effect (Log Scale)",
    shape = "Significance"
  ) +
  scale_y_continuous(labels = scales::comma) +  # Clean Y-axis formatting
  
  # Adjusted Theme
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),  # Larger title font
    axis.title.x = element_text(size = 16),  # X-axis label font size
    axis.title.y = element_text(size = 16),  # Y-axis label font size
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1, vjust = 1),   # Adjusted text angle and alignment
    axis.text.y = element_text(size = 14),   # Y-axis ticks font size
    plot.margin = margin(20, 20, 50, 50)     # Adjust margins: Top, Right, Bottom, Left
  ) +
  coord_cartesian(clip = "off")  # Prevents cutting off any text outside the plot area

# Display the plot
state_results_merged
state_agg

###### Robustness Checks ####

# Define and summarize models for robustness analysis
models <- list(
  Weighted = feols(
    log(avg_budget) ~ Post_t * FTI_st,
    data = state_year_data,
    weights = ~ n_films,
    vcov = ~ filmingState
  ),
  Unweighted = feols(
    log(avg_budget) ~ Post_t * FTI_st,
    data = state_year_data,
    vcov = ~ filmingState
  ),
  Clustered = feols(
    log(avg_budget) ~ Post_t * FTI_st,
    data = state_year_data,
    weights = ~ n_films
  ),
  No_Clustering = feols(
    log(avg_budget) ~ Post_t * FTI_st,
    data = state_year_data
  ),
  Reduced_Data = feols(
    log(budget) ~ Post_t * FTI_st,
    data = final_data %>% filter(!filmingState %in% c("California", "New York")),
    vcov = ~ filmingState
  ),
  Trimmed_Budget = feols(
    log(budget) ~ Post_t * FTI_st,
    data = final_data %>% filter(budget <= 200000000),
    vcov = ~ filmingState
  ),
  Clustered_By_Year = feols(
    log(budget) ~ Post_t * FTI_st,
    data = final_data,
    vcov = ~ releaseYear
  ),
  Clustered_By_Film_Year = feols(
    log(budget) ~ Post_t * FTI_st,
    data = final_data,
    vcov = ~ usedYear
  )
)

# Summarize results into a cohesive table
model_summaries <- bind_rows(
  lapply(names(models), function(model_name) {
    summary_model <- summary(models[[model_name]])
    data.frame(
      Model = model_name,
      Estimate = summary_model$coeftable["Post_t:FTI_st", "Estimate"],
      StdError = summary_model$coeftable["Post_t:FTI_st", "Std. Error"],
      PValue = summary_model$coeftable["Post_t:FTI_st", "Pr(>|t|)"],
      Significant = ifelse(summary_model$coeftable["Post_t:FTI_st", "Pr(>|t|)"] < 0.05, "Yes", "No")
    )
  })
)

### Expanded Relative Year Windows ###
relative_year_ranges <- list(-5:5, -6:6, -7:7, -8:8, -9:9, -10:10)
expanded_results <- list()

for (range in relative_year_ranges) {
  range_data <- final_data %>%
    filter(relativeYear >= min(range) & relativeYear <= max(range)) %>%
    group_by(filmingState, relativeYear, Post_t, FTI_st) %>%
    summarise(
      avg_budget = mean(log(budget), na.rm = TRUE),
      n_films = n(),
      .groups = "drop"
    )
  
  model <- feols(
    log(avg_budget) ~ Post_t * FTI_st,
    data = range_data,
    weights = ~ n_films,
    vcov = ~ filmingState
  )
  
  expanded_results[[paste0("Range_", min(range), "_to_", max(range))]] <- data.frame(
    Model = paste0("Range_", min(range), " to ", max(range)),
    Estimate = coef(model)["Post_t:FTI_st"],
    StdError = sqrt(diag(vcov(model)))["Post_t:FTI_st"],
    PValue = summary(model)$coeftable["Post_t:FTI_st", "Pr(>|t|)"],
    Significant = ifelse(summary(model)$coeftable["Post_t:FTI_st", "Pr(>|t|)"] < 0.05, "Yes", "No")
  )
}

# Combine all summaries
final_summaries <- bind_rows(model_summaries, bind_rows(expanded_results))

# Print final summary table
print(final_summaries)

### Additional Robustness Tests ###

# Placebo Tests
placebo_results <- bind_rows(lapply(-3:-1, function(shift) {
  placebo_data <- final_data %>%
    mutate(
      fake_treatment = incentive_year + shift,
      relativeYear = usedYear - fake_treatment,
      Post_t = ifelse(relativeYear >= 0, 1, 0)
    )
  
  model <- feols(log(budget) ~ Post_t * FTI_st, data = placebo_data, vcov = ~ filmingState)
  data.frame(
    Shift = shift,
    Estimate = coef(model)["Post_t:FTI_st"],
    PValue = summary(model)$coeftable["Post_t:FTI_st", "Pr(>|t|)"]
  )
}))

print(placebo_results)

# Cumulative and Lagged Effects
cumulative_data <- final_data %>%
  mutate(
    Post_t_1 = ifelse(relativeYear == 1, 1, 0),
    Post_t_2 = ifelse(relativeYear == 2, 1, 0),
    Post_t_3 = ifelse(relativeYear == 3, 1, 0),
    Cumulative_Post = ifelse(relativeYear >= 1, 1, 0)
  )

lagged_model <- feols(
  log(budget) ~ Post_t + Post_t_1 + Post_t_2 + Post_t_3,
  data = cumulative_data,
  vcov = ~ filmingState
)

cumulative_model <- feols(
  log(budget) ~ Post_t + Cumulative_Post,
  data = cumulative_data,
  vcov = ~ filmingState
)

# Print summaries of lagged and cumulative models
print(summary(lagged_model))
print(summary(cumulative_model))

gc()
save.image("Analysis.RData")

##### Staggered DiD #####
load("Analysis.RData")

final_data_staggered <- final_data %>%
  mutate(
    log_budget = log(budget),  # Log-transform budget
    state_id = as.numeric(as.factor(filmingState)),  # Numeric state IDs
    gname = if_else(role == "Control", 0, filmingTreatmentYear),  # Cohort assignment
    state_year_weight = n()  # Weight by the number of films in state-year
  )

group_sizes <- final_data_staggered %>%
  group_by(gname) %>%
  summarise(
    n_obs = n(),
    unique_states = n_distinct(state_id),
    .groups = "drop"
  )

###### Remove Small Groups ####
small_group_threshold <- 100

# Identify groups with fewer than the threshold number of observations
small_groups <- group_sizes %>%
  filter(n_obs < small_group_threshold) %>%
  pull(gname)  # Extract only the group names (gname)

# Filter out rows with small groups
final_data_staggered <- final_data_staggered %>%
  filter(!(gname %in% small_groups))

summary(final_data_staggered$log_budget)

if (any(is.na(final_data_staggered$log_budget))) {
  stop("Missing values found in 'log_budget'. Ensure all budgets are positive and non-zero.")
}

###### Regression ####
staggered_results <- att_gt(
  yname = "log_budget",
  tname = "usedYear",
  idname = "state_id",
  gname = "gname",
  data = final_data_staggered,
  control_group = "nevertreated",
  panel = FALSE,
  allow_unbalanced_panel = TRUE,
  clustervars = "state_id",
  weightsname = "state_year_weight"
) %>%
  tidy() %>%
  mutate(
    relative_year = time - group,
    lower_ci = estimate - 1.96 * std.error,
    upper_ci = estimate + 1.96 * std.error,
    Significance = ifelse(abs(estimate / std.error) > 1.96, "Significant", "Not Significant")) %>%
  filter(!is.na(estimate)) # Remove that which is NA, as this could cause noise by increasing the budgets and the observations

###### Dynamic Effect Analysis ####
# Filter the results for relevant time periods (relative years)
dynamic_results <- staggered_results %>%
  mutate(period = case_when(
    relative_year >= 0 & relative_year <= 3 ~ "Short-Term",  # Define short-term period
    relative_year > 3 ~ "Long-Term"  # Define long-term period
  )) %>%
  group_by(period) %>%
  summarise(
    avg_estimate = mean(estimate, na.rm = TRUE),  # Average ATT for the period
    lower_ci = mean(lower_ci, na.rm = TRUE),  # Average lower confidence interval
    upper_ci = mean(upper_ci, na.rm = TRUE),  # Average upper confidence interval
    .groups = "drop"  # Drop grouping for a clean summary
  )

print(dynamic_results)

short_term_percent <- (exp(dynamic_results$avg_estimate[2]) - 1) * 100
short_term_percent
long_term_percent <- (exp(dynamic_results$avg_estimate[2]) - 1) * 100
short_term_percent

# Visualization of Dynamic Effects
ggplot(dynamic_results, aes(x = period, y = avg_estimate)) +
  geom_col(fill = "skyblue", width = 0.6) +  # Create bar plot for average estimates
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "darkblue") +
  labs(
    title = "Dynamic Effects: Short-Term vs Long-Term",
    x = "Period",
    y = "Average ATT (Log Budget)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, hjust = 0.5)
  )


# Short-Term Analysis by Cohort
staggered_results %>%
  filter(relative_year >= 0 & relative_year <= 3) %>%
  group_by(group) %>%  # Group by cohort (adoption year)
  summarise(
    avg_estimate = mean(estimate, na.rm = TRUE),
    lower_ci = mean(lower_ci, na.rm = TRUE),
    upper_ci = mean(upper_ci, na.rm = TRUE),
    .groups = "drop"
  )


# Visualization of Short-Term Effects by Cohort
ggplot(staggered_results %>%
         filter(relative_year >= 0 & relative_year <= 3) %>%
         group_by(group) %>%  # Group by cohort (adoption year)
         summarise(
           avg_estimate = mean(estimate, na.rm = TRUE),
           lower_ci = mean(lower_ci, na.rm = TRUE),
           upper_ci = mean(upper_ci, na.rm = TRUE),
           .groups = "drop"
         ), aes(x = factor(group), y = avg_estimate)) +
  geom_col(fill = "lightgreen", width = 0.6) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2, color = "darkgreen") +
  labs(
    title = "Short-Term Effects by Cohort (t = 0 to t = 3)",
    x = "Cohort (Adoption Year)",
    y = "Average ATT (Log Budget)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, hjust = 0.5)
  )

###### Cohort-Specific Analysis ####
cohort_results <- staggered_results %>%
  group_by(group) %>%
  summarise(
    avg_estimate = mean(estimate, na.rm = TRUE),
    lower_ci = mean(lower_ci, na.rm = TRUE),
    upper_ci = mean(upper_ci, na.rm = TRUE),
    .groups = "drop"
  ) 

ggplot(cohort_results, aes(x = group, y = avg_estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) +
  labs(title = "Cohort-Specific ATT Estimates", x = "Cohort", y = "Average ATT") +
  theme_minimal()

staggered_results_significant <- staggered_results %>% 
  filter(relative_year >= -5 & relative_year <= 5) %>%
  group_by(group) %>%  # Group by cohort (or the grouping variable)
  filter(any(Significance == "Significant")) %>%  # Keep groups with at least one significant result
  ungroup() # Ungroup for further analysis

ATT_Cohorts_sig <- ggplot(staggered_results_significant,
                          aes(x = relative_year, y = estimate)) +
  geom_line(color = "darkblue") +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "lightblue", alpha = 0.2) +
  geom_point(
    data = staggered_results_significant %>% filter(Significance == "Significant"),  # Filter significant points
    aes(x = relative_year, y = estimate),
    color = "red",  # Highlight significant points in red
    size = 2  # Adjust point size as needed
  ) +
  labs(
    title = "ATT Estimates Across Relative Years by Cohort with Significance",
    x = "Relative Year",
    y = "ATT"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, hjust = 0.5)
  ) +
  facet_wrap(~group, ncol = 2, scales = "fixed") +  # Same x and y axes across facets
  scale_y_continuous(limits = c(min(staggered_results_significant$lower_ci, na.rm = TRUE), 
                                max(staggered_results_significant$upper_ci, na.rm = TRUE))) +
  scale_x_continuous(limits = c(min(staggered_results_significant$relative_year, na.rm = TRUE), 
                                max(staggered_results_significant$relative_year, na.rm = TRUE)))


print(staggered_results_significant, n = 36)

ATT_Cohorts_sig

###### Robustness Checks ####
# Placebo Tests
placebo_results_by_group <- staggered_results_significant %>% # Filter placebo test results for each group
  filter(relative_year < 0)

t.test(placebo_results_by_group$estimate, mu = 0)

# Plot: Placebo Test ATT Estimates by Group
ggplot(placebo_results_by_group, aes(x = relative_year, y = estimate_scaled)) +
  geom_line(color = "darkred", size = 1) +
  geom_ribbon(
    aes(ymin = lower_ci_scaled, ymax = upper_ci_scaled),
    fill = "lightpink",
    alpha = 0.2
  ) +
  geom_point(
    data = placebo_results_by_group %>% filter(Significance == "Significant"),
    aes(x = relative_year, y = estimate_scaled),
    color = "red",
    size = 2
  ) +
  labs(
    title = "Placebo Test: Pre-Treatment ATT Estimates by Group",
    x = "Years Relative to Policy Implementation",
    y = "ATT (Log Budget)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, hjust = 0.5)
  ) +
  scale_y_continuous() +
  scale_x_continuous() +
  facet_wrap(~group, scales = "free_y")  # Create a panel for each group

# Weighting Sensitivity
# Equal Weights
# Perform ATT with equal weights
final_data_staggered <- final_data_staggered %>%
  mutate(state_year_weight = 1)  # Equal weights

equal_weight_results <- att_gt(
  yname = "log_budget",
  tname = "usedYear",
  idname = "state_id",
  gname = "gname",
  data = final_data_staggered,
  control_group = "nevertreated",
  panel = FALSE,
  allow_unbalanced_panel = TRUE,
  clustervars = "state_id",
  weightsname = "state_year_weight"
) %>%
  tidy() %>%
  mutate(
    relative_year = time - group,
    lower_ci = estimate - 1.96 * std.error,
    upper_ci = estimate + 1.96 * std.error,
    Significance = ifelse(abs(estimate / std.error) > 1.96, "Significant", "Not Significant")) %>%
  filter(!is.na(estimate)) # Remove that which is NA, as this could cause noise by increasing the budgets and the observations

equal_weight_results %>% 
  filter(relative_year >= -5 & relative_year <= 5) %>%
  group_by(group) %>%  # Group by cohort (or the grouping variable)
  filter(any(Significance == "Significant")) %>%  # Keep groups with at least one significant result
  ungroup() # Ungroup for further analysis

# Pre-treatment Covariate Balance
covariate_balance <- final_data_staggered %>%
  filter(gname == 0) %>%
  group_by(state_id) %>%
  summarise(
    avg_budget = mean(log_budget, na.rm = TRUE),
    avg_films_per_year = n() / n_distinct(usedYear),
    .groups = "drop"
  )

print(covariate_balance)

gc()
save.image("Analysis.RData")

##### Synth DiD #####
load("Analysis.RData")

###### Aggregate ####
synth_data <- final_data %>%
  filter(relativeYear >= -5 & relativeYear <= 5) %>%  
  group_by(filmingState, relativeYear) %>%
  summarise(
    avg_budget = mean(budget, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = relativeYear,
    values_from = avg_budget,
    values_fill = list(avg_budget = NA)
  )

Y <- as.matrix(synth_data %>% select(-filmingState))
Y <- apply(Y, 2, function(col) ifelse(is.na(col), mean(col, na.rm = TRUE), col))

# Define Treatment Parameters. Make sure that no "analyzesStates are counted"
N0 <- sum(!(synth_data$filmingState %in% unique(final_data$analyzedState)))
T0 <- sum(as.integer(colnames(Y)) < 0)

cat("N0 (number of control units):", N0, "\n")
cat("T0 (number of pre-treatment periods):", T0, "\n")

# Run Synthetic DiD
synth_did <- synthdid_estimate(Y, N0 = N0, T0 = T0)
summary(synth_did)

# Plot Synthetic DiD Results
synth_plot <- synthdid_plot(synth_did) +
  labs(
    title = "Aggregate Synth DiD",
    x = "Relative Year",
    y = "ATT Estimate"
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) +
  scale_color_grey() + 
  scale_fill_grey()+
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, hjust = 0.5))

print(synth_plot)

###### Key State Cases ####
# Initialize lists to store results
synth_summaries <- list()
synth_plots <- list()

# Define the states to analyze
target_states <- c("California", "New York")

# Loop through each target state
for (state in target_states) {
  cat("\nRunning Synthetic DiD for:", state, "\n")
  
  # Filter data for the current state and prepare for Synth DiD
  synth_data <- final_data %>%
    filter(relativeYear >= -5 & relativeYear <= 5, analyzedState == state) %>%  # Select current state
    group_by(filmingState, relativeYear) %>%
    summarise(
      avg_budget = mean(budget, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = relativeYear,
      values_from = avg_budget,
      values_fill = list(avg_budget = NA)
    )
  
  # Handle missing values (mean imputation)
  Y <- as.matrix(synth_data %>% select(-filmingState))
  Y <- apply(Y, 2, function(col) ifelse(is.na(col), mean(col, na.rm = TRUE), col))
  
  # Define treatment parameters
  N0 <- sum(!(synth_data$filmingState %in% unique(final_data$analyzedState)))
  T0 <- sum(as.integer(colnames(Y)) < 0)
  
  cat("N0 (number of control units):", N0, "\n")
  cat("T0 (number of pre-treatment periods):", T0, "\n")
  
  # Run Synthetic DiD
  synth_did <- synthdid_estimate(Y, N0 = N0, T0 = T0)
  
  # Store summary dynamically
  synth_summaries[[state]] <- summary(synth_did)
  
  # Plot the results and store dynamically
  synth_plots[[state]] <- synthdid_plot(synth_did) +
    labs(
      title = paste("Synthetic DiD Results for", state),
      x = "Relative Year",
      y = "ATT Estimate"
    ) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    scale_color_grey() + 
    scale_fill_grey()
  
  # Display the plot
  print(synth_plots[[state]])
}


# Example to access saved results
cat("Summary for California:\n")
print(synth_summaries[["California"]])

cat("Summary for New York:\n")
print(synth_summaries[["New York"]])

###### Robustness Check ####
# Donor Pool Adjustments
restricted_donor_pool <- final_data %>%
  filter(relativeYear >= -5 & relativeYear <= 5,  # Standard window
         !filmingState %in% c("California", "New York"))  # Exclude dominant states

# Re-run Synthetic DiD with restricted donor pool
synth_data_restricted <- restricted_donor_pool %>%
  group_by(filmingState, relativeYear) %>%
  summarise(avg_budget = mean(budget, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = relativeYear,
    values_from = avg_budget,
    values_fill = list(avg_budget = NA)
  )

Y_restricted <- as.matrix(synth_data_restricted %>% select(-filmingState))
Y_restricted <- apply(Y_restricted, 2, function(col) ifelse(is.na(col), mean(col, na.rm = TRUE), col))

synth_did_restricted <- synthdid_estimate(Y_restricted, N0 = N0, T0 = T0)
synth_did_restricted


# Pre-Treatment MSPE Evaluation
pre_treatment_mspe <- apply(Y[, 1:T0], 1, function(row) mean((row - mean(row))^2, na.rm = TRUE))
cat("Mean MSPE for pre-treatment periods:", mean(pre_treatment_mspe), "\n")

# Placebo Tests
placebo_years <- -3:-1  # Example pre-treatment placebo years

# Initialize an empty list to store results
placebo_results <- list()

# Loop through each placebo year shift
for (shift in placebo_years) {
  # Shift the treatment year and prepare placebo data
  placebo_data <- final_data %>%
    mutate(relativeYear = relativeYear + shift) %>%  # Shift treatment year
    filter(relativeYear >= -5 & relativeYear <= 5) %>%  # Restrict to relevant window
    group_by(filmingState, relativeYear) %>%
    summarise(avg_budget = mean(budget, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      names_from = relativeYear,
      values_from = avg_budget,
      values_fill = list(avg_budget = NA)
    )
  
  # Prepare matrix for synthetic DID
  Y_placebo <- as.matrix(placebo_data %>% select(-filmingState))
  Y_placebo <- apply(Y_placebo, 2, function(col) ifelse(is.na(col), mean(col, na.rm = TRUE), col))
  
  # Run synthetic DID and capture the output
  synth_did_placebo <- synthdid_estimate(Y_placebo, N0 = N0, T0 = T0)
  synth_did_summary <- summary(synth_did_placebo)
  
  # Extract ATT estimate and standard error
  att <- synth_did_summary$estimate
  se <- synth_did_summary$se[1, 1]  # Standard error (1st row, 1st column of $se matrix)
  
  # Store the results
  placebo_results[[as.character(shift)]] <- list(
    Shift = shift,
    Estimate = att,
    SE = se
  )
}

# Combine results into a data frame for easier handling
placebo_results_df <- do.call(rbind, lapply(placebo_results, function(x) {
  data.frame(Shift = x$Shift, Estimate = x$Estimate, SE = x$SE)
}))

# Print the placebo results
print(placebo_results_df)

# Visualize the Placebo Results
ggplot(placebo_results_df, aes(x = Shift, y = Estimate)) +
  geom_point(size = 3, color = "blue") +
  geom_errorbar(aes(ymin = Estimate - SE, ymax = Estimate + SE), width = 0.2, color = "blue") +
  labs(
    title = "Placebo Test Results",
    x = "Placebo Year Shift",
    y = "Estimated ATT"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, hjust = 0.5)
  )

gc()
save.image("Analysis.RData")