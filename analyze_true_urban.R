library(tidyverse)

# 1. Load Data
message("Loading national tract data...")

# Population (B01003)
# Col 1: GEOID, Col 2: Total Population
pop <- read_delim("True_Urban_Ranking/raw_census/B01003_all.dat", 
                  delim = "|", col_names = FALSE, show_col_types = FALSE) %>%
  select(GEOID = X1, Population = X2)

# Housing Units (B25001)
# Col 1: GEOID, Col 2: Total Housing Units
housing <- read_delim("True_Urban_Ranking/raw_census/B25001_all.dat", 
                     delim = "|", col_names = FALSE, show_col_types = FALSE) %>%
  select(GEOID = X1, Housing_Units = X2)

# Land Area (Tract Gazetteer)
# Note: GEOID in gazetteer is 11 digits (no prefix)
gazetteer <- read_tsv("True_Urban_Ranking/data/2022_Gaz_tracts_national.txt", 
                     show_col_types = FALSE) %>%
  select(GEOID, Land_Area_SqMi = ALAND_SQMI) %>%
  mutate(GEOID = paste0("1400000US", GEOID))

# Combine
df <- pop %>%
  inner_join(housing, by = "GEOID") %>%
  inner_join(gazetteer, by = "GEOID") %>%
  mutate(across(c(Population, Housing_Units, Land_Area_SqMi), as.numeric)) %>%
  filter(Land_Area_SqMi > 0) %>%
  mutate(
    Pop_Density = Population / Land_Area_SqMi,
    Housing_Density = Housing_Units / Land_Area_SqMi
  )

# 2. Extract County/State for Grouping
# We'll use the County as a proxy for the "City Region"
df <- df %>%
  mutate(
    County_FIPS = str_sub(GEOID, 10, 14),
    State_FIPS = str_sub(GEOID, 10, 11)
  )

# 3. Define the "True Urban" Thresholds
# Threshold A: 5,000 ppsm (General Urban)
# Threshold B: 10,000 ppsm (Dense Urban / Real City)
df_urban <- df %>%
  mutate(
    is_urban_5k = Pop_Density >= 5000,
    is_urban_10k = Pop_Density >= 10000
  )

# 4. Map Counties to Major Cities (Manual map for the "Problem" cities + Top 20)
# This allows us to attribute the "True Urban" population to a City Name
county_to_city <- tribble(
  ~County_FIPS, ~City_Name,
  "18097", "Indianapolis",
  "39049", "Columbus",
  "21111", "Louisville",
  "39061", "Cincinnati",
  "17031", "Chicago",
  "36061", "New York (Manhattan)",
  "36047", "New York (Brooklyn)",
  "36081", "New York (Queens)",
  "36005", "New York (Bronx)",
  "36085", "New York (Staten Island)",
  "06075", "San Francisco",
  "06037", "Los Angeles",
  "25025", "Boston",
  "42101", "Philadelphia",
  "11001", "Washington, DC",
  "48201", "Houston",
  "48113", "Dallas",
  "04013", "Phoenix",
  "13121", "Atlanta",
  "53033", "Seattle",
  "26163", "Detroit",
  "12086", "Miami",
  "27053", "Minneapolis",
  "06081", "San Jose"
)

df_mapped <- df_urban %>%
  left_join(county_to_city, by = "County_FIPS") %>%
  filter(!is.na(City_Name))

# 5. Summarize populations
results <- df_mapped %>%
  group_by(City_Name) %>%
  summarise(
    Official_County_Pop = sum(Population),
    Urban_Pop_5k = sum(Population * is_urban_5k),
    Urban_Pop_10k = sum(Population * is_urban_10k),
    Urban_Land_10k = sum(Land_Area_SqMi * is_urban_10k),
    .groups = "drop"
  ) %>%
  mutate(
    Density_10k = Urban_Pop_10k / Urban_Land_10k,
    Urbanicity_Index = Urban_Pop_10k / Official_County_Pop
  ) %>%
  arrange(desc(Urban_Pop_10k))

# 6. Re-ranking Analysis
# Compare "Official" (Full County) vs "True Urban" (10k+)
results_ranked <- results %>%
  mutate(
    Official_Rank = min_rank(desc(Official_County_Pop)),
    True_Urban_Rank = min_rank(desc(Urban_Pop_10k)),
    Rank_Delta = Official_Rank - True_Urban_Rank
  )

write_csv(results_ranked, "True_Urban_Ranking/urban_reranking_results.csv")
print(results_ranked %>% select(City_Name, Urban_Pop_10k, True_Urban_Rank, Rank_Delta, Urbanicity_Index))
