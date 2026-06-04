library(tidyverse)

# 1. Load Data
message("Loading national tract data...")

pop <- read_delim("True_Urban_Ranking/raw_census/B01003_top50.dat", 
                  delim = "|", col_names = FALSE, show_col_types = FALSE) %>%
  select(GEOID = X1, Population = X2)

housing <- read_delim("True_Urban_Ranking/raw_census/B25001_top50.dat", 
                     delim = "|", col_names = FALSE, show_col_types = FALSE) %>%
  select(GEOID = X1, Housing_Units = X2)

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

# 2. Assign Regions (Expanded Mapping)
df <- df %>%
  mutate(
    County_FIPS = str_sub(GEOID, 10, 14),
    Region = case_when(
      str_detect(GEOID, "US(39061|21117|21037|21015)") ~ "Cincinnati",
      str_detect(GEOID, "US(53033|53053|53061)") ~ "Seattle",
      str_detect(GEOID, "US(04013|04021)") ~ "Phoenix",
      str_detect(GEOID, "US(06075|06001|06013|06081|06041)") ~ "San Francisco",
      str_detect(GEOID, "US(26163|26125|26099)") ~ "Detroit",
      str_detect(GEOID, "US(12057|12103)") ~ "Tampa",
      str_detect(GEOID, "US(37183|37063)") ~ "Durham/Raleigh",
      str_detect(GEOID, "US(09001|09009)") ~ "Bridgeport/New Haven",
      str_detect(GEOID, "US(06065|06071)") ~ "Riverside",
      str_detect(GEOID, "US(17031|17043|17089|17097|17197)") ~ "Chicago",
      str_detect(GEOID, "US(36061|36047|36081|36005|36085|34013|34017|34031)") ~ "New York",
      str_detect(GEOID, "US(06037|06059)") ~ "Los Angeles",
      str_detect(GEOID, "US(25025|25017|25021|25009)") ~ "Boston",
      str_detect(GEOID, "US(42101)") ~ "Philadelphia",
      str_detect(GEOID, "US(11001|51013|51059|24031|24033)") ~ "Washington, DC",
      str_detect(GEOID, "US(48201|48157)") ~ "Houston",
      str_detect(GEOID, "US(48113|48085|48121|48439)") ~ "Dallas",
      str_detect(GEOID, "US(13121|13089|13067|13135)") ~ "Atlanta",
      str_detect(GEOID, "US(12086|12011|12099)") ~ "Miami",
      str_detect(GEOID, "US(27053|27037)") ~ "Minneapolis",
      str_detect(GEOID, "US(18097)") ~ "Indianapolis",
      str_detect(GEOID, "US(39049)") ~ "Columbus",
      str_detect(GEOID, "US(21111)") ~ "Louisville",
      str_detect(GEOID, "US(26081)") ~ "Grand Rapids",
      str_detect(GEOID, "US(29189|29510)") ~ "St. Louis",
      str_detect(GEOID, "US(47037)") ~ "Nashville",
      str_detect(GEOID, "US(39035)") ~ "Cleveland",
      str_detect(GEOID, "US(31055)") ~ "Omaha",
      str_detect(GEOID, "US(32003)") ~ "Las Vegas",
      str_detect(GEOID, "US(41051)") ~ "Portland",
      str_detect(GEOID, "US(55079)") ~ "Milwaukee",
      TRUE ~ "Other"
    )
  ) %>%
  filter(Region != "Other")

# 3. Apply 5k Cutoff
df_urban <- df %>%
  mutate(
    is_urban_5k = Pop_Density >= 5000
  )

# 4. Aggregate
results <- df_urban %>%
  group_by(Region) %>%
  summarise(
    Official_Region_Pop = sum(Population),
    True_Urban_Pop_5k = sum(Population * is_urban_5k),
    True_Urban_Land_5k = sum(Land_Area_SqMi * is_urban_5k),
    .groups = "drop"
  ) %>%
  mutate(
    Urbanicity_Index = True_Urban_Pop_5k / Official_Region_Pop
  ) %>%
  arrange(desc(True_Urban_Pop_5k)) %>%
  mutate(
    True_Urban_Rank = row_number(),
    Official_Rank = min_rank(desc(Official_Region_Pop)),
    Rank_Delta = Official_Rank - True_Urban_Rank
  )

write_csv(results, "True_Urban_Ranking/urban_reranking_5k_results.csv")
message("Analysis complete for 5k threshold.")
print(results %>% select(Region, True_Urban_Pop_5k, True_Urban_Rank, Rank_Delta, Urbanicity_Index))
