# True Urban Ranking: Redefining Density via the 10k Threshold

## Overview
This project challenges traditional US city population rankings, which are often skewed by arbitrary political boundaries. It introduces the **True Urban Core (TUC)** metric: the contiguous population within Census Tracts exceeding 10,000 people per square mile (ppsm).

## Key Data Science Skills
*   **Geospatial Aggregation:** Summing population data based on density thresholds rather than municipal borders.
*   **Policy Analysis:** Evaluating how 20th-century consolidations (e.g., Indianapolis, Louisville) mask true urban intensity.
*   **Data Visualization:** Mapping "True Urban Density" using leaflet and sf in R.

## Tech Stack
*   **R (sf, leaflet, tidyverse):** Geospatial analysis and mapping.
*   **Quarto:** Interactive research documentation.
*   **US Census API:** Leveraging ACS 5-Year Estimates.

## Data Sources
*   **US Census Bureau:** [2022 ACS 5-Year Estimates via Tidycensus](https://data.census.gov/)

## Key Insight
When applying a consistent 10k ppsm threshold, "mega-cities" like Phoenix drop significantly in ranking, while physically constrained hubs like Boston and San Francisco emerge as the nation's true urban heavyweights.
