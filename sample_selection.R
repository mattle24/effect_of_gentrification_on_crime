### Finding Cases ###
# Purpose: use random stratified sampling to choose Census divisions to sample,
# and then find the most populous cities (2015 ACS) in each division that have
# crime data

library(tidycensus)
library(dplyr)
library(tidyr)
library(janitor)
library(tibble)

set.seed(20190418)
# divisions <- 
regions <- 1:4
sample(regions, 3)# 1, 4, 3 = Northeast, West, and South

# Northeast sampling
sample(c(1,2), 1) # 2 = Middle Atlantic
middle_atlantic <- c("New Jersey", "New York", "Pennsylvania")

# West sampling
sample(c(8, 9), 1) # 8 = Mountain
mountain <- c("Arizona", "Colorado", "Idaho", "New Mexico", "Montana", "Utah",
              "Nevada", "Wyoming")

# South Sampling
sample(c(5, 6, 7), 1) # 5 = South Atlantic
south_atlantic <- c("Delaware", "District of Columbia", "Florida", "Georgia",
                    "Maryland", "North Carolina", "South Carolina", "Virginia",
                    "West Virginia")
# resample
sample(c(6, 7), 1) # 6 = East South Central
east_south_central <- c("Alabama", "Kentucky", "Mississippi", "Tennessee")

cities <- tidycensus::get_acs("place", "B01003_001", year = 2015)

cities <- cities %>% 
  clean_names() %>% 
  extract(name, regex = "((?<=,).*)", into = "state_name", remove = FALSE, perl = TRUE) %>% 
  mutate(state_name = trimws(state_name))


# Middle Atlantic ---------------------------------------------------------

middle_atlantic_cities <- cities %>% 
  semi_join(tibble(state_name = middle_atlantic), by = "state_name")

top_n(middle_atlantic_cities, 5, wt = estimate) %>% 
  arrange(desc(estimate))


# West --------------------------------------------------------------------

mountain_cities <- cities %>% 
  semi_join(tibble(state_name = mountain), by = "state_name")

top_n(mountain_cities, 10, wt = estimate) %>% 
  arrange(desc(estimate))

pacific <- c("Alaska", "California", "Hawaii", "Oregon", "Washington")
pacific_cities <- cities %>% 
  semi_join(tibble(state_name = pacific), by = "state_name")

top_n(pacific_cities, 5, wt = estimate) %>% 
  arrange(desc(estimate))


# South ------------------------------------------------------------------

south_atlantic_cities <- cities %>% 
  semi_join(tibble(state_name = south_atlantic), by = "state_name")

top_n(south_atlantic_cities, 10, wt = estimate) %>% 
  arrange(desc(estimate))

east_south_central_cities <- cities %>% 
  semi_join(tibble(state_name = east_south_central), by = "state_name")

top_n(east_south_central_cities, 10, wt = estimate) %>% 
  arrange(desc(estimate))
