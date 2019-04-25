#' Calculate Gentrification Calculate gentrification from Freeman (2003):
#'
#' 1.	Have a median income less than the median for that metropolitan area at
#' the beginning of the intercensal period.
#' 
#' 2.	Have a proportion of housing built within the past 20 years lower than the
#' proportion found at the median for the respective metropolitan area.
#' 
#' 3.	Have a percentage increase in educational attainment greater than the
#' median increase in educational attainment for that metropolitan area.
#' 
#' 4.	Have an increase in real housing prices during the intercensal period.
#'
#' @param tracts_neighborhoods_2015_sf The sf objects created to match tracts
#'   and neighborhoods.
#' @param tracts_neighborhoods_2010_sf The sf objects created to match tracts
#'   and neighborhoods.
#' @param tracts_neighborhoods_2000_sf The sf objects created to match tracts
#'   and neighborhoods.
#' @param tracts_2015 A dataframe with tract data for 2015
#' @param tracts_2010 A dataframe with tract data for 2010
#' @param tracts_2000 A dataframe with tract data for 2000
#' @param msa_2015 A dataframe with MSA data for 2015
#' @param msa_2010 A dataframe with MSA data for 2010
#' @param msa_2000 A dataframe with MSA data for 2000
#' @param city_name character. A name to use to find the city for filtering the
#'   MSA dfs
#'
#' @return A dataframe with one row per neighborhood per year and columns for
#'   gentrifiable and gentrifying.
calculate_gentrification <- function(tracts_neighborhoods_2015_sf,
                                     tracts_neighborhoods_2010_sf,
                                     tracts_neighborhoods_2000_sf,
                                     tracts_2015,
                                     tracts_2010,
                                     tracts_2000,
                                     msa_2015,
                                     msa_2010,
                                     msa_2000,
                                     city_name
                                     ) {
  # Make adjustments for inflation for house value
  # https://www.bls.gov/data/inflation_calculator.htm
  adj_2010 <- 1.0785
  
  neighborhoods_final_2015 <- tracts_neighborhoods_2015_sf %>% 
    inner_join(tracts_2015, by = "gisjoin") %>% 
    group_by(neighborhood, year) %>% 
    summarise(
      prop_college_degree = mean(prop_college_degree * total_pop * wt, na.rm = TRUE) / sum(total_pop * wt)
      ,prop_housing_built_last_20 = mean(prop_housing_built_last_20 * total_pop * wt, na.rm = TRUE) / sum(total_pop * wt)
      ,median_rent = mean(median_rent * total_pop * wt, na.rm = TRUE) / sum(total_pop * wt)
      ,median_family_income = mean(median_family_income * total_pop * wt, na.rm = TRUE) / sum(total_pop * wt)
    ) %>% 
    ungroup()
  
  neighborhoods_final_2010 <- tracts_neighborhoods_2010_sf %>% 
    inner_join(tracts_2010, by = "gisjoin") %>% 
    group_by(neighborhood, year) %>% 
    summarise(
      prop_college_degree = mean(prop_college_degree * total_pop * wt, na.rm = TRUE) / sum(total_pop * wt)
      ,prop_housing_built_last_20 = mean(prop_housing_built_last_20 * total_pop * wt, na.rm = TRUE) / sum(total_pop * wt)
      ,median_rent = adj_2010 * mean(median_rent * total_pop * wt, na.rm = TRUE) / sum(total_pop * wt)
      ,median_family_income = mean(median_family_income * total_pop * wt, na.rm = TRUE) / sum(total_pop * wt)
    ) %>% 
    ungroup()
  
  neighborhoods_final_2000 <- tracts_neighborhoods_2000_sf %>% 
    inner_join(tracts_2000, by = "gisjoin") %>% 
    group_by(neighborhood, year) %>% 
    summarise(
      prop_college_degree = mean(prop_college_degree * total_pop * wt, na.rm = TRUE) / sum(total_pop * wt)
      ,prop_housing_built_last_20 = mean(prop_housing_built_last_20 * total_pop * wt, na.rm = TRUE) / sum(total_pop * wt)
      ,median_rent = mean(median_rent * total_pop * wt, na.rm = TRUE) / sum(total_pop * wt)
      ,median_family_income = mean(median_family_income * total_pop * wt, na.rm = TRUE) / sum(total_pop * wt)
    ) %>% 
    ungroup()
  
  neighborhoods_final <- neighborhoods_final_2015 %>% 
    rbind(neighborhoods_final_2010) %>% 
    rbind(neighborhoods_final_2000) %>% 
    group_by(neighborhood) %>% 
    mutate(prop_college_degree_prior = lag(prop_college_degree, order_by = year)
           ,median_rent_prior = lag(median_rent, order_by = year)
    )
  
  neighborhoods_final$geometry <- NULL

  msa_stats_2015 <- msa_2015 %>% 
    filter(grepl(city_name, name)) %>% 
    select(-(1:2))
  
  msa_stats_2010 <- msa_2010 %>% 
    filter(grepl(city_name, name)) %>% 
    select(-(1:2))
  
  msa_stats_2000 <- msa_2000 %>% 
    filter(grepl(city_name, name)) %>% 
    select(-(1:2))

  msa_stats <- msa_stats_2015 %>% 
    bind_rows(msa_stats_2010) %>% 
    bind_rows(msa_stats_2000 %>% mutate(year = as.character(year))) %>% 
    mutate(prop_college_degree_prior = lag(prop_college_degree, order_by = year))
  
  neighborhoods_final %>% 
    inner_join(msa_stats, by = "year", suffix = c("", "_msa")) %>% 
    group_by(neighborhood, year) %>% 
    mutate(
      percent_change_college_degree = (prop_college_degree - prop_college_degree_prior) / prop_college_degree_prior
      ,percent_change_college_degree_msa = (prop_college_degree_msa - prop_college_degree_prior_msa) / prop_college_degree_prior_msa
      ,gentrifiable = median_family_income < median_family_income_msa & 
          prop_housing_built_last_20 < prop_housing_built_last_20_msa
      ,gentrifying = case_when(
        year == "2000" ~ NA
        ,is.na(gentrifiable) ~ NA
        ,!gentrifiable ~ FALSE
        ,percent_change_college_degree > percent_change_college_degree_msa & median_rent > median_rent_prior ~ TRUE
        ,!(percent_change_college_degree > percent_change_college_degree_msa & median_rent > median_rent_prior) ~ FALSE
        ,TRUE ~ NA
      )
    ) %>%
    ungroup() %>% 
    # select(neighborhood, year, gentrifiable, gentrifying) %>% 
    identity()
}
