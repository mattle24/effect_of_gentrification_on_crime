munge_data_for_stan <- function(type = "all", plot = FALSE, lm = FALSE) {
  # format incident data
  data <- la_neighborhood_incident_count %>% 
    bind_rows(philly_neighborhood_incident_count) %>% 
    bind_rows(pitt_neighborhood_incident_count) %>% 
    bind_rows(sf_neighborhood_incident_count) %>% 
    filter(type == !!type) %>% 
    group_by(city, neighborhood) %>% 
    mutate(count_next = lead(count, order_by = incident_year)) %>%
    filter(incident_year == 2012 | incident_year == 2017) %>%
    ungroup() %>% 
    inner_join(neighborhood_gentrification, by = c("city" = "city",
                                                   "neighborhood" = "neighborhood",
                                                   "incident_year" = "year")) %>% 
    group_by(city, neighborhood) %>% 
    mutate(
      rate = 1000 * (count + 1) / total_pop # rate x 1k
      ,rate_next = 1000 * (count_next + 1) / total_pop
      ,pct_change = (count_next - count) / count
      ,division = factor(ifelse(city == "Pittsburgh" | city == "Philadelphia", "north_atlantic", "pacific")
                         ,levels = c("pacific", "north_atlantic"))
    ) %>% 
    ungroup()
  
  # prep for modeling
  data <- data %>%  
    filter(gentrifiable_prior) %>% 
    mutate_at(vars(city, division, incident_year, gentrifiable, gentrifying), as.factor) %>% 
    mutate_at(vars(rate, rate_next), log) %>%
    rename_at(vars(rate, rate_next), ~paste0(., "_log")) %>%
    filter(complete.cases(.)) %>% 
    identity()
  
  X <- model.matrix(rate_next_log ~ rate_log + division  * incident_year + gentrifying + division:gentrifying, data)
  pos <- grep("gentrifying", colnames(X))
  
  if (plot) {
    ggplot(data, aes(x = rate_next_log)) +
      geom_density(aes(fill = city, group = city), alpha = 0.4)
    
    ggplot(data, aes(x = rate_log)) +
      geom_density(aes(fill = city, group = city), alpha = 0.4)
  }
  
  if (lm) {
    lm <- lm(rate_next_log ~ rate_log + city + division  * incident_year + gentrifying + division:gentrifying, data)
    summary(lm)
  }
  
  stan_dat <- list(
    N = nrow(X)
    ,y = data$pct_change
    ,K = ncol(X)
    ,m = length(pos)
    ,pos = pos
    ,X = X
  )
  
  return(stan_dat)
}
