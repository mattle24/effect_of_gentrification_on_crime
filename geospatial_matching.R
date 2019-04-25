#' Get Tracts
#' 
#' Get the tracts for a neighborhood
#' @param neighborhood_sf An `sf` object for a neigborhood
#' @param tracts_sf An `sf` object for census tracts that might
#' be in the neighborhood
#' @return An tidy `sf` for tracts that intersect the neighborhood
get_tracts <- function(neighborhood_sf, tracts_sf) {
  tracts_sf$a1 <- st_area(tracts_sf)
  tracts_intersect <- st_intersection(neighborhood_sf, tracts_sf)
  tracts_intersect$a2 <- st_area(tracts_intersect)
  tracts_intersect$wt <- as.numeric(tracts_intersect$a2 / tracts_intersect$a1)
  tracts_intersect %>%
    filter(wt > 0.01) %>% 
    select(gisjoin, neighborhood, wt, geometry)
  
}

#' Tract to Neighborhood
#' 
#' Match tracts to neighborhoods in a city
#' @param neighborhoods_sf The `sf` object with neighborhoods
#' @param state_tract_sf The `sf` object with tract data for the correct state
#' @return A tidy `sf` object with tract and their corresponding neighborhoods
tract_to_neighborhood <- function(neighborhoods_sf, state_tracts_sf) {
  intersects_matrix <- st_intersects(neighborhoods_sf, state_tracts_sf, sparse = FALSE)
  # one row for each neighborhood, one col for each tract
  
  res <- NULL # init
  for (n in 1:nrow(intersects_matrix)) {
    tracts_in_neighborhood <- get_tracts(neighborhoods_sf[n, ], state_tracts_sf[as.vector(intersects_matrix[n, ]), ])
    if (is.null(res)) {
      res <- tracts_in_neighborhood
    } else {
      res <- res %>% 
        rbind(tracts_in_neighborhood)
    }
  }
  return(res)
}
