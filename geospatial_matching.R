#' Get Sub-Geometries
#' 
#' Get the sub-geometries for a neighborhood
#' @param neighborhood_sf An `sf` object for a neigborhood
#' @param sub_geometry_sf An `sf` object for sub-geometries (blocks or tracts)
#'   that might be in the neighborhood.
#' @return An tidy `sf` for tracts that intersect the neighborhood
get_sub_geometries <- function(neighborhood_sf, sub_geometry_sf) {
  sub_geometry_sf$a1 <- st_area(sub_geometry_sf)
  intersect_sf <- st_intersection(neighborhood_sf, sub_geometry_sf)
  intersect_sf$a2 <- st_area(intersect_sf)
  intersect_sf$wt <- as.numeric(intersect_sf$a2 / intersect_sf$a1)
  intersect_sf %>%
    filter(wt > 0.01) %>% 
    select(gisjoin, neighborhood, wt, geometry)
  
}

#' Sub-geometry to Neighborhood
#' 
#' Match sub-geometries to neighborhoods in a city
#' @param neighborhoods_sf The `sf` object with neighborhoods
#' @param sub_geometry_sf The `sf` object with sub-geometry (block or tract)
#'   data
#' @return A tidy `sf` object with sub-geometries and their corresponding neighborhoods
sub_geometry_to_neighborhood <- function(neighborhoods_sf, state_tracts_sf) {
  intersects_matrix <- st_intersects(neighborhoods_sf, state_tracts_sf, sparse = FALSE)
  # one row for each neighborhood, one col for each tract
  
  res <- NULL # init
  for (n in 1:nrow(intersects_matrix)) {
    sub_geometries_in_neighborhood <- get_sub_geometries(neighborhoods_sf[n, ],
                                                         state_tracts_sf[as.vector(intersects_matrix[n, ]), ])
    if (is.null(res)) {
      res <- sub_geometries_in_neighborhood
    } else {
      res <- res %>% 
        rbind(sub_geometries_in_neighborhood)
    }
  }
  return(res)
}

#' Point to Neighborhood
#' 
#' Match points to neighborhoods in a city
#' @param neighborhoods_sf The `sf` object with neighborhoods
#' @param points_sf The `sf` object with points
#' @return A tidy neighborhoods_sf with one row per neighborhood per year with
#'   columns for number of points in neighborhood per year
incidents_in_neighborhood <- function(neighborhoods_sf, points_sf) {
  
  years <- unique(points_sf$incident_year)
  
  for (y in years) {
    # one row for each neighborhood, one col for each point
    covers_matrix <- st_contains(neighborhoods_sf, points_sf %>% filter(incident_year == y), sparse = FALSE)
    neighborhoods_sf[ ,as.character(y)] <- rowSums(covers_matrix)
  }
  
  return(neighborhoods_sf)
}
