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
#' @param type What kind of points to get. "all", "violent", "disorderly", or
#'   custom regex
#' @return A tidy `tibble` with one row per neighborhood per year with
#'   columns for number of points of certain type in neighborhood per year
incidents_in_neighborhood <- function(neighborhoods_sf, points_sf, type = "all") {
  disorderly_crimes_regex = "g"
  violent_crimes_regex <- "(assault)|(arson)|(homicide)|(robbery)|(rape)|(battery)|(abuse)|(shots fired)|(sex offenses, forcible)"
  
  # TODO: handle different types
  res <- tibble::tibble()

  covers_matrix <- sf::st_contains(neighborhoods_sf, points_sf, sparse = FALSE)
  points_sf$geometry <- NULL
  
  neighborhoods_vector <- neighborhoods_sf$neighborhood
  for (i in 1:length(neighborhoods_vector)) {
    # all 
    res <- res %>% 
      dplyr::bind_rows(
        points_sf[covers_matrix[i, ], ]  %>% # all points in ith neighborhood 
          dplyr::group_by(incident_year) %>% 
          dplyr::count() %>%
          dplyr::mutate(type = "all", neighborhood =  neighborhoods_vector[i])
      )
    
    # violent 
    res <- res %>% 
      dplyr::bind_rows(
        points_sf[covers_matrix[i, ], ]  %>% # all points in ith neighborhood 
          dplyr::filter(grepl(violent_crimes_regex, description, ignore.case = TRUE)) %>% 
          dplyr::group_by(incident_year) %>% 
          dplyr::count() %>% 
          dplyr::mutate(type = "violent", neighborhood =  neighborhoods_vector[i])
      )
    
    # disorderly 
    res <- res %>% 
      dplyr::bind_rows(
        points_sf[covers_matrix[i, ], ]  %>% # all points in ith neighborhood 
          dplyr::filter(grepl(disorderly_crimes_regex, description, ignore.case = TRUE)) %>% 
          dplyr::group_by(incident_year) %>% 
          dplyr::count() %>% 
          dplyr::mutate(type = "disorderly", neighborhood =  neighborhoods_vector[i])
      )
  }
    
  res %>% 
    dplyr::rename(count = n)
}
