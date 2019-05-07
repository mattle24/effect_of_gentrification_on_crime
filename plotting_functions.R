plot_neighborhoods_gentrification <- function(neighborhoods_sf, neighborhood_gentrification, city_name) {
  pal <- c("gray31", "gray65", "gray99")
  plot_sf <- neighborhoods_sf %>% 
    left_join(neighborhood_gentrification, by = "neighborhood") %>% 
    group_by(neighborhood) %>% 
    mutate(
      gentrifiable_prior = lag(gentrifiable, order = year)
      ,gentrifying = case_when(
        !gentrifying ~ FALSE
        ,is.na(gentrifying) ~ NA
        ,gentrifiable_prior & gentrifying ~ TRUE
        ,!gentrifiable_prior & gentrifying ~ FALSE
        ,TRUE ~ NA
      )
    ) %>% 
    ungroup() %>% 
    filter(year != "2000") %>% 
    mutate(
      gentrifying = case_when(
        gentrifying ~ "Gentrifying"
        ,!gentrifiable ~ "Gentrified/ Wealthy"
        ,gentrifiable & !gentrifying ~ "Gentrifiable"
        ,TRUE ~ NA_character_
      ) %>% 
        factor(levels = c("Gentrifiable", "Gentrifying", "Gentrified/ Wealthy"))
    )
  
    ggplot(plot_sf) +
      geom_sf(aes(fill = gentrifying), color = "black", stroke = 1) +
      facet_grid(~year) +
      scale_fill_manual(name = "Classification",
                        values = c("Gentrifying" = pal[2],
                                   "Gentrified/ Wealthy" = pal[3],
                                   "Gentrifiable" = pal[1]), na.value = "black") +
      labs(
        title = glue("Gentrification in {city_name} Neighborhoods")
      ) +
      coord_sf(datum = NULL) +
      theme(panel.grid.major = element_line(colour = "white")
            ,axis.text = element_blank()
            ,axis.ticks = element_blank())
}
