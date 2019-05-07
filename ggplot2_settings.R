theme_set(
  theme_bw() +
  theme(
    text = element_text(family = "serif")
    ,plot.title = element_text(hjust = 0.5)
    ,plot.subtitle = element_text(hjust = 0.5)
    ,panel.grid = element_blank()
    ,panel.border = element_rect(fill = NULL, color = "black", size = 1)
  )
)
