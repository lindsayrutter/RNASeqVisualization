server <- function(input, output) {
}

ui <- fluidPage(
  bs_carousel(id = "tabPrev", use_indicators = TRUE) %>%
    bs_append(
      content = bs_carousel_image(src = "knitr/birdSnow.jpg"),
      caption = bs_carousel_caption("Snow bird")
    ) %>%
    bs_append(
      content = bs_carousel_image(src = "knitr/GreenLakeHouse.jpeg"),
      caption = bs_carousel_caption("Green lake house")
    ) %>%
    bs_append(
      content = bs_carousel_image(src = "knitr/igloo.jpg"),
      caption = bs_carousel_caption("Glowing igloo")
    ) %>%
    bs_append(
      content = bs_carousel_image(src = "knitr/purpleLake.jpg"),
      caption = bs_carousel_caption("Purple lake")
    )
)

shinyApp(ui = ui, server = server)
