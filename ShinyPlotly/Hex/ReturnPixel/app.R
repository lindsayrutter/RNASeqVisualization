# Note: The argument click only work for R base graphics (see the graphics package). It does not work for grid-based graphics, such as ggplot2, lattice, and so on.

library(png)

ui <- fluidPage(
  # Some custom CSS for a smaller font for preformatted text
  # tags$head(
  #   tags$style(HTML("pre, table.table {font-size: smaller;}"))
  #   ),
    imageOutput("image1", height = 350, click = "image_click"),
   verbatimTextOutput("click_info")
)

server <- function(input, output, session) {
  
  set.seed(1)
  dat <- data.frame(ID = paste0("ID",1:100), A = runif(100), B = runif(100))
  dat$ID <- as.character(dat$ID)
  data <- dat
  p <- qplot(x=A, y=B, data=dat)
  
  # Generate an image with black lines every 10 pixels
  output$image1 <- renderImage({
    # Get width and height of image output
    width  <- session$clientData$output_image1_width
    height <- session$clientData$output_image1_height
    npixels <- width * height
    
    # Fill the pixels for R, G, B
    m <- matrix(1, nrow = height, ncol = width)
    # Add gray vertical and horizontal lines every 10 pixels
    m[seq_len(ceiling(height/10)) * 10 - 9, ] <- 0.75
    m[, seq_len(ceiling(width/10)) * 10 - 9]  <- 0.75
    
    # Convert the vector to an array with 3 planes
    img <- array(c(m, m, m), dim = c(height, width, 3))
    
    # Write it to a temporary file
    outfile <- tempfile(fileext = ".png")
    writePNG(img, target = outfile)
    
    # Return a list containing information about the image
    list(
      src = outfile,
      contentType = "image/png",
      width = width,
      height = height,
      alt = "This is alternate text"
    )
  })
  
  output$click_info <- renderPrint({
    cat("input$image_click:\n")
    str(input$image_click)
  })

  
}


shinyApp(ui, server)