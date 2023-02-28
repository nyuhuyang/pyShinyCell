library(shiny)

ui <- pageWithSidebar(
    headerPanel("renderImage example"),
    sidebarPanel(
        sliderInput("obs", "Number of observations:",
                    min = 0, max = 1000,  value = 500)
    ),
    mainPanel(
        # Use imageOutput to place the image on the page
        imageOutput("myImage")
    )
)

server <- function(input, output, session) {
    #mpl <- reticulate::import("matplotlib")
    #sc <- reticulate::import("scanpy")

    output$myImage <- renderImage({
        # A temp file to save the output.
        # This file will be removed later by renderImage
        #outfile <- tempfile(fileext = '.png')

        # Generate the PNG
        #mpl$pyplot$plot(c(1, 2, 3, 4,5), c(1, 4, 9, 16,32))
        #mpl$pyplot$savefig("tempData/plt.png")
        #mpl$pyplot$show()
        # Return a list containing the filename
        return(list(src = "tempData/plt.png",
             contentType = 'image/png',
             width = 400,
             height = 300,
             alt = "This is alternate text"))
    }, deleteFile = FALSE)
}

shinyApp(ui, server)
