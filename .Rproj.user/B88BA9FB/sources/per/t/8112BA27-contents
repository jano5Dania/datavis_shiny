#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

pacman::p_load(shiny, tidyverse, ggplot2, gapminder, ggthemes, readxl, ggrepel, plotly, ggnewscale)

options(scipen = 100)

passat <- read_excel("data/passat.xlsx")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  title = "Datavisualisering",

    # Application title
    titlePanel("EA Dania 2025"),


# Sidepanel ---------------------------------------------------------------

      # Sidebar
    sidebarLayout(
        sidebarPanel(
          

# Logo ----------

          div(img(height = 100, width = 250, src = "true.png"),
              style = "text-align: center;"),
br(),


# Sliders ----------

          sliderInput("year",
                      "Vælg årgang",
                      min = min(passat$year, na.rm = TRUE),
                      max = max(passat$year, na.rm = TRUE),
                      value = 2014,
                      sep = ""),

          sliderInput("motor_size",
                      "Vælg motorstørrelse",
                      min = min(passat$motor_size, na.rm = TRUE),
                      max = max(passat$motor_size, na.rm = TRUE),
                      value = 1.5),

          helpText("Punkter der opfylder valgte krav vil blive markeret med en lilla cirkel.",
                   br(),
                   "Markeringer gælder fra valgte værdi og op.")
          
        ),


# Main panel --------------------------------------------------------------

        mainPanel(
          tabsetPanel(tabPanel("Plot 1",
           plotOutput("passatPlot")))
        )
    )
)

# Server ------------------------------------------------------------------

server <- function(input, output) {

    output$passatPlot <- renderPlot({
      
      filt_passat <- passat |> filter(motor_size >= input$motor_size & year >= input$year)
      
      plot <- ggplot(passat, aes(x = km_per_liter, y = price, color = as.factor(motor_size)))
      
      plot +
        geom_point() +
        scale_color_colorblind() +
        facet_wrap(vars(dealer_type), ncol = 1) +
        geom_point(data = filt_passat,
                   shape = 1,
                   size = 4,
                   color = "purple") +
        labs(x = "Kilometer per liter", y = "Pris", title = "Pris vs. motorstørrelse", color = "Motorstørrelse")
    })
}

# Running -----------------------------------------------------------------

shinyApp(ui = ui, server = server)
