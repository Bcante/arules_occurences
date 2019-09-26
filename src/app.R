
library(shiny)
library(plyr)
library(dplyr)
library(scales)
library(ggplot2)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # App title ----
  titlePanel("Démo"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      radioButtons("lhs", "Apparition en antécédent:",
                   c("Oui" = "T",
                     "Non" = "F"),selected = "F"),
      radioButtons("rhs", "Apparition en conséquence:",
                   c("Oui" = "T",
                     "Non" = "F"),selected = "F"),
      radioButtons("lhs_exclusif", "Antécédent exclusif:",
                   c("Oui" = "T",
                     "Non" = "F"),selected = "F"),
      radioButtons("rhs_exclusif", "Conséquence exclusive:",
                   c("Oui" = "T",
                     "Non" = "F"),selected = "F"),
      radioButtons("rhs", "Mesure à mettre en évidence:",
                   c("Support" = "mean_support",
                     "Confiance" = "mean_confidence",
                     "Lift" = "mean_lift"),selected = "F")
    ),
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Visualisation graphique",plotOutput(outputId = "arules")),
                  tabPanel("Visualisation tabulaire",plotOutput(outputId = "table_arules"))
                  ) #Vue barchart"
                  # tabPanel("Evolution ca et nb de membres" # vue tabulaire
      )
      
    )
  )
  # Main panel for displaying outputs ----
  

server <- function(input, output) {
  output$arules = renderPlot({
    
  })
}


shinyApp(ui = ui, server = server)

