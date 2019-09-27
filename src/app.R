#setwd(dir = "./Bureau/arules_occurences/")
source("src/fonctions_occurences.R")
data("Groceries")
conf = 0.3
sup = 0.01
input_labels_utilisateurs = c("whole milk","pork","shoe","root vegetables","tropical fruit","shoe")
mesure="mean_confidence"
grocery_rules <- apriori(Groceries, parameter = list(support = sup, confidence = conf))

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
                   c("Oui" = T,
                     "Non" = F),selected = T),
      radioButtons("rhs", "Apparition en conséquence:",
                     "Non" = F),selected = F),
      radioButtons("lhs_exclusif", "Antécédent exclusif:",
                   c("Oui" = T,
                     "Non" = F),selected = F),
      radioButtons("rhs_exclusif", "Conséquence exclusive:",
                   c("Oui" = T,
                     "Non" = F),selected = F),
      radioButtons("mesure", "Mesure à mettre en évidence:",
                   c("Support" = "mean_support",
                     "Confiance" = "mean_confidence",
                     "Lift" = "mean_lift"),selected = "mean_lift")
    ),
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Visualisation graphique",plotOutput(outputId = "arules")),
                  tabPanel("Visualisation tabulaire",tableOutput(outputId = "table_arules"))
                  # tabPanel("Evolution ca et nb de membres" # vue tabulaire
      )
  )
  )

  # Main panel for displaying outputs ----
  

server <- function(input, output) {
  output$arules = renderPlot({
    df=affiche_occurences(grocery_rules,input_labels_utilisateurs,input$lhs,input$rhs,input$lhs_exclusif,input$rhs_exclusif,input$mesure) %>% 
      mutate_if(is.numeric, round, 5)
    genere_plot(df,input$lhs,input$rhs,input$lhs_exclusif,input$rhs_exclusif,conf,sup,10,input$mesure)
  })
  output$table_arules=renderTable({
    df=affiche_occurences(grocery_rules,input_labels_utilisateurs,input$lhs,input$rhs,input$lhs_exclusif,input$rhs_exclusif,input$mesure)
  })
}



shinyApp(ui = ui, server = server)

  