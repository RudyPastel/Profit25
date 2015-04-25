require(rCharts)
require(shiny)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Profit 25"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(width = 2,
      numericInput("mise", label = h3("Mise initiale"), value = 4000),
      numericInput("benefice_initial", label = h3("Bénéfice initial"), value = 0),
      numericInput("cout_part", label = h3("Coût d'une part"), value = 25),
      numericInput("salaire_part", label = h3("Salaire d'une part"), value = 2.5),
      numericInput("duree_cycle", label = h3("Durée d'un cycle"), value = 14),
      sliderInput("nbr_cycle",
                  "Nombre de cycles joués",
                  min = 1,
                  max = 10,
                  value = 5),
      uiOutput("lastInvestmentWeek")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(width = 10,
      tabsetPanel(
        tabPanel("Argent",
                 fluidRow(column(width = 6,showOutput("revenu", "highcharts")),
                          column(width = 6,showOutput("portemonnaie", "highcharts"))),
                 fluidRow(column(width = 6,showOutput("retrait", "highcharts")),
                          column(width = 6,showOutput("benefice", "highcharts")))
                 ),
        tabPanel("Situation initiale",
                 uiOutput("age_part_initial")
        ),
        tabPanel("Nombre de parts d'un âge donné en début de semaine", dataTableOutput("PartTable"))
      )
    )
  )
))
