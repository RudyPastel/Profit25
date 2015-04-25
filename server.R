require(shiny)
require(rCharts)
source("sousou.R")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Last week when the money is reinvested
  output$lastInvestmentWeek <- renderUI({
    sliderInput("lastInvestmentWeek",
                "Dernière semaine d'investissement",
                min = 1,
                max = input$duree_cycle*input$nbr_cycle,
                value = 15) 
  })
  # The input is used to run the simulation
  info  <- reactive({
    sousou(mise = input$mise,
           cout_part = input$cout_part,
           nbr_cycle = input$nbr_cycle,
           salaire_part = input$salaire_part,
           derniereSemaineDeReinvestissement = input$lastInvestmentWeek,
           benefice_initial = input$benefice_initial,
           age_part_initial = age_part_initial())
  })
  # Plot le revenu en début de semaine
  output$revenu <- renderChart(expr = {
    Argent = info()$Argent
    chart = Highcharts$new()
    chart$chart(type = "line")
    chart$title(text = "Revenu en début de semaine")
    chart$xAxis(type = "linear")
    chart$yAxis(title=list(text = "Salaire des parts en jeu en euro"),min = 0)
    chart$series(name = "Revenu",data = toJSONArray2(obj = Argent[,c("semaine","revenu")],
                                                     json = FALSE,
                                                     names = FALSE))
    chart$tooltip(headerFormat = '<span style="font-size: 10px">Début de la semaine {point.key}</span><br/>',
                  valueSuffix = '€')
    chart$set(dom = "revenu")
    return(chart)
  })
  
  # Plot le portemonnaie en début de semaine
  output$portemonnaie <- renderChart(expr = {
    Argent = info()$Argent
    chart = Highcharts$new()
    chart$chart(type = "line")
    chart$title(text = "Reste après avoir acheté des parts")
    chart$xAxis(type = "linear")
    chart$yAxis(title=list(text = "Reste en euro"),min = 0)
    chart$series(name = "Reste",data = toJSONArray2(obj = Argent[,c("semaine","portemonnaie")],
                                                     json = FALSE,
                                                     names = FALSE))
    chart$tooltip(headerFormat = '<span style="font-size: 10px">Début de la semaine {point.key}</span><br/>',
                  valueSuffix = '€')
    chart$set(dom = "portemonnaie")
    return(chart)
  })
  
  # Plot les sommes retirées du jeu en début de semaine
  output$retrait <- renderChart(expr = {
    Argent = info()$Argent
    chart = Highcharts$new()
    chart$chart(type = "line")
    chart$title(text = "Somme retirée du jeu en début de semaine")
    chart$xAxis(type = "linear")
    chart$yAxis(title=list(text = "Somme retirée en euro"),min = 0)
    chart$series(name = "Retrait",data = toJSONArray2(obj = Argent[,c("semaine","retrait")],
                                                     json = FALSE,
                                                     names = FALSE))
    chart$tooltip(headerFormat = '<span style="font-size: 10px">Début de la semaine {point.key}</span><br/>',
                  valueSuffix = '€')
    chart$set(dom = "retrait")
    return(chart)
  })
  
  # Plot le bénéfice en début de semaine
  output$benefice <- renderChart(expr = {
    Argent = info()$Argent
    chart = Highcharts$new()
    chart$chart(type = "line")
    chart$title(text = "Bénéfice en début de semaine")
    chart$xAxis(type = "linear")
    chart$yAxis(title=list(text = "Bénéfice en euro"))
    chart$series(name = "Bénéfice",data = toJSONArray2(obj = Argent[,c("semaine","benefice")],
                                                     json = FALSE,
                                                     names = FALSE))
    chart$tooltip(headerFormat = '<span style="font-size: 10px">Début de la semaine {point.key}</span><br/>',
                  valueSuffix = '€')
    chart$set(dom = "benefice")
    return(chart)
  })
  
  # The table of the age of all parts
  output$PartTable <- renderDataTable(expr = {
    info()$Part
  })
  
  # Number of parts of a given age at the beginning of the game
  output[["age_part_initial"]] <- renderUI({
    info = lapply(seq(input$duree_cycle-1), function(i) {
      numericInput(inputId = paste0("age_",i),
                   label = h3(paste("Nombre de parts d'âge",i)),
                   value = 0)
    })
    info = split(info,seq(0,input$duree_cycle-2) %/% ceiling((input$duree_cycle-1)/3))
    fluidRow(lapply(info,column,width=4))
  })
  age_part_initial <- reactive({
    info = c()
    for(i in seq(input$duree_cycle-1)){info = c(info,input[[paste0("age_",i)]])}
    return(info)
  })
})

