library(shiny)
library(acs)
library(choroplethr)

source("get_demographics_hack.R")

shinyServer(function(input, output) {

  output$stateMap = renderPlot({
    year                         = as.numeric(input$year)
    df_state_demographics        = get_state_demographics_2(year, 5)
    df_state_demographics$value  = df_state_demographics[, input$demographic]
    state_choropleth(df_state_demographics, num_colors = input$num_colors)
  })
  
  output$countyMap = renderPlot({
    year                          = as.numeric(input$year)
    df_county_demographics        = get_county_demographics_2(year, 5)
    df_county_demographics$value  = df_county_demographics[, input$demographic]
    county_choropleth(df_county_demographics, num_colors = input$num_colors)
  })
  

})
