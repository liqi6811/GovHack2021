#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinythemes)
library(leaflet)
library(magrittr)
source("utils/utils_map.R")
source("utils/utils_price.R")

ui <- fluidPage(
    useShinyjs(),
    titlePanel("Discover your pricing"),
    sidebarLayout(
      sidebarPanel(
        style = "background-color: #f4f4f4; border-radius: 2px;",
        helpText("Your household summary."),
        textOutput("water_company"),
        textOutput("sa1"),
        textOutput("sa2"),
        tags$h5(textOutput("pricing"), style="color: #222")
      ),
      mainPanel(
        column(
          width=6,
          div(
              id = "form",
              titlePanel("Water Costs"),
              # Add select input named "sex" to choose between "M" and "F"
              textInput("Address", "Residential Address?"),
              #selectInput('sex', 'Select Sex', choices = c("F", "M")),
              selectInput('Housing', 'Accomodation Type', choices = c("House","Townhouse", "Unit","Apartment")),
              selectInput('Pool', 'Swimming Pool Exclusive to the property?', choices = c("No","Yes")),
              selectInput('Rainwater_tank', 'Rainwater tank Exclusive to the property?', choices = c("No","Yes")),
              selectInput('Occupants', 'Usual number of occupants?', choices = c("1","2","3","4","5", "6", "7", "8 or more","0/vacant")),
              selectInput('Concession_eligibility', 'Eligible for discount? (For low incomes. Refer to services.dffh.vic.gov.au/water to check eligibility', choices = c("Yes","No")),
              textInput('Consumption', 'Based on your last bill, what was your total consumption?'),
          ),
          actionButton('resetAll', 'reset'),
        ),
        column(
          width=6,
          leafletOutput("mymap"),
        )
      )
    ),
    #textOutput("Summary2")
    # (IE on the property grounds and not part of a common area if the property is a unit or apartment'
    # Add slider input named "year" to select year between 1900 and 2010
    #sliderInput('year', 'Select Year', min = 1900, max = 2010, value = 1900)
    # CODE BELOW: Add table output named "table_top_10_names"
    theme=shinytheme("paper")
)
server = function(input, output){
    # Function to create a data frame of top 10 names by sex and year
    #top_10_names <- function(){
    #top_10_names <- babynames %>%
    #filter(sex == input$sex) %>%
    #filter(year == input$year) %>%
    #top_n(10, prop)
    point <- reactive({
      if (is.null(input$Address))
        list(lat = NA, lon = NA)
      else
        address_to_coords(input$Address)
    })
    point <- debounce(point, 500)
    sa1 <- reactive({coords_to_sa1(point()["lat"], point()["lon"])})

    output$water_company <- renderText({paste(
      "Water Authority: ",
      if (is.null(input$Address))
        "please enter your address"
      else
        #"Fixing"
        get_company(sa1()$SA1_MAIN16)
    )})
    output$sa2 <- renderText({paste(
      "SA2 Name: ",
      if (is.null(input$Address))
        "No residence"
      else
        sa1()$SA2_NAME16
    )})
    output$sa1 <- renderText({paste(
      "SA1 Code: ",
      if (is.null(input$Address))
        "No residence"
      else
        sa1()$SA1_MAIN16
    )})
    output$pricing <- renderText({paste(
      "Bill: $",
      round(runif(1, 800, 5000),2),
      "p/a"
    )})

    lon <- -37.8136
    lat <- 144.9631
    zoom_level <- reactive({
      if (is.null(input$Address))
        8
      else
        13
    })

    points <- reactive({
      if (is.null(input$Address))
        cbind(
          c(144.9631),
          c(-37.8136)
        )
      else
        cbind(
          c(point()["lon"]),
          c(point()["lat"])
      )
    })

    # this is initial render, observer will update in memory reactively
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Esri.WorldImagery,
          options = providerTileOptions(noWrap = TRUE)
        ) %>%
        addMarkers(data = points()) %>%
        setView(lng = lon, lat = lat, zoom = 8)
    })

    observe({
      input$Address
      isolate({
        zoom_lvl <- 8
        if (length(input$Address) > 1) zoom_lvl <- 13
        leafletProxy("mymap") %>%
          setView(lng = point()["lon"], lat = point()["lat"], zoom = zoom_lvl)
      })
    })

    observeEvent(input$resetAll, {reset("form")})
  }
# CODE BELOW: Render a table output named "table_top_10_names"
shinyApp(ui = ui, server = server)