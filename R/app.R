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
ui <- fluidPage(
    useShinyjs(),
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
    ),
    actionButton('submit_button1', 'Submit'),
    actionButton('resetAll', 'reset'),
    htmlOutput("Summary"),
    #textOutput("Summary2")
    # (IE on the property grounds and not part of a common area if the property is a unit or apartment'
    # Add slider input named "year" to select year between 1900 and 2010
    #sliderInput('year', 'Select Year', min = 1900, max = 2010, value = 1900)
    # CODE BELOW: Add table output named "table_top_10_names"
)
server = function(input, output){
    # Function to create a data frame of top 10 names by sex and year
    #top_10_names <- function(){
    #top_10_names <- babynames %>%
    #filter(sex == input$sex) %>%
    #filter(year == input$year) %>%
    #top_n(10, prop)
    text_reactive <- eventReactive(input$submit_button1, {HTML(paste("<b/> Report Summary </b>",
                                                                     paste("Address:  ", input$Address),
                                                                     paste("Housing type:  ", input$Housing),
                                                                     paste("Pool:  ", input$Pool),
                                                                     paste("Rainwater tank:  ", input$Rainwater_tank),
                                                                     paste("Occupants:  ", input$Occupants),
                                                                     paste('Concession eligibility:  ', input$Concession_eligibility),
                                                                     sep ="<br/>"))})
    output$Summary <- renderUI({text_reactive()})
    observeEvent(input$resetAll, {reset("form")})}
# CODE BELOW: Render a table output named "table_top_10_names"
shinyApp(ui = ui, server = server)