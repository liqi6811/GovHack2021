library(shiny)

ui <- fluidPage(
  titlePanel("Water Costs"),
  textInput("Address", "Residential Address?"),
  selectInput(
    "Housing",
    "Accomodation Type",
    choices = c("House", "Townhouse", "Unit", "Apartment")
  ),
  selectInput(
    "Pool",
    "Swimming Pool Exclusive to the property?",
    choices = c("No", "Yes")
  ),
  selectInput(
    "Rainwater tank",
    "Rainwater tank Exclusive to the property?",
    choices = c("No", "Yes")
  ),
  selectInput(
    "Occupants",
    "Usual number of occupants?",
    choices = c("1", "2", "3", "4", "5", "6", "7", "8 or more", "0/vacant")
  ),
  selectInput(
    "Concession_eligibility",
    "Eligible for discount? (For low incomes. Refer to services.dffh.vic.gov.au/water to check eligibility",
    choices = c("Yes", "No")
  ),
  textOutput("Summary"),
  # Add something like (IE on the property grounds and not part of a common area if the property is a unit or apartment'
)

server <- function(input, output, session) {
  output$Summary <- renderText({
    paste("Report Summary")
  })
}

shinyApp(ui = ui, server = server)