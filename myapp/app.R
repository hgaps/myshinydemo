library(shiny)
library(dplyr)

# Initialize the data
set.seed(123) # For reproducibility
academics <- data.frame(
  name = paste("Academic", 1:50),
  weight = rep(1, 50),
  allocations = rep(0, 50),
  unavailable_rounds = rep(0, 50)
)

# Function to select a pair of academics
select_pair <- function(academics, exclude = NULL) {
  if (!is.null(exclude)) {
    academics <- academics[!academics$name %in% exclude, ]
  }
  selected <- sample_n(academics, 2, weight = academics$weight)
  return(selected)
}

# Function to update the list after selection
update_academics <- function(academics, selected, unavailable = NULL) {
  if (!is.null(unavailable)) {
    academics$weight[academics$name %in% unavailable$name] <- academics$weight[academics$name %in% unavailable$name] * 5
    academics$unavailable_rounds[academics$name %in% unavailable$name] <- academics$unavailable_rounds[academics$name %in% unavailable$name] + 1
    
    # Replace unavailable academics
    replacements <- academics[!academics$name %in% c(selected$name, unavailable$name), ]
    replacements <- sample_n(replacements, nrow(unavailable), weight = replacements$weight)
    for (i in 1:nrow(unavailable)) {
      selected[selected$name == unavailable$name[i], ] <- replacements[i, ]
    }
  }
  
  academics$allocations[academics$name %in% selected$name] <- academics$allocations[academics$name %in% selected$name] + 1
  academics$weight[academics$name %in% selected$name] <- academics$weight[academics$name %in% selected$name] * 0.5
  
  academics <- bind_rows(academics[!academics$name %in% selected$name, ], academics[academics$name %in% selected$name, ])
  return(list(academics = academics, selected = selected))
}

# Define UI for application
ui <- fluidPage(
  titlePanel("Examiner Allocation System"),
  sidebarLayout(
    sidebarPanel(
      actionButton("request_pair", "Request Pair of Examiners"),
      textInput("unavailable_1", "Unavailable Examiner 1 (if any)", ""),
      textInput("unavailable_2", "Unavailable Examiner 2 (if any)", ""),
      actionButton("update_pair", "Update Pair")
    ),
    mainPanel(
      textOutput("selected_pair"),
      textOutput("final_pair")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  selected_pair <- reactiveVal(NULL)
  academics_data <- reactiveVal(academics)
  
  observeEvent(input$request_pair, {
    selected <- select_pair(academics_data())
    selected_pair(selected)
    output$selected_pair <- renderText({
      paste("Initially selected pair: ", paste(selected$name, collapse = " and "))
    })
    output$final_pair <- renderText("")
  })
  
  observeEvent(input$update_pair, {
    if (is.null(selected_pair())) return(NULL)
    
    unavailable_1 <- input$unavailable_1
    unavailable_2 <- input$unavailable_2
    unavailable <- academics_data() %>% filter(name %in% c(unavailable_1, unavailable_2))
    
    if (nrow(unavailable) > 0) {
      result <- update_academics(academics_data(), selected_pair(), unavailable)
      academics_data(result$academics)
      selected_pair(result$selected)
      output$final_pair <- renderText({
        paste("Updated pair: ", paste(result$selected$name, collapse = " and "))
      })
    } else {
      academics_data(update_academics(academics_data(), selected_pair(), NULL)$academics)
      output$final_pair <- renderText({
        paste("Final pair: ", paste(selected_pair()$name, collapse = " and "))
      })
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
