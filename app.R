library(shiny)
library(tidyverse)

## Include only data sets that have at least one numeric column


## Base plot
base_plot <- ggplot( ) + 
  theme_classic() +
  labs( x = "Observation Index") +
  theme( axis.title.x = element_text( size = 16), 
         axis.text.x = element_text( size = 14),
         axis.title.y = element_text( size = 16),
         axis.text.y = element_text( size = 14),
         plot.title = element_text( size = 20) )

ui <- fluidPage(
  
  headerPanel( title = "Mean"),
  
  tags$p("Some other text"),
  
  sidebarPanel( 
    width = 3,
    
    selectInput( "datasets", 
                 "Select a dataset", 
                 choices = data( package = "datasets")$results[,"Item"],
                 selected = "iris", selectize = T, width = "80%"),
    
    
    
    uiOutput("numeric_vars"),
    
    uiOutput("select_value"),
    
    checkboxInput( "show_errors", "Residuals")
    
    ),
  
  mainPanel( 
    
    fluidRow(
      column(
        width = 8,
        plotOutput("res_plot", height = "300px")
      )
      
    )
    
    
  )
  

)

server <- function(input, output, session) {
  
  data <- reactive({
    get( input$datasets )
  })
  
  output$numeric_vars <- renderUI({
    
    req( input$datasets )
    
    valid_vars <- get( input$datasets ) %>% 
                  select( where( is.numeric )) %>% 
                  names()
    
    selectInput( "variable",
                 "Select a variable",
                 choices = valid_vars,
                 selected = valid_vars[1],
                 selectize = T, width = "80%"
    )
    
  })
  
  output$select_value <- renderUI({
    
    req( input$variable )
    
    var <- data()[,input$variable]
    
  sliderInput("select", "Change values", min = min(var), max = max(var), value = mean(var) )
    
  })
  
  
  output$res_plot <- renderPlot({
    
    req( input$select )
    
    var <- data()[,input$variable]
    
    ob_mean   <- input$select
    ob_index  <- 1:length( var) 
    ob_error  <-  var - ob_mean
    ob_sse    <- sum( ob_error ^ 2 )
  
    
    res_plot <- base_plot +
                geom_segment( aes(x = ob_index, xend = ob_index, y = ob_mean, yend = ob_mean + ob_error), 
                              color = ifelse(input$show_errors, "red", "white" ) ) +
                geom_point(data = data(), aes(x = ob_index, y = var), size = 2) +
                geom_hline( yintercept = ob_mean, col = "red") +
                labs( title = input$variable,
                      y = input$variable )
    
    res_plot
    
  })
  
  
}

shinyApp(ui, server)