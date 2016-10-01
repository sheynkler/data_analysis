csvFileInput <- function(id = "csv_input") {
  # Create a namespace function using the provided id
  ns <- NS("csv_input")
  
  tagList(
    uiOutput("myFileUI"),
    tags$div(id = "enter",
          
    tags$hr(),
    
    #actionButton("clear", "Clear"),
    checkboxInput('header', 'Header', TRUE),
    radioButtons(
      'sep',
      'Separator',
      c(
        Comma = ',',
        Semicolon = ';',
        Tab = '\t'
      ),
      ';'
    ),
    radioButtons('dec', 'Decrement',
                 c(
                   'Comma' = ',',
                   'Point' = "."
                 ),
                 ','),
    radioButtons(
      'quote',
      'Quote',
      c(
        None = '',
        'Double Quote' = '"',
        'Single Quote' = "'"
      ),
      '"'
    )
    )
    

  )
}

table_undependet_var <- function(id = "table_undependet") {
  # Create a namespace function using the provided id
  ns <- NS("table_undependet")
  
  tagList(
    tags$table(
      tags$tr(
        tags$th(" q "),
        tags$th(" q "),
        tags$th(" q ")
        
      ),

      
      tags$tr(
        tags$td(" q "),
        tags$td(" dq "),
        tags$td(" q ")
        
      ),
      tags$tr(
        tags$td(" q "),
        tags$td(" dq "),
        tags$td(" q ")
        
      )
    )
    
  )
}

