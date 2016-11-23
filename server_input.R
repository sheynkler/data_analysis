server_input <- function(input, output, session){
  observe({
    if (input$submit_csv_options > 0) {
      shinyjs::toggle(id = "enter", anim = TRUE)
    }
  })
  output$myFileUI <- renderUI({
    input$clear
    input$uploadFormat
    fileInput(
      'file1',
      ' ',
      accept = c(
        'text/csv',
        'text/comma-separated-values',
        'text/tab-separated-values',
        'text/plain',
        '.csv',
        '.tsv'
      )
    )
  })
  test_data <- reactive({
    print(input$file1)
    if (input$tabs_enter == "demo") {
      inFileString <- input$selectDemoData
      if (inFileString == "")
        return(NULL)
      test_data <- eval(as.symbol(inFileString))
    }
    
    
    else {
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      test_data <- read.csv(
        inFile$datapath,
        header = input$header,
        sep = input$sep,
        dec = input$dec,
        quote = input$quote
      )
    }
    
    
    test_data
    
    
  })
  
}