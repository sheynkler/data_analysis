out_first_text <- function(input, output, session, test_data ){
  renderUI({
    if (is.null(test_data()) == F)
      return(NULL)
    HTML(first_text)
  })
}

out_myFileUI <- function(input, output, session){
  renderUI({
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
}



out_radio_dependent <- function(input, output, session, test_data){  # don't use
  print("")
  renderUI({
    if (is.null(test_data))
      return(NULL)
    nams <- names(test_data)
    print(nams)
    radioButtons('dependet_pro', 'Dependent variable', choices = as.list(nams))
  })
}


out_box_undependent <- function(input, output, session, test_data ){  # don't use
  renderUI({
    if (is.null(test_data))
      return(NULL)
    nams <- names(test_data)
    dependet <- input$dependet_pro
    nams <- nams[nams %in% dependet == F]
    checkboxGroupInput(
      'undependet_pro',
      'Undependent variable',
      choices = as.list(nams),
      selected = nams
    )
  })
}
