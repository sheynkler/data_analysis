
###################### First Text

firstTextUI <- function(id) {
  ns <- NS(id)
  htmlOutput(ns("m_firstText"))
}



m_out_first_text <- function(input, output, session, test_data ){
  output$m_firstText <- renderUI({
    if (is.null(test_data()) == F)    return(NULL)
    HTML(first_text)
  })
}









