##################################  ZUm cluster 2d 

out_n_variable <-
  function(input,
           output,
           session,
           cluster_data,
           achse, add_id = NULL) {
    renderUI({
      ns <- session$ns
      if (is.null(cluster_data()))
        return(NULL)
      nams <- names(cluster_data())
      if (is.null(add_id) == F) add_id <- paste0("_", add_id)
      input_id_x <- paste0("m_id_select_x_variable", add_id)
      input_id_y <- paste0("m_id_select_y_variable", add_id)
      'if (achse != "x") {
        dependet <- input[[input_id_x]]
        nams <- nams[nams %in% dependet == F]

      }'
      nams <- switch(achse,
                     "x" = nams,
                     "y" = nams[nams %in% input[[input_id_x]] == F],
                     "z" = nams[nams %in% input[[input_id_x]] == F & nams %in% input[[input_id_y]] == F]
      )
      if (achse == "x") {
        label = "Choose of names"
      } else
        label = NULL
      
      id_ <- paste0("m_id_select_", achse, "_variable", add_id)
      radioButtons(
        ns(id_),
        label = label,
        choices = as.list(nams),
        inline = T
      )
    })
  }


#########################################################

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



####################################################################

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
