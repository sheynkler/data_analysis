################## Legende:
# mf_ui_    module function vor ui
# m_name_   namespace for the module
# mf_s_     module function vor server
# m_id_      module id






###################### First Text

mf_ui_firstText <- function(id) {
  ns <- NS(id)
  htmlOutput(ns("m_id_firstText"))
}



mf_s_first_text <- function(input, output, session, test_data ){
  output$m_id_firstText <- renderUI({
    if (is.null(test_data()) == F)    return(NULL)
    HTML(first_text)
  })
}

###################  Hierarchical cluster


mf_ui_hierarchicalCluster  <- function(id) {
  ns <- NS(id)
  plotOutput(ns("m_hierarchicalClusterPlot"))
}


mf_s_hierarchicalCluster <- function(input, output, session, test_data, Hierarchical_cluster_fit,
                                      number_clusters, h_cluster_borders ){
  output$m_hierarchicalClusterPlot <- renderPlot({
    if (is.null(test_data()))     return(NULL)

    plot(Hierarchical_cluster_fit())
    if (h_cluster_borders())
      rect.hclust(Hierarchical_cluster_fit(),
                  k = number_clusters(),
                  border = "red")
    
  })
  
}

##################################### plot cluster 2d

mf_ui_drawPlot2dCluster  <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(
      ns("m_id_select_x_variable_ui")
    ),
    uiOutput(
      ns("m_id_select_y_variable_ui")
    ),
    checkboxInput(
      ns("m_id_drawPlot2dCluster_change"),
      label = "Change Axis",
      value = F
    ),
    plotOutput(ns("m_id_drawPlot2dCluster"))
  )
  
}

mf_s_drawPlot2dCluster <- function(input, output, session, cluster_data, part_cluster_fit ){
  
  
  output$m_id_select_x_variable_ui <- out_n_variable(input, output, session, cluster_data, "x")
  
  output$m_id_select_y_variable_ui <- out_n_variable(input, output, session, cluster_data, "y")
  
  output$m_id_drawPlot2dCluster <- renderPlot({
    
    cluster_data <- cluster_data()
    part_cluster_fit <- part_cluster_fit()
    part_cluster <- part_cluster_fit$cluster
    
    
    if (is.null(cluster_data()))     return(NULL)
    x = cluster_data[,input$m_id_select_x_variable]
    y = cluster_data[,input$m_id_select_y_variable]
    xlab <- input$m_id_select_x_variable
    ylab <- input$m_id_select_y_variable
    
    if(input$m_id_drawPlot2dCluster_change){
      y = cluster_data[,input$m_id_select_x_variable]
      x = cluster_data[,input$m_id_select_y_variable]
      xlab <- input$m_id_select_y_variable
      ylab <- input$m_id_select_x_variable
    }
    main <- paste( input$m_id_select_x_variable, "and",input$m_id_select_y_variable)
    plot(x, y, col = part_cluster, type = "p", pch = 16, cex = 2, main = main, xlab = xlab, ylab = ylab)
    
  })
  
}



##################################### plot cluster 3d

mf_ui_drawPlot3dCluster  <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(
      ns("m_id_select_x_variable_ui_3d")
    ),
    uiOutput(
      ns("m_id_select_y_variable_ui_3d")
    ),
    uiOutput(
      ns("m_id_select_z_variable_ui_3d")
    ),
    scatterplotThreeOutput(ns("m_id_drawPlot3dCluster"))
  )
  
}

mf_s_drawPlot3dCluster <- function(input, output, session, cluster_data, part_cluster_fit ){
  
  
  output$m_id_select_x_variable_ui_3d <- out_n_variable(input, output, session, cluster_data, "x", "3d")
  output$m_id_select_y_variable_ui_3d <- out_n_variable(input, output, session, cluster_data, "y", "3d")
  output$m_id_select_z_variable_ui_3d <- out_n_variable(input, output, session, cluster_data, "z", "3d")
  
  

  
  output$m_id_drawPlot3dCluster <- renderScatterplotThree({
    

    cluster_data <- cluster_data()
    part_cluster_fit <- part_cluster_fit()
    part_cluster <- part_cluster_fit$cluster

    if (is.null(cluster_data()))     return(NULL)
    x = cluster_data[,input$m_id_select_x_variable_3d]
    y = cluster_data[,input$m_id_select_y_variable_3d]
    z = cluster_data[,input$m_id_select_z_variable_3d]
    xlab <- input$m_id_select_x_variable_3d
    ylab <- input$m_id_select_y_variable_3d
    
    part_cluster_color <- mypalette[part_cluster]
    #main <- paste( input$m_id_select_x_variable, "and",input$m_id_select_y_variable_3d)
    
    plot_data <- cluster_data[,c(input$m_id_select_x_variable_3d,input$m_id_select_y_variable_3d,input$m_id_select_z_variable_3d)]
    scatterplot3js(plot_data, color = part_cluster_color, grid = F)
  })
  
}
