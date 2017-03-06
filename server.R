

library(shiny)
library(shinyjs)
#library(cluster)
library(fBasics)
library(corrplot)
library(ggplot2)
library(mclust)
library(rmarkdown)
library(tibble)

source("server_file/server_output.R")

options(shiny.maxRequestSize = 30 * 1024 ^ 2)


shinyServer(function(input, output, session) {
  
'  output$first_text <-   renderUI({
    if (is.null(test_data()) == F)
      return(NULL)
    HTML(first_text)
  })'
  
  callModule(m_out_first_text, "m_test_firstText", test_data)
  
  
  
  observe({
    if (input$submit_csv_options > 0) {
      shinyjs::toggle(id = "enter", anim = TRUE)
    }
  })
  
  output$myFileUI <- out_myFileUI(input, output, session)
  
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
  
  # HANDLING
  
  temp_data <- reactive({
    
    
    temp_data <- test_data()
    if (is.null(test_data()))
      return(NULL)

    #  
    names_all <- input$select_variables
    if (input$select_data_cluster_model == "Model") names_all <- c(input$undependet_pro, input$dependet_pro)
    
    names_temp_data <- names(temp_data)
    names_off <-
      names_temp_data[names_temp_data %in% names_all == F]
    temp_data[, names(temp_data) %in% names_all == T, drop = F]
    
  })
  
  temp_data_2000 <- reactive({
    if (is.null(test_data()))
      return(NULL)
    
    temp_data <- temp_data()
    if (nrow(temp_data) <= 2000)
      return(temp_data)
    
    index <- sample(1:nrow(temp_data), 2000)
    temp_data_2000 <- temp_data[index, ]
    temp_data_2000
    
  })
  
  
  output$dependent <- renderUI({
    if (is.null(test_data()))
      return(NULL)
    nams <- names(test_data())
    print(nams)
    radioButtons('dependet_pro', 'Dependent variable', choices = as.list(nams))
  })
  
  output$undependent <- renderUI({
    if (is.null(test_data()))
      return(NULL)
    nams <- names(test_data())
    dependet <- input$dependet_pro
    nams <- nams[nams %in% dependet == F]
    checkboxGroupInput(
      'undependet_pro',
      'Undependent variable',
      choices = as.list(nams),
      selected = nams
    )
  })
  
  
  
  output$select_variables_ui <- renderUI({
    if (is.null(test_data()))
      return(NULL)
    nams <- names(test_data())
    #dependet <- input$dependet_pro
    #nams <- nams[nams %in% dependet == F]
    checkboxGroupInput(
      'select_variables',
      'Variables',
      choices = as.list(nams),
      selected = nams
    )
  })
  # OUTPUT DATA
  
  output$data_table <- renderDataTable({
    temp_data <- temp_data()
    if (is.null(temp_data))
      return(temp_data)
    if (class(temp_data) == "data.frame" &
        is.null(temp_data) == F)
      DT::datatable(temp_data,
                    options = list(searching = F))
    
    
  })
  output$plotMatrix <- renderPlot({
    if (is.null(temp_data()))
      return(NULL)
    temp_data <- temp_data()
    nams <- names(temp_data)
    if (nrow(temp_data > 2000))
      temp_data <- temp_data_2000()
    
    
    nams <- paste(nams, collapse = "+")
    nams <- paste0("~", nams)
    nams <- as.formula(nams)
    
    pairs(nams,
          data = temp_data,
          main = "Simple Scatterplot Matrix",
          col = "blue")
    
  },
  height = function() {
    session$clientData$output_plotMatrix_width
  })
  temp_data_numeric <- reactive({
    if (is.null(temp_data()))
      return(NULL)
    temp_data <- temp_data()
    
    for (j in ncol(temp_data):1) {
      if (class(temp_data[, j]) %in% num_classes == F)
        temp_data <- temp_data[, -j, drop = F]
    }
    #print(temp_data)
    temp_data
  })
  output$data_titel <- renderText({
    inFileString <- input$selectDemoData
    if (inFileString == "")
      return(NULL)
    as.character(dates_data_frame$Title[dates_data_frame$Item == input$selectDemoData])
    
  })
  output$summary <- renderPrint(summary(temp_data()))
  
  output$summary_precisely <- renderDataTable({
    temp_data_numeric <- temp_data_numeric()
    #print(temp_data_numeric)
    DT::datatable(basicStats(temp_data_numeric) ,
                  options = list(
                    pageLength = 16,
                    paging = F,
                    searching = F
                  ))
    
  })
  
  
  output$str <- renderPrint(str(temp_data()))
  output$korr_table <- renderTable({
    if (is.null(test_data()))
      return(NULL)
    
    temp_data <- temp_data_numeric()
    
    
    if (input$cor_cov == 1)
      cor(temp_data)
    else
      cov(temp_data)
  })
  
  output$korrelation_plot <- renderPlot({
    if (is.null(test_data()))
      return(NULL)
    
    temp_data <- temp_data_numeric()
    M <- cor(temp_data)
    
    if (input$cor_cov == 1)
      corrplot(M, method = input$selectKorr)
    else
      NULL
  })
  
  output$simple_scatter <- renderUI({
    if (is.null(test_data()))
      return(NULL)
    
    checkboxGroupInput(
      "simple_scatter_names",
      label = ("change variable"),
      inline = T,
      choices = as.list(c(names(temp_data(
      )))),
      selected = names(temp_data())[1:2]
    )
    
    
  })
  output$hist_change_names_ui <- renderUI({
    if (is.null(test_data()))
      return(NULL)
    
    temp_data <- temp_data_numeric()
    
    
    
    radioButtons(
      "hist_change_names",
      label = ("change variable"),
      inline = F,
      choices = as.list(c(names(temp_data))),
      selected = names(temp_data)[1]
    )
    
    
  })
  output$hist_plot <- renderPlot({
    if (is.null(temp_data_numeric()))
      return(NULL)
    temp_data <- temp_data_numeric()
    if (nrow(temp_data > 2000))
      temp_data <- temp_data_2000()
    
    nams <- input$hist_change_names
    
    hist(
      temp_data[, nams],
      main = paste("Histogram of", nams),
      xlab = nams,
      col = "cornflowerblue",
      probability = input$hist_probability,
      breaks = input$number_breaks
    )
    if (input$hist_probability)
      lines(density(temp_data[, nams]), col = "green", lwd = 3)
    
    
    
  })
  output$simple_scatter_plot <- renderPlot({
    if (is.null(temp_data()))
      return(NULL)
    temp_data <- temp_data()
    if (nrow(temp_data > 2000))
      temp_data <- temp_data_2000()
    
    #nams <- input$simple_scatter_names
    
    nams_new_x <- input$select_x_variable
    nams_new_y <- input$select_y_variable
    
    if (input$simple_scatter_names_change) {
      nams1 <- nams_new_x
      nams2 <- nams_new_y
    }
    else{
      nams1 <- nams_new_y
      nams2 <- nams_new_x
    }
    
    
    if (input$ggplot2) {
        c <- ggplot(temp_data, aes_string(nams1, nams2))
        c + stat_smooth(method = "lm") + geom_point() +  ggtitle(paste(nams1, "vs", nams2))
      }
    else{
      plot(
        temp_data[, nams1],
        temp_data[, nams2],
        main = paste(nams1, "vs", nams2),
        xlab = nams1,
        ylab = nams2,
        col = "blue",
        pch = 19
      )
    }

  })
  output$select_x_variable_ui <- renderUI({
    if (is.null(test_data()))
      return(NULL)
    nams <- names(test_data())
    radioButtons('select_x_variable', label = NULL, choices = as.list(nams))
  })
  output$select_y_variable_ui <- renderUI({
    if (is.null(test_data()))
      return(NULL)
    nams <- names(test_data())
    dependet <- input$select_x_variable
    nams <- nams[nams %in% dependet == F]
    radioButtons(
      'select_y_variable',
      label = NULL,
      choices = as.list(nams)
    )
  })
  
  # REGRESSIA
  
  lm_fit <- reactive({
    if (is.null(temp_data()))
      return(NULL)
    
    temp_data <- temp_data()
    dependent_ <-  input$dependet_pro
    ss <- paste(dependent_, "~ .")
    
    formula_ <- as.formula(ss)
    
    lm(formula_, data = temp_data)
  })
  output$summary_lm <- renderPrint({
    summary(lm_fit())
    
  })
  output$diagnostic_Plot <- renderPlot({
    fit <- lm_fit()
    #layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
    plot(fit,  which = as.integer(input$select_diagnostic_Plot))
  })
  
  # CLUSTER
  
  cluster_data <- reactive({
    if (is.null(temp_data_numeric()))
      return(NULL)
    mydata <- na.omit(temp_data_numeric())
    #mydata <- scale(mydata)
    mydata
  })
  scale_cluster_data <- reactive({
    if (is.null(cluster_data()))
      return(NULL)
    #mydata <- na.omit(temp_data_numeric())
    mydata <- scale(cluster_data())
    mydata
  })
  Hierarchical_cluster_fit <- reactive({
    if (is.null(test_data()))
      return(NULL)
    
    if (input$cluster_scale)
      mydata <- scale_cluster_data()
    else
      mydata <- cluster_data()
    # Ward Hierarchical Clustering
    d <- dist(mydata, method = "euclidean")
    fit <- hclust(d, method = "ward.D2")
    fit
  })
  output$Hierarchical_cluster_plot <- renderPlot({
    if (is.null(test_data()))
      return(NULL)
    
    plot(Hierarchical_cluster_fit())
    if (input$h_cluster_borders)
      rect.hclust(Hierarchical_cluster_fit(),
                  k = input$number_clusters,
                  border = "red")
    
  })
  
  aggregate_groups <- reactive({
    if (is.null(test_data()))
      return(NULL)
        groups <-
      cutree(Hierarchical_cluster_fit(), k = input$number_clusters)
    aggregate(cluster_data(), by = list(groups), FUN = mean)
  })
  output$cluster_center <- renderPrint({
    if (is.null(test_data()))
      return(NULL)
    
    aggregate_groups()
  })
  
  output$cluster_center_heatmap <- renderD3heatmap({
    if (is.null(test_data()))
      return(NULL)

    D3heatmap_data <- as.data.frame(aggregate_groups())
    d3heatmap(
      D3heatmap_data[,-1],
      colors = "Blues",
      dendrogram = "none", scale="column", width = 100
    )
  })
  
  output$number_of_clusters <- renderPlot({
    if (is.null(test_data()))
      return(NULL)
    
    if (input$cluster_scale)
      mydata <- scale_cluster_data()
    else
      mydata <- cluster_data()
    wss <- (nrow(mydata) - 1) * sum(apply(mydata, 2, var))
    for (i in 2:15)
      wss[i] <- sum(kmeans(mydata,
                           centers = i)$withinss)
    plot(1:15,
         wss,
         type = "b",
         xlab = "Number of Clusters",
         ylab = "Within groups sum of squares")
  })
  part_cluster_fit <- reactive({
    if (is.null(test_data()))
      return(NULL)
    
    if (input$cluster_scale)
      mydata <- scale_cluster_data()
    else
      mydata <- cluster_data()
    fit <- kmeans(mydata, input$number_clusters)
    
    fit
  })
  
  output$part_cluster_center <- renderPrint({
    if (is.null(test_data()))
      return(NULL)
    
    fit <- part_cluster_fit()
    aggregate(cluster_data(),
              by = list(fit$cluster),
              FUN = mean)
    
  })
  output$part_cluster_center_heatmap <- renderD3heatmap({
    if (is.null(test_data()))
      return(NULL)
    fit <- part_cluster_fit()
    aggregate_groups <-     aggregate(cluster_data(),
                                      by = list(fit$cluster),
                                      FUN = mean)
    D3heatmap_data <- as.data.frame(aggregate_groups)
    d3heatmap(
      D3heatmap_data[,-1],
      colors = "Blues",
      dendrogram = "none", scale="column", width = 100
    )
  })
  
  output$part_cluster_plot <- renderPlot({
    if (is.null(test_data()))
      return(NULL)
    
    fit <- part_cluster_fit()
    clusplot(
      cluster_data(),
      fit$cluster,
      color = TRUE,
      shade = TRUE,
      labels = 2,
      lines = 0
    )
  })
'  output$part_cluster_plot_2d <- renderPlot({
    if (is.null(test_data()))
      return(NULL)
    
    fit <- part_cluster_fit()

  })'
  
  
  based_fit <- reactive({
    if (is.null(test_data()))
      return(NULL)
    if (input$cluster_scale)
      mydata <- scale_cluster_data()
    else
      mydata <- cluster_data()
    fit <- Mclust(mydata)
    fit
  })
  output$based_summary <- renderPrint({
    if (is.null(test_data()))
      return(NULL)
    fit <- based_fit()
    summary(fit)
  })
  output$based_plot <- renderPlot({
    if (is.null(test_data()))
      return(NULL)
    
    fit <- based_fit()
    which_plot <- input$selectbased_plot
    plot(fit, which_plot)
    },
    height = function() {
      session$clientData$output_based_plot_width
  })
  output$based_center <- renderPrint({
    if (is.null(test_data()))
      return(NULL)
    fit <- based_fit()
    mydata <- cluster_data()
    aggregate(mydata,
              by = list(fit$classification),
              FUN = mean)
  })
  output$based_center_heatmap <- renderD3heatmap({
    if (is.null(test_data()))
      return(NULL)
    fit <- based_fit()
    mydata <- cluster_data()
    D3heatmap_data <- as.data.frame(aggregate(mydata,
              by = list(fit$classification),
              FUN = mean))
    d3heatmap(
      D3heatmap_data[,-1],
      colors = "Blues",
      dendrogram = "none", scale="column"
    )
  })
  
  output$select_number_of_cluster_ui <- renderUI({
    
    numericInput("number_clusters",
                 label = ("Number of clusters"),
                 value = 5)
  })
  observe({
    print(session$clientData$output_based_plot_width)
  })
  
  output$report_pdf <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "new.Rmd")
      file.copy("new.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(
        number_breaks = input$number_breaks,
        temp_data = temp_data()

      )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  
})
