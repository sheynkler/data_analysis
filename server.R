
library(shiny)
library(shinyjs)
library(cluster)
# source("functions.R")
options(shiny.maxRequestSize = 9 * 1024 ^ 2)
#source("server_functions.R")

shinyServer(function(input, output, session) {
  
  
  # INPUT
  


  observe({
    if (input$submit > 0) {
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
    if (input$tabs == "demo") {
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
    
    if (is.null(input$dependet_pro))
      return(temp_data)
    names_all <- c(input$undependet_pro, input$dependet_pro)
    names_temp_data <- names(temp_data)
    names_off <-
      names_temp_data[names_temp_data %in% names_all == F]
    temp_data[, names(temp_data) %in% names_all == T]
    
  })
  output$dependent <- renderUI({
    if(is.null(test_data())) return(NULL)
    nams <- names(test_data())
    radioButtons('dependet_pro', 'Dependent variable', choices = as.list(nams))
  })
  output$undependent <- renderUI({
    if(is.null(test_data())) return(NULL)
    nams <- names(test_data())
    dependet <- input$dependet_pro
    nams <- nams[nams %in% dependet == F]
    checkboxGroupInput('undependet_pro', 'Undependent variable', choices = as.list(nams), selected = nams)
  })
  # OUTPUT DATA
  
  output$contents <- renderDataTable({
    temp_data <- temp_data()
    if (is.null(temp_data))
      return(temp_data)
    if (class(temp_data) == "data.frame" &
        is.null(temp_data) == F)
      temp_data
    
    
  })
  output$plotMatrix <- renderPlot({
    if (is.null(temp_data()))
      return(NULL)
    nams <- names(temp_data())
    
    
    nams <- paste(nams, collapse = "+")
    nams <- paste0("~", nams)
    nams <- as.formula(nams)
    
    pairs(nams, data = temp_data(),  main = "Simple Scatterplot Matrix", col = "blue")
    
  })
  temp_data_numeric <- reactive({
    if (is.null(temp_data()))
      return(NULL)
    temp_data <- temp_data()
    
    for (j in ncol(temp_data):1) {
      if (class(temp_data[, j]) %in% num_classes == F)
        temp_data <- temp_data[,-j]
    }
    temp_data
  })
  output$data_titel <- renderText({ 
    inFileString <- input$selectDemoData
    if (inFileString == "")
      return(NULL)
    as.character(dates_data_frame$Title[ dates_data_frame$Item == input$selectDemoData])
    
})
  output$summary <- renderPrint(summary(temp_data()))
  output$str <- renderPrint(str(temp_data()))
  output$korr <- renderPrint({
    temp_data <- temp_data_numeric()
    
    
    if (input$cor_cov == 1)
      cor(temp_data)
    else
      cov(temp_data)
  })
  output$simple_scatter <- renderUI({
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
    temp_data <- temp_data_numeric()
    
    
    
    radioButtons(
      "hist_change_names",
      label = ("change variable"),
      inline = T,
      choices = as.list(c(names(temp_data))),
      selected = names(temp_data)[1]
    )
    
    
  })
  output$hist_plot <- renderPlot({
    if (is.null(temp_data_numeric()))
      return(NULL)
    temp_data <- temp_data_numeric()
    
    nams <- input$hist_change_names
    
    hist(
      temp_data[, nams],
      main = paste("Histogram of", nams),
      xlab = nams,  col = "blue",
      probability = input$hist_probability
    )
    if(input$hist_probability) lines(density(temp_data[, nams]), col = "green", lwd = 3)
    
    
    
  })
  output$simple_scatter_plot <- renderPlot({
    if (is.null(temp_data()))
      return(NULL)
    temp_data <- temp_data()
    nams <- input$simple_scatter_names
    if (input$simple_scatter_names_change) {
      nams1 <- nams[1]
      nams2 <- nams[2]
    }
    else{
      nams1 <- nams[2]
      nams2 <- nams[1]
    }
    
    if (length(nams) != 2)
      return(NULL)
    else
      if(input$ggplot2){
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
  
  # REGRESSIA
  
  lm_fit <- reactive({
        if(is.null(temp_data())) return(NULL)
    
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
    if(is.null(temp_data_numeric())) return(NULL)
    mydata <- na.omit(temp_data_numeric()) 
    #mydata <- scale(mydata) 
    mydata
  })
  scale_cluster_data <- reactive({
    if(is.null(cluster_data())) return(NULL)
    #mydata <- na.omit(temp_data_numeric()) 
    mydata <- scale(cluster_data()) 
    mydata
  })
  Hierarchical_cluster_fit <- reactive({
    if(input$cluster_scale) mydata <- scale_cluster_data()
    else mydata <- cluster_data()
    # Ward Hierarchical Clustering
    d <- dist(mydata, method = "euclidean") 
    fit <- hclust(d, method="ward.D2")
    fit
  })
  output$cluster_plot <- renderPlot({
    plot(Hierarchical_cluster_fit())
  })
  output$cluster_center <- renderPrint({
    groups <- cutree(Hierarchical_cluster_fit(), k=input$number_clusters)
    aggregate(cluster_data(),by=list(groups),FUN=mean)
  } )
  output$number_of_clusters <- renderPlot({
    if(input$cluster_scale) mydata <- scale_cluster_data()
    else mydata <- cluster_data()
    wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
    for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                         centers=i)$withinss)
    plot(1:15, wss, type="b", xlab="Number of Clusters",
         ylab="Within groups sum of squares") 
  })
  part_cluster_fit <- reactive({
    if(input$cluster_scale) mydata <- scale_cluster_data()
    else mydata <- cluster_data()
    fit <- kmeans(mydata, input$number_clusters)

    fit
  })
  output$part_cluster_center <- renderPrint({
    fit <- part_cluster_fit()
    aggregate(cluster_data(), by=list(fit$cluster),FUN=mean)
    
  })
  output$part_cluster_plot <- renderPlot({
    fit <- part_cluster_fit()
    clusplot(cluster_data(), fit$cluster, color=TRUE, shade=TRUE,
             labels=2, lines=0)
  })


  
  
})