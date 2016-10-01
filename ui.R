


library(shiny)
library(shinythemes)
library(DT)
source("modules_ui.R")

lm_diagnostik_list <- c(
  "Residuals vs Fitted" = 1,
  "Normal Q-Q" = 2,
  "Scale-Location" = 3,
  "Cook's distance" = 4,
  "Residuals vs Leverage" = 5
)

shinyUI(
  fluidPage(
    theme = shinytheme("cerulean"),
    includeCSS("css/mycss.css"),
    shinyjs::useShinyjs(),
    

    
    titlePanel("Data analysis"),
    fluidRow(column(
      4,
      
      tags$div(
        tags$h3("Enter the data"),
        tabsetPanel(
          id = "tabs",
          selected = "demo",
          type = "tabs",
          tabPanel(
            "from computer",
            value = "from_computer",
            tags$div(class = "small", csvFileInput()),
            actionButton("submit", "collapse/expand")
          ),
          tabPanel(
            "demo",
            value = "demo",
            selectInput(
              "selectDemoData",
              label = h4("Demo data"),
              selectize = F,
              choices = list(
                "datasets" =  names_dates_datasets,
                "MASS" =  names_dates_mass,
                "cluster" =  names_dates_cluster
              ),
              selected = NULL
            ),
            tags$h3(textOutput("data_titel")),
            br()
            
          )
          
          
          
        )
        #,
        #uiOutput("dependent_2")
      ),
      uiOutput("dependent")
      ,
      uiOutput("undependent")
      #,
      
      #'table_undependet_var()',
      #table_undependet_var()
      
      
      
    ),
    column(
      8,
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Data",
          tabsetPanel(
            type = "tabs",
            
            tabPanel("Head",
                     #numericInput("nrow", label = h3("Показать строк"), value = 10),
                     dataTableOutput("contents")),
            tabPanel("Summary",
                     verbatimTextOutput("summary")),
            tabPanel("Str",
                     verbatimTextOutput("str")),
            tabPanel(
              "Korr",
              radioButtons(
                "cor_cov",
                label = "",
                inline = T,
                choices = list("Korrelation" = 1, "Kovariation" = 2),
                selected = 1
              ),
              verbatimTextOutput("korr")
            ),
            tabPanel(
              "Plots",
              tabsetPanel(
                type = "tabs",
                tabPanel("Matrices",
                         plotOutput("plotMatrix")),
                tabPanel(
                  "Simple",
                  tags$div(
                    class = "form-group-sm  form-inline",
                    
                    uiOutput("simple_scatter"),
                    checkboxInput(
                      "simple_scatter_names_change",
                      label = "Change Axis",
                      value = F
                    ),
                    checkboxInput(
                      "ggplot2",
                      label = "ggplot2",
                      value = F
                    )
                  ),
                  
                  plotOutput("simple_scatter_plot")
                ),
                tabPanel(
                  "Hist",
                  uiOutput("hist_change_names_ui"),
                  checkboxInput("hist_probability", label = "Probability with density", value = T),
                  plotOutput("hist_plot")
                )
                
                
              )
              
            )
            
            
          )
        ),
        tabPanel(
          "Cluster",
          fluidRow(column(
            6,
            checkboxInput('cluster_scale', 'Standardize variables', F)
          ),
          column(
            6,
            tags$div(class = "form-group-sm  form-inline",
                     numericInput(
                       "number_clusters",
                       label = ("Number of clusters"),
                       value = 5
                     ))
            
          )),
          
          
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Hierarchical",
              tabsetPanel(
                type = "tabs",
                tabPanel("Plot",
                         plotOutput("cluster_plot")),
                tabPanel("Center",
                         verbatimTextOutput("cluster_center"))
              )
            ),
            tabPanel(
              "Partitioning",
              tabsetPanel(
                type = "tabs",
                tabPanel("Number of clusters",
                         plotOutput("number_of_clusters")),
                tabPanel("Center",
                         verbatimTextOutput("part_cluster_center")),
                tabPanel("Plot",
                         plotOutput("part_cluster_plot"))
              )
            )
          )
        ),
        tabPanel("Model",
                 tabsetPanel(
                   type = "tabs",
                   tabPanel("Regression",
                            tabsetPanel(
                              type = "tabs",
                              tabPanel("Summary",
                                       verbatimTextOutput("summary_lm")),
                              tabPanel(
                                "Diagnostic plot",
                                selectInput(
                                  "select_diagnostic_Plot",
                                  label = ("Select diagnostic"),
                                  choices = lm_diagnostik_list,
                                  selected = 1
                                ),
                                plotOutput("diagnostic_Plot")
                              )
                            )),
                   tabPanel("Tree",
                            tabsetPanel(
                              type = "tabs",
                              tabPanel(
                                "Random Forests "
                              )
                            )
                            )
                 ))
      )
    ))
  )
)