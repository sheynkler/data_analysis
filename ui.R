



library(shiny)
library(shinythemes)
library(DT)
source("modules_ui.R")



shinyUI(
  fluidPage(
    theme = shinytheme("cerulean"),
    includeCSS("css/mycss.css"),
    shinyjs::useShinyjs(),
    
    
    
    titlePanel("Data analysis"),
    fluidRow(
      column(
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
      column(8,
             tabsetPanel(
               type = "tabs",
               tabPanel(
                 "Data",
                 tabsetPanel(
                   type = "tabs",
                   
                   tabPanel("Head",
                            #numericInput("nrow", label = h3("Показать строк"), value = 10),
                            dataTableOutput("data_table")),
                   tabPanel(
                     "Summary",
                     radioButtons(
                       "summary_choos",
                       label = "",
                       inline = T,
                       choices = list("short" = 1, "precisely (only numeric)" = 2),
                       selected = 1
                     ),
                     conditionalPanel(condition = "input.summary_choos == 1",
                                      verbatimTextOutput("summary")),
                     conditionalPanel(condition = "input.summary_choos == 2",
                                      dataTableOutput("summary_precisely"))
                   ),
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
                     tableOutput("korr_table"),
                     conditionalPanel(
                       condition = "input.cor_cov == 1",
                       br(),
                       fluidRow(column(2,
                                       h4("Method of plot")),
                                column(
                                  10,
                                  selectInput(
                                    "selectKorr",
                                    label = NULL,
                                    selectize = F,
                                    choices = as.list(plot_korrelation),
                                    selected = "circle"
                                  )
                                )),
                       
                       
                       
                       
                       
                       plotOutput("korrelation_plot", width = 400)
                       
                       
                     )
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
                           checkboxInput("ggplot2",
                                         label = "ggplot2",
                                         value = F)
                         ),
                         
                         plotOutput("simple_scatter_plot")
                       ),
                       tabPanel(
                         "Hist",
                         
                         fluidRow(
                           column(4,
                                  uiOutput("hist_change_names_ui")),
                           column(
                             4,
                             checkboxInput("hist_probability", label = "Probability with density", value = T)
                           ),
                           column(4,
                                  numericInput(
                                    "number_breaks",
                                    label = ("Number breaks"),
                                    value = 10
                                  ))
                           
                         ),
                         
                         
                         
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
                   conditionalPanel(
                     condition = "input.select_tabset_cluster=='Hierarchical' || input.select_tabset_cluster=='Partitioning'",
                     tags$div(class = "form-group-sm  form-inline",
                              uiOutput("select_number_of_cluster_ui"))
                   )
                   
                   
                 )),
                 
                 
                 tabsetPanel(
                   type = "tabs",
                   id = "select_tabset_cluster",
                   tabPanel(
                     "Hierarchical",
                     tabsetPanel(
                       type = "tabs",
                       tabPanel(
                         "Plot",
                         checkboxInput('h_cluster_borders', ' Borders', F),
                         plotOutput("Hierarchical_cluster_plot")
                       ),
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
                   ),
                   tabPanel(
                     "Based",
                     tabsetPanel(
                       type = "tabs",
                       tabPanel("Summary",
                                verbatimTextOutput("based_summary")),
                       tabPanel("Center",
                                verbatimTextOutput("based_center")),
                       tabPanel(
                         "Plot",
                         selectInput(
                           "selectbased_plot",
                           label = NULL,
                           selectize = F,
                           choices = as.list(based_cluster_list),
                           selected = "classification"
                         ),
                         plotOutput("based_plot")
                       )
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
                                   tabsetPanel(type = "tabs",
                                               tabPanel("Random Forests ")))
                        ))
             ))
    )
  )
)