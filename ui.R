





library(shiny)
library(shinythemes)
library(DT)
source("ui_file/modules_ui.R")



shinyUI(
  fluidPage(
    theme = shinytheme("cerulean"),
    includeCSS("css/mycss.css"),
    shinyjs::useShinyjs(),
    
    
    
    titlePanel("Data analysis"),
    fluidRow(
      column(
        3,
        
        tags$div(
          tags$h3("Enter the data"),
          tabsetPanel(
            id = "tabs_enter",
            selected = "demo",
            type = "tabs",
            tabPanel(
              "from computer",
              value = "enter_from_computer",
              tags$div(class = "small", show_csvFileInput()),
              actionButton("submit_csv_options", "collapse/expand")
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
        conditionalPanel(condition = "input.select_data_cluster_model=='Model'",
                         uiOutput("dependent")
                         ,
                         uiOutput("undependent"))
        ,
        conditionalPanel(condition = "input.select_data_cluster_model!='Model'",
                         uiOutput("select_variables_ui")),
        br()
        
        #,        downloadButton("report_pdf", "Generate report PDF")
        
        
        
      ),
      column(
        9,
        
        tabsetPanel(
          type = "tabs",
          id = "select_data_cluster_model",
          tabPanel(
            "Data",
            tabsetPanel(
              type = "tabs",
              
              tabPanel(
                "Head",
                #numericInput("nrow", label = h3("Показать строк"), value = 10),
                br(),
                h4(mf_ui_firstText("m_name_firstText")),
                #h4(htmlOutput("first_text")),
                dataTableOutput("data_table")
              ),
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
              tabPanel("Data Structure",
                       verbatimTextOutput("str")),
              tabPanel(
                "Correlation",
                radioButtons(
                  "cor_cov",
                  label = "",
                  inline = T,
                  choices = list("Correlation" = 1, "Covariance" = 2),
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
                  tabPanel("Simple",
                           fluidRow(
                             column(
                               2,
                               fluidRow(column(6,
                                               "X",
                                               uiOutput(
                                                 "select_x_variable_ui"
                                               )),
                                        column(6,
                                               "Y"  ,
                                               uiOutput(
                                                 "select_y_variable_ui"
                                               ))),
                               checkboxInput(
                                 "simple_scatter_names_change",
                                 label = "Change Axis",
                                 value = F
                               ),
                               checkboxInput("ggplot2",
                                             label = "ggplot2",
                                             value = F)
                               
                             ),
                             column(10,
                                    
                                    
                                    #uiOutput("simple_scatter"),
                                    
                                    
                                    
                                    plotOutput("simple_scatter_plot"))
                           )),
                  tabPanel("Hist",
                           
                           fluidRow(
                             column(
                               2,
                               uiOutput("hist_change_names_ui"),
                               checkboxInput("hist_probability", label = "Probability with density", value = T),
                               numericInput(
                                 "number_breaks",
                                 label = ("Number breaks"),
                                 value = 10
                               )
                             ),
                             column(10,
                                    plotOutput("hist_plot"))
                             
                             
                           ))
                )
                
              )
              
              
            )
          ),
          tabPanel(
            "Cluster",
            fluidRow(
              column(4,
                     checkboxInput(
                       'cluster_scale', 'Standardize variables', F
                     )),
              column(
                4,
                conditionalPanel(
                  condition = "input.select_tabset_cluster=='Hierarchical' || input.select_tabset_cluster=='Partitioning'",
                  tags$div(class = "form-group-sm  form-inline",
                           uiOutput("select_number_of_cluster_ui"))
                )
                
                
              ),
              column(
                4,
                actionButton("add_cluster_var", "Add cluster groups"),
                textOutput("cluster_added")
                
                
              )
              
            ),
            
            
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
                    selectInput(
                      "metrix_dist",
                      label = "Method of the distance measure",
                      choices = as.list(dist_method)
                    ),
                    selectInput(
                      "metode_h_cluster",
                      label = "Method of the agglomeration",
                      choices = as.list(metode_hierarchical_cluster)
                    ),
                    #plotOutput("Hierarchical_cluster_plot"),
                    mf_ui_hierarchicalCluster("m_name_hierarchicalCluster")
                  ),
                  tabPanel(
                    "Center",
                    verbatimTextOutput("cluster_center"),
                    br(),
                    d3heatmapOutput("cluster_center_heatmap")
                  )
                  
                )
              ),
              tabPanel(
                "Partitioning",
                tabsetPanel(
                  type = "tabs",
                  tabPanel("Number of clusters",
                           plotOutput("number_of_clusters")),
                  tabPanel(
                    "Center",
                    verbatimTextOutput("part_cluster_center"),
                    br(),
                    d3heatmapOutput("part_cluster_center_heatmap")
                  ),
                  tabPanel("Plot pca",
                           plotOutput("part_cluster_plot")),
                  tabPanel(
                    "Plot 2d",
                    
                    
                    mf_ui_drawPlot2dCluster("m_name_drawPlot2dCluster")
                  ),
                  tabPanel(
                    "Plot 3d",
                    
                    
                    mf_ui_drawPlot3dCluster("m_name_drawPlot3dCluster")
                  )
                )
              ),
              tabPanel(
                "Model-Based",
                tabsetPanel(
                  type = "tabs",
                  tabPanel("Summary",
                           verbatimTextOutput("based_summary")),
                  tabPanel(
                    "Center",
                    verbatimTextOutput("based_center"),
                    br(),
                    d3heatmapOutput("based_center_heatmap")
                  ),
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
                     tabPanel(
                       "Regression",
                       checkboxInput("regression_by_AIC",
                                     label = "Model AIC",
                                     value = F),
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
                       )
                     ) #, tabPanel("Tree",     tabsetPanel(type = "tabs",   tabPanel("Random Forests ")))
                   ))
        )
      )
    )
  )
)