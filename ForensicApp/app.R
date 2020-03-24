library(summarytools)
library(shinythemes)
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(ggthemr) 
library(shinyBS)
library(rio)
library(GGally)
library(klaR)
library(caret)
library(shinycssloaders)
library(e1071)
library(randomForest)
library(rpart)
library(mgcv)
library(glmnet)
library(arm)
library(logistf)
library(comparison)
library(mlbench)
library(nnet)
library(gridExtra)
library(plotly)

# set working directory and source functions_rshiny.R 
source("functions_rshiny.R")
st_options("round.digits")

# make datasets used in the app available
data("Glass")
data("BreastCancer")
data("PimaIndiansDiabetes")

# install_formats()   # to install formats for use with rio import() functions

# This will add a generic error message for users everytime somethings goes wrong
# options(shiny.sanitize.errors = TRUE) 
# To hide all red error messages in the app - uncomment this! Not recommended 
# tags$style(type="text/css",
#            ".shiny-output-error { visibility: hidden; }",
#            ".shiny-output-error:before { visibility: hidden; }"
# ),

ggthemr('solarized', layout = 'clear', spacing = 1, text_size = 14)

# Define UI for application that draws the app layout and components
ui <- navbarPage( theme = shinytheme("flatly"),
    
    title = "Forensic Classification",
                 
    ###############################################################
    # About tab
    tabPanel(title = "Home", 
             icon = icon("home", "fa"),
             
             tabsetPanel( tabPanel("About this app", 
                                   tags$br(),
                                   tags$p(
                                       tags$img(height = 50, src = "UoG.png")),
                                   tags$h1("About"),
                                   tags$body("This app provides a flexible tool for classification 
                                             and allows the user to input their own dataset in a .txt 
                                             or .csv format."  ),
                                   # tags$h1("Methodology"),
                                   tags$body("Classification methods implemented in this app include 
                                             standard multivariate methods such as Linear Discriminant 
                                             Analysis, likelihood ratio and logistic regression-based 
                                             approaches.
                                             Numerical and visual summaries of the classification results
                                             are also provided.",
                                             # tags$ul( tags$li(" "), 
                                             #          tags$li(" "),
                                             #          tags$li(" "),
                                             #          tags$li(" ")
                                             #          
                                             #        ),
                                    tags$h1("References"),
                                             " For more details about the methods available in this app, 
                                               please refer to our paper ...
                                               For a detailed description of the alcohol dataset please see ...",
                                    tags$h1("Citation info"),
                                             " You can cite this app as ... (download reference here)."
                                             )
                                   ), 
                          
                          tabPanel("Credits", 
                                   tags$br(),
                                   "This app was developed by Diana Giurghita,  with contributions from 
                                    Giulia Biosa, Eugenio Alladio, Tereza Neocleous and Tejaswini Reddy. 
                                    We thank ", tags$em("Dimitra Eleftheriou"), " and ",
                                    tags$a( href ="https://chenjr-jacob.idv.tw", "Jacob Chen"), 
                                    "whose shiny apps were used as a reference."
                                   )
                        )
             
             ),
    
    ###############################################################  
    # Data tab
    tabPanel(title = "Data", 
             icon = icon( "database"),
             
             sidebarLayout(
                sidebarPanel(width = 3,
                    conditionalPanel( condition = "input.DatasetSummaries==1",  
                        h5(strong("Data set selection")),
              
                        selectInput("dataset", label = NULL, 
                               choices = c( "", "Iris", "Diamonds", "Glass","Diabetes", "Upload data")), 
                        
                        uiOutput("UpSelect") ,
             
                        br(),       
   
                        actionButton("GoData", label = "Select dataset",icon("file-import"), width = '100%',
                                     style="color: #fff; background-color: #28bb9b; border-color: #87d5c5" ), 
                
                        bsTooltip("dataset", "Select an exisiting data set or upload your own", 
                                  placement = "top", trigger = "hover",
                                    options = NULL) ),
                    
                    conditionalPanel( condition = "input.DatasetSummaries==2", 
                        
                        h5(strong("Data summaries")),
                        
                        checkboxInput( "checkNum", label = "Numerical summaries", value = FALSE),
                        
                        checkboxInput( "checkCat", label = "Frequency summaries", value = FALSE),
                        
                        uiOutput("CatVariableSum") ),
                    
                    conditionalPanel( condition = "input.DatasetSummaries==3", 
                        h5(strong("Exploratory plots options")),
                        
                        uiOutput( "Variable_select" ) %>% withSpinner(color="#0dc5c1") 
                        
                        )),
                
                mainPanel(
                    
                     tabsetPanel(
                         id = 'DatasetSummaries',
                         
                         tabPanel( value = 1, 
                                  h5( strong("Data table") ), 
                                  tags$br(),
                                  DT::dataTableOutput("DataTab") %>% withSpinner(color="#0dc5c1")
                                  ),
                         
                  
                         tabPanel( value = 2, 
                                  h5( strong("Data summaries") ),
                                  tags$br(),
                                     
                                  h5( "Numerical variables summary"),
                                  tags$br(),
                                  DT::dataTableOutput("DataSumN"),
                                  tags$br(),         
                                  
                                  h5( "Categorical variables summary"),
                                  tags$br(),
                                  column(8,
                                  uiOutput("Freq") ),
                                  tags$br()
                                  
                                  ),
                                            
                          tabPanel( value = 3, 
                                    h5( strong("Exploratory plots")), 
                                    plotOutput("ExPlot") %>% withSpinner(color="#0dc5c1") 
                                 )     
                                
                        )
                    )
             )),
                    
                  
                    
            
    ################################################################
    ## Classification tab
    tabPanel(title = "Classification",
             icon = icon( "bar-chart-o"),

             sidebarLayout(
                 sidebarPanel(width = 3,
                    conditionalPanel( condition = "input.Classification == 'Res' ||  input.Classification == 'TestTrain'  ||
                                      input.Classification == 'ClassPerf' ||  input.Classification == 'ROut' ",      
                        selectInput("method", label = h5( strong("Classification method")),
                                                    choices = c( "LDA","QDA", "Logistic regression", "Firth logistic regression", "Multinomial logistic regression" #, 
                                                                 #"kNN", "SVM", "Random forest", "Decision trees", "Naive Bayes classifier", "Neural networks"
                                                                 )),
                        uiOutput("VariableSelectX"),
                     #   uiOutput("VarLab"),
                        uiOutput("VariableSelectY"),

                        bsCollapse(id = "collapseExample", open = "Panel 2",
                                    bsCollapsePanel("Advanced options",
                                        numericInput("DataSplit", "Enter the training percentage split", 80, min = 1),
                                        numericInput("RandSeed", "Enter random seed number", 23, min = 0 )  )
                                   ),
                       
                        bsTooltip("DataSplit", "This number represents the percentage of the data set that is assigned to the training set", placement = "bottom", trigger = "hover",
                                                options = NULL),
                        bsTooltip("RandSeed", "To ensure the reproducibility of your results (should you wish to come back to your analysis at a later point), just use the same number for the random seed every time!", placement = "bottom", trigger = "hover",
                                                options = NULL),
                        actionButton("GoClassify", label = "Run model", icon("play"), width = '100%',
                                     style="color: #fff; background-color: #28bb9b; border-color: #87d5c5" )
                        ),
                    
                    conditionalPanel( condition = "input.Classification=='ClassPlots'", 
                                      h5( strong("Classification plot options")),
                                      uiOutput("VariableSelectClassPlot")
                        ),
                    
                    conditionalPanel( condition = "input.Classification =='pPred'",     
                        h5( strong("Predictions")),
                        fileInput("PredData", "Upload file containing new data", 
                                  accept = c( "text/csv",  "text/comma-separated-values,text/plain", ".csv")  ),
                        actionButton("GoPredUpload", "Predict", icon("file-import"), width = '100%',
                                     style="color: #fff; background-color: #28bb9b; border-color: #87d5c5" ),
                        h5( strong("Prediction plot")),
                        uiOutput("VariableSelectPlot")
                        )
                        ),
                            
            mainPanel(
                    tabsetPanel( id = 'Classification',

                    tabPanel(value = "ClassPerf",
                             h5(strong("Classification performance")),
                             verticalLayout(  
                                              column( 3, DT::dataTableOutput("ConfMat" )%>% withSpinner(color="#0dc5c1") ),
                                              tags$br(),
                                              column( 12, DT::dataTableOutput("OverKappa")%>% withSpinner(color="#0dc5c1") )),
                             tags$br(),
                             verticalLayout(  column( 12, DT::dataTableOutput("ByClass") %>% withSpinner(color="#0dc5c1") )
                             )),
                    
                    tabPanel( value = "Res",
                             h5(strong("Analysis results")),
                             verbatimTextOutput("ClassOutput")
                            ),
                    
                    tabPanel( value = "TestTrain",
                             h5(strong("Training/testing dataset")),
                             h4("Training dataset"),
                             DT::dataTableOutput("training_set")%>% withSpinner(color="#0dc5c1"),
                             h4("Testing dataset"),
                             DT::dataTableOutput("testing_set")%>% withSpinner(color="#0dc5c1")
                             #"Maybe some plots showing the percentages of data allocated to train/test"
                            ),
                        
                    tabPanel( value = "ClassPlots",
                             h5(strong("Plots")),
                                  splitLayout(
                                      plotOutput("ClassPlot") %>% withSpinner(color="#0dc5c1")
                                     # uiOutput( "ClassPlot")
                                    )
                                  ),
        
                    tabPanel( value = "pPred", 
                             h5(strong("Predictions")), 
                                  fluidRow( column( 6,
                                            h5(strong("Model predictions")), 
                                            DT::dataTableOutput("PredDataTab") %>% withSpinner(color="#0dc5c1"),
                                            tags$style(type="text/css", "#PredDataTab td:nth-child(1) {text-align:center;background-color:#ffd800;color: black;text-align:center}"),
                                            tags$style(type="text/css", "#PredDataTab td:nth-child(2) {text-align:center;background-color:#ffb000;color: black;text-align:center}") ),
                                        #    tags$br(),    
                                            column( 6,
                                            h5(strong("Plot predictions")),
                                            tags$br(),
                                            tags$br() ,
                                            plotOutput("PredPlot") %>% withSpinner(color="#0dc5c1")
                                           # tags$br() 
                                           )
                                          #"TBC - user can choose what plot they want to display the predicted data"
                                            )
                            ),

                    tabPanel( value = "ROut",
                              h5(strong("R output")),
                                    
                                    verbatimTextOutput("AnalysisRes")
                            )
                    )
            )
    )
    ),

    ########################
    # Evidence 
    tabPanel( title = "Likelihood ratio",
              icon = icon( "balance-scale"),
              
              sidebarLayout(
                  sidebarPanel(width = 3,
                        conditionalPanel( condition =  "input.EvidenceResults == 'ePlots' ||  input.EvidenceResults == 'eTable'",         
                                selectInput("EviMethod", label = h5( strong("Method")),
                                           choices = c( "Firth Glm" = "FirthGlm", "Bayes Glm" = "Bayes Glm", "Glm Net" = "GlmNet"), multiple = T),
                                uiOutput("varsYevidence"),
                                checkboxGroupInput("EviOptions",  label = h5( strong("LR estimation type")),  
                                                  choices = c( "Univariate Gaussian LR" = "gaussian", "Univariate KDE LR" = "kernel"
                                                               #, "Multivariate Gaussian LR -- coming soon" = "mgaussian"
                                                               ), selected = 1),
                                bsTooltip("EviOptions", "Select all options you want included in your comparison. For datasets with more than 6 variables only univariate LR options are available",       
                                         options = NULL),
                                h5( strong("Add other classification methods:")),
                                selectInput("addMethodsList", "", 
                                           c("LDA","QDA","Logistic regression", "Firth logistic regression"), 
                                                multiple = T ),
                                bsCollapse(id = "collapseLRCV", open = "Panel 2",
                                          bsCollapsePanel(h5( strong("Cross-validation")),
                                                          textInput("pTrain", "Enter the % of data for training", "50"),
                                                          textInput("pValid", "Enter the % of data for validation", "30"), 
                                                          textInput("pTest", "Enter the % of data for testing", "20"),
                                                          textInput("RepeatN", "Enter the number of repeated iterations", "5"),
                                                          numericInput("ESeed", "Enter random seed number", 23, min = 0 ) 
                                                          )),
                                actionButton("GoEvidence", label = "Run evidence model", icon("play"), width = '100%',
                                             style="color: #fff; background-color: #28bb9b; border-color: #87d5c5" )
                                ),
                        
                        conditionalPanel( condition =  "input.EvidenceResults == 'eSumTable' ", 
                                          h5( strong("LR results summary")),
                                          selectInput( "eSum", "Choose how to summarise the data:", choices = c("mean", "median", "min", "max"))
                                          
                                ),
                        
                        conditionalPanel( condition =  "input.EvidenceResults == 'ePred' ", 
                                h5( strong("Predictions")),
                                fileInput("EviPredData", "Upload file containing new data", 
                                         accept = c( "text/csv",  "text/comma-separated-values,text/plain", ".csv")  ),
                                checkboxInput("EviPredDataCheck", "Use the same data imported previously"),
                                selectInput("EviPredMethod", label = h5( strong("Method")),
                                            choices = c( "", "Firth Glm" = "FirthGlm", "Bayes Glm" = "BayesGlm", "Glm Net" = "GlmNet"), multiple = F),
                                selectInput("EviPredOptions",  label = h5( strong("LR estimation type")),  
                                                   choices = c( "", "Univariate Gaussian LR" = "gaussian", "Univariate KDE LR" = "kernel" ),  multiple = F),
                                actionButton("GoEviPredUpload", "Upload", icon("file-import"), width = '100%',
                                            style="color: #fff; background-color: #28bb9b; border-color: #87d5c5" )
                                )
                        
                        
                  ),
                  
                  mainPanel(
                      tabsetPanel(
                          id = 'EvidenceResults',
                          
                          tabPanel(value = "eTable",
                                   h5(strong("Analysis results")),
                                   tags$br(),
                                   "Table of performance measures using selected methods for all simulations",
                                   DT::dataTableOutput("evidence_results") %>% withSpinner(color="#0dc5c1")
                          ),
                          
                          tabPanel(value = "eSumTable",
                                   h5(strong("Analysis results summary")),
                                   tags$br(),
                                   "Summary of performance measures using selected methods",
                                   DT::dataTableOutput("evidence_results_sum") %>% withSpinner(color="#0dc5c1")
                          ),
                          
                          tabPanel(value = "ePlots",
                                   h5(strong("Plots")),
                                   fluidRow( column(6, plotOutput("EviPlots1")  %>% withSpinner(color="#0dc5c1")),
                                             column(6, plotOutput("EviPlots2")  %>% withSpinner(color="#0dc5c1"))),
                                   fluidRow( column(6, plotOutput("EviPlots3")  %>% withSpinner(color="#0dc5c1")),
                                             column(6, plotOutput("EviPlots4")  %>% withSpinner(color="#0dc5c1"))),
                                   fluidRow( column(6, plotOutput("EviPlots5")  %>% withSpinner(color="#0dc5c1")),
                                             column(6, plotOutput("EviPlots6")  %>% withSpinner(color="#0dc5c1"))),
                                   fluidRow( column(6, plotOutput("EviPlots7")  %>% withSpinner(color="#0dc5c1")))
                          ),
                          
                          tabPanel(value = "ePred",
                                   h5(strong("Model predictions")), 
                                   DT::dataTableOutput("EviPredDataTab") %>% withSpinner(color="#0dc5c1"),
                                   tags$style(type="text/css", "#EviPredDataTab td:nth-child(1) {text-align:center;background-color:#ffd800;color: black;text-align:center}"),
                                   tags$style(type="text/css", "#EviPredDataTabtd:nth-child(2) {text-align:center;background-color:#ffb000;color: black;text-align:center}") 
                          
                                    
                          )
                          
                          )
                  )
              )
    ) #,
    
    
    ###############################################################
    # Download report
    # tabPanel( title = "Report",
    #           icon = icon( "download"),
    #           "TBC",
    #           tags$br(),
    #           "Have a list of methods that the user can select to produce a report. Or a 'add to report' button throught the app",
    #           tags$br(),
    #           "Make appropriate plots and perhaps a table comparing the selected methods' results.",
    #           tags$br(),
    #           img(src="NotReady.png", height = 300)
    #           
    #           )
)

    





##############################################################################################################################################################
# Define server logic required to generate objects 
server <- function( input, output, session) {
   
   
    
    ### Data set assignment
    datasetInput <- eventReactive( input$GoData, {
		req( input$dataset )
        switch(input$dataset,
               "Select dataset" = NULL,
               "Iris" = iris,
               "Diamonds" = diamonds,
               "Glass" = Glass,
               "Diabetes" = PimaIndiansDiabetes,
               "Upload data" =  { req (input$UpData$datapath)
				   				  import( input$UpData$datapath ) }
               )
    })
    
    ### Numerical and categorical variables are identified and placed into different data sets
    NumData <- reactive ( { GetDataByClass ( dataset = datasetInput(), cls = "numeric")   } )
    #CatData <- reactive ( { GetDataByClass ( dataset = datasetInput(), cls = "factor")   } )
    
    CatData <- reactiveVal( )
    
    observeEvent(input$GoData, { 
        d <- GetDataByClass ( dataset = datasetInput(), cls = "factor")
        CatData(d)
    } )
    
    observeEvent(input$GoData, {
        claim <- CatData() # read the data from the reactiveVal
        if ( "Class" %in% colnames(claim))
        { claim$Class <- as.factor( claim$Class)
        CatData(claim) # write back the modified data to the reactiveVal
        }
    })
    
    # Upload data fileInput box
    
    output$UpSelect <- renderUI ( { 
        switch( input$dataset,
                "Upload data" = fileInput("UpData", "",
                   accept = c( "text/csv", "text/comma-separated-values,text/plain", ".csv")))
        
        })
    
    

    # Select variables for plotting 
    
    output$Variable_select <- renderUI( { 
                
        verticalLayout( 
                         
                         varSelectInput("varX", h5("X variable"), NumData() )  ,
                                 
                         varSelectInput("varZ", h5("Grouping variable"), CatData() ),

                         bsTooltip("varZ", "Choose a factor variable", placement = "top", trigger = "hover",
                                           options = NULL),
                         
                         checkboxInput( "addY", label = "Add Y variable", value = FALSE) ,
                         
                         renderUI ( { 
                             if( input$addY == TRUE)
                                 varSelectInput("varY", h5("Y variable"), NumData() ) 
                         }),
                         
                         checkboxInput( "allVars", label = "Produce overall plot", value = FALSE),
                         
                             tags$br(),
                             tags$br(),
                             tags$br()
                            )  
     })
    
    
    # Generate plots according to selected variables
    
    output$ExPlot <- renderPlot (
         {  req( input$varX) 
            
            if ( input$allVars == FALSE)
            {    
            if( input$addY == TRUE )
                ggplot(datasetInput(), aes( x = !!input$varX , y = !!input$varY, colour = !!input$varZ, shape = !!input$varZ) ) + 
                    geom_point( alpha = 0.6, size = 4) 
            else
                ggplot(datasetInput(), aes( y = !!input$varX, colour = !!input$varZ, fill = !!input$varZ) ) + 
                    geom_boxplot( alpha = 0.8 ) + coord_flip() 
            }
            else
                ggpairs(datasetInput(), mapping = aes( fill = !!input$varZ, colour = !!input$varZ, alpha =0.8),
                        upper = list(continuous = wrap("cor", hjust=0.15, alignPercent=1))) +
                theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())  # lose the grid lines
        } )


    # Generate dataset table to display the input data
    output$DataTab <- renderDataTable( { 
       
        DT::datatable (datasetInput(), rownames = F,
                       options = list(lengthMenu = c(5, 10, 20), 
                                      pageLength = 5, scrollX = TRUE,
                                      initComplete = JS(
                                          "function(settings, json) {",
                                          "$(this.api().table().header()).css({'background-color': '#556271', 'color': '#fff'});",
                                          "}")) 
                       )
        } )
    
    
    ### Generating summary statistics for numerical variables 
    output$DataSumN <- renderDataTable ( { 
        if(input$checkNum == TRUE)
        DT::datatable (  round( descr( NumData(), stats = c( "min", "q1", "med", "mean" ,"q3", "max", "sd"), 
                                transpose = TRUE, headings = FALSE, justify = "c", style = "simple"), 3),
                         options = list(  dom = 't',
                                          scrollX = TRUE,
                                          initComplete = JS(
                                              "function(settings, json) {",
                                              "$(this.api().table().header()).css({'background-color': '#556271', 'color': '#fff'});",
                                              "}"))  
                     )
         } )
    

    ### Generating summary statistics for categorical variables 
    output$CatVariableSum <- renderUI( {
        if(input$checkCat == TRUE)
            selectInput("CatVar", h5("Choose your variables"), multiple = T, names( CatData() )  )
    } )
    
    
    ## Generating summary statistics for factor variables by first generating a list of dataTables 
    ## based on the number of variables selected
    observeEvent(input$CatVar,  output$Freq <- renderUI({
                     lapply( as.list( seq_len( length( input$CatVar ) )), function(i) {
                         id <- paste0("Freq", i)
                         DT::dataTableOutput( id )
                     })
                 })  )
    
    ## In each position of the list output$id, generate the id name and table containing summary statistics 
    ## for the categorical variables selected in the "CatVar" box
    # !
    observeEvent(input$CatVar, {
                 req( input$CatVar)
                 for (i in 1: length( input$CatVar )  ) {
                     id <- paste0( "Freq", i)
                     f <- freq( CatData() [ , input$CatVar[i] ], report.nas = F, headings = F,
                                cumul = F, justify = "c", totals = F, style = "grid")
                     f <- data.frame(f)
                     f <- round( f[  -( nrow(f) -1  ), c(1,2,3) ], 3)
                     colnames(f) <- c("Frequency", "%", "Cumulative %")
                     output[[id]] <- DT::renderDataTable( f, caption = input$CatVar[i], 
                                                          options = list( dom = 't',
                                                                          initComplete = JS(
                                                                              "function(settings, json) {",
                                                                              "$(this.api().table().header()).css({'background-color': '#556271', 'color': '#fff'});",
                                                                              "}") ) )
                    }
                }
    )
    
    ############## Analysis tab ----------
    
    # Initialize the object that contains the model outputs and inputs ----
    ClassRes <- reactiveValues( )
       ClassRes$model <- NULL
       ClassRes$method <- NULL
       ClassRes$data <- NULL
       ClassRes$training_dataset <- NULL
       ClassRes$testing_dataset  <- NULL
       ClassRes$testing_result   <- NULL
       ClassRes$seed <- NULL    # make output dependant on seed if user chooses to input a seed 
       ClassRes$confusion_matrix <- NULL
       ClassRes$prediction <- NULL

    # Selecting variables for class and predictors 
       
    output$VariableSelectX <- renderUI( { 
           verticalLayout(
               selectInput("varXm", h5("Choose class variable"), names(CatData())  ),
               bsTooltip("varXm", "Choose a factor (labels) variable", placement = "top", trigger = "hover",
                         options = NULL)
               # if ( input$method == "Logistic regression")
               #      selectInput("varXlevel", h5("Choose level"), levels( datasetInput()[, input$varXm] )  )
           )
       } )
       
    output$VariableSelectY <- renderUI( { 
           selectInput("varYm", h5("Choose predictors"), multiple = T, names(datasetInput()) [ names(datasetInput()) != input$varXm] )
       } )
       
   
    # Set Seed
    observeEvent( input$RandSeed,  
                 {ClassRes$seed <- input$RandSeed
                  set.seed( input$RandSeed )
                })
    
    #### Error notifications for classification tab: training percentage choice, random seed choice and choosing predictors
    observeEvent(input$GoClassify, {

    # Conditions for error/warning messages -----------------------------------

    
        if ( is.null( input$varYm) )
            showNotification(  "Choose at least one predictor from the list!", duration = 2, type = "error")
    
        if ( input$DataSplit >100 | input$DataSplit <=0 )
            showNotification(  "Your training dataset should be greater than 0 and less than 100% of your data!", duration = 2, type = "error")
        
        if ( input$DataSplit ==100  )
            showNotification(  "Your whole data set is used to train the classifier, which might lead to overfitting!", duration = 5, type = "warning")
        
        if ( input$RandSeed %% 1 != 0 |  input$RandSeed <=0)
            showNotification(  "The random seed should be a positive integer!", duration = 2, type = "error")
        
        if( input$method == "Multinomial logistic regression"  &  length( levels( CatData()[, input$varXm] )  ) < 3 ) 
            showNotification(  "Multinomial logistic regression can only be applied to variables with more than 2 classes!", duration = 2, type = "error")
        
        if( (input$method == "Logistic regression" | input$method == "Firth logistic regression") &  length( levels( CatData()[, input$varXm] )  ) != 2 ) 
            showNotification(  "This method can only be applied to data with a binary response variable!", duration = 2, type = "error")
        
           
    })
    
    
    # Selecting method ------
    observeEvent( input$GoClassify,  {
        
        # Ensure user selects required options ------------------------------------------------------
        req(input$varXm, input$varYm, input$method, input$DataSplit <=100, input$DataSplit >0, 
            input$RandSeed %% 1 == 0, input$RandSeed >0)
        
        # set seed to whatever the user input------------------------------------------------------
        set.seed(ClassRes$seed )
        
        # split data set into testing/training ------------------------------------------------------
        ClassRes$data <- DataTrainTest( data = datasetInput(), per = input$DataSplit)
        ClassRes$training_dataset <- as.data.frame(ClassRes$data[1])
        ClassRes$testing_dataset  <- as.data.frame(ClassRes$data[2])



        # fit selected model ------------------------------------------------------

    
        switch(input$method,
               "LDA" =  {ClassRes$model <-  RunLDA( input$varXm, input$varYm, ClassRes$training_dataset  )
                         ClassRes$testing_result <- EvaluateLDA( ClassRes$model, ClassRes$testing_dataset )
                         ClassRes$method <- input$method},

               "QDA" =  {ClassRes$model <-  RunQDA( input$varXm, input$varYm, ClassRes$training_dataset  )
                         ClassRes$testing_result <- EvaluateQDA( ClassRes$model, ClassRes$testing_dataset )
                         ClassRes$method <- input$method},

               "Logistic regression" = {req(  length( levels( CatData()[, input$varXm] ) ) == 2 ) 
                                        ClassRes$model <-  try( RunLR( input$varXm, input$varYm, ClassRes$training_dataset  ) )
                                        ClassRes$testing_result <- try( EvaluateLR( ClassRes$model, ClassRes$testing_dataset ) )
                                        ClassRes$method <- input$method },

               "Firth logistic regression" = {req(  length( levels( CatData()[, input$varXm] ) ) == 2 )
                                              ClassRes$model <-  try( RunLRF( input$varXm, input$varYm, ClassRes$training_dataset ) )
                                              ClassRes$testing_result <- try( EvaluateLRF( ClassRes$model, ClassRes$testing_dataset ) )
                                              ClassRes$method <- input$method },

               "Multinomial logistic regression" =  {req( length( levels( CatData()[, input$varXm] ) ) > 2 )
                                                     ClassRes$model <-  try( RunMLR( input$varXm, input$varYm, ClassRes$training_dataset ) )
                                                     ClassRes$testing_result <- try( EvaluateMLR( ClassRes$model, ClassRes$testing_dataset ) )
                                                     ClassRes$method <- input$method } #, 
               # "kNN" = {ClassRes$model <- RunKNN(  )}, 
               # "SVM" = {ClassRes$model <- RunSVM(   )}, 
               # "Random forest" = {ClassRes$model <- RunRF(   )}, 
               # "Decision trees" = {ClassRes$model <- RunDC(   )}, 
               # "Naive Bayes classifier" = {ClassRes$model <- RunNB(   )}, 
               # "Neural networks" = {ClassRes$model <- RunNN(   )} 
               )
    
    })

    # Results - show specific output for each method !
    # currently displays a summary of each model
    observeEvent( input$GoClassify, {
    output$ClassOutput <- renderPrint ({
        req( ClassRes$method )
        switch( ClassRes$method,
               "LDA" =  { print( ClassRes$model)  },
               "QDA" =  { print( ClassRes$model)  },
               "Logistic regression" = { print( summary( ClassRes$model) ) },
               "Firth logistic regression" = { print( summary( ClassRes$model) )  },
               "Multinomial logistic regression" =  {   print( summary( ClassRes$model) )
                                                        print(  "p-values ")
                                                        print( ClassRes$model$pval )  } #,
               # "kNN" = {print("TBC") },
               # "SVM" = {print("TBC") },
               # "Random forest" = { print("TBC") },
               # "Decision trees" = {print("TBC") },
               # "Naive Bayes classifier" = {print("TBC") },
               # "Neural networks" = {print("TBC") }
        )
    }) 
    })

    
    # Render model output for the last tab
    output$AnalysisRes <- renderPrint( {   
         print( paste ("Random seed set to: ", ClassRes$seed))
         print( summary(ClassRes$model) )
         print( "Prediction")
         print( ClassRes$prediction ) 
       })
    
    
    # Import data set for prediction
    datasetPred <- eventReactive( input$GoPredUpload, 
        { req( input$PredData$datapath, input$GoPredUpload  )
          datasetPred <- import( input$PredData$datapath )        
    })
    
    #### Error notifications for classification, prediction tab: 
    observeEvent(input$GoPredUpload, {
        if ( FALSE %in% ( names( datasetPred() ) %in%  names( datasetInput() ) )  )
            showNotification(  "The variables in the uploaded dataset do not match the dataset used for training!", duration = 10, type = "error")
    })
    
    
    # Predict for selected methods
    observeEvent( input$GoPredUpload,  {
        
        # requirements 
        req ( class( ClassRes$model) != "try-error", 
              names( datasetPred() ) %in%  names( datasetInput() ),
              names( ClassRes$training_dataset) %in%  names( datasetInput() ) )
        
        switch(input$method,
               "LDA" =  {ClassRes$prediction <-  EvaluateLDA( ClassRes$model,  datasetPred() ) },
               "QDA" =  {ClassRes$prediction <-  EvaluateQDA( ClassRes$model,  datasetPred() ) },
               "Logistic regression" = {ClassRes$prediction <-  EvaluateLR( ClassRes$model,  datasetPred() ) },
               "Firth logistic regression" = {ClassRes$prediction <-  EvaluateLRF( ClassRes$model,  datasetPred() ) },
               "Multinomial logistic regression" =  {ClassRes$prediction <-  EvaluateMLR( ClassRes$model,  datasetPred() ) }
               # "kNN" = {  },
               # "SVM" = {  },
               # "Random forest" = { },
               # "Decision trees" = { },
               # "Naive Bayes classifier" = { },
               # "Neural networks" = { } 
               )
        } )
    
    ### Generate dataset table to display the data uploaded for prediction
    output$PredDataTab <- renderDataTable( {
        req( ClassRes$prediction$class, names( datasetPred() ) %in%  names( datasetInput() ) )
        dataset <- cbind( Prediction = ClassRes$prediction$class, LR =  round(ClassRes$prediction$LR, 5), datasetPred() )
        out <- tryCatch( 
                DT::datatable (dataset, rownames = F,
                       options = list(lengthMenu = c(5, 10, 15),
                                      pageLength = 5, scrollX = TRUE,
                                      initComplete = JS(
                                          "function(settings, json) {",
                                          "$(this.api().table().header()).css({'background-color': '#556271', 'color': '#fff'});",
                                          "}")  ),
                       callback = JS("var tips = [             
                                                'Predicted class label using the model chosen',
                                                'Likelihood Ratio - currently only available for binary classification'
                                                ],
                                     header = table.columns().header();
                                     for (var i = 0; i < 2; i++) {
                                     $(header[i]).attr('title', tips[i]);
                                     }
                                     ")),
                   error = function(e) NULL)
        return( out )
    } )
    
    
    # Show the training and testing data sets in different tables
    output$training_set <- renderDataTable( {
        DT::datatable (ClassRes$training_dataset,
                        options = list(lengthMenu = c(5, 10, 20),
                                        pageLength = 5,
                                        initComplete = JS(
                                           "function(settings, json) {",
                                           "$(this.api().table().header()).css({'background-color': '#556271', 'color': '#fff'});",
                                           "}")  ))
    } )
    
    output$testing_set <- renderDataTable( {
        DT::datatable (ClassRes$testing_dataset,
                       options = list(lengthMenu = c(5, 10, 20),
                                      pageLength = 5,
                                      initComplete = JS(
                                          "function(settings, json) {",
                                          "$(this.api().table().header()).css({'background-color': '#556271', 'color': '#fff'});",
                                          "}")  ))
    } )

    # Classification measures and confusion matrix 
    output$ConfMat   <- renderDataTable( {
        req( class(ClassRes$model) != "try-error")
        req( ClassRes$testing_dataset, input$varXm %in% names(ClassRes$testing_dataset) )
        
        cm <- confusionMatrix(  reference = as.factor(ClassRes$testing_dataset[ , input$varXm]) , data = as.factor( ClassRes$testing_result$class ) )
        mtable <- NULL
        for ( i in 1 :  length( levels( as.factor(ClassRes$testing_dataset[ , input$varXm]) )))
            mtable <- rbind( mtable, cm$table[,i] )
        rownames(mtable) <- colnames(cm$table)
        DT::datatable( mtable, options = list(ordering=F, dom = 't'),
                       caption = "Confusion matrix")

})

    output$OverKappa <- renderDataTable( {
    req( class(ClassRes$model) != "try-error") 
    req(   ClassRes$testing_dataset, input$varXm %in% names(ClassRes$testing_dataset) )
        
    cm <- confusionMatrix(  reference = as.factor(ClassRes$testing_dataset[ , input$varXm]), data = as.factor( ClassRes$testing_result$class ) )
    DT::datatable( t(round( cm$overall,3)), rownames = T,
                   options = list(ordering=F, dom = 't', scrollX = TRUE,
                                  initComplete = JS(
                                      "function(settings, json) {",
                                      "$(this.api().table().header()).css({'background-color': '#556271', 'color': '#fff'});",
                                      "}")      ),
                   caption = "Overall accuracy and Kappa statistic",
                   callback = JS(" var tips = ['Row Names',
                                                ' ', ' '],
                                 header = table.columns().header();
                                 for (var i = 0; i < tips.length; i++) {
                                 $(header[i]).attr('title', tips[i]);
                                 }
                                 "))
})

    output$ByClass  <- renderDataTable( {
    req( class(ClassRes$model) != "try-error")
    req(   ClassRes$testing_dataset, input$varXm %in% names(ClassRes$testing_dataset) )
        
     cm <- confusionMatrix(  reference = as.factor( ClassRes$testing_dataset[ , input$varXm] ), data = as.factor( ClassRes$testing_result$class ) )
    cmc <- round ( cm$byClass, 3)
    
    # for binary data the output needs to be converted to a matrix from a vector
    if ( class (cmc) == "numeric")
        cmc <- matrix( cmc, ncol = 11, dimnames = list( " ", names(cmc) ))

    DT::datatable( cmc , rownames = T, 
                   caption = "Classification statistics by class",
                   options = list( ordering=F, scrollX = TRUE,  dom = 't',
                                   initComplete = JS(
                                       "function(settings, json) {",
                                       "$(this.api().table().header()).css({'background-color': '#556271', 'color': '#fff'});",
                                       "}")),
                   callback = JS("
            var tips = ['',
                        'Sensitivity (true positive rate or recall): is the fraction of relevant instances that have been retrieved over the total amount of relevant instances. A/(A+C)',
                        'Specificity (true negative rate):  D/(B+D)',
                        'Positive Predictive Value (Precision): proportions of positive results that are true positive results.   (sensitivity * prevalence)/((sensitivity*prevalence) + ((1-specificity)*(1-prevalence)))',
                        'Negative Predictive Value: proportions of negative results that are true negative results   (specificity * (1-prevalence))/(((1-sensitivity)*prevalence) + ((specificity)*(1-prevalence))) ',
                        ' ',
                        ' ',
                        'F1: a measure of a test`s accuracy based on the precision and recall. A value of 1 indicates perfect precision and recall while the worst is indicated by a value close to 0. (1+beta^2)*precision*recall/((beta^2 * precision)+recall), where beta = 1 for this function ',
                        'Prevalence: the proportion of the population that has been correctly classified.  (A+C)/(A+B+C+D)',
                        'Detection Rate: the proportion of objects in a sample with a particular label who were correctly classified.   A/(A+B+C+D)  ',
                        'Detection Prevalence: the proportion of objects in a sample that were assigned a particular label.  (A+B)/(A+B+C+D)  ',
                        'Balanced Accuracy: measures the accuracy of the overall model performance.   (sensitivity+specificity)/2  '
                        ],
                                 header = table.columns().header();
                                 for (var i = 0; i < tips.length; i++) {
                                 $(header[i]).attr('title', tips[i]);
                                 }
                                 "))
 })
    
    # Generate plots for classification data according to selected variables
    
    output$VariableSelectClassPlot <- renderUI( { 
        verticalLayout(
            varSelectInput("varXc", h5("Choose variable for x axis"),  datasetInput()  ),
            varSelectInput("varYc", h5("Choose variable for y axis"),  datasetInput()  ),
            varSelectInput("varCc",  h5("Choose variable for point colour"), CatData()  )
        )
    } )
    
    # Generate plots for predicted data according to selected variables
    
    output$VariableSelectPlot <- renderUI( { 
        verticalLayout(
            varSelectInput("varXp", h5("Choose variable for x axis"),  datasetInput()  ),
            varSelectInput("varYp", h5("Choose variable for y axis"),  datasetInput()  ),
            varSelectInput("varC",  h5("Choose variable for point colour"), CatData()  )
            )
    } )
    
    
    # Classification data plot
    output$ClassPlot <- renderPlot (
        {   req( class(ClassRes$model) != "try-error")
            req( input$varXm, input$varXc, input$varYc, input$varCc,  
                 ClassRes$training_dataset,  ClassRes$testing_dataset, ClassRes$testing_result$class,
                 names( datasetInput() ) %in% names( ClassRes$training_dataset) )
            
            # Training dataset points
            set1 <- ClassRes$training_dataset
            set1[, input$varXm ] <- factor( set1[,input$varXm ], ordered = F )
          
            # Testing dataset points with the model predicted class
            set2 <- ClassRes$testing_dataset 
      
            set2[, input$varXm] <- ClassRes$testing_result$class
            
            # Make sure both factors are unordered otherwise subsetting will cause problems if one of the cateogries doens't have all the levels as the other
            # a factor can only be compared to another factor with an identical set of levels
            set3 <- subset( ClassRes$testing_dataset, factor( ClassRes$testing_dataset [, input$varXm ], ordered = F) != ClassRes$testing_result$class)
            set3[, input$varXm ] <- factor( set3[,input$varXm ], ordered = F )
           
            # Show different plots (training, prediction, missclassified points) or 
            # just missclassified observations based on training dataset split
            if (  input$DataSplit < 100) { 
                if ( nrow(set3) != 0 )
                  
                    ggplot( data = set1, aes ( x = !!input$varXc , y = !!input$varYc) ) +
                    geom_point( alpha = 0.5, size = 2, aes( colour = !!input$varCc, shape = '20')) +
                    geom_point( data = set2, aes( x = !!input$varXc  , y =  !!input$varYc, colour = !!input$varCc, shape = '8'), size = 4) +
                    geom_point( data = set3, aes( x = !!input$varXc , y =  !!input$varYc , colour =!!input$varCc, shape ='diamond open'), size = 3) +
                    scale_shape_manual(name = 'Data', guide = 'legend', labels = c('training', 'testing', 'misclassified'), values = c('circle', 'asterisk', 'diamond open')) 
                else
                    ggplot( data = set1, aes ( x = !!input$varXc , y = !!input$varYc) ) +
                    geom_point( alpha = 0.5, size = 2, aes( colour = !!input$varCc, shape = '20')) +
                    geom_point( data = set2, aes( x = !!input$varXc , y =  !!input$varYc, colour = !!input$varCc, shape = '8'), size = 4) +
                    scale_shape_manual(name = 'Data', guide = 'legend', labels = c('training', 'testing'), values = c('circle', 'asterisk')) 
                    
            }
            else {
              if ( input$DataSplit == 100){ 
                if ( nrow(set3) != 0 )
                     ggplot( data = set1, aes ( x = !!input$varXc , y = !!input$varYc) ) +
                     geom_point( alpha = 0.5, size = 2, aes( colour = !!input$varCc, shape = '20')) +
                     geom_point( data = set3, aes( x = !!input$varXc , y =  !!input$varYc , colour =!!input$varCc, shape ='diamond open'), size = 3) +
                     scale_shape_manual(name = 'Data', guide = 'legend', labels = c('training', 'misclassified'), values = c('circle', 'diamond open'))
                 
                else
                    ggplot( data = set1, aes ( x = !!input$varXc , y = !!input$varYc) ) +
                    geom_point( alpha = 0.5, size = 2, aes( colour = !!input$varCc, shape = '20')) +
                    scale_shape_manual(name = 'Data', guide = 'legend', labels = c('training'), values = c('circle', 'asterisk'))
             }}

    } )
    
    # Prediction data plot
    output$PredPlot <- renderPlot(
        {  
           # requirements 
           req( input$varXm, input$varXp, input$varYp, input$varC,  
                names( datasetPred() ) %in%  names( datasetInput() ),  
                ClassRes$prediction)
            
            predSet <- cbind ( datasetPred(), ClassRes$prediction$class )
            colnames( predSet) [ length( colnames( predSet ))] <- input$varXm
            ggplot( data = ClassRes$training_dataset, aes ( x = !!input$varXp , y = !!input$varYp) ) + 
                geom_point( alpha = 0.3, size = 2, aes( colour = !!input$varC, shape = '20')) +
                geom_point( data = predSet, aes( x = !!input$varXp , y =  !!input$varYp, colour = !!input$varC, shape = '8'), size = 4) +
                scale_shape_manual(name = 'Data', guide = 'legend', labels = c('training', 'predicted'), values = c(20, 8)) +
                theme(legend.position="bottom", legend.box = "vertical", legend.text=element_text(size=10))

        } )

    ###### Evidence tab -----
    
    # Initialize the object that contains the model outputs and inputs
    EviRes <- reactiveValues()
    EviRes$model <- NULL
    EviRes$data <- NULL
    EviRes$training_dataset <- NULL
    EviRes$testing_dataset  <- NULL
    EviRes$validation_dataset  <- NULL
    EviRes$testing_result   <- NULL
    EviRes$seed <- NULL    # make output dependant on seed if user chooses to input a seed 
    EviRes$predm <- data.frame()
    EviRes$prediction <- NULL

    output$varsYevidence <- renderUI ( {
        verticalLayout( 
            selectInput("varXe", label = h5( strong("Response variable")),
                            names(CatData()) ),
            varSelectInput("varYe", label = h5( strong("Variables")),
                           NumData(), multi = T  )    
        )
    })
    
    observeEvent( input$GoEvidence, {
        
    # Requirements so app doesn't crash or show errors
    req( input$varXe,  length( input$varYe ) > 0, input$EviMethod, input$EviOptions, length ( levels( CatData()[, input$varXe] ) ) == 2,
         as.numeric(input$pTrain) > 0  , as.numeric(input$pValid) > 0  , as.numeric(input$pTest) > 0,
         as.numeric(input$pTrain) < 100, as.numeric(input$pValid) < 100, as.numeric(input$pTest) < 100,
         as.numeric(input$RepeatN) > 0,  as.numeric(input$RepeatN) %% 1 == 0 )
    
    # set random seed  
    EviRes$seed <- input$ESeed
    set.seed(EviRes$seed )  
    
    EviRes$predm <- data.frame()    
    # Repetitions loop        
    for ( i in 1 :  input$RepeatN ){
        
        # split data set into testing/training
        EviRes$data <- DataTrainTestValid( input$varXe, data = datasetInput(), ptrain = as.numeric(input$pTrain), pvalid = as.numeric(input$pValid), ptest = as.numeric(input$pTest))
        EviRes$training_dataset   <- as.data.frame( EviRes$data[[1]])
        EviRes$testing_dataset    <- as.data.frame( EviRes$data[[2]])
        EviRes$validation_dataset <- as.data.frame( EviRes$data[[3]])
        
        # for each LR method, produce and store performance measure
        for ( j in 1 : length( input$EviMethod) ) {
            for( k in 1 : length( input$EviOptions) ) {
                p <- try (EvRun( EviRes$training_dataset, EviRes$validation_dataset, EviRes$testing_dataset, input$varXe, input$varYe, input$EviMethod[j], input$EviOptions[k], fmode = "comparison") )
                if ( class(p) == "data.frame" )
                    EviRes$predm  <- rbind( EviRes$predm, p)
                #print(input$EviMethod[j] )
            }
        }
        
        # Run other classification methods as requested by user
        if ( length( input$addMethodsList ) >=1 )
        {
            req( input$addMethodsList ) 
            for ( i in 1: length( input$addMethodsList ) )
                switch(input$addMethodsList[i],
                       "LDA" =  {   EviRes$model <-  try( RunLDA( input$varXe, input$varYe, rbind( EviRes$training_dataset, EviRes$validation_dataset) ))
                                    EviRes$testing_result <- try( EvaluateLDA( EviRes$model, EviRes$testing_dataset ))
    
                                    # Get classification measures for test data
                                    cm <- comp_measures ( actual_class = EviRes$testing_dataset [, input$varXe], model_prediction = EviRes$testing_result$class, LR =  EviRes$testing_result$LR)
                                    p <- data.frame(  t(cm),  Method = "LDA",  EstimationType = "Classification" )
                                    #print( p)
                                    if ( class(p) == "data.frame" )
                                        # Add to prediction matrix along all results 
                                        EviRes$predm  <- rbind( EviRes$predm, p)
                                },
                       
                       "QDA" =  {   EviRes$model <-  try(RunQDA( input$varXe, input$varYe, rbind( EviRes$training_dataset, EviRes$validation_dataset)  ))
                                    EviRes$testing_result <- try( EvaluateQDA( EviRes$model, EviRes$testing_dataset ))
                                    
                                    # Get classification measures for test data
                                    cm <- comp_measures ( actual_class = EviRes$testing_dataset [, input$varXe], model_prediction = EviRes$testing_result$class, LR =  EviRes$testing_result$LR)
                                    p <- data.frame(  t(cm),  Method = "QDA",  EstimationType = "Classification" )
                                    
                                    if ( class(p) == "data.frame" )    
                                        # Add to prediction matrix along all results 
                                        EviRes$predm  <- rbind( EviRes$predm, p)
                                    },
                       
                       "Logistic regression" = {
                                    EviRes$model <-  try( RunLR( input$varXe, input$varYe, rbind( EviRes$training_dataset, EviRes$validation_dataset)  ))
                                    EviRes$testing_result <- try( EvaluateLR( EviRes$model, EviRes$testing_dataset ) )
                                    print("logistic regression")
                                    # print(EviRes$model )
                                    # print(EviRes$testing_result )
                                    # Get classification measures for test data
                                    cm <- comp_measures ( actual_class = EviRes$testing_dataset [, input$varXe], model_prediction = EviRes$testing_result$class, LR =  EviRes$testing_result$LR)
                                    p <- data.frame(  t(cm),  Method = "LogReg Class",  EstimationType = "Classification" )
                                   # print(p)
                                    if ( class(p) == "data.frame" )    
                                        # Add to prediction matrix along all results 
                                        EviRes$predm  <- rbind( EviRes$predm, p)
                                    },
                       
                       "Firth logistic regression" = {
                                    EviRes$model <-  try( RunLRF( input$varXe, input$varYe, rbind( EviRes$training_dataset, EviRes$validation_dataset) ))
                                    EviRes$testing_result <- try( EvaluateLRF( EviRes$model, EviRes$testing_dataset ) )
                                    #print(EviRes$testing_result )
                                    # Get classification measures for test data
                                    cm <- comp_measures ( actual_class = EviRes$testing_dataset [, input$varXe], model_prediction = EviRes$testing_result$class, LR =  EviRes$testing_result$LR)
                                    p <- data.frame(  t(cm),  Method = "Firth Class",  EstimationType = "Classification" )
                                   # print(p)
                                    if ( class(p) == "data.frame" )
                                        # Add to prediction matrix along all results 
                                        EviRes$predm  <- rbind( EviRes$predm, p)
                                    })
            }
                   
    }
})

    
    ### Evidence Prediction tab ------
    # Import data set for prediction
    datasetEviPred <- eventReactive( input$GoEviPredUpload, 
                                  { req( input$EviPredData$datapath, input$GoEviPredUpload  )
                                    if( input$EviPredDataCheck == TRUE ) 
                                        datasetEviPred <- datasetPred ()
                                    else    
                                        datasetEviPred <- import( input$EviPredData$datapath )        
                                  })
    
    
    #### Error notifications for classification, prediction tab: 
    observeEvent(input$GoEviPredUpload, {
        if ( FALSE %in% ( names( datasetEviPred() ) %in%  names( datasetInput() ) )  )
            showNotification(  "The variables in the uploaded dataset do not match the dataset used for training!", duration = 10, type = "error")
        
        # if (  input$EviPredDataCheck == TRUE &  class( try( datasetPred())) != "try-error" )  
        #     showNotification( "No dataset was previously imported for prediction in the Classification Prediction tab!", duration = 2, type = "error")
        # 
        
        if( input$EviPredMethod == FALSE | input$EviPredOptions == FALSE )
            showNotification( "Choose options for the prediction method!", duration = 2, type = "error")
        })
    
  
    # Evidence Prediction for selected method -----
    observeEvent( input$GoEviPredUpload,  {
        
        EviRes$prediction <- NULL
        
        # requirements
        req ( names( datasetEviPred() ) %in%  names( datasetInput() ),
              input$EviPredMethod, 
              input$EviPredOptions)

        p <- try ( EvRun( datasetInput(), datasetInput(), datasetEviPred(), input$varXe, input$varYe, input$EviPredMethod, input$EviPredOptions, fmode = "prediction") )
        if ( class(p) == "data.frame" )
            EviRes$prediction  <- p

    } )
    
    
    ### Generate dataset table to display the data uploaded for prediction
    output$EviPredDataTab <- renderDataTable( {
        
        req( EviRes$prediction, 
             names( datasetEviPred() ) %in%  names( datasetInput() ) )
        
        evd <- cbind( Prediction = EviRes$prediction$class, LR =  prettyNum( EviRes$prediction$LR, format = "fg", digits =3), datasetEviPred() ) 
        DT:: datatable ( evd,  rownames = F,
                         options = list(lengthMenu = c(5, 10, 15),
                                        pageLength = 5, scrollX = TRUE,
                                        initComplete = JS(
                                            "function(settings, json) {",
                                            "$(this.api().table().header()).css({'background-color': '#556271', 'color': '#fff'});",
                                            "}")  ))
        
        
        # req( ClassRes$prediction$class, names( datasetPred() ) %in%  names( datasetInput() ) )
        # dataset <- cbind( Prediction = ClassRes$prediction$class, LR =  round(ClassRes$prediction$LR, 5), datasetPred() )
        # out <- tryCatch( 
        #     DT::datatable (dataset, rownames = F,
        #                    options = list(lengthMenu = c(5, 10, 15),
        #                                   pageLength = 5, scrollX = TRUE,
        #                                   initComplete = JS(
        #                                       "function(settings, json) {",
        #                                       "$(this.api().table().header()).css({'background-color': '#556271', 'color': '#fff'});",
        #                                       "}")  ),
        #                    callback = JS("var tips = [             
        #                                  'Predicted class label using the model chosen',
        #                                  'Likelihood Ratio - currently only available for binary classification'
        #                                  ],
        #                                  header = table.columns().header();
        #                                  for (var i = 0; i < 2; i++) {
        #                                  $(header[i]).attr('title', tips[i]);
        #                                  }
        #                                  ")),
        #     error = function(e) NULL)
        # return( out )
    } )
    
    ## 

    
    ### Error notifications for evidence tab: 
    observeEvent( input$GoEvidence, {
        if ( length ( levels( CatData()[, input$varXe] ) ) != 2 )
            showNotification(  "The methods available at the moment can only deal with binary data!", duration = 2, type = "error")

        if ( ! ( length( input$varYe ) > 0 ) )
            showNotification(  "Choose at least one predictor from the list!", duration = 2, type = "error")
        
        if ( is.null( input$EviMethod ) )
            showNotification(  "Choose at least one estimation method from the list!", duration = 2, type = "error")

        if ( is.null( input$EviOptions ) )
            showNotification(  "Choose at least one LR estimation method from the list!", duration = 2, type = "error")
        
        if ( as.numeric(input$pTrain) <= 0 | as.numeric(input$pValid) <= 0 | as.numeric(input$pTest) <= 0 |
             as.numeric(input$pTrain) >= 100 | as.numeric(input$pValid) >= 100 | as.numeric(input$pTest) >= 100 )
            showNotification(  "Percentage allocation of any of the training, validation and testing datasets has to be greater than 0 and less than 100% ", duration = 5, type = "error")
        
        if ( as.numeric(input$RepeatN) <= 0 )
            showNotification(  "The number of repetitions has to be a positive integer!", duration = 2, type = "error")
        
        if ( input$ESeed %% 1 != 0 |  input$ESeed <=0)
            showNotification(  "The random seed should be a positive integer!", duration = 2, type = "error")
        
        
        
    })

    # Displaying a summary table of the computed measures for all selected methods and estimation types
    output$evidence_results_sum <- renderDataTable( {
        req( dim(EviRes$predm)[1] > 0, isolate( input$GoEvidence), input$eSum )
        cm <- EviRes$predm
        #iris %>% group_by(Species) %>% summarise_all(list(M=mean, Med=median))
        # c
        cms <-  cm %>% group_by( Method, EstimationType) %>% summarise_all( funs( !!input$eSum), na.rm = TRUE ) %>% mutate_if( is.numeric, round, 3)
        #print(cms)
        DT::datatable ( cms , 
                        rownames= FALSE, 
                        caption = paste("LR summary results table using: ", input$eSum ),
                        options = list(dom = "t", scrollX = TRUE,
                                       initComplete = JS(
                                           "function(settings, json) {",
                                           "$(this.api().table().header()).css({'background-color': '#556271', 'color': '#fff'});",
                                           "}")  ))
    } )

    
    # Displaying a table of the computed measures for all selected methods and estimation types
    output$evidence_results <- renderDataTable( {
        req( dim(EviRes$predm)[1] > 0, isolate( input$GoEvidence) )
        cm <- EviRes$predm
        DT::datatable ( cbind( cm[, 8:9], round( cm[,1:7], 3) ),
                        rownames= FALSE,
                        options = list(lengthMenu = c(5, 10, 20),
                                       pageLength = 5,
                                       initComplete = JS(
                                           "function(settings, json) {",
                                           "$(this.api().table().header()).css({'background-color': '#556271', 'color': '#fff'});",
                                           "}")  ))
    } )

    
    ##### Evidence Comparison plots ----
    ## Plots of the measures computed: Generating the output list for the plots for each measure computed
    observeEvent(input$GoEvidence,
                 output$EviPlots <- renderUI({
                     req( EviRes$predm )
                     lapply( as.list( seq_len( 7 )), function(i) {
                         id <- paste0("EviPlots", i)
                         plotOutput( id )
                     })
                 })  )
    
    observeEvent(input$GoEvidence, {
        req( dim(EviRes$predm)[1] >0 )
        
        output$EviPlots1 <- renderPlot( {
            CM <- EviRes$predm
            ggplot( CM, aes( x =  Method, y = Precision ) ) +
                geom_boxplot( aes(fill =  EstimationType, col =  EstimationType), alpha =0.7) + theme(legend.position="bottom")
        })
        
        output$EviPlots2 <- renderPlot( {
            CM <- EviRes$predm
            ggplot( CM , aes( x =  Method, y = Recall ) ) +
                geom_boxplot( aes(fill =  EstimationType, col =  EstimationType), alpha =0.7) + theme(legend.position="bottom")
        })
        
        output$EviPlots3 <- renderPlot( {
            CM <- EviRes$predm
            ggplot( CM , aes( x =  Method, y = Specificity ) ) +
                geom_boxplot( aes(fill =  EstimationType, col =  EstimationType ), alpha =0.7) + theme(legend.position="bottom")
        })
        
        output$EviPlots4 <- renderPlot( {
            CM <- EviRes$predm
            ggplot( CM , aes( x =  Method, y = Accuracy ) ) +
                geom_boxplot( aes(fill =  EstimationType, col =  EstimationType ), alpha =0.7) + theme(legend.position="bottom")
        })
        
        output$EviPlots5 <- renderPlot( {
            CM <- EviRes$predm
            ggplot( CM , aes( x =  Method, y = F1 ) ) +
                geom_boxplot( aes(fill =  EstimationType, col =  EstimationType ), alpha =0.7 ) + theme(legend.position="bottom")
        })
        
        output$EviPlots6 <- renderPlot( {
            CM <- EviRes$predm
            ggplot( CM , aes( x =  Method, y = MissClassification ) ) +
                geom_boxplot( aes(fill =  EstimationType, col =  EstimationType ), alpha =0.7 ) + theme(legend.position="bottom")
        })
        
        output$EviPlots7 <- renderPlot( {
            CM <- EviRes$predm
            ggplot( CM , aes( x =  Method, y = Ece ) ) +
                geom_boxplot( aes(fill =  EstimationType, col =  EstimationType) , alpha =0.7 ) + theme(legend.position="bottom")
        })
    })
    

}

# Run the application 
shinyApp( ui = ui, server = server)



