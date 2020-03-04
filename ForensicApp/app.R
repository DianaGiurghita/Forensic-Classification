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

# set working directory and source functions_rshiny.R
source("functions_rshiny.R")
st_options("round.digits")
data("Glass")
data("BreastCancer")
data("PimaIndiansDiabetes")

#theme_set(theme_bw(base_size = 18))

#ggthemr('light', layout = 'clear', spacing = 1)
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
                                   tags$h1("Introduction"),
                                   tags$body("This app was produced in June 2019..."),
                                   tags$h1("Aim"),
                                   tags$body("This app provides a flexible tool for classification to be used with forensic data and beyond.
                                              The app allows the user to input their own dataset in a .txt or .csv format."  ),
                                   tags$h1("Methodology"),
                                   tags$body("A variety of state-of-the-art classification methods are implemented in this app, which are accompanied by useful numerical and visual summaries:",
                                             tags$ul( tags$li(" "),
                                                      tags$li(" "),
                                                      tags$li(" "),
                                                      tags$li(" ")

                                                    ),
                                             " For more details about the methods available in this app, please refer to our paper ...",
                                             " You can reference this app like this ... (download reference here)."

                                             )

                                   ),

                          tabPanel("Credits",
                                   tags$br(),
                                   "We thank ", tags$em("Dimitra Eleftheriou"), " and ",
                                    tags$a( href ="https://chenjr-jacob.idv.tw", "Jacob Chen.")
                                   )
                        )

             ),

    ###############################################################
    # Data tab
    tabPanel(title = "Data",
             icon = icon( "database"),

             # To hide all rd error messages in the app - uncomment this 
             # tags$style(type="text/css",
             #            ".shiny-output-error { visibility: hidden; }",
             #            ".shiny-output-error:before { visibility: hidden; }"
             # ),
             sidebarLayout(
                sidebarPanel(width = 3,

                        h5(strong("Data set selection")),

                        selectInput("dataset", label = NULL,
                               choices = c( "", "Iris", "Diamonds", "Glass","Diabetes", "Upload data")),

                        uiOutput("UpSelect") ,

                        br(),

                        actionButton("GoData", label = "Select/ upload data",icon("file-import"), width = '100%',
                                     style="color: #fff; background-color: #28bb9b; border-color: #87d5c5" ),

                        hr(style="border-color: black;"),

                        bsTooltip("dataset", "Select an exisiting data set or upload your own", placement = "top", trigger = "hover",
                                    options = NULL),

                        h5(strong("Data summaries")),

                        checkboxInput( "checkNum", label = "Numerical summaries", value = FALSE),

                        checkboxInput( "checkCat", label = "Frequency summaries", value = FALSE),

                        uiOutput("CatVariableSum"),

                        hr(style="border-color: black;"),

                        h5(strong("Exploratory plots options")),

                        uiOutput( "Variable_select" ) %>% withSpinner(color="#0dc5c1")

                        ),

                mainPanel(

                     tabsetPanel(
                         id = 'DatasetSummaries',

                         tabPanel(h5( strong("Data table") ), value = "DataTable",
                                  tags$br(),
                                  DT::dataTableOutput("DataTab") %>% withSpinner(color="#0dc5c1")
                                  ),


                         tabPanel(h5( strong("Data summaries") ), value = "DataSum",
                                  tags$br(),

                                  h5( "Numerical variables summary"),
                                  tags$br(),
                                  DT::dataTableOutput("DataSumN") %>% withSpinner(color="#0dc5c1"),

                                  tags$br(),

                                  h5( "Categorical variables summary"),
                                  tags$br(),
                                  column(8,
                                  uiOutput("Freq") )#  %>% withSpinner(color="#0dc5c1"))
                                  ),

                          tabPanel( h5( strong("Exploratory plots")), value = "DataPlots",
                                plotOutput("ExPlot") %>% withSpinner(color="#0dc5c1")
                                            )

                        )
                    )
             )),




    # ###############################################################
    # # Classification tab
    tabPanel(title = "Classification",
             icon = icon( "bar-chart-o"),

             sidebarLayout(
                 sidebarPanel(width = 3,

                        selectInput("method", label = h5( strong("Classification method")),
                                                    choices = c( "LDA","QDA", "Logistic regression", "Firth logistic regression", "Multinomial logistic regression" #,
                                                                 #"kNN", "SVM", "Random forest", "Decision trees", "Naive Bayes classifier", "Neural networks"
                                                                 )),
                        uiOutput("VariableSelectX"),
                        uiOutput("VariableSelectY"),

                        bsCollapse(id = "collapseExample", open = "Panel 2",
                                    bsCollapsePanel("Advanced options",
                                       # numericInput("CvFold", "Enter the cross validation fold", 1, min = 1 ),
                                        numericInput("DataSplit", "Enter the training percentage split", 50, min = 1),
                                        numericInput("RandSeed", "Enter random seed number", 23, min = 0 )  )
                                   ),
                       # bsTooltip("CvFold", "This number represents the 'k' in k-fold cross validation", placement = "bottom", trigger = "hover",
                        #            options = NULL),
                        bsTooltip("DataSplit", "This number represents the percentage of the data set that is assigned to the training set", placement = "bottom", trigger = "hover",
                                                options = NULL),
                        bsTooltip("RandSeed", "To ensure the reproducibility of your results (should you wish to come back to your analysis at a later point), just use the same number for the random seed every time!", placement = "bottom", trigger = "hover",
                                                options = NULL),
                        actionButton("GoClassify", label = "Run model", icon("play"), width = '100%',
                                     style="color: #fff; background-color: #28bb9b; border-color: #87d5c5" ),

                        hr(style="border-color: black;"),

                        h5( strong("Predictions")),
                        fileInput("PredData", "Upload file containing new data",
                                  accept = c( "text/csv",  "text/comma-separated-values,text/plain", ".csv")  ),
                        actionButton("GoPredUpload", "Predict", icon("file-import"), width = '100%',
                                     style="color: #fff; background-color: #28bb9b; border-color: #87d5c5" ),
                        h5( strong("Prediction plot")),
                        uiOutput("VariableSelectPlot"),
                        hr(style="border-color: black;")

                        ),

            mainPanel(
                    tabsetPanel(
                         id = 'AnalysisSummaries',

                    tabPanel(h5(strong("Analysis results")), value = "tAnalysis",
                             verbatimTextOutput("ClassOutput")
                            ),
                    tabPanel(h5(strong("Training/testing dataset")),
                             h3("Training dataset"),
                             DT::dataTableOutput("training_set")%>% withSpinner(color="#0dc5c1"),
                             h3("Testing dataset"),
                             DT::dataTableOutput("testing_set")%>% withSpinner(color="#0dc5c1"),
                             "Maybe some plots showing the percentages of data allocated to train/test"
                            ),




                    tabPanel(h5(strong("Plots")),
                                  splitLayout(
                                      uiOutput( "ClassPlot")
                                    )
                                  ),

                    tabPanel(h5(strong("Classification performance")),
                                  h4("Overall performance measures"),
                                  verticalLayout( column( 3, h5("Misclassification matrix")) ,
                                                  column( 3, DT::dataTableOutput("ConfMat")%>% withSpinner(color="#0dc5c1") ),
                                                  column( 9, h5("Overall statistics")),
                                                  column( 9, DT::dataTableOutput("OverKappa")%>% withSpinner(color="#0dc5c1") )),
                                  br(),
                                  h4("Performance measures by class") ,
                                  verticalLayout( column( 9, DT::dataTableOutput("ByClass") %>% withSpinner(color="#0dc5c1") )
                                  )),

                    tabPanel(h5(strong("Predictions")), value = "tPred",
                                  fluidRow(
                                            h5(strong("Model predictions")),
                                            DT::dataTableOutput("PredDataTab") %>% withSpinner(color="#0dc5c1"),
                                            tags$style(type="text/css", "#PredDataTab td:nth-child(1) {text-align:center;background-color:#ffd800;color: black;text-align:center}"),
                                            tags$style(type="text/css", "#PredDataTab td:nth-child(2) {text-align:center;background-color:#ffb000;color: black;text-align:center}"),
                                            tags$br(),
                                            h5(strong("Plot predictions")),
                                            plotOutput("PredPlot") %>% withSpinner(color="#0dc5c1")
                                          #"TBC - user can choose what plot they want to display the predicted data"
                                            )
                            ),




                    tabPanel(h5(strong("R output")),

                                  verbatimTextOutput("AnalysisRes")
                                 ))
                        )
    )
    ),

    ########################
    # Evidence
    tabPanel( title = "Evidence evaluation",
              icon = icon( "balance-scale"),

              sidebarLayout(
                  sidebarPanel(width = 3,
                                selectInput("EviMethod", label = h5( strong("Evidence method")),
                                           choices = c( "Firth GLM" = "firth", "Bayes GLM" = "bayes", "GLM Net" = "net"), multiple = T),
                                checkboxGroupInput("EviOptions",  label = h5( strong("LR estimation type")),
                                                  choices = c( "Univariate Gaussian LR" = "gaussian", "Univariate KDE LR" = "kernel", "Multivariate Gaussian LR -- coming soon" = "mgaussian"), selected = 1),
                                bsTooltip("EviOptions", "Select all options you want included in your comparison. For datasets with more than 6 variables only univariate LR options are available",
                                          options = NULL),
                                uiOutput("varsYevidence"),
                                bsCollapse(id = "EviCV", open = "Panel 2",
                                          bsCollapsePanel(h5( strong("Cross validation options")),
                                                          textInput("pTrain", "Enter the % of data for training",   "50"),
                                                          textInput("pValid", "Enter the % of data for validation", "30"),
                                                          textInput("pTest" , "Enter the % of data for testing",    "20"),
                                                          textInput("RepeatN","Enter the number of repeated iterations", "5")
                                          )),
                                actionButton("GoEvidence", label = "Run evidence model", icon("play"), width = '100%',
                                            style="color: #fff; background-color: #28bb9b; border-color: #87d5c5" ),

                                hr(style="border-color: black;"),

                                h5( strong("Predictions")),
                                fileInput("EvidPredData", "Upload file containing new data",
                                         accept = c( "text/csv",  "text/comma-separated-values,text/plain", ".csv")  ),
                                actionButton("GoEvidPredUpload", "Upload", icon("file-import"), width = '100%',
                                            style="color: #fff; background-color: #28bb9b; border-color: #87d5c5" ),

                                hr(style="border-color: black;")

                                ),
                  mainPanel(
                      tabsetPanel(
                          id = 'EvidenceResults',
                            tabPanel(h5(strong("Analysis results")), value = "tEviRes",
                                    tags$br(),
                                    "Only available for binary class data!",
                                    tags$br(),
                                    "Table of performance measures using selected methods",
                                    tags$br(),
                                    DT::dataTableOutput("evidence_results") %>% withSpinner(color="#0dc5c1"),
                                    tags$br()
                                    ),
                            tabPanel(h5(strong("Plots")),
                                    fluidRow( column(6, plotOutput("EviPlots1")  %>% withSpinner(color="#0dc5c1")),
                                              column(6, plotOutput("EviPlots2")  %>% withSpinner(color="#0dc5c1"))),
                                    fluidRow( column(6, plotOutput("EviPlots3")  %>% withSpinner(color="#0dc5c1")),
                                              column(6, plotOutput("EviPlots4")  %>% withSpinner(color="#0dc5c1"))),
                                    fluidRow( column(6, plotOutput("EviPlots5")  %>% withSpinner(color="#0dc5c1")),
                                              column(6, plotOutput("EviPlots6")  %>% withSpinner(color="#0dc5c1"))),
                                    fluidRow( column(6, plotOutput("EviPlots7")  %>% withSpinner(color="#0dc5c1")))
                                    ),
                            tabPanel(h5(strong("Prediction")), value = "tEviPred",
                                    "Evidence prediction for new observations - coming soon"
                                    )

                          )
                  )
              )

              ),


    ###############################################################
    # Download report
    tabPanel( title = "Report",
              icon = icon( "download"),
              "TBC",
              tags$br(),
              "Have a list of methods that the user can select to produce a report. Or a 'add to report' button throught the app",
              tags$br(),
              "Make appropriate plots and perhaps a table comparing the selected methods' results.",
              tags$br(),
              img(src="NotReady.png", height = 300)

              )
)







##############################################################################################################################################################
# Define server logic required to generate objects
server <- function( input, output, session) {



    ### Data set assignment
    datasetInput <- eventReactive( input$GoData, {
        switch(input$dataset,
               "Select dataset" = NULL,
               "Iris" = iris,
               "Diamonds" = diamonds,
               "Glass" = Glass,
               "Diabetes" = PimaIndiansDiabetes,
               "Upload data" =  import( input$UpData$datapath )
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

                         varSelectInput("varX", h5("X variable"), NumData(), selected = names(NumData())[2])  ,

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
         {

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
                                      pageLength = 5, scrollX = TRUE )
                       )
        } )


    ### Generating summary statistics for numerical variables
    output$DataSumN <- renderDataTable ( {
        if(input$checkNum == TRUE)
        DT::datatable (  round(descr( NumData() , stats = c( "min", "q1", "med", "mean" ,"q3", "max", "sd"),
                                transpose = TRUE, headings = FALSE, justify = "c", style = "simple"), 3),
                         options = list(  dom = 't')
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
    observeEvent(input$CatVar,
                 for (i in 1: length( input$CatVar )  ) {
                     id <- paste0( "Freq", i)
                     f <- freq( CatData() [ , input$CatVar[i] ], report.nas = F, headings = F,
                                cumul = F, justify = "c", totals = F, style = "grid")
                     f <- data.frame(f)
                     f <- round( f[  -( nrow(f) -1  ), c(1,2,3) ], 3)
                     colnames(f) <- c("Frequency", "%", "Cumulative %")
                     output[[id]] <- DT::renderDataTable( f, caption = input$CatVar[i], options = list( dom = 't') )
                    }
                 )

    ############## Analysis tab

    # Initialize the object that contains the model outputs and inputs
    ClassRes <- reactiveValues()
       ClassRes$model <- NULL
       ClassRes$data <- NULL
       ClassRes$training_dataset <- NULL
       ClassRes$testing_dataset  <- NULL
       ClassRes$testing_result   <- NULL
       ClassRes$seed <- NULL    # make output dependant on seed if user chooses to input a seed
       ClassRes$confusion_matrix <- NULL
       ClassRes$prediction <- NULL
       ClassRes$summary <- NULL

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
    observeEvent( input$RandSeed,  {ClassRes$seed <- input$RandSeed
                      set.seed( input$RandSeed )
                         })


    # Selecting method
    observeEvent( input$GoClassify,  {

        # set seed to whatever the user input
        set.seed(ClassRes$seed )

        # split data set into testing/training
        ClassRes$data <- DataTrainTest( data = datasetInput(), per = input$DataSplit)
        ClassRes$training_dataset <- as.data.frame(ClassRes$data[1])
        ClassRes$testing_dataset  <- as.data.frame(ClassRes$data[2])

    
        # fit selected model
        switch(input$method,
               "LDA" =  {ClassRes$model <-  RunLDA( input$varXm, input$varYm, ClassRes$training_dataset  )
                         ClassRes$testing_result <- EvaluateLDA( ClassRes$model, ClassRes$testing_dataset )
                         #ClassRes$summary <- ClassRes$model 
                         },

               "QDA" =  {ClassRes$model <-  RunQDA( input$varXm, input$varYm, ClassRes$training_dataset  )
                         ClassRes$testing_result <- EvaluateQDA( ClassRes$model, ClassRes$testing_dataset )
                        # ClassRes$summary <- ClassRes$model 
                         },

               "Logistic regression" = {ClassRes$model <-  RunLR( input$varXm, input$varYm, ClassRes$training_dataset  )
                                        ClassRes$testing_result <- EvaluateLR( ClassRes$model, ClassRes$testing_dataset )
                                        #ClassRes$summary <- summary( ClassRes$model) 
                                        },

               "Firth logistic regression" = {ClassRes$model <-  RunLRF( input$varXm, input$varYm, ClassRes$training_dataset )
                                              ClassRes$testing_result <- EvaluateLRF( ClassRes$model, ClassRes$testing_dataset )
                                             # ClassRes$summary <- summary( ClassRes$model)
                                              },

               "Multinomial logistic regression" =  {ClassRes$model <-  RunMLR( input$varXm, input$varYm, ClassRes$training_dataset )
                                                     ClassRes$testing_result <- EvaluateMLR( ClassRes$model, ClassRes$testing_dataset )} #,
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
        switch( isolate(input$method),
               "LDA" =  { print( ClassRes$model)  },
               "QDA" =  { print( ClassRes$model)  },
               "Logistic regression" = { print( summary( ClassRes$model) ) },
               "Firth logistic regression" = { print( summary( ClassRes$model) )  },
               "Multinomial logistic regression" =  { print( summary( ClassRes$model) )
                   print(  " p-values ")
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

    datasetPred <- eventReactive( input$GoPredUpload, {
        datasetPred <- import( input$PredData$datapath )
    })

    # Predict for selected methods
    observeEvent( input$GoPredUpload,  {
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

    ### Generate dataset table to display the data upladed for prediction
    output$PredDataTab <- renderDataTable( {
        dataset <- cbind( Prediction = ClassRes$prediction$class, LR =  ClassRes$prediction$LR, datasetPred() )
        DT::datatable (dataset, rownames = F,
                       options = list(lengthMenu = c(5, 10, 15),
                                      pageLength = 5, scrollX = TRUE,  dom = 't'))
    } )


    # Show the training and testing data sets in different tables
    output$training_set <- renderDataTable( {
        DT::datatable (ClassRes$training_dataset,
                        options = list(lengthMenu = c(5, 10, 20),
                                        pageLength = 5 ))
    } )

    output$testing_set <- renderDataTable( {
        DT::datatable (ClassRes$testing_dataset,
                       options = list(lengthMenu = c(5, 10, 20),
                                      pageLength = 5 ))
    } )




    # Make specific tabs active when clicking predict and analysis/go buttons
    observeEvent( input$GoClassify, {
        updateTabsetPanel(session, "AnalysisSummaries",
                          selected = "tAnalysis")

    })

    observeEvent( input$GoPredUpload, {
        updateTabsetPanel(session, "AnalysisSummaries",
                          selected = "tPred")

    })

    observeEvent( input$checkNum | input$checkCat, {
        if( input$checkNum !=0 | input$checkCat !=0)
            updateTabsetPanel(session, "DatasetSummaries",
                              selected = "DataSum")
    })

    observeEvent( input$GoData, {
        updateTabsetPanel(session, "DatasetSummaries",
                              selected = "DataTable")
    })

    observeEvent( input$GoEvidence, {
        updateTabsetPanel(session, "EvidenceResults",
                          selected = "tEviRes")

    })

    observeEvent( input$GoEvidPredUpload, {
        updateTabsetPanel(session, "EvidenceResults",
                          selected = "tEviPred")
    })


    # Classification measures and confusion matrix (buggy for uploaded dataset)
    output$ConfMat   <- renderDataTable( {
        
        cm <- confusionMatrix(  reference = ClassRes$testing_dataset[ , input$varXm] , data = as.factor( ClassRes$testing_result$class ) )
        mtable <- NULL
        for ( i in 1 :  length( levels(ClassRes$testing_dataset[ , input$varXm] )))
            mtable <- rbind( mtable, cm$table[,i] )
        rownames(mtable) <- colnames(cm$table)
        DT::datatable( mtable, options = list(ordering=F, dom = 't' ) )

})

    output$OverKappa <- renderDataTable( {

    cm <- confusionMatrix(  reference = ClassRes$testing_dataset[ , input$varXm] , data = as.factor( ClassRes$testing_result$class ) )
    DT::datatable( t(round( cm$overall,3)), rownames = T,
                   options = list(ordering=F, dom = 't'),
                   callback = JS(" var tips = ['Row Names',
                                                ' ', ' '],
                                 header = table.columns().header();
                                 for (var i = 0; i < tips.length; i++) {
                                 $(header[i]).attr('title', tips[i]);
                                 }
                                 ")  )
})

    output$ByClass   <- renderDataTable( {

    cm <- confusionMatrix(  reference = ClassRes$testing_dataset[ , input$varXm] , data = as.factor( ClassRes$testing_result$class ) )
    DT::datatable( round( cm$byClass,3), rownames = T, options = list(ordering=F, dom = 't'),
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


    # Generate plots for predicted data according to selected variables
    output$VariableSelectPlot <- renderUI( {
        verticalLayout(
            varSelectInput("varXp", h5("Choose variable for x axis"),  datasetInput() ),
            varSelectInput("varYp", h5("Choose variable for y axis"),  datasetInput(), selected = names( datasetInput() )[2] ),
            varSelectInput("varC",  h5("Choose variable for point colour"), CatData()  )
            )
    } )

    # Prediction data plot
    output$PredPlot <- renderPlot (
        {
            predSet <- cbind ( datasetPred(), ClassRes$prediction$class )
            colnames( predSet) [ length( colnames( predSet ))] <- input$varXm
            ggplot( data = ClassRes$training_dataset, aes ( x = !!input$varXp , y = !!input$varYp) ) +
                geom_point( alpha = 0.5, size = 2, aes( colour = !!input$varC, shape = '20')) +
                geom_point( data = predSet, aes( x = !!input$varXp , y =  !!input$varYp, colour = !!input$varC, shape = '8'), size = 4) +
                scale_shape_manual(name = 'Data', guide = 'legend', labels = c('training', 'predicted'), values = c(20, 8)) #+
            #   geom_text ( data = predSet, aes( label =!!input$varC), check_overlap = TRUE, angle = 45, size = 3)

        } )

    ###### Evidence tab

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

    output$varsYevidence <- renderUI ( {
        verticalLayout(
            selectInput("varXe", label = h5( strong("Response variable")),
                            names(CatData()) ),
            varSelectInput("varYe", label = h5( strong("Explanatory variables")),
                           NumData(), multi = T  )
        )
    })

    observeEvent( input$GoEvidence, {

    # Repetitions loop
    for ( i in 1 :  input$RepeatN ){

        # split data set into testing/training
        EviRes$data <- DataTrainTestValid( input$varXe, data = datasetInput(), ptrain = as.numeric(input$pTrain), pvalid = as.numeric(input$pValid), ptest = as.numeric(input$pTest))
        EviRes$training_dataset   <- as.data.frame( EviRes$data[[1]])
        EviRes$testing_dataset    <- as.data.frame( EviRes$data[[2]])
        EviRes$validation_dataset <- as.data.frame( EviRes$data[[3]])

        # #for each method, produce and store performance measure
        for ( j in 1 : length( input$EviMethod) ) {
            for( k in 1 : length( input$EviOptions) ) {
                #print( c(i,j,k) )
                p <- try (EvRun( EviRes$training_dataset, EviRes$validation_dataset, EviRes$testing_dataset, input$varXe, input$varYe, input$EviMethod[j], input$EviOptions[k]) )
                if ( class(p) == "data.frame" )
                        EviRes$predm  <- rbind( EviRes$predm, p)
            }
        }
    }
    })

    # Displaying a table of the computed measures for all selected methods and estimation types
    output$evidence_results <- renderDataTable( {
        cm <- EviRes$predm
        DT::datatable ( cbind( round( cm[,1:7], 3), cm[, 8:9] ),
                        rownames= FALSE,
                        options = list(lengthMenu = c(5, 10, 20),
                                       pageLength = 5 ))
    } )




    ## Plots of the measures computed: Generating the output list for the plots for each measure computed
    observeEvent(input$GoEvidence,
                 output$EviPlots <- renderUI({
                        lapply( as.list( seq_len( 7 )), function(i) {
                                id <- paste0("EviPlots", i)
                                plotOutput( id )
                                })
    })  )

    observeEvent(input$GoEvidence, {
         output$EviPlots1 <- renderPlot( {
            CM <- EviRes$predm
            ggplot( CM, aes( x =  Method, y = Precision ) ) +
                         geom_boxplot( aes(fill =  EstimationType )) + theme(legend.position="bottom")
         })

         output$EviPlots2 <- renderPlot( {
             CM <- EviRes$predm
             ggplot( CM , aes( x =  Method, y = Recall ) ) +
                 geom_boxplot( aes(fill =  EstimationType )) + theme(legend.position="bottom")
         })

         output$EviPlots3 <- renderPlot( {
             CM <- EviRes$predm
             ggplot( CM , aes( x =  Method, y = Specificity ) ) +
                 geom_boxplot( aes(fill =  EstimationType )) + theme(legend.position="bottom")
         })

         output$EviPlots4 <- renderPlot( {
             CM <- EviRes$predm
             ggplot( CM , aes( x =  Method, y = Accuracy ) ) +
                 geom_boxplot( aes(fill =  EstimationType )) + theme(legend.position="bottom")
         })

         output$EviPlots5 <- renderPlot( {
             CM <- EviRes$predm
             ggplot( CM , aes( x =  Method, y = F1 ) ) +
                 geom_boxplot( aes(fill =  EstimationType )) + theme(legend.position="bottom")
         })

         output$EviPlots6 <- renderPlot( {
             CM <- EviRes$predm
             ggplot( CM , aes( x =  Method, y = MissClassification ) ) +
                 geom_boxplot( aes(fill =  EstimationType )) + theme(legend.position="bottom")
         })

         output$EviPlots7 <- renderPlot( {
             CM <- EviRes$predm
             ggplot( CM , aes( x =  Method, y = Ece ) ) +
                 geom_boxplot( aes(fill =  EstimationType )) + theme(legend.position="bottom")
         })
    })



}

# Run the application
shinyApp( ui = ui, server = server)


# Plots for each method
# observeEvent( input$GoClassify,  {
#
#     # fit selected model
#     switch(input$method,
#            "LDA" =  {  },
#
#            "QDA" =  { },
#
#            "Logistic regression" = { },
#
#            "Firth logistic regression" = { },
#
#            "Multinomial logistic regression" =  { } ,
#            "kNN" = { },
#            "SVM" = { },
#            "Random forest" = { },
#            "Decision trees" = { },
#            "Naive Bayes classifier" = { },
#            "Neural networks" = { } )
#
# })
#
