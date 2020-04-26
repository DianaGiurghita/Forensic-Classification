# Returns the data set variables that are either numeric or factor variables

GetDataByClass <- function ( dataset, cls = "numeric")
{
    
    dataset <- as_tibble( dataset)
    data_class <- lapply( dataset, class)
    
    if ( cls == "numeric")
        return (  data.frame(  dataset[, sapply(data_class, function(y) "numeric" %in% y)], 
                               dataset[, sapply(data_class, function(y) "integer" %in% y)], 
                               dataset[, sapply(data_class, function(y) "double" %in% y)] ))
    if ( cls == "factor"){
        d <- data.frame(  dataset[, sapply(data_class, function(y) "factor" %in% y)], 
                            dataset[, sapply(data_class, function(y) "character" %in% y)], 
                               dataset[, sapply(data_class, function(y) "logical" %in% y)] ) 
        d %>% mutate_all(as.factor) 
        return (d)}
}



DataTrainTest <- function ( data, per = 50)
{   
    if ( per <100) {
        trainInd <- sample( 1: dim( data )[1], size = per/100 *dim( data )[1] )
        dataTrain <- data [ trainInd, ]
        dataTest  <- data [ -trainInd,  ] } 
    
    # if percentage split =100 then both training and testing sets are the data (
    # thus obtaining fitted values instead of predictions 
    if ( per == 100) {
        dataTrain <- data  
        dataTest  <- data   } 
    
    return( list( dataTrain, dataTest ))
}



#### Run classifiers 

# LDA classifier
RunLDA <- function ( varX, varY, dataset  )
{
  f <- NULL
  outcome <- varX
  variables <- varY  
  f <- as.formula(   paste(outcome, paste(variables, collapse = " + "),  sep = " ~ "))
  m <- lda ( formula = f, data = dataset)
  return( m )
}

# Evaluate LDA classifier
EvaluateLDA <- function ( model, testing_dataset )
{
    Predict_result <- predict(model, testing_dataset)
    
    # If the model has binary outcome, produce an LR p/(1-p)
    LR <- NA
    if ( length( model$lev ) == 2)
		# This should probably be [,2]!! Check
		
        LR <- Predict_result$posterior[,1] / ( 1 - Predict_result$posterior[,1] )
    
    return( list( class = Predict_result$class, LR = LR) )
}


# QDA classifier 
RunQDA <- function ( varX, varY, dataset  )
{   
    f <- NULL
    outcome <- varX
    variables <- varY 
    f <- as.formula(   paste(outcome, paste(variables, collapse = " + "),  sep = " ~ "))
    m <- qda ( formula = f, data = dataset)
    return( m )
}


# Evaluate QDA classifier 
EvaluateQDA <- function ( model, testing_dataset )
{
    Predict_result <- predict(model, testing_dataset)
    
    # If the model has binary outcome, produce an LR p/(1-p)
    LR <- NA
	
	# This should probably be [,2]!! Check
    if ( length( model$lev ) == 2)
        LR <- Predict_result$posterior[,1] / ( 1 - Predict_result$posterior[,1] )
    
    
    return( list( class = Predict_result$class, LR = LR) )
}

# Logistic regression classifier
RunLR <- function ( varX,  varY, dataset )
{   
    dataset[, varX] <- as.factor( dataset[, varX] )
  
    f <- NULL
    outcome <- varX
    variables <- varY 
    f <- as.formula(   paste(outcome, paste(variables, collapse = " + "),  sep = " ~ "))
    
    m <- glm( formula = f, family = binomial (link='logit'), data = dataset )
    return( m )
}

# Evaluate LR 
EvaluateLR <- function ( model, testing_dataset )
{   
    # returns the class probability p
    p <-  predict(model, testing_dataset, type = 'response')
    
    # labels for response in order (first one is variable of interest)
    lab <- levels ( model$model[,1] )
    
    # predicted class labels
    class <- as.factor(ifelse( p > 0.5, lab[2], lab[1]))
    
    # predicted LR 
    LR <-  p/ (1-p) 
    
    return( list( class =  class, LR = LR ) )
}

# mod_firth <- function( lLR, lLR_p, t_valid_set)
# {
#     # put all variables in a matrix or predict will not recognize the data and will return fitted values instead of predictions
#     datafit <- cbind( lLR, F = t_valid_set$F)
#     
#     # fit Firth logisitic regression using the brglm2 package
#     # sometimes the algorithm doesn not converge (probably because of imbalanced dataset)
#     model_firth <- glm(  F ~ . , 
#                          data = datafit, 
#                          family = "binomial", type = "AS_mean", method = "brglmFit", control = list(maxit=500) ) 
#     #print( summary(model_firth))
#     
#     # class probability 
#     pred_firth <- predict( model_firth, newdata =  lLR_p, type = "response")
#     
#     # logLR given by default type prediction 
#     LR <- exp( predict( model_firth, newdata = lLR_p ) )
#     
#     # class
#     pred_firth_class <- ifelse( pred_firth >= 0.5, 1, 0)
#     
#     return( data.frame( class = c(pred_firth_class), LR  = c(LR )) )
#     
# } 

# Firth logistic regression classifier
RunLRF <- function ( varX, varY, dataset   )
{   
    dataset[, varX] <- as.factor( dataset[, varX] )
    f <- NULL
    outcome <- varX
    variables <- varY 
    f <- as.formula(   paste(outcome, paste(variables, collapse = " + "),  sep = " ~ "))
    
    m <-  glm(  formula = f, 
          data = dataset, 
          family = "binomial", type = "AS_mean", method = "brglmFit", control = list(maxit=500) ) 
    
    return( m )
}

# Evaluate Firth logistic regression classifier
EvaluateLRF <- function (  model, testing_dataset)
{
    
    # class probability 
    p <- predict( model, newdata = testing_dataset, type = "response")
    
    # logLR given by default type prediction so we exp() for LR
    LR <- exp( predict( model, newdata = testing_dataset) )
    
    # Class labels
    lab <- levels ( model$model[,1])
    
    # Predicted class labels with 0.5 cutoff point
    pred_firth_class <- ifelse( p >= 0.5, lab[2], lab[1])
    
    return( data.frame( class = c(pred_firth_class), LR  = c(LR )) )
    
}

# Multinomial logistic regression classifier
RunMLR <- function ( varX, varY, dataset )
{   
    f <- NULL
    outcome <- varX
    variables <- varY 
    f <- as.formula(   paste(outcome, paste(variables, collapse = " + "),  sep = " ~ "))
    
    m <- nnet::multinom( formula = f, data = dataset )
    
    z <- summary(m)$coefficients / summary(m)$standard.errors
    m$pval <- (1 - pnorm( abs(z), 0, 1) )*2
    
    return( m )
}

# Evaluate multinomial logistic regression classifier
EvaluateMLR <- function ( model, testing_dataset )
{   
    # returns the class probability p
    p <- predict( model, testing_dataset, type="probs")
     # labels for response in order (first one is variable of interest)
    lab <-   model$lev
     
    # gives the column number where max is for each row
    pmax <- apply ( p, MARGIN = 1, which.max)
     
    # predicted class labels
    class <- lab [ pmax]
     
    # predicted LR 
    # LR <-  p/ (1-p) 
    
    return( list( class =  class, LR = NA ) )
}


### EVIDENCE functions

ef <- function(df, name){
    eval( substitute( name ), df)
}


DataTrainTestValid <- function ( varXm, data, ptrain, pvalid, ptest)
{   
    dataTrain <- NULL
    dataValid <- NULL
    
    # repeat {
        data$ind <- sample(1:3, size= dim(data)[1], replace=T, prob=c(ptrain/100, pvalid/100, ptest/100) )
        dataTrain <- data[data$ind ==1, 1: (dim(data)[2]-1) ]
        dataValid <- data[data$ind ==2, 1: (dim(data)[2]-1) ]
        dataTest  <- data[data$ind ==3, 1: (dim(data)[2]-1) ]

    #       if ( (CheckVar( dataTrain, varXm ) * CheckVar( dataValid, varXm)  ==1) ) {break}
    # }
    #
    return( list( dataTrain, dataValid, dataTest))
}

# DataTrainTestValid <- function ( varXm, data, ptrain, pvalid, ptest)
# {   
#     dataTrain <- NULL
#     dataValid <- NULL
#     
#     repeat {
#     data$ind <- sample(1:3, size= dim(data)[1], replace=T, prob=c(ptrain/100, pvalid/100, ptest/100) )
#     dataTrain <- data[data$ind ==1, 1: (dim(data)[2]-1) ]
#     dataValid <- data[data$ind ==2, 1: (dim(data)[2]-1) ]
#     dataTest  <- data[data$ind ==3, 1: (dim(data)[2]-1) ]
#     
#            if ( (CheckVar( dataTrain, varXm ) * CheckVar( dataValid, varXm)  ==1) ) {break}
#     }
#     #
#     return( list( dataTrain, dataValid, dataTest))
# }
# Checks to make sure the training and validation sets have more than 8 observations in each class
# which is required by some of the glm methods
# !Make this work for when only one category is in the factor variable
# 
# CheckVar <- function ( data, varXm )
# { 
# 
#     class(data)
#     print ( data)
#     
#     class(varXm)
#     print ( varXm)
#     v1 <- data[, varXm]
#     if ( is.null( v1 ) ){
#         return ( 0 )
#     }else {if (  min ( table( v1 ))  < 8 ){
#         return( 0 )
#     }else{
#         return( 1 )}
#     }    
# }


# Calculates log LR for data,
lLRuni <- function( data, varXm, var = 1, pred_data, method = "gaussian")
{
   
    data[ ,varXm] <- as.factor( data[ ,varXm] ) 
    
    
    # SUBSET DATA FOR EACH CLASS 
    data_1 <- data[ , var] [ data[, varXm] == levels( data[,varXm] ) [1] ] 
    data_2 <- data[ , var] [ data[, varXm] == levels( data[,varXm] ) [2] ] 
    
    # return( list(data_1, data_2) )
    if ( method == "gaussian")
    {
        # FIT UNIVARIATE NORMAL
        data_1_fit <- fitdistr( data_1, "normal")
        data_2_fit <- fitdistr( data_2, "normal")

        # # CALCULATE log LIKELIHOOD RATIOS
        logLR <- numeric( dim(pred_data)[1] )
        for ( i in 1: length(logLR))
            # work on log scale for more numerical stability
            logLR[i] <- dnorm ( pred_data[ i, var], mean = data_1_fit$estimate[1],  sd = data_1_fit$estimate[2], log = T ) -
            dnorm ( pred_data[ i, var], mean = data_2_fit$estimate[1],  sd = data_2_fit$estimate[2], log = T )
    }

    if ( method == "kernel")
    {
        # FIT KDE NORMAL
        # density() uses Silverman's rule by default. Biased or unbiased cv also available to set the bandwidth
        data_1_fit   <- density( data_1 )
        data_2_fit   <- density( data_2 )
        
        # CALCULATE log LIKELIHOOD RATIOS
        logLR <- numeric( dim(pred_data)[1] )

        for ( i in 1: length(logLR))

            logLR[i] <- log( myKDE( t=pred_data[ i, var], xs = data_1, h = data_1_fit$bw ) ) -
            log( myKDE( t=pred_data[ i, var], xs = data_2, h = data_2_fit$bw  ) )

    }
    return( logLR )
    
}

# myKDE( pred_data[i, var])
myKDE <- function(t, xs, h){
    kernelValues <- rep(0, length(xs))
    for(i in 1:length(xs)){
        transformed = (t - xs[i]) / h
        kernelValues[i] <- dnorm(transformed, mean = 0, sd = 1) / h
    }
    return( max( c( sum(kernelValues) / length(xs), 1e-10)) )
}

# Creates a matrix of LR one column per variable selected

lLR_mat <- function ( method = "gaussian", data, pred_data, varXm, varYm)
{
    lLR <- NULL
    LR <- NULL
    vname <- NULL
    for ( i in 1: length(varYm) ) {
        vname[i] <- paste0( "v", i)
        LR <- lLRuni (data, varXm, var = as.character( varYm[i] ) , pred_data, method )
        lLR <- cbind( lLR, LR)
    }
    colnames( lLR ) <- vname
    return( lLR)
    
}

# returns a very small value for values waaay outside the range 
# myKDE( pred_data[i, var])


### HGLM:Returns predicted class using GLM NET
mod_glm_net <- function(lLR, lLR_p, valid, varXm)
{
 
    valid[ ,varXm] <- as.factor( valid[ ,varXm] ) 
    
    # fit glm_net regression model
    model_glm_net <- cv.glmnet( y = data.matrix( valid[ ,varXm] ), x = data.matrix(lLR), family = "binomial")
    
    # predictions i.e. log(LR)
    pred_glm_net <- predict( model_glm_net , newx = data.matrix(lLR_p), s = "lambda.min", type = "response")
    
    # Factor labels 
    Flab <- levels( valid[ ,varXm] )
    
    # class
    pred_glm_net_class <- ifelse( pred_glm_net  > 0.5, Flab[2], Flab[1])
    
 
    return( data.frame( class = as.factor(pred_glm_net_class),  LR = exp (c(pred_glm_net )))) 
}    

### BGLM: Returns predicted class using BAYES GLM
mod_bayes_glm <- function( lLR, lLR_p, valid, varXm)
{

    valid[ ,varXm] <- as.factor( valid[ ,varXm] )

    # fit bayes_glm regression model
    model_bayes_glm <- bayesglm ( valid[ ,varXm] ~ lLR , family=binomial(link="logit") ) 

    # predictions (i.e. logLR)!
    # This returns fitted values, not predictions for the new data!!
    # pred_bayes_glm <- predict( model_bayes_glm, newdata =  data.frame(lLR_p), type = "response")

    logLR <- c( model_bayes_glm$coefficients  %*%  t( cbind( 1,  lLR_p) ) )
    
    # probability of class membership
    pred_bayes_glm  <- exp ( logLR ) / (1 + exp ( logLR ) )

    # Factor labels 
    Flab <- levels( valid[ ,varXm] )
    
    # class
    pred_bayes_glm_class <- ifelse(  pred_bayes_glm  > 0.5, Flab[2] , Flab[1] )

    return( data.frame ( class = as.factor(pred_bayes_glm_class), LR = exp(c( logLR ))))
}


### FGLM: Returns predicted class using Firth LR
mod_firth <- function( lLR, lLR_p, valid, varXm)
{
   
    valid[ , varXm] <- as.factor( valid[ , varXm] )
    
    # put all variables in a matrix or predict will not recognize the data and will return fitted values instead of predictions
    datafit <- data.frame( lLR, F = valid[, varXm])
    
    # fit Firth logisitic regression
    # sometimes the algorithm doesn not converge (probably because of imbalanced dataset)
    model_firth <- glm(  F ~ . , 
                         data = datafit, 
                         family = "binomial", type = "AS_mean", method = "brglmFit", control = list(maxit=500) ) 
    
    # class probability 
    pred_firth <- predict( model_firth, newdata =  data.frame(lLR_p), type = "response")
    
    # logLR given by default type prediction 
    LR <- exp( predict( model_firth, newdata = data.frame(lLR_p) ) )
    
    # Factor labels 
    Flab <- levels( valid[ ,varXm] )
    
    # class
    pred_firth_class <- ifelse( pred_firth >= 0.5, Flab[2], Flab[1])
    
    return( data.frame( class = c(pred_firth_class), LR  = c(LR )) )
    #######
   
    
    # log LR i.e. response
    # logLR <- c( model_firth$coefficients  %*%  t( mlLR_p) ) 
    # 
    # # probability of class membership
    # pred_firth <- exp( model_firth$coefficients %*%  t(mlLR_p) ) / ( 1 +  exp( model_firth$coefficients %*%  t(mlLR_p) ))
    # 
    # 
    # # Factor labels 
    # Flab <- levels( valid[ ,varXm] )
    # 
    # # class
    # pred_firth_class <- ifelse( pred_firth > 0.8, Flab[2], Flab[1])
    # 
    # return( data.frame( class = as.factor(pred_firth_class), LR = exp( c(logLR  )) ) )
    # 
}  

# Classification performance measures
comp_measures <- function ( actual_class, model_prediction, LR)
{
    actual_class <- as.factor(actual_class)
    dataset <- NULL
    dataset$Actual    <- ifelse( actual_class == levels ( actual_class)[1],  0, 1)
    dataset$Predicted <- ifelse( model_prediction == levels ( actual_class)[1],  0, 1) 
    dataset$Odds      <- LR

 
    # odds for class "1" in the test set
    LR.same <- dataset$Odds [ dataset$Actual == 1]
    
  
    # odds for class "0" in the test set
    LR.different <- dataset$Odds [ dataset$Actual == 0]
   
    #####

    # calculate the ECE/ CLLR
    ece <- calc.ece(LR.same, LR.different, prior = 0.5 )

    # True Positive (1 classified as 1)
    TP <- sum( dataset$Actual == 1 & dataset$Predicted == 1)

    # True Negative (0 classified as 0)
    TN <- sum( dataset$Actual == 0 & dataset$Predicted == 0)

    # False Negative (1 classified as 0)
    FN <- sum( dataset$Actual == 1 & dataset$Predicted == 0)

    # False Positive (0 classified as 1)
    FP <- sum( dataset$Actual == 0 & dataset$Predicted  == 1)

    # Calculate other classification measures
    precision   <- TP / (TP + FP)
    recall      <- TP / (TP + FN)
    specificity <- TN / (TN + FP)
    accuracy    <- (TP + TN) / (TP + TN + FP + TP)
    F1          <- 2 * precision * recall / (precision + recall)

    # Mis-classification rate:
    (mis_classif <- (FP + FN) / ( TP + TN + FP + FN))

    return (  c(  Precision=precision,  Recall=recall, Specificity=specificity,  Accuracy=accuracy,  F1= F1,  MissClassification=mis_classif,  Ece=ece@ece) )
 
  
}

EvRun <- function( train, valid, test, varXm, varYe, method, method2, fmode = "comparison") 
{
    # get LR matrices for the variables selected, using either gaussian or kernel method (method2 option)
    # lLR matrix of LRs for validation data, lLR_p matri of LRs for prediction data
    lLR   <- lLR_mat ( method2, data=train, pred_data=valid, varXm, varYe)
    lLR_p <- lLR_mat ( method2, data=train, pred_data=test,  varXm, varYe)
    
    
    # Fit selected model (indicated in method) to the LR matrices
    if ( method == "FirthGlm"){
        m <- try( mod_firth( lLR, lLR_p, valid, varXm) )
        
    }else if (method == "BayesGlm"){
            m <- try( mod_bayes_glm( lLR, lLR_p, valid, varXm) )

        }else if (method == "GlmNet") {
                m <- try ( mod_glm_net( lLR, lLR_p, valid, varXm) )
        }
    
    if ( fmode == "comparison"){
        if ( class(m) != "try-error"){
            # Get classification measures for test data
            cm <- comp_measures ( actual_class = test[, varXm], model_prediction = m$class, LR = m$LR)
            
            # Add method and LR estimation method to the return values
            cmm <- data.frame(  t(cm),  Method = method,  EstimationType = method2 )
            
            colnames(cmm)[1:7] <- c( "Precision", "Recall", "Specificity", "Accuracy",  "F1",  "MissClassification",  "Ece")
            
            return( cmm )
        }
        else
            return( NULL )
    }
    else
        if (fmode == "prediction")
            if ( class(m) != "try-error")
             {   
                return( data.frame(m) )
            }
    
            else
                return( NULL )
}

# # SVM classifier
# RunSVM <- function(   ) 
# {   
#     m <- svm( Species ~., iris)
#     return( m )
# }
# 
# 
# # Naive Bayes classifier
# RunNB <- function(   ) 
# {   
#     m <- naiveBayes( Species ~., iris)
#     return( m )
# }
# 
# 
# 
# 
# ## NKnn classifier %%%%% Needs fix %%%%% 
# RunKNN <- function(   ) 
# {   
#     training_dataset <- iris[1:100, ]
#     testing_dataset <- iris[ 100:150, 1:4]
#     m <- knn( training_dataset[,1:4], testing_dataset, k = 3, cl = as.factor(training_dataset$Species ), prob = T)
#     return( m )
# }
# 
# ## NN classifier
# RunNN <- function(   ) 
# {   
#     m <- nnet( Species ~., iris, size = 10)
#     return( m )
# }
# 
# 
# ## Random forest classifier 
# RunRF <- function(   ) 
# {   
#     m <- randomForest( Species ~., iris)
#     return( m )
# }
# 
# ## Random forest classifier 
# RunDC <- function(   ) 
# {   
#     m <- rpart( Species ~., iris)
#     return( m )
# }
# 
# 
# ##### 
# # Classification performance ( confussion matrix)
# 
# 
# 
# 
# # Multinomial logistic regression classifier
# EvaluateMLR <- function ( model, testing_dataset )
# {
#     testing_dataset$Predict_result <- predict(model, testing_dataset, type = "class")
#     return(testing_dataset)
# }
# 
# # SVM classifier
# EvaluateSVM <- function( model, testing_dataset  ) 
# {   
#     testing_dataset$Predict_result <- predict(model, testing_dataset, type = "class")
#     return(testing_dataset)
# }
# 
# 
# # Naive Bayes classifier
# EvaluateNB <- function( model, testing_dataset  ) 
# {   
#     testing_dataset$Predict_result <- predict(model, testing_dataset, type = "class")
#     return(testing_dataset)
# }
# 
# 
# 
# ## NKnn classifier %%%%% Needs fix %%%%% 
# EvaluateKNN <- function( model, testing_dataset  ) 
# {   
#      
# }
# 
# ## NN classifier
# EvaluateNN <- function( model, testing_dataset  ) 
# {   
#     testing_dataset$Predict_result <- predict(model, testing_dataset, type = "class")
#     return(testing_dataset)
# }
# 
# 
# ## Random forest classifier 
# EvaluateRF <- function( model, testing_dataset  ) 
# {   
#     testing_dataset$Predict_result <- predict(model, testing_dataset, type = "class")
#     return(testing_dataset)
# }
# 
# ## Decision trees classifier 
# EvaluateDC <- function( model, testing_dataset  ) 
# {   
#     testing_dataset$Predict_result <- predict(model, testing_dataset, type = "class")
#     return(testing_dataset)
#     
# }
# 
# 


#### For loading bars
#devtools::install_github('andrewsali/shinycssloaders')
