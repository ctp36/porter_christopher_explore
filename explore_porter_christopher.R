# Math 510
# Homework 7
# Christopher Porter
# 10.27.2016

# Define a function to create a data frame for testing purposes
createDF <- function(nrows){
  # Define a function to create a data frame for testing purposes
  # which contains 4 continuous random variables,
  # one logical variable which is a function of the second continuous r.v,
  # and a randomly generated factor variable.
  
  # Generate a dataframe of the 4 continuous r.v.s
  df <- data.frame(matrix(rnorm(nrows*4), nrow=nrows))
  
  # Create the logical variable where the 2nd r.v exceeds negative one-half
  df$lgcX2 <- df$X2 > -0.5
  
  # Create a random factor variable with factor size ranging from 1 to 10.
  df$rndFact <- as.factor(rbinom(nrows, max(rbinom(1, 10, 0.4),1), 0.5))
  
  #return the dataframe
  return(df)  
}

# Create a dataframe to work with.
# First set the seed for testing
set.seed(15243)
theData <- createDF(500)


explore <- function(df, swtch="off", thrshld=0.5, bnVct=NULL){
  
  df      <- theData 
  swtch   <- "off" 
  thrshld <- 0.5 
  bnVct   <- NULL
  
  #################################
  #### Load dependent libraries ###
  #################################
  library(ggplot2)
  library(grid)
  library(gridExtra)
  
  ###################################################
  #### Preliminary Error Handling for Parameters ####
  ###################################################
  
  #Create a boolean vector to test the class of each parameter
  classTestVect <- c(dfTest        = is.data.frame(df),
                     swtchTest     = is.character(swtch), 
                     thrshldTest   = is.double(thrshld), 
                     bnVctTest     = (is.vector(bnVct) | is.null(bnVct)))
  
  #If any of the tests fail, find the location of the first test. If no tests fail, value is NA
  classTest <- match(FALSE,classTestVect)
  
  #Create a message indicating the variable that is not of the right class, optionaly stop the program 
  errorMessage <- function(message_start="Please check the class of", stop_on=TRUE){
    message <- paste(message_start, labels(classTestVect)[classTest])
    if (stop_on) stop(message)
  }
  
  #If there are any failed tests, return the error and stop the program
  if (is.na(classTest)==F) errorMessage()
  
  
  
  ########################
  ### Define Functions ###
  ########################
  
  
  
  # Create Function which returns a frequency table on the logical or categorical variables
  freqTable <- function(df){
    # Create boolean vector to find all factor variables 
    fact <- unlist(lapply(df, is.factor))
    
    # Create boolean vector to find all logical variables 
    bool <- unlist(lapply(df, is.logical))
    
    # Create boolean vector where true is logical or boolean vector
    or <- fact | bool  
    
    #create a frequency table for each boolean and logical vector 
    returnValue <- lapply(df[,or],table)
    
    ########################
    #### Error Checking ####
    ########################
    
    #Check that each element in returnValue is a table 
    classTestVect <- unlist(lapply(returnValue,is.table))    
    
    #If any of the tests fail, find the location of the first test. If no tests fail, value is NA
    classTest <- match(FALSE,classTestVect)
    
    #Report any issues, but continue to run the program
    if (is.na(classTest)==F) errorMessage(
      message_start = "The frequency table for the following variable is is not of class table", 
      stop_on = F)
    
    return(returnValue)
  }  
  
  
  # Create a Function which returns a summary table on the numeric variables
  sumTable <- function(df){
    # Create boolean vector to find all numeric variables 
    num <- unlist(lapply(df, is.numeric))
    
    # Create boolean vector to find all logical variables 
    bool <- unlist(lapply(df, is.logical))
    
    # Remove all boolean vectors
    num <- num & !bool
    
    #create a summary table for each numeric vector 
    returnValue <- lapply(df[,num],summary)
    
    ########################
    #### Error Checking ####
    ########################
    
    #Check that each element in returnValue is a table 
    classTestVect <- unlist(lapply(returnValue,is.table))    
    
    #If any of the tests fail, find the location of the first test. If no tests fail, value is NA
    classTest <- match(FALSE,classTestVect)
    
    #Report any issues, but continue to run the program
    if (is.na(classTest)==F) errorMessage(
      message_start = "The frequency table for the following variable is is not of class table", 
      stop_on = F)
    
    return(returnValue)
  }
  
  
  # Subset the Dataframe to the Numeric variables only (exclude logical)
  createNumericDf <- function(df){
    
    # Boolean vector for numeric and boolean tests
    num  <- unlist(lapply(df, is.numeric))
    bool <- unlist(lapply(df, is.logical))
    
    # Numeric only boolean
    num  <- num & !bool
    
    # Create numeric dataframe and return 
    numericDf <- df[ ,num]
    
    ########################
    #### Error Checking ####
    ########################
    
    #Check that the return value is a dataframe 
    classTestVect <- is.data.frame(numericDf)
    
    #If any of the tests fail, find the location of the first test. If no tests fail, value is NA
    classTest <- match(FALSE,classTestVect)
    
    #Report any issues, and end the program
    if (is.na(classTest)==F) errorMessage(
      message_start = "Failed to create the following dataframe:", 
      stop_on = T)    
    
    return(numericDf)
    }
  
  # Subset the Dataframe to the Categorical  variables only (include logical)
  createCatDf <- function(df){
    
    # Create boolean vector to find all factor variables 
    fact <- unlist(lapply(df, is.factor))
    
    # Create boolean vector to find all logical variables 
    bool <- unlist(lapply(df, is.logical))
    
    # Create boolean vector where true is logical or boolean vector
    or <- fact | bool  
    
    # Create a dataframe from the vector
    catDf <- df[,or]
    
    ########################
    #### Error Checking ####
    ########################
    
    #Check that the return value is a dataframe 
    classTestVect <- is.data.frame(catDf)
    
    #If any of the tests fail, find the location of the first test. If no tests fail, value is NA
    classTest <- match(FALSE,classTestVect)
    
    #Report any issues, but continue to run the program
    if (is.na(classTest)==F) errorMessage(
      message_start = "Failed to create the following dataframe:", 
      stop_on = T)
    
    return(catDf)
  }
  
  
  
  # This function calculates the r-squared for two vectors
  calcRSqrd <- function(x){  
    # x is an iterator which goes from 1 to the combinations of data frames 
    # rSqDF is calculted before executing this function and is the two dim combination of every
    # numeric column in our dataframe
    
    # Linear regression of two columns in our Numeric Dataframe
    yhat <- lm(numericDf[,x_y[x,1]] ~ numericDf[,x_y[x,2]])
    
    # Mean of our Dependendt Variable
    ymean <- mean(numericDf[,x_y[x,1]])
    
    # Total Sum of Squares
    SSEtot <- sum((numericDf[,x_y[x,1]] - ymean)^2)  
    
    #Residual Sum of Squares
    SSEres <- sum( (yhat$residuals)^2)  
    
    # R-Squared Calculation
    rSq <- 1 - (SSEres / SSEtot)
    
    ########################
    #### Error Checking ####
    ########################
    
    #Check that the return value is a dataframe 
    classTestVect <- c(varClass = is.numeric(rSq),
                       varRange = (rSq >= 0) & (rSq <= 1) #Since linear regression 
    )
    
    #If any of the tests fail, find the location of the first test. If no tests fail, value is NA
    classTest <- match(FALSE,classTestVect)
    
    #Report any issues, but continue to run the program
    if (is.na(classTest)==F) errorMessage(
      message_start = "The R-Squared Value had the following error:", 
      stop_on = F)
    
    #return the R-Squared Value
    return(rSq)
  }
  
  
  
  
  # This function creates a dataframe for the pearson correlation 
  calcCorr <- function(df){
    colLen <- ncol(df)
    outputLen <- (colLen^2 - colLen) / 2
    
    # Create the pearson correlation matrix of the numeric columns, 
    corMat <- cor(df, method = "pearson")
    
    # Create a logical matrix of the upper triangle of the correlation matrix, no diagonal
    corTriLog <- upper.tri(corMat)
    
    # Name the dimenstions according to the column names of numericDf
    dimnames(corTriLog) <- list(colNames,colNames)
    
    # Create a vector of the unique correlation values, this is the second column of our output DF
    corVec <- corMat[corTriLog]
    
    # Create the Column Name Vector for the output DF
    nameVec <- vector(mode="character", length = outputLen)  
    
    # initialize a counter variable
    n = 1
    
    # Loop through the Logical Matrix going down each column.
    # If the entry in the matrix is true, overwrite each sequential
    # position in the nameVec with the defined format in the paste function ('x-y')
    # nameVec <- paste(colNames,colNames,sep="-")
    # nameVec = nameVec[corTriLog]
    for (y in colNames) {
      for (x in colNames){      
        if (corTriLog[x,y]){         
          nameVec[n] <- paste(x,y,sep="-")
          n = n + 1                
        }      
      }
    }  
    
    # Create the output dataframe
    corrDf <- data.frame(nameVec,corVec)
    
    #name the columns
    colnames(corrDf) <- c('CorrelatedColumns','Correlation')
    
    ########################
    #### Error Checking ####
    ########################    
  
    maxCorr <- max(abs(corrDf[2]))
    
    #Check that the return value is a dataframe 
    classTestVect <- c(varClass = is.data.frame(corrDf),
                       varRange = (maxCorr > 1)
    )
    
    #If any of the tests fail, find the location of the first test. If no tests fail, value is NA
    classTest <- match(FALSE,classTestVect)
    
    #Report any issues, but continue to run the program
    if (is.na(classTest)==F) errorMessage(
      message_start = "The Correlation Matrix had the following error:", 
      stop_on = F)
    
    #return dataframe
    return(corrDf)  
  }
  
  
  # This function creates a density plot of the numeric vectors in a dataframe
  plotDensity <- function(numVec,x=1, b){  
    
    # the numeric vector (numVec) is one of the numeric vectors in our data frame
    # x is an indexing value for our vector (default = 1)
    # b is the number of bins, given by bnVct
    
    # Calculate the Mean
    meanX <- mean(numVec)
    
    # Create a dataframe from the vector
    numDf <- as.data.frame(numVec)  
    
    # create the density plot for the given vector and bin size
    densityPlot <- ggplot(data=numDf, aes(numDf[,x])) + 
      geom_histogram(aes(y=..density..), bins = b, colour= 'Blue', fill= 'Blue') +
      geom_vline(xintercept = meanX, color = 'Red')  
    
    ########################
    #### Error Checking ####
    ########################    
    
    #Check that the return value is a plot 
    classTestVect <- c(varClass = is.ggplot(densityPlot))
    
    #If any of the tests fail, find the location of the first test. If no tests fail, value is NA
    classTest <- match(FALSE,classTestVect)
    
    #Report any issues, but continue to run the program
    if (is.na(classTest)==F) errorMessage(
      message_start = "The density plot has the wrong class:", 
      stop_on = F)
    
    return(densityPlot)
  }
  
  plotFreq <- function(numVec,x=1,b){  
    
    # the numeric vector (numVec) is one of the numeric vectors in our data frame
    # x is an indexing value for our vector (default = 1)
    # b is the number of bins, given by bnVct
    
    # Calculate the Mean  
    meanX <- mean(numVec)
    
    # Create a dataframe from the vector
    numDf <- as.data.frame(numVec)  
    
    # create the density plot for the given vector and bin size
    freqPlot <- ggplot(data=numDf, aes(numDf[,x])) + 
      geom_histogram(stat="bin", position="identity", bins = b, color = "Blue", fill = "Blue") +
      geom_vline(xintercept = meanX, color = 'Red') 
    
    ########################
    #### Error Checking ####
    ########################    
    
    #Check that the return value is a plot 
    classTestVect <- c(varClass = is.ggplot(freqPlot))
    
    #If any of the tests fail, find the location of the first test. If no tests fail, value is NA
    classTest <- match(FALSE,classTestVect)
    
    #Report any issues, but continue to run the program
    if (is.na(classTest)==F) errorMessage(
      message_start = "The freq plot has the wrong class:", 
      stop_on = F)
    
    return(freqPlot)
  }  
  
  plotBar <- function(vect,x=1){    
    #The vect variable is a factor vector from the categorical datafreame   
    #Set Logical to factor
    if (is.logical(vect)){
      vect <- as.factor(vect)
    }
    
    # Convert vector to data frame for ggplot
    vectDf <- as.data.frame(vect)
    
    # create the bar plot for the given vector
    barPlot <- ggplot(data=vectDf, aes(vectDf[,x])) + 
      geom_bar(stat="count", position="stack",  color = "Gray", fill = "Gray")
    
    ########################
    #### Error Checking ####
    ########################    
    
    #Check that the return value is a plot 
    classTestVect <- c(varClass = is.ggplot(barPlot))
    
    #If any of the tests fail, find the location of the first test. If no tests fail, value is NA
    classTest <- match(FALSE,classTestVect)
    
    #Report any issues, but continue to run the program
    if (is.na(classTest)==F) errorMessage(
      message_start = "The bar plot has the wrong class:", 
      stop_on = F)
    
    return(barPlot)
  }
  

  runByBin <- function(thePlot,binVar=NULL){
    # binVar is the binsize value
    # the plot is the type of plot function to run
    # loop through the dataframe and plot for the given bin size.
    lapply(numericDf, thePlot, b=binVar)
  }
  
  ##############################
  ### The Main Run Procedure ###
  ##############################
  
  
  # 1.) Function Defined Above
  # Run the frequency table function on the dataframe
  
  freq <- freqTable(df)
  
  
  
  #2. a.) Function Defined Above
  # Run the Summary function on the dataframe
  
  sum <-sumTable(df)
  
  
  #2. b.) TWo of the above functions are used to create the r-squared data frame.
  #        createNumericDf and calcRSqrd
  
  # create numeric dataframe
  numericDf <- createNumericDf(df)
  
  #################################################################
  # Initialize Vectors Needed for R-Squared and Correlation Tables#
  #################################################################
  
  # create a vector of the column names
  colNames <- colnames(numericDf)
  
  # calculate the length of unique combinations numeric variables
  outputLen <- (length(colNames)^2) / 2
  
  # Initialize 3 vectors: A vector for the names, two iterators
  nameVec <- vector(mode="character", length = outputLen)
  i <- vector(mode="integer", length = outputLen)
  j <- vector(mode="integer", length = outputLen)
  
  
  # Create the Name Vector, and the index vectors to calculate the R-Squared.
  n = 1
  for (y in colNames) {
    for (x in colNames){      
      nameVec[n] <- paste(x,y,sep="-")
      i[n] = match(x,colNames)
      j[n] = match(y,colNames)
      n = n + 1      
    }
  }
  
  #######################
  # Calculate R-Squared #
  #######################
  
  # Create the x, y column position matrix (2 by n) to iterate over 
  x_y <- t(rbind(i,j))
  
  # Loop through (x,y) to  calculate R - Squared
  rSqVec <- as.numeric(unlist(lapply(1:nrow(x_y), calcRSqrd)))
  
  # Create a Dataframe with the column of name vectors
  rSqDFout <- as.data.frame(nameVec)
  
  # Add the r Square Vector
  rSqDFout$rSQ <- rSqVec 
      
  # Store DF in memory
  rSqr <- rSqDFout
  
  
  #2. c.) Create a data frame with the correlation coefficient of each numeric vector 
  # The function is defined above
  
  # Store DF in memory
  corr <- calcCorr(numericDf)
  
  
  ####################
  ### Create Plots ###
  ####################
  
  #3.a. & b.) Plot two histograms (count, density) for each numeric vector and bin size  
  #           And a bar graph for the categorical values
  
  # Create Categorical Dataframe
  catDf <- createCatDf(df)
  
  # Run the Plots on a conditional basis defined in function Call
  if (swtch == "on" | swtch == "grid") {
    # Create the frequency, density and bar plots if on or grid. 
    freqPlots  <- lapply(bnVct, runByBin, thePlot=plotFreq)
    densePlots <- lapply(bnVct, runByBin, thePlot=plotDensity)  
    barPlots   <- lapply(catDf, plotBar)
    
    # If switch is on, print the plots
    if (swtch == "on"){
      print(freqPlots)
      print(densePlots)
      print(barPlots)
    }
    
    # If switch is grid, print the plots in a grid fashion    
    else if (swtch == "grid") {
      # This step doesn't work. Since my plots result in nested - lists 
      # And since I don't know the amount of plots that could be made,
      # I am not using facet_grid here.
      
      # I have tried to figure out a way to create the grid pattern logically,
      # like something similar to the code below.
      
      # Loop through the number of different bin sizes
      for (i in length(bnVct)){
        
        # Calculate the nx and ny for our plot spaces
        n <- ceiling(ncol(numericDf) / 2) 
        m <- floor(ncol(numericDf) / 2)
        # reset the mfrow parameter
        par(mfrow=c(n,m))    
        
        # loop through the number of numeric columns
        for (j in ncol(numericDf)){
          # plot the (i,j) plot
          freqPlots[[i]][j]          
        }
        
      }       
    }    
  }
  
  resultList <- list(freq, sum, rSqr, corr)
  
  
  ########################
  #### Error Checking ####
  ########################    
  
  #Check that the return value is a plot 
  classTestVect <- c(varClass = is.list(resultList))
  
  #If any of the tests fail, find the location of the first test. If no tests fail, value is NA
  classTest <- match(FALSE,classTestVect)
  
  #Report any issues, but continue to run the program
  if (is.na(classTest)==F) errorMessage(
    message_start = "The list of data exploration items is of the wrong class:", 
    stop_on = T)
  return(resultList)
}

theResults <- explore(theData, swtch="on", bnVct = c(30,20,10,49))

print(theResults)
