# Math 510
# Homework 7
# Christopher Porter
# 10.27.2016

#Comments: For assignment 7 you get perfect work and I can see your understanding of the functions and the thoughts to break
# a large function into small functions. But for assignment 8 I found several problems. I tested your function with your own
# test case but I didn't get the desired output. And after study of your code, I think the problem is with the "runByBin". If 
# I have any misunderstanding of your code, please feel free to email me.

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
  
  #################################
  #### Load dependent libraries ###
  #################################
  library(ggplot2)
  library(grid)
  library(gridExtra)
  
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
    return(lapply(df[,or],table))
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
    return(lapply(df[,num],summary))
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
    return(numericDf)
  }
  
  # Subset the Dataframe to the Numeric variables only (exclude logical)
  createCatDf <- function(df){
    
    # Create boolean vector to find all factor variables 
    fact <- unlist(lapply(df, is.factor))
    
    # Create boolean vector to find all logical variables 
    bool <- unlist(lapply(df, is.logical))
    
    # Create boolean vector where true is logical or boolean vector
    or <- fact | bool  
    
    # Create a dataframe from the vector
    catDf <- df[,or]
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
    
    return(barPlot)
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
  
  return(resultList)
}

theResults <- explore(theData, swtch="on", bnVct = c(30,20,10,49))

print(theResults)
