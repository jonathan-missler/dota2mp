#loading the packages
install.packages('tidyverse', dependencies = TRUE)
install.packages('sm', dependencies = TRUE)
install.packages('extrafont', dependencies = TRUE)

library(tidyverse)
library(sm)
library(extrafont)

#only done for aesthetic reasons, if the font 'Charter' is not available, this can be skipped
font_import()
loadfonts()

#loading the data
rawdata <-read_csv("~/desktop/Bachelorarbeit/Daten/MPData2.csv")

#converting the boolean win-variables to take values of 0 and 1
rawdata$radiant_win <- as.numeric(rawdata$radiant_win)
rawdata$win <- as.numeric(rawdata$win)

head(rawdata)



#writing functions for analysis

#the log-likelihood function 

logLikelihood <- function(beta, response, design){
  
  l <- sum(-response*log(1 + exp(-(design%*%beta))) - (1-response)*log(1 + exp(design%*%beta)))
  return(-l) #returns the negative log-likelihood
}


#estimating parameters beta
betaEstimation <- function(init,response,design){
  fit <-optim(par=init, fn=logLikelihood, response = response, design= design, method="BFGS", hessian = TRUE)#optimizes the log-likelihood
  betahat <- fit$par#assigns parameters to betahat
  
  AIC <- 2*logLikelihood(betahat,response, design)+2*length(betahat)#calculates the akkaike information criterion
  covBeta <- solve(fit$hessian)#covariance matrix of beta
  

  
  pvalues<-vector()#empty vector for p-values
  t <- vector()#empty vector for t-statistics
  
  #calculating t-statistics for every parameter
  for(i in 1:length(betahat)){
    tSquared <- betahat[i]^2/covBeta[i,i]
    t <- c(t,sqrt(tSquared))
  }
  
  #calculating p-value for every parameter
  for(i in 1:length(betahat)){
    #p <- 2*(1-pnorm(t[i],mean=0, sd=1))
    p <- 2*pnorm(-abs(t[i]))
    pvalues <- c(pvalues, p)
  }
  
  #Standard deviation
  sd <- sqrt(diag(covBeta))
  
  
  #Confidence interval at 95%
  CI95min <- vector()
  
  for(i in 1:length(betahat)){
    
    cmin <- betahat[i]- qnorm(0.975) * sqrt(covBeta[i,i])
    CI95min <- c(CI95min, cmin)
  } #vector of lower bounds
  
  CI95max <- vector()
  
  for(i in 1:length(betahat)){
    
    cmax <- betahat[i] + qnorm(0.975) * sqrt(covBeta[i,i])
    CI95max <- c(CI95max, cmax)
  }  #vector of upper bounds
  
  #likelihood ratio test for model significance
  lrt <- 2*(logLikelihood(rep(0, length(betahat)), response, design) - logLikelihood(betahat, response, design)) #likelihood ratio
  pValLRT <- pchisq(lrt, df = length(betahat), lower.tail = FALSE)#corresponding p-value
  
  output <- data.frame(betahat, sd, t, pvalues, CI95min, CI95max)
  
  returnList <- list("Output"= output, "AIC" = AIC, "Fit" = fit, "LRT" = c(lrt,pValLRT))
  
  return(returnList)
}

#counts observations that fulfill a certain condition
ifCounter <- function(x, condi){
  counter <- 0
  for(i in 1:length(x)){
    if(x[i] == condi){
      counter <- counter+1
    }
  }
  return(counter)
}

#data manipulation to transform player data into match data
dataManip <- function(){
  k <- 1
  
  radiant_gpm <- vector()
  radiant_xpm <- vector()
  radiant_gpm10 <- vector()
  radiant_xpm10 <- vector()
  radiant_dn10 <- vector()
  
  dire_gpm <- vector()
  dire_xpm <- vector()
  dire_gpm10 <- vector()
  dire_xpm10 <- vector()
  dire_dn10 <- vector()
  
  match_radiant_win <- vector()
  matchID <- vector()
  
  while(k <= length(rawdata$match_id)){
    radgpmhelp <- 0
    radxpmhelp <- 0
    radgpm10help <- 0
    radxpm10help <- 0
    raddn10help <- 0
    
    diregpmhelp <- 0
    direxpmhelp <- 0
    diregpm10help <- 0
    direxpm10help <- 0
    diredn10help <- 0
    
    for(j in 0:9){
      if((rawdata$radiant_win[k+j] == 1 & rawdata$win[k+j] == 1) | (rawdata$radiant_win[k+j] == 0 & rawdata$win[k+j] == 0)){
      radgpmhelp <- radgpmhelp + rawdata$gold_per_min[k+j]
      radxpmhelp <- radxpmhelp + rawdata$xp_per_min[k+j]
      radgpm10help <- radgpm10help + rawdata$gold_t10[k+j]/10
      radxpm10help <- radxpm10help + rawdata$xp_t10[k+j]/10
      raddn10help <- raddn10help + rawdata$dn_t10[k+j]/10
      }
      
      if((rawdata$radiant_win[k+j] == 1 & rawdata$win[k+j] == 0) | (rawdata$radiant_win[k+j] == 0 & rawdata$win[k+j] == 1)){
      diregpmhelp <- diregpmhelp + rawdata$gold_per_min[k+j]
      direxpmhelp <- direxpmhelp + rawdata$xp_per_min[k+j]
      diregpm10help <- diregpm10help + rawdata$gold_t10[k+j]/10
      direxpm10help <- direxpm10help + rawdata$xp_t10[k+j]/10
      diredn10help <- diredn10help + rawdata$dn_t10[k+j]/10
      }
    }
  radiant_gpm <- c(radiant_gpm, radgpmhelp)
  radiant_xpm <- c(radiant_xpm, radxpmhelp)
  radiant_gpm10 <- c(radiant_gpm10, radgpm10help)
  radiant_xpm10 <- c(radiant_xpm10, radxpm10help)
  radiant_dn10 <- c(radiant_dn10, raddn10help)
    
  dire_gpm <- c(dire_gpm, diregpmhelp)
  dire_xpm <- c(dire_xpm, direxpmhelp)
  dire_gpm10 <- c(dire_gpm10, diregpm10help)
  dire_xpm10 <- c(dire_xpm10, direxpm10help)
  dire_dn10 <- c(dire_dn10, diredn10help)
  
  match_radiant_win <- c(match_radiant_win, rawdata$radiant_win[k])
  matchID <- c(matchID, rawdata$match_id[k])
  
  k <- k + 10
  }
  output <- data.frame(radiant_gpm,radiant_xpm,radiant_gpm10,radiant_xpm10,
                       radiant_dn10,dire_gpm,dire_xpm,dire_gpm10,
                       dire_xpm10,dire_dn10,match_radiant_win, matchID)
  return(output)
}

#creates a vector of ones for the design matrix
ones <- function(x){
  one <- rep(1, length(x))
  return(one)
}

#calculates accuracy of model
accuracy <- function(x,y){
  counter <- 0
  for(i in 1:length(x)){
    if(round(x[i]) == y[i]){
      counter <- counter + 1
    }
  }
  output <- counter/length(y)
  return(output)
}

#counts radiant wins
wincounter <- function(){
  counter <- 0
  for(i in 1:length(winprobs)){
    
    if(winprobs[i] >= 0.5){
      counter <- counter + 1
    }
  }
  return(counter)
}

#counts variables within an interval
intervalCounter <- function(x, lower_bound, upper_bound){
  counter <- 0
  for(i in 1:length(x)){
    if(lower_bound <= x[i] & x[i] <= upper_bound){
      counter <- counter+1
    }
  }
  return(counter/length(x))
}



#actual analysis

#transforming the data
matchdata <- dataManip()

#assigning initial values for optim()
initial <- c(0.001,0.001,0.001)



#exploratory analysis
ggplot(data=matchdata, aes(x=(radiant_gpm10-dire_gpm10), y= match_radiant_win))+
  geom_point()+
  labs(x="GPM advantage at 10min.", y="Radiant win")+
  theme(text=element_text(family="Charter", size= 11))

ggplot(data=matchdata, aes(x=(radiant_xpm10-dire_xpm10), y= match_radiant_win))+
  geom_point()+
  labs(x="XPM advantage at 10min.", y="Radiant win")+
  theme(text=element_text(family="Charter", size= 11))

ggplot(data=matchdata, aes(x=(radiant_dn10-dire_dn10), y= match_radiant_win))+
  geom_point()+
  labs(x="DPM advantage at 10min.", y="Radiant win")+
  theme(text=element_text(family="Charter", size= 11))

mean(matchdata$match_radiant_win)

#creating a training and a testing subset of the match data
set.seed(1866)

sample <- sample.int(n = nrow(matchdata), size = floor(0.75*nrow(matchdata)), replace = F)
train_matchdata <- matchdata[sample, ]
test_matchdata  <- matchdata[-sample, ]


#training the model with the training set

#creating a data frame for training data
trainDF <- data.frame(ones(train_matchdata$radiant_gpm), train_matchdata$radiant_gpm-train_matchdata$dire_gpm, 
                      train_matchdata$radiant_xpm-train_matchdata$dire_xpm)

#turning it into a matrix
trainDesign <- data.matrix(trainDF)

#training the model
trained_model <- betaEstimation(initial, train_matchdata$match_radiant_win, trainDesign)
trained_model$Fit$convergence

#testing the model on the test data

#creating a dataframe for the test data
testDF <- data.frame(ones(test_matchdata$radiant_gpm), test_matchdata$radiant_gpm-test_matchdata$dire_gpm, 
                     test_matchdata$radiant_xpm-test_matchdata$dire_xpm)

#turning it into a matrix
testDesign <- data.matrix(testDF)

#calculating the predicted probabilities
predictedprobs <- exp(testDesign %*% trained_model$Output$betahat)/(1+exp(testDesign %*% trained_model$Output$betahat))

#calculating the accuracy of the predicted probabilities
accuracy(predictedprobs, test_matchdata$match_radiant_win)

ggplot(data= data.frame(predictedprobs, 1:length(predictedprobs)),aes(x=1:length(predictedprobs), y=predictedprobs))+
  geom_point()

intervalCounter(predictedprobs, 0.2, 0.8)



#using data at the 10 minute mark gpm
initialgpm <- c(0.001,0.001)

trainDF10gpm <- data.frame(ones(train_matchdata$radiant_gpm10), (train_matchdata$radiant_gpm10-train_matchdata$dire_gpm10))

#turning it into a matrix
trainDesign10gpm <- data.matrix(trainDF10gpm)

#training the model
trained_model10gpm <- betaEstimation(initialgpm, train_matchdata$match_radiant_win, trainDesign10gpm)

#testing the model on the test data

#creating a dataframe for the test data
testDF10gpm <- data.frame(ones(test_matchdata$radiant_gpm10), test_matchdata$radiant_gpm10-test_matchdata$dire_gpm10)

#turning it into a matrix
testDesign10gpm <- data.matrix(testDF10gpm)

#calculating the predicted probabilities
predictedprobs10gpm <- exp(testDesign10gpm %*% trained_model10gpm$Output$betahat)/(1+exp(testDesign10gpm %*% trained_model10gpm$Output$betahat))

#calculating the accuracy of the predicted probabilities
accuracy(predictedprobs10gpm, test_matchdata$match_radiant_win)

ggplot(data= data.frame(predictedprobs10gpm, 1:length(predictedprobs10gpm)),aes(x=1:length(predictedprobs10gpm), y=predictedprobs10gpm))+
  geom_point()






#using data at the 10 minute mark xpm


trainDF10xpm <- data.frame(ones(train_matchdata$radiant_gpm10), (train_matchdata$radiant_xpm10-train_matchdata$dire_xpm10))

#turning it into a matrix
trainDesign10xpm <- data.matrix(trainDF10xpm)

#training the model
trained_model10xpm <- betaEstimation(initialgpm, train_matchdata$match_radiant_win, trainDesign10xpm)

#testing the model on the test data

#creating a dataframe for the test data
testDF10xpm <- data.frame(ones(test_matchdata$radiant_gpm10), test_matchdata$radiant_xpm10-test_matchdata$dire_xpm10)

#turning it into a matrix
testDesign10xpm <- data.matrix(testDF10xpm)

#calculating the predicted probabilities
predictedprobs10xpm <- exp(testDesign10xpm %*% trained_model10xpm$Output$betahat)/(1+exp(testDesign10xpm %*% trained_model10xpm$Output$betahat))

#calculating the accuracy of the predicted probabilities
accuracy(predictedprobs10xpm, test_matchdata$match_radiant_win)

ggplot(data= data.frame(predictedprobs10xpm, 1:length(predictedprobs10xpm)),aes(x=1:length(predictedprobs10xpm), y=predictedprobs10xpm))+
  geom_point()







#using data at the 10 minute mark gpm + xpm


trainDF10xg <- data.frame(ones(train_matchdata$radiant_gpm10), (train_matchdata$radiant_gpm10-train_matchdata$dire_gpm10), 
                      (train_matchdata$radiant_xpm10-train_matchdata$dire_xpm10))

#turning it into a matrix
trainDesign10xg <- data.matrix(trainDF10xg)

#training the model
trained_model10xg <- betaEstimation(initial, train_matchdata$match_radiant_win, trainDesign10xg)

#testing the model on the test data

#creating a dataframe for the test data
testDF10xg <- data.frame(ones(test_matchdata$radiant_gpm10), test_matchdata$radiant_gpm10-test_matchdata$dire_gpm10, 
                    test_matchdata$radiant_xpm10-test_matchdata$dire_xpm10)

#turning it into a matrix
testDesign10xg <- data.matrix(testDF10xg)

#calculating the predicted probabilities
predictedprobs10xg <- exp(testDesign10xg %*% trained_model10xg$Output$betahat)/(1+exp(testDesign10xg %*% trained_model10xg$Output$betahat))

#calculating the accuracy of the predicted probabilities
accuracy(predictedprobs10xg, test_matchdata$match_radiant_win)

#plotting the data
ggplot(data= data.frame(predictedprobs10xg, test_matchdata$radiant_gpm10-test_matchdata$dire_gpm10), 
       aes(x= test_matchdata$radiant_gpm10-test_matchdata$dire_gpm10, y=predictedprobs10xg))+
      geom_point()+
      labs(x="GPM advantage at 10 min.", y="Predicted probabilities")+
      theme(text=element_text(family="Charter", size=11))

ggplot(data= data.frame(predictedprobs10xg, test_matchdata$radiant_xpm10-test_matchdata$dire_xpm10), 
       aes(x= test_matchdata$radiant_xpm10-test_matchdata$dire_xpm10, y=predictedprobs10xg))+
      geom_point()+
      labs(x="XPM advantage at 10 min.", y="Predicted probabilities")+
      theme(text=element_text(family="Charter", size=11))

#Analysing distributional characteristics
intervalCounter(predictedprobs10xg, 0.2, 0.8)
intervalCounter(test_matchdata$radiant_gpm10-test_matchdata$dire_gpm10, -500, 500)
intervalCounter(test_matchdata$radiant_xpm10-test_matchdata$dire_xpm10, -500, 500)


#using data at the 10 minute mark gpm + xpm + denies
initialxgd <- c(0.001,0.001,0.001,0.001)

trainDF10xgd <- data.frame(ones(train_matchdata$radiant_gpm10), (train_matchdata$radiant_gpm10-train_matchdata$dire_gpm10), 
                          (train_matchdata$radiant_xpm10-train_matchdata$dire_xpm10), train_matchdata$radiant_dn10-train_matchdata$dire_dn10)

#turning it into a matrix
trainDesign10xgd <- data.matrix(trainDF10xgd)

#training the model
trained_model10xgd <- betaEstimation(initialxgd, train_matchdata$match_radiant_win, trainDesign10xgd)

#testing the model on the test data

#creating a dataframe for the test data
testDF10xgd <- data.frame(ones(test_matchdata$radiant_gpm10), test_matchdata$radiant_gpm10-test_matchdata$dire_gpm10, 
                         test_matchdata$radiant_xpm10-test_matchdata$dire_xpm10, test_matchdata$radiant_dn10-test_matchdata$dire_dn10)

#turning it into a matrix
testDesign10xgd <- data.matrix(testDF10xgd)

#calculating the predicted probabilities
predictedprobs10xgd <- exp(testDesign10xgd %*% trained_model10xgd$Output$betahat)/(1+exp(testDesign10xgd %*% trained_model10xgd$Output$betahat))

#calculating the accuracy of the predicted probabilities
accuracy(predictedprobs10xgd, test_matchdata$match_radiant_win)

ggplot(data= data.frame(sort(predictedprobs10xgd), 1:length(predictedprobs10xgd)),aes(x=1:length(predictedprobs10xgd), y=sort(predictedprobs10xgd)))+
     geom_point()+
     labs(x="index", y="predicted probabilities")+
     theme(text=element_text(family="Charter", size=12))

intervalCounter(predictedprobs10xgd, 0.2, 0.8)





#Non-parametric approach using local likelihood estimation

#choosing the smoothing parameter h for sm.binomial() for gpm
smParg <- h.select(x=train_matchdata$radiant_gpm10-train_matchdata$dire_gpm10,
                  y=train_matchdata$match_radiant_win, method ="aicc")

#train the model with train_data set and evaluate it at the test_data set
npEstg <-sm.binomial(x=train_matchdata$radiant_gpm10-train_matchdata$dire_gpm10,
                    y=train_matchdata$match_radiant_win,
                    h=smParg, eval.points= test_matchdata$radiant_gpm10-test_matchdata$dire_gpm10)

#accuracy of estimated probabilities for the test_data set
accuracy(npEstg$estimate, test_matchdata$match_radiant_win)

#plotting the estimated probabilities
plot(sort(npEstg$estimate))




#choosing the smoothing parameter h for sm.binomial() for xpm
smParx <- h.select(x=train_matchdata$radiant_xpm10-train_matchdata$dire_xpm10,
                  y=train_matchdata$match_radiant_win, method ="aicc")

#train the model with train_data set and evaluate it at the test_data set
npEstx <-sm.binomial(x=train_matchdata$radiant_xpm10-train_matchdata$dire_xpm10,
                    y=train_matchdata$match_radiant_win,
                    h=smParx, eval.points= test_matchdata$radiant_xpm10-test_matchdata$dire_xpm10)

#accuracy of estimated probabilities for the test_data set
accuracy(npEstx$estimate, test_matchdata$match_radiant_win)

#plotting the estimated probabilities
plot(sort(npEstx$estimate))



#choosing the smoothing parameter h for sm.binomial() for xpm
smPardn <- h.select(x=train_matchdata$radiant_dn10-train_matchdata$dire_dn10,
                   y=train_matchdata$match_radiant_win, method ="aicc")

#train the model with train_data set and evaluate it at the test_data set
npEstdn <-sm.binomial(x=train_matchdata$radiant_dn10-train_matchdata$dire_dn10,
                     y=train_matchdata$match_radiant_win,
                     h=smPardn, eval.points= test_matchdata$radiant_dn10-test_matchdata$dire_dn10)

#accuracy of estimated probabilities for the test_data set
accuracy(npEstdn$estimate, test_matchdata$match_radiant_win)

#plotting the estimated probabilities
plot(sort(npEstdn$estimate))
