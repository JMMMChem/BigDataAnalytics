###Francesco Mora 100439601
###Ignacio Soria Ramírez 100443660
###José María Martínez Marín 100443343

######PREDICTIVE MODELING

###EXERCISE A3###

##################################################################
#1###PRELIMINARY OPERATIONS#######################################
##################################################################


#1.1###SETUP######################################################

rm(list = ls()); 
gc();

library(MASS)

color_1 <- "deepskyblue2"
color_2 <- "seagreen2"
color_3 <- "orange2"
color_4 <- "darkorchid4"
color_5 <- "aquamarine"
color_6 <- "darkslategrey"
color_7 <- "chocolate3"
color_8 <- "deeppink"
color_9 <- "darkgoldenrod"
color_10 <- "black"

#1.2###LOAD THE DATASET###########################################

Original <- read.delim(file="challenger.txt", header=TRUE, sep="\t")
challenger <- Original

attach(challenger)



##################################################################
#####PART 1#######################################################
##################################################################

#Do a Poisson regression of the total number of incidents, 
#nfails.field + nfails.nozzle, on temp. Interpret the regression.
#Are the effect of temp significant with alpha=0.01?

challenger$tot_inc = challenger$nfails.field + challenger$nfails.nozzle
plot(tot_inc ~ temp, data=challenger)
pois1 <- glm(tot_inc ~ temp, data = challenger, family = poisson)
summary(pois1)
exp(pois1$coefficients)



##################################################################
#####PART 2#######################################################
##################################################################

#Plot the data and the fitted Poisson regression curve.

xaxis <- seq(-2, 30, l = 100)
z <- pois1$coefficients[1] + pois1$coefficients[2]*xaxis
plot(tot_inc ~ temp, data=challenger, xlab="Temperature [°C]", ylab="Incidents", main="Poisson Regression of Incidents VS Temperature", xlim=c(-2,30))
lines(xaxis, exp(z), col = color_2)
legend(-3,2,legend="Poisson linear",col=color_2, lty=1, cex=0.9)


##################################################################
#####PART 3#######################################################
##################################################################

#Predict the expected number of incidents at temperatures -0.6 
#and 11.67.

new_data <- data.frame(temp=c(-0.6,11.67))
inc <- predict(pois1, newdata=new_data, type="response")
print(paste0("The prediction for temp=-0.6 is: ", inc[1]))
print(paste0("The prediction for temp=11.67 is: ", inc[2]))


##################################################################
#####PART 4#######################################################
##################################################################

#What are the confidence intervals for the expected number of 
#incidents at the previous temperatures? Draw the confidence 
#intervals curves onto the plot of step A.


predictCIsPoisson <- function(object, newdata, level = 0.99) {
  
  prediction <- predict(object = object, newdata = newdata, se.fit = TRUE)
  za <- qnorm(p = (1 - level) / 2)
  lwr <- prediction$fit + za * prediction$se.fit
  upr <- prediction$fit - za * prediction$se.fit
  fit <- exp(prediction$fit)
  lwr <- exp(lwr)
  upr <- exp(upr)
  tot <- cbind(fit, lwr, upr)
  colnames(tot) <- c("fit", "lwr", "upr")
  return(tot)
}

CI <- predictCIsPoisson(pois1,newdata=new_data)
print(paste0("The confidence interval for temp=-0.6 is between ", round(CI[1,2],2)," and ", round(CI[1,3],2)))
print(paste0("The confidence interval for temp=11.67 is between ", round(CI[2,2],2)," and ", round(CI[2,3],2)))

new_data2 <- data.frame(temp=seq(-2, 30, l = 100))
matrix <- predictCIsPoisson(pois1,newdata=new_data2)
fittedval <- matrix[,1]
lwrCI <- matrix[,2]
uprCI <- matrix[,3]
lines(xaxis, lwrCI, lty='dashed', col = color_3)
lines(xaxis, uprCI, lty='dashed', col = color_3)
polygon(c(xaxis,rev(xaxis)),c(lwrCI,rev(uprCI)),col = adjustcolor(color_3,alpha.f=0.2) , border = NA)



##################################################################
#####PART 5#######################################################
##################################################################

#Can you improve the explanation of nfails.field + nfails.nozzle 
#by using a polynomial model in temp? Explore and comment on your
#results.

pois2 <- glm(tot_inc ~ temp + I(temp^2), data = challenger, family = poisson)
z2 <- pois2$coefficients[1] + pois2$coefficients[2]*xaxis + pois2$coefficients[3]*(xaxis^2)
lines(xaxis, exp(z2), lty='dotted',col = color_4)
legend(-3,1.3,legend="Poisson quadratic",col=color_4, lty='dotted', cex=0.9)
summary(pois2)


pois3 <- glm(tot_inc ~ temp + I(temp^2) + I(temp^3), data = challenger, family = poisson)
z3 <- pois3$coefficients[1] + pois3$coefficients[2]*xaxis + pois3$coefficients[3]*(xaxis^2) + pois3$coefficients[4]*(xaxis^3)
lines(xaxis, exp(z3), lty='dotted',col = color_7)
legend(-3,0.6,legend="Poisson cubic",col=color_7, lty='dotted', cex=0.9)
summary(pois3)

BIC(pois1,pois2,pois3)

print("The best model is Pois1 beause it has the minimum BIC")


###EXERCISE B4###

##################################################################
#####PART 2#######################################################
##################################################################

#0) Parameters

set.seed(123)
size <- 50 #This value is just an hypothesis
N <- 20 #This value is given
level=0.90 #This value is given

#Function that calculates and plots the confidence set for a group of X values

confset <- function(object, newdata, x_check, level=0.90) {
  pred_ <- predict(object=object, newdata=newdata, se.fit=TRUE)
  z <- qnorm(p = (1 - level) / 2)
  lwr_ <- pred_$fit + z * pred_$se.fit
  upr_ <- pred_$fit - z * pred_$se.fit
  upr_ <- N/(1+exp(-upr_))
  lwr_ <- N/(1+exp(-lwr_))
  upr_floor_ <- floor(upr_)
  lwr_ceil_ <- ceiling(lwr_)
  fl_ceil_ <- cbind(lwr_ceil_,upr_floor_)
  for (j in x_check) {
    aaa <- fl_ceil_[j,1]:fl_ceil_[j,2]
    for (i in aaa) {
      points(x_vect[j],i, col="green", pch=17)
    }
  }
  print("The confidence sets for the given x are: ")
  return(fl_ceil_[x_check,1:2])
}

#Function that returns the confidence set of an individual x

confset2 <- function(object, x, level=0.90) {
  pred_ <- predict(object=object, newdata=data.frame(x = x), se.fit=TRUE)
  z <- qnorm(p = (1 - level) / 2)
  lwr_ <- pred_$fit + z * pred_$se.fit
  upr_ <- pred_$fit - z * pred_$se.fit
  upr_ <- N/(1+exp(-upr_))
  lwr_ <- N/(1+exp(-lwr_))
  upr_floor_ <- floor(upr_)
  lwr_ceil_ <- ceiling(lwr_)
  fl_ceil_ <-  seq(lwr_ceil_, upr_floor_, by=1)
  return(fl_ceil_)
}

##################################################################
#####PART 3 and 4#################################################
##################################################################

#Plot population binomial regression curve and confidence set

#Generate random data

set.seed(123)
x <- rnorm(size, 0, sd = 1)
y <- 1 + 2 * x
y <- rbinom(size, N, prob = 1 / (1 + exp(-y)))
df <- data.frame(x = x, y=y)
plot(x,y)
df$prop <- df$y/N

#Fit model

df$y <- as.factor(df$y)
bin <- glm(prop ~ x, data = df, family = "binomial")
summary(bin)

#Fit the predicted function

x_vect <- seq(-3, 3, l = 100)
eta <- bin$coefficients[1] + bin$coefficients[2]*x_vect
prediction <- N/(1+exp(-eta))
lines(x_vect,prediction)

#Draw the CIs

newdata <- data.frame(x = x_vect)
pred <- predict(bin, newdata = newdata, se.fit=TRUE)
za <- qnorm(p = (1 - level) / 2)
lwr <- pred$fit + za * pred$se.fit
upr <- pred$fit - za * pred$se.fit
upr <- N/(1+exp(-upr))
lwr <- N/(1+exp(-lwr))
lines(x_vect,upr, lty="dashed", col="red")
lines(x_vect,lwr, lty="dashed", col="red")

#Draw the Confidence sets for some X

x_to_check <- c(30,45,80)
upr_floor <- floor(upr)
lwr_ceil <- ceiling(lwr)
fl_ceil <- cbind(lwr_ceil,upr_floor)

confset(bin,newdata,x_to_check)


##################################################################
#####PART 5#######################################################
##################################################################

#Validation of the confidence set through 1000 simulations

x_simulated <- c(-1, 0.25, 2) #values of x to simulate
n_simulations = 1000
sim_conf_interval <- matrix(0,length(x_simulated),1) #initialise variable to store results

#Perform 1000 simulations for each of the elements in x_simulated

for(i in 1:length(x_simulated)) {
  confint_for_x <- confset2(bin, x_simulated[i]) #define the confidence interval for the current X
  inside_interval <- matrix(0,n_simulations,1) #initialise variable where results will be stored
  y_trial <- 1 + 2 * x_simulated[i]
  
  #1000 simulations
  
  for (j in 1:n_simulations) {
    y_trial_rand <- rbinom(1, N, prob = 1 / (1 + exp(-y_trial))) #random generation of y
    if (is.element(y_trial_rand, confint_for_x)) {
      inside_interval[j] <- 1
    }
  }
  
  sim_conf_interval[i] <- 100*sum(inside_interval)/n_simulations #calculate and store the results for each x
}

#Print the results

for(i in 1:length(x_simulated)) {
  print(paste0("Simulated effectiveness of confidence set ",sim_conf_interval[i],"% for x=",x_simulated[i]))
}



###EXERCISE C7###

##################################################################
#####PART 1, 2 & 3################################################
##################################################################


library(glmnet)
library(Matrix)

l <- 4 #sample size (n = 2^l)
m <- 5 #predictors
k <- 4 #folds for cross validation
iter <- 20 #number of iterations)



########PARTS 1,2 & 3 - LASSO VARIABLE SELECTION

#Define function that generates random data, selects variables using lasso, 
#repeats the process several times and computes probability of selecting true model given:
#---l: sample size (n=2^l) (l can have decimals and n will be rounded)
#---m: number of predictors (siempre es 5, no tiene sentido)
#---iter: number of monte carlo simulations
#---k: number of folds in lasso's cross validation

prob_true_model <- function(l,m,iter,k) {
  ###------1------VARIABLES TO DEFINE TRUE MODEL
  #Initialise vector in which we will store if the selected model was the true model
  true_mod_found <- matrix(0,iter,1)
  
  #sample size
  n <- round(2^l, digits = 0) 
  
  #True model betas and shape
  beta <-  c(0.5, 1, 1, 0, 0, 0)
  true_model <- matrix(c(1, 1, 0, 0, 0), nrow = 5, ncol = 1)
  
  ###------2------SIMULATION LOOP
  for(i in 1:iter) {
    
    #Generate random data sample
    x <- matrix(rnorm(n*m,mean=0,sd=1), n, m)  #X matrix without intercept
    x1 <- matrix(1, n, 1) #column of ones
    x_with_intetrcept <- cbind(x1,x) #matrix with ones and Xs
    eps <-  matrix(rnorm(n,mean=0,sd=1), n, 1) #random error
    
    y <- x_with_intetrcept%*%beta+eps #response variable
    
    #Create a 4-fold cross validation lasso fit
    kcvLasso <- cv.glmnet(x = x, y = y, alpha = 1, nfolds = k, grouped=FALSE)
    
    modLassoCV <- kcvLasso$glmnet.fit
    
    #Select predictors using only the nonzero betas regressed and with the minimum lambda corresponding to the cross validation
    selPreds <- predict(modLassoCV, type = "coefficients",s=kcvLasso$lambda.min)[-1, ] != 0
    
    #Generate a vector of the fitted model shape (1-> predictor selected, 0-> predictor not selected)
    model <- matrix(1, 5, 1) #vector of ones
    model <- model*selPreds #predictors not selected are transformed to zero
    
    check <- all.equal(true_model,model)
    check <- identical(TRUE,check)
    if (check == TRUE) {
      true_mod_found[i] <- 1
    }
    
  }
  ###------3------PROBABILITY OF SELECTED MODEL
  
  probability = sum(true_mod_found)/iter
  return(probability)
}


##################################################################
#####PART 4#######################################################
##################################################################

#compute the probability of obtaining the true model for 2^2 = 4 samples, 200 simulations and 3 folds
prob_true_model(2,m,200,3)

#with a larger data sample (n=2^4=16) the probability is much larger
prob_true_model(7,m,200,3) 



##################################################################
#####PART 5#######################################################
##################################################################

sample_sizes = seq(3, 10, by=1) #define a list of sample sizes
prob_3_folds <- matrix(0,length(sample_sizes),1) #initialise a list with probabilities of 0

#Compute the probability of finding the true model for a fixed number of validation folds of 3
for (i in 1:length(sample_sizes)) {
  prob_3_folds[i] = prob_true_model(sample_sizes[i],m,200,3) #probability for sample size i (2^2, 2^3, ..., 2^10), 200 simulations, 3 folds
}

#plot the results
plot(sample_sizes,prob_3_folds, ylim=c(0,0.5))
lines(sample_sizes,prob_3_folds)


##################################################################
#####VARIATIONS OF PART 2-DIFFERENT SIZES OF K (n, n/2, n/4, n/8)#
##################################################################

sample_sizes_2 = seq(3, 10, by=1) #list of sample sizes to test
folds_div <- c(2,1) #list of values dividing n for k fold cross validation (n, n/2)

folds_text <- c("n/2", "n") #text to be stored in the results

results <- data.frame("l"=integer(), "n"=integer(), "k"=character(), "Probability_true_model"=double()) #create dataframe where results will be stored

#This takes 10 minutes

#Iterate for 
###k = (n, n/2)###
#and all sample sizes

for (j in 1:length(folds_div)){
  for (i in 1:length(sample_sizes_2)) {
    print(paste0("sample size ", i," of ",length(sample_sizes_2), ", folds ",j," of ", length(folds_div)))
    probability <- prob_true_model(sample_sizes_2[i],5,200,(2^sample_sizes_2[i])/folds_div[j]) #probability for sample size i (2^3, 2^4, ..., 2^10), 200 simulations, n/2 and n folds
    
    results[nrow(results)+1,] <- list(sample_sizes_2[i],2^sample_sizes_2[i],folds_text[j],probability) #store the results
  }
}



#Do k=n/4 and k=n/8 separately as some k values don't work for small sample sizes when the folds are too small

###k=n/4###

#k=n/4
sample_sizes_4 = seq(4, 10, by=1)  #define sample sizes for k = n/4

for (i in 1:length(sample_sizes_4)) {
  print(paste0("Step ", i," of ",length(sample_sizes_4)))
  probability <- prob_true_model(sample_sizes_4[i],5,200,(2^sample_sizes_4[i])/4) #probability for sample size i (2^4, 2^5, ..., 2^10), 200 simulations, n/4 folds
  
  results[nrow(results)+1,] <- list(sample_sizes_4[i],2^sample_sizes_4[i],"n/4",probability) #store the results
}


#k=n/8
sample_sizes_8 = seq(5, 10, by=1)  #define sample sizes for k = n/8

for (i in 1:length(sample_sizes_8)) {
  print(paste0("Step ", i," of ",length(sample_sizes_8)))
  probability <- prob_true_model(sample_sizes_8[i],5,200,(2^sample_sizes_8[i])/8) #probability for sample size i (2^5, 2^6, ..., 2^10), 200 simulations, n/8 folds
  
  results[nrow(results)+1,] <- list(sample_sizes_8[i],2^sample_sizes_8[i],"n/8",probability) #store the results
}


#Plot the results

plot(results$l[results$k=="n"], results$Probability_true_model[results$k=="n"],main="Consistency of Lasso Selection for Different k Cross Validations", ylab="Probability of selecting the right model", xlab = "log2 of sample size", type="l", col="blue", ylim = c(0,0.4))
lines(results$l[results$k=="n/2"],results$Probability_true_model[results$k=="n/2"], col="red")
lines(results$l[results$k=="n/4"],results$Probability_true_model[results$k=="n/4"], col="green")
lines(results$l[results$k=="n/8"],results$Probability_true_model[results$k=="n/8"], col="purple")

legend("topleft", c("n","n/2","n/4","n/8"), fill=c("blue","red","green","purple"))
