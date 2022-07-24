setwd("C:/Users/DELL/Desktop/R Files")
insur = read.csv("Assignment 1 - data set csv.csv", stringsAsFactors = TRUE)
head(insur)
summary(insur)
dim(insur)
colSums(is.na(insur))

##Correct Variables as Kilometres, Zone, Bonus and Make are categorical variables
insur$Kilometres <-as.factor(insur$Kilometres)
insur$Zone <-as.factor(insur$Zone)
insur$Bonus <-as.factor(insur$Bonus)
insur$Make <-as.factor(insur$Make)

##Recheck Data Again
summary(insur)


#Install if the package doesn't exist 
install.packages("DataExplorer")
library(DataExplorer)

plot_str(insur)
plot_missing(insur)
plot_histogram(insur)

#Box Plots
boxplot(Insured ~ Kilometres, data = insur,
        varwidth = TRUE, log = "y", las = 1)
title("Insured Vs Kilometres Index")

boxplot(Insured ~ Zone, data = insur,
        varwidth = TRUE, log = "y", las = 1)
title("Insured Vs Zone Index")

boxplot(Insured ~ Bonus, data = insur,
        varwidth = TRUE, log = "y", las = 1)
title("Insured Vs Bonus Index")

#the dimensions of the dataset are 2182 x 7 
#through summary we can see that the mean of kilometre is 3 ie, 15,000-20,000
#similarly the maximum number of claims are 3338 and mean is 51.97 
##we perform corelation to find out the relation between Claims and Payment
b=cor(insur$Claims,insur$Payment)
b*100 #percentage of corelation between claims and payments
#hence we can see that the corelation between claims and payment are 0.9954 ie, 99% corelated , 
plot(insur$Claims,insur$Payment)
c=cor(insur$Claims,insur$Insured)
c*100
#claims and insured have corelation of 91% 
d=cor(insur$Insured,insur$Payment)
d*100
##Payment and Insured has Corelation of 93% 
plot(insur$Claims,insur$Insured)
plot(insur$Payment,insur$Insured)
plot_correlation(insur, type = 'continuous','Insured')

##we can see that the plot between claims and payment was linear

#c#
##we now use linear regression for Payments vs the rest of the data

reg=lm(insur$Payment~insur$Claims+insur$Insured+insur$Make+insur$Bonus+insur$Zone+insur$Kilometres)
summary(reg)
plot(reg)

#RSE (Residual Standard Error)
confint(reg)
sigma(reg)*100/mean(insur$Payment)


aggkm=apply(insur[,c(5,6,7)],2,function(x) tapply(x,insur$Kilometres,mean))
aggkm
##in these 5 zones we could see 1st km dist has been more insured than others with 2nd being close to it
##the amount of claims in the 2nd km distribution is higher hence more payments
aggzone=apply(insur[,c(5,6,7)],2,function(x) tapply(x,insur$Zone,mean))
aggzone
##zone number 4 has higher number of insured vehicle and more amount of claims and payment is more due to claims
aggbonus = apply(insur[,c(5,6,7)],2,function(y) tapply(y,insur$Bonus,mean))
aggbonus

aggmake=apply(insur[,c(5,6,7)],2,function(x) tapply(x,insur$Make,mean))
aggmake

##here we can see that variations are low with 7th group having more insured count

#d#
#to find if claims is affected by Insured amount, Kilometres, zone,bonus, and make

regcl=lm(insur$Claims ~ insur$Insured+insur$Make+insur$Bonus+insur$Zone+insur$Kilometres)
summary(regcl)
plot(regcl)

#create LM model for Payments without claims
regpy=lm(insur$Payment ~ insur$Insured+insur$Make+insur$Bonus+insur$Zone+insur$Kilometres)

#Create Model Equation from LM model

library(dplyr)

model_equation <- function(model, ...) {
  format_args <- list(...)
  
  model_coeff <- model$coefficients
  format_args$x <- abs(model$coefficients)
  model_coeff_sign <- sign(model_coeff)
  model_coeff_prefix <- case_when(model_coeff_sign == -1 ~ " - ",
                                  model_coeff_sign == 1 ~ " + ",
                                  model_coeff_sign == 0 ~ " + ")
  model_eqn <- paste(strsplit(as.character(model$call$formula), "~")[[2]], # 'y'
                     "=",
                     paste(if_else(model_coeff[1]<0, "- ", ""),
                           do.call(format, format_args)[1],
                           paste(model_coeff_prefix[-1],
                                 do.call(format, format_args)[-1],
                                 " * ",
                                 names(model_coeff[-1]),
                                 sep = "", collapse = ""),
                           sep = ""))
  return(model_eqn)
}

library(MASS)

model_equation(reg, digits = 3, trim = TRUE)
model_equation(regcl, digits = 3, trim = TRUE)
model_equation(regpy, digits = 3, trim = TRUE)

##here we can see that all the factors affect the Claims they are significant (pvalues <0.05)

##CASE 1 Interpretation:

#Small city in North		 - 		Zone 5
#8500 km per year		-		2 (1000-15000)
#Bonus (2years)		-		3
#Car Make Type		-		3
#Insured Amount 		-		4621

#Model Equation
Case1_Claim_Model= 71.2982 + 0.0292 * 4621 -  17.2704 * insur$Make3 - 33.3439 * insur$Bonus3 - 35.7368 * insur$Zone5 + 14.2270 * insur$Kilometres2
#Case1_Claim_Model= 
71.2982 + 0.0292 * 4621 -  17.2704 * 3 - 33.3439 * 3 - 35.7368 * 5 + 14.2270 * 2

Case1_Payment_Model= 292306 + 153 * insur$Insured - 86891 * insur$Make3 - 138625 * insur$Bonus3  - 146746 * insur$Zone5  + 83374 * insur$Kilometres2 
#Case1_Claim_Model= 
292306 + 153 * 4621 - 86891 * 3 - 138625 * 3  - 146746 * 5  + 83374 * 2

##CASE 2 Interpretation:

#Halmstad and outskirt		 - 		Zone 1
#12500 km per year		-		2 (1000-15000)
#Bonus (0years)		-		1
#Car Make Type		-		9
#Insured Amount 		-		9500

Case2_Claim_Model= 71.2982 + 0.0292 * 9500 + 117.9532 * insur$Make9 + 14.2270 * insur$Kilometres2
#Case2_Claim_Model= 
71.2982 + 0.0292 * 9500 + 117.9532 * 9 + 14.2270 * 2

Case2_Payment_Model= 292306 + 153 * insur$Insured + 499037 * insur$Make9 + 83374 * insur$Kilometres2 
#Case1_Claim_Model= 
292306 + 153 * 9500 + 499037 * 9 + 83374 * 2

##CASE 3  CLaims Interpretation:

#Uppsala (A large city)		 - 		Zone 2
#22300 km per year		-		4 (20000-25000)
#Bonus (4years)		-		5
#Car Make Type		-		3
#Insured Amount 		-		17500 to 25416 estimation
# let's consider 2 different estimates for Insured amount A & B


Case3A_Claim_Model= 71.2982 + 0.0292 * 17500 - 11.6529 * insur$Zone2 - 36.1410 * insur$Bonus5 - 13.1715 * insur$Kilometres4 - 17.2704 * insur$Make3
#Case3A_Claim_Model= 
71.2982 + 0.0292 * 17500 - 11.6529 * 2 - 36.1410 * 5 - 13.1715 * 4 - 17.2704 * 3

Case3B_Claim_Model= 71.2982 + 0.0292 * 25416 - 11.6529 * insur$Zone2 - 36.1410 * insur$Bonus5 - 13.1715 * insur$Kilometres4 - 17.2704 * insur$Make3
#Case3B_Claim_Model= 
71.2982 + 0.0292 * 25416 - 11.6529 * 2 - 36.1410 * 5 - 13.1715 * 4 - 17.2704 * 3

Case3A_Payment_Model= 292306 + 153 * insur$Insured - 86891 * insur$Make3 - 156346 * insur$Bonus5  - 48571 * insur$Zone2  - 34881 * insur$Kilometres4 
#Case3A_Payment_Model= 
292306 + 153 * 17500 - 86891 * 3 - 156346 * 5  - 48571 * 2  - 34881 * 4 

Case3B_Payment_Model= 292306 + 153 * insur$Insured - 86891 * insur$Make3 - 156346 * insur$Bonus5  - 48571 * insur$Zone2  - 34881 * insur$Kilometres4 
#Case3B_Payment_Model= 
292306 + 153 * 25416 - 86891 * 3 - 156346 * 5  - 48571 * 2  - 34881 * 4 


#d#
#to find if claims is affected by Insured amount, Kilometres, zone,bonus, and make

regnew=lm(insur$Payment ~ insur$Claims+insur$Insured+insur$Make+insur$Bonus+insur$Zone+insur$Kilometres)
summary(regnew)
plot(regnew)

