# Problem Statement
# A Chinese automobile company XYZ Auto aspires to enter the US market by setting up their 
# manufacturing unit there and producing cars locally to give competition to their US and 
# European counterparts

# Business Strategy : the company wants to know
# 1. Which variables are significant in predicting the price of a car
# 2. How well those variables describe the price of a car

# Goal of analysis
# To model the price of cars with the available independent variables. It will be used by 
# the management to understand how exactly the prices vary with the independent variables

#################################### load essential libraries ################################## 
# If packages are not installed in the environment, uncomment below lines and execute one by one
#install.packages("ggplot2")
#install.packages("lubridate")
#install.packages("MASS")
#install.packages("car")
library(ggplot2)
library(lubridate)
library(MASS)
library(car)

# load the Car Price data
CarPrice <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = F)
# quick view of the structure of dataset
str(CarPrice)

#################################  High level analysis ####################################
# lets check for any NA values in the dataset
sum(is.na(CarPrice))
# found that there are no NA values

# Lets see if there are any duplicates 
sum(duplicated(CarPrice$car_ID))
# found to be all unique

# lets see if there are any empty values
sapply(CarPrice, function(x)all(x==""))
# found that there are no empty values

################################ Business Rule ###########################################
# there is a business rule which states that there is a variable named CarName which is 
# comprised of two parts - the first word is the name of 'car company' and the second is the 
#'car model'. For example, chevrolet impala has 'chevrolet' as the car company name and 
#'impala' as the car model name

# let's target the carName column and take only the company name 
CarPrice$CarName <- sapply(strsplit(CarPrice$CarName, " "), "[", 1)
# lets rename the carName column to CarCompany for better understanding
colnames(CarPrice)[3] <- "CarCompany"

#########################  Data Treatment ##################################################

# If we look into unique values of car names, it seems some values are similar but with a different spelling 
# like maxda and mazda,nissan and Nissan,toyota and toyouta,vokswagen and volkswagen and vw, porsche and porcshce
# So, lets merge them
CarPrice$CarCompany[which(CarPrice$CarCompany == "maxda")] <- "mazda"
CarPrice$CarCompany[which(CarPrice$CarCompany == "Nissan")] <- "nissan"
CarPrice$CarCompany[which(CarPrice$CarCompany == "toyouta")] <- "toyota"
CarPrice$CarCompany[which(CarPrice$CarCompany == "vokswagen")] <- "volkswagen"
CarPrice$CarCompany[which(CarPrice$CarCompany == "vw")] <- "volkswagen"
CarPrice$CarCompany[which(CarPrice$CarCompany == "porcshce")] <- "porsche"

# If we look into unique values of drivewheel, it seems two values are similar but with different nomenclature 
# like fwd and 4wd. Lets merge both together and keep fwd
CarPrice$drivewheel[which(CarPrice$drivewheel == "4wd")] <- "fwd"

###################### Character variables into factor ####################################
# convert all the character columns to factor except CarName column 
# also convert the symboling column to factor
FactorsColumns <- c("symboling","fueltype","aspiration","doornumber","carbody",
                    "drivewheel","enginelocation","enginetype","cylindernumber",
                    "fuelsystem")

CarPrice[FactorsColumns] <- lapply(CarPrice[FactorsColumns], as.factor)

#################### Convert factor variables to numeric variables and assig levels ############

# convert factors with 2 levels to numerical variables
# i.e. fueltype, aspiration, doornumber, enginelocation
levels(CarPrice$fueltype)<-c(1,0)
CarPrice$fueltype<- as.numeric(levels(CarPrice$fueltype))[CarPrice$fueltype]

levels(CarPrice$aspiration)<-c(1,0)
CarPrice$aspiration<- as.numeric(levels(CarPrice$aspiration))[CarPrice$aspiration]

levels(CarPrice$doornumber)<-c(1,0)
CarPrice$doornumber<- as.numeric(levels(CarPrice$doornumber))[CarPrice$doornumber]

levels(CarPrice$enginelocation)<-c(1,0)
CarPrice$enginelocation<- as.numeric(levels(CarPrice$enginelocation))[CarPrice$enginelocation]

levels(CarPrice$drivewheel)<-c(1,0)
CarPrice$drivewheel<- as.numeric(levels(CarPrice$drivewheel))[CarPrice$drivewheel]

############################ Dummy variables for factors > 2 ##########################

# symboling column was of type integer, however it belongs to a categorical variable and hence we factored it
# at the same time we can categorize it into "Safe : -2 and -1", "LowRisk : 0 and 1" and "Risky : 2 and 3"

levels(CarPrice$symboling)[1:2] <- "Safe"
levels(CarPrice$symboling)[2:3] <- "LowRisk"
levels(CarPrice$symboling)[3:4] <- "Risky"

dummysymboling <- model.matrix(~symboling -1, data=CarPrice)
dummysymboling<- dummysymboling[,-1]
CarPrice <- cbind(CarPrice[ , -which(names(CarPrice) %in% c("symboling"))],dummysymboling)

# repeat the same process of creating dummy variables for other factor variables
dummycarbody <- model.matrix(~carbody -1, data=CarPrice)
dummycarbody<- dummycarbody[,-1]
CarPrice <- cbind(CarPrice[ , -which(names(CarPrice) %in% c("carbody"))],dummycarbody)

dummyenginetype <- model.matrix(~enginetype -1, data=CarPrice)
dummyenginetype<- dummyenginetype[,-1]
CarPrice <- cbind(CarPrice[ , -which(names(CarPrice) %in% c("enginetype"))],dummyenginetype)

dummycylindernumber <- model.matrix(~cylindernumber -1, data=CarPrice)
dummycylindernumber<- dummycylindernumber[,-1]
CarPrice <- cbind(CarPrice[ , -which(names(CarPrice) %in% c("cylindernumber"))],dummycylindernumber)

dummyfuelsystem <- model.matrix(~fuelsystem -1, data=CarPrice)
dummyfuelsystem<- dummyfuelsystem[,-1]
CarPrice <- cbind(CarPrice[ , -which(names(CarPrice) %in% c("fuelsystem"))],dummyfuelsystem)

dummyCarCompany <- model.matrix(~CarCompany -1, data=CarPrice)
dummyCarCompany<- dummyCarCompany[,-1]
CarPrice <- cbind(CarPrice[ , -which(names(CarPrice) %in% c("CarCompany"))],dummyCarCompany)

# Remove the car_ID variable as it is just an identifier and not significant in any way
CarPrice$car_ID<- NULL


######################## Check for Outliers in each numeric variables #################################

# if there are no outliers leave the data as is. If there are outlier, treat them

quantile(CarPrice$wheelbase,probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9))
quantile(CarPrice$carlength,probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9))
quantile(CarPrice$carwidth,probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9))
quantile(CarPrice$carheight,probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9))

quantile(CarPrice$curbweight,probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9))
# looks like curbweight has some outliers going from 60% to 70%, let us treat this
CarPrice$curbweight[which(CarPrice$curbweight > 2579.0)] <- 2579.0

quantile(CarPrice$enginesize,probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9))
# looks like enginesize has some outliers going from 60% to 70%, let us treat this
CarPrice$enginesize[which(CarPrice$enginesize > 122)] <- 122

quantile(CarPrice$boreratio,probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9))
quantile(CarPrice$stroke,probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9))
quantile(CarPrice$compressionratio,probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9))

quantile(CarPrice$horsepower,probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9))
# looks like horsepower has some outliers going from 60% to 70%, let us treat this
CarPrice$horsepower[which(CarPrice$horsepower > 102.0)] <- 102.0

quantile(CarPrice$peakrpm,probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9))
quantile(CarPrice$citympg,probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9))
quantile(CarPrice$highwaympg,probs = c(.1,.2,.3,.4,.5,.6,.7,.8,.9))

############################# create derived metrics ####################################
# create a new metric of car -> total size which will be length*width*height
# this might be significant
CarPrice$totalsize <- CarPrice$carlength * CarPrice$carwidth * CarPrice$carheight

############################# Data arrangement ##########################################
# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(CarPrice), 0.7*nrow(CarPrice))
train = CarPrice[trainindices,]
test = CarPrice[-trainindices,]

############################# Start building the models and evaluate ######################
# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1)
# R^2 came to .9651

# In stepAIC function, we pass our first model i.e model_1 and 
# direction is set as both, because in stepwise,  both the forward selection 
# of variables and backward elimination of variables happen simultaneously 


step <- stepAIC(model_1, direction="both")
# now we need to know our model equation so lets write the Step command here. 

step
# stepAIC makes multiple calls while checking which variables to keep
# The last call that step makes, contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. 
# Now store the last model equation of stepwise method into an object called model_2

# Let's execute this model here, 
model_2 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                carlength + carheight + curbweight + enginesize + boreratio + 
                stroke + compressionratio + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree + fuelsystem2bbl + 
                fuelsystemmpfi + CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                CarCompanyisuzu + CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
                CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen + 
                CarCompanyvolvo + totalsize, data = train)
# Let us look at the summary of the model
summary(model_2)
## Let us check for multicollinearity 
# If the VIF is above 2, you would remove the variables if they are statistically insignificant
vif(model_2)


# curbweight has greater VIF and also insignificant as per P value, 
# let us remove this variable and evaluate the model again
# we will find that the R^2 is  .9689 compared to model 2 where it was .9693

model_3 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                carlength + carheight + enginesize + boreratio + 
                stroke + compressionratio + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree + fuelsystem2bbl + 
                fuelsystemmpfi + CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                CarCompanyisuzu + CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
                CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen + 
                CarCompanyvolvo + totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_3)
summary(model_3)


# fuelsystem2bbl has high VIF and insignificant
# let us remove this variable and evaluate the model again
# we will find that the R^2 is .9685  compared to model 3 where it was .9689

model_4 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                carlength + carheight + enginesize + boreratio + 
                stroke + compressionratio + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree +  
                fuelsystemmpfi + CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                CarCompanyisuzu + CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
                CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen + 
                CarCompanyvolvo + totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_4)
summary(model_4)


# CarCompanyisuzu has low VIF but insignificant. let us remove this variable
# we will find that the R^2 is .9681  compared to model 4 where it was .9685

model_5 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                carlength + carheight + enginesize + boreratio + 
                stroke + compressionratio + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree +  
                fuelsystemmpfi + CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
                CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen + 
                CarCompanyvolvo + totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_5)
summary(model_5)

# CarCompanysaab has high VIF and also insignificant. let us remove this variable
# we will find that the R^2 is .9678  compared to model 5 where it was .9681

model_6 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                carlength + carheight + enginesize + boreratio + 
                stroke + compressionratio + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree +  
                fuelsystemmpfi + CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
                CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                CarCompanyrenault + CarCompanytoyota + CarCompanyvolkswagen + 
                CarCompanyvolvo + totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_6)
summary(model_6)

# stroke has high VIF and also insignificant. let us remove this variable
# we will find that the R^2 is .9674  compared to model 6 where it was .9678

model_7 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                carlength + carheight + enginesize + boreratio + 
                compressionratio + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree +  
                fuelsystemmpfi + CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
                CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                CarCompanyrenault + CarCompanytoyota + CarCompanyvolkswagen + 
                CarCompanyvolvo + totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_7)
summary(model_7)


# CarCompanyvolvo has high VIF and also insignificant. let us remove this variable
# we will find that the R^2 is .9672  compared to model 7 where it was .9674

model_8 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                carlength + carheight + enginesize + boreratio + 
                compressionratio + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree +  
                fuelsystemmpfi + CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                CarCompanyjaguar + CarCompanymazda + CarCompanymercury + 
                CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                CarCompanyrenault + CarCompanytoyota + CarCompanyvolkswagen + 
                totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_8)
summary(model_8)

# CarCompanymercury is insignificant. let us remove this variable
# we will find that the R^2 is .9669  compared to model 8 where it was .9672

model_9 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                carlength + carheight + enginesize + boreratio + 
                compressionratio + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + enginetypel + enginetypeohcf + 
                enginetyperotor + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree +  
                fuelsystemmpfi + CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                CarCompanyjaguar + CarCompanymazda +  
                CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                CarCompanyrenault + CarCompanytoyota + CarCompanyvolkswagen + 
                totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_9)
summary(model_9)

# fuelsystemmpfi is insignificant and also has high VIF. let us remove this variable
# we will find that the R^2 is  .9664 compared to model 9 where it was .9669

model_10 <- lm(formula = price ~ fueltype + aspiration + enginelocation + 
                 carlength + carheight + enginesize + boreratio + 
                 compressionratio + carbodyhardtop + carbodyhatchback + 
                 carbodysedan + carbodywagon + enginetypel + enginetypeohcf + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree +  
                 CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyjaguar + CarCompanymazda +  
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanytoyota + CarCompanyvolkswagen + 
                 totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_10)
summary(model_10)


# fueltype is insignificant and also has high VIF. let us remove this variable
# we will find that the R^2 is .9662  compared to model 10 where it was .9664

model_11 <- lm(formula = price ~  aspiration + enginelocation + 
                 carlength + carheight + enginesize + boreratio + 
                 compressionratio + carbodyhardtop + carbodyhatchback + 
                 carbodysedan + carbodywagon + enginetypel + enginetypeohcf + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree +  
                 CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyjaguar + CarCompanymazda +  
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanytoyota + CarCompanyvolkswagen + 
                 totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_11)
summary(model_11)


# enginesize is insignificant and also has high VIF. let us remove this variable
# we will find that the R^2 is .9663  compared to model 11 where it was .9662

model_12 <- lm(formula = price ~  aspiration + enginelocation + 
                 carlength + carheight + boreratio + 
                 compressionratio + carbodyhardtop + carbodyhatchback + 
                 carbodysedan + carbodywagon + enginetypel + enginetypeohcf + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree +  
                 CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyjaguar + CarCompanymazda +  
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanytoyota + CarCompanyvolkswagen + 
                 totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_12)
summary(model_12)


# compressration and boreratio both are insignificant but compressratio has higher p value. let us remove this variable
# we will find that the R^2 is .9663  equivalent to model 12 where it was .9663

model_13 <- lm(formula = price ~  aspiration + enginelocation + 
                 carlength + carheight + boreratio + 
                 carbodyhardtop + carbodyhatchback + 
                 carbodysedan + carbodywagon + enginetypel + enginetypeohcf + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree +  
                 CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyjaguar + CarCompanymazda +  
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanytoyota + CarCompanyvolkswagen + 
                 totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_13)
summary(model_13)



# boreration is insignificant with higher P value. let us remove this variable
# we will find that the R^2 is .9661  equivalent to model 13 where it was .9663

model_14 <- lm(formula = price ~  aspiration + enginelocation + 
                 carlength + carheight +  
                 carbodyhardtop + carbodyhatchback + 
                 carbodysedan + carbodywagon + enginetypel + enginetypeohcf + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree +  
                 CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyjaguar + CarCompanymazda +  
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanytoyota + CarCompanyvolkswagen + 
                 totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_14)
summary(model_14)


# carlength has higher P value than others and also VIF value is very high. let us remove this variable
# we will find that the R^2 is .9646  compared to model 14 where it was .9661

model_15 <- lm(formula = price ~  aspiration + enginelocation + 
                 carheight +  
                 carbodyhardtop + carbodyhatchback + 
                 carbodysedan + carbodywagon + enginetypel + enginetypeohcf + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree +  
                 CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyjaguar + CarCompanymazda +  
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanytoyota + CarCompanyvolkswagen + 
                 totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_15)
summary(model_15)

# CarCompanynissan has higher P value than others . let us remove this variable
# we will find that the R^2 is.9629   compared to model 15 where it was .9646

model_16 <- lm(formula = price ~  aspiration + enginelocation + 
                 carheight +  
                 carbodyhardtop + carbodyhatchback + 
                 carbodysedan + carbodywagon + enginetypel + enginetypeohcf + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree +  
                 CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyjaguar + CarCompanymazda +  
                 CarCompanymitsubishi + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanytoyota + CarCompanyvolkswagen + 
                 totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_16)
summary(model_16)


# CarCompanydodge has higher P value than others . let us remove this variable
# we will find that the R^2 is .9619  compared to model 16 where it was .9629

model_17 <- lm(formula = price ~  aspiration + enginelocation + 
                 carheight +  
                 carbodyhardtop + carbodyhatchback + 
                 carbodysedan + carbodywagon + enginetypel + enginetypeohcf + 
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree +  
                 CarCompanybmw + CarCompanybuick +  
                 CarCompanyjaguar + CarCompanymazda +  
                 CarCompanymitsubishi + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanytoyota + CarCompanyvolkswagen + 
                 totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_17)
summary(model_17)


# enginetypeohcf has higher P value than others . let us remove this variable
# we will find that the R^2 is .961   compared to model 17 where it was .9619

model_18 <- lm(formula = price ~  aspiration + enginelocation + 
                 carheight +  
                 carbodyhardtop + carbodyhatchback + 
                 carbodysedan + carbodywagon + enginetypel +  
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree +  
                 CarCompanybmw + CarCompanybuick +  
                 CarCompanyjaguar + CarCompanymazda +  
                 CarCompanymitsubishi + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanytoyota + CarCompanyvolkswagen + 
                 totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_18)
summary(model_18)


# CarCompanyplymouth is insignificant based on p value . let us remove this variable
# we will find that the R^2 is  .9605 compared to model 18 where it was .961

model_19 <- lm(formula = price ~  aspiration + enginelocation + 
                 carheight +  
                 carbodyhardtop + carbodyhatchback + 
                 carbodysedan + carbodywagon + enginetypel +  
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree +  
                 CarCompanybmw + CarCompanybuick +  
                 CarCompanyjaguar + CarCompanymazda +  
                 CarCompanymitsubishi +  
                 CarCompanyrenault + CarCompanytoyota + CarCompanyvolkswagen + 
                 totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_19)
summary(model_19)

# CarCompanyrenault has higher p value than others . let us remove this variable
# we will find that the R^2 is  .9598 compared to model 19 where it was .9605

model_20 <- lm(formula = price ~  aspiration + enginelocation + 
                 carheight +  
                 carbodyhardtop + carbodyhatchback + 
                 carbodysedan + carbodywagon + enginetypel +  
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree +  
                 CarCompanybmw + CarCompanybuick +  
                 CarCompanyjaguar + CarCompanymazda +  
                 CarCompanymitsubishi +  
                 CarCompanytoyota + CarCompanyvolkswagen + 
                 totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_20)
summary(model_20)

# CarCompanymitsubishi has higher p value than others . let us remove this variable
# we will find that the R^2 is .9581  compared to model 20 where it was .9598

model_21 <- lm(formula = price ~  aspiration + enginelocation + 
                 carheight +  
                 carbodyhardtop + carbodyhatchback + 
                 carbodysedan + carbodywagon + enginetypel +  
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree +  
                 CarCompanybmw + CarCompanybuick +  
                 CarCompanyjaguar + CarCompanymazda +    
                 CarCompanytoyota + CarCompanyvolkswagen + 
                 totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_21)
summary(model_21)


# It seems all the variables are having p value less than .05, lets look into the maximum P value
# among all and higher VIF and try to remove that variable.
# carheight has higher VIF value than others . let us remove this variable
# we will find that the R^2 is .9565  compared to model 21 where it was .9581

model_22 <- lm(formula = price ~  aspiration + enginelocation + 
                 carbodyhardtop + carbodyhatchback + 
                 carbodysedan + carbodywagon + enginetypel +  
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree +  
                 CarCompanybmw + CarCompanybuick +  
                 CarCompanyjaguar + CarCompanymazda +    
                 CarCompanytoyota + CarCompanyvolkswagen + 
                 totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_22)
summary(model_22)


# CarCompanybuick has higher P value than others and also high VIF . let us remove this variable
# we will find that the R^2 is.9548   compared to model 22 where it was .9565

model_23 <- lm(formula = price ~  aspiration + enginelocation + 
                 carbodyhardtop + carbodyhatchback + 
                 carbodysedan + carbodywagon + enginetypel +  
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree +  
                 CarCompanybmw +   
                 CarCompanyjaguar + CarCompanymazda +    
                 CarCompanytoyota + CarCompanyvolkswagen + 
                 totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_23)
summary(model_23)

# CarCompanytoyota has higher P value than others . let us remove this variable
# we will find that the R^2 is .9519 compared to model 23 where it was .9548 

model_24 <- lm(formula = price ~  aspiration + enginelocation + 
                 carbodyhardtop + carbodyhatchback + 
                 carbodysedan + carbodywagon + enginetypel +  
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree +  
                 CarCompanybmw +   
                 CarCompanyjaguar + CarCompanymazda +    
                 CarCompanyvolkswagen + 
                 totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_24)
summary(model_24)

# CarCompanymazda has higher P value than others . let us remove this variable
# we will find that the R^2 is .9493 compared to model 24 where it was .9519 

model_25 <- lm(formula = price ~  aspiration + enginelocation + 
                 carbodyhardtop + carbodyhatchback + 
                 carbodysedan + carbodywagon + enginetypel +  
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree +  
                 CarCompanybmw +   
                 CarCompanyjaguar +     
                 CarCompanyvolkswagen + 
                 totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_25)
summary(model_25)

# enginetypel has higher P value than others . let us remove this variable
# we will find that the R^2 is .9457 compared to model 25 where it was .9493 

model_26 <- lm(formula = price ~  aspiration + enginelocation + 
                 carbodyhardtop + carbodyhatchback + 
                 carbodysedan + carbodywagon +   
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree +  
                 CarCompanybmw +   
                 CarCompanyjaguar +     
                 CarCompanyvolkswagen + 
                 totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_26)
summary(model_26)

# all the variables are now having 3 stars i.e. *** i.e. all are quite significant
# carbodysedan has higher VIF value 11.90 than others . let us remove this variable
# we will find that the R^2 is .9298 compared to model 26 where it was .9457 

model_27 <- lm(formula = price ~  aspiration + enginelocation + 
                 carbodyhardtop + carbodyhatchback + 
                 carbodywagon +   
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree +  
                 CarCompanybmw +   
                 CarCompanyjaguar +     
                 CarCompanyvolkswagen + 
                 totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_27)
summary(model_27)


# carbodyhatchback and  carbodyhardtop both are insifgnificant but carbodyhatchback has more VIF.
#let us remove this variable
# we will find that the R^2 is .9301 compared to model 27 where it was .9298 

model_28 <- lm(formula = price ~  aspiration + enginelocation + 
                 carbodyhardtop +  
                 carbodywagon +   
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree +  
                 CarCompanybmw +   
                 CarCompanyjaguar +     
                 CarCompanyvolkswagen + 
                 totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_28)
summary(model_28)

# carbodyhardtop is insignificant.let us remove this variable
# we will find that the R^2 is .9297 compared to model 28 where it was .9301 

model_29 <- lm(formula = price ~  aspiration + enginelocation +
                 carbodywagon +   
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree +  
                 CarCompanybmw +   
                 CarCompanyjaguar +     
                 CarCompanyvolkswagen + 
                 totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_29)
summary(model_29)

# CarCompanyvolkswagen has higher P value than any other variables.let us remove this variable
# we will find that the R^2 is.926  compared to model 29 where it was .9297 

model_30 <- lm(formula = price ~  aspiration + enginelocation +
                 carbodywagon +   
                 enginetyperotor + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree +  
                 CarCompanybmw +   
                 CarCompanyjaguar +
                 totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_30)
summary(model_30)

# all the variables are having 3 stars i.e. *** i.e. significant
# However, cylindernumberfour has higher VIF value than any other variables.let us remove this variable
# we will find that the R^2 is .7227  compared to model 30 where it was .926 

model_31 <- lm(formula = price ~  aspiration + enginelocation +
                 carbodywagon + 
                 enginetyperotor + cylindernumberfive + 
                 cylindernumbersix + cylindernumberthree +  
                 CarCompanybmw +   
                 CarCompanyjaguar +
                 totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_31)
summary(model_31)

# cylindernumberthree is insignificant now.let us remove this variable
# we will find that the R^2 is .7231  compared to model 31 where it was .7227 

model_32 <- lm(formula = price ~  aspiration + enginelocation +
                 carbodywagon + 
                 enginetyperotor + cylindernumberfive + 
                 cylindernumbersix +   
                 CarCompanybmw +   
                 CarCompanyjaguar +
                 totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_32)
summary(model_32)

# aspiration has highter p value than any other.let us remove this variable
# we will find that the R^2 is  .7193 compared to model 32 where it was .7231 

model_33 <- lm(formula = price ~  enginelocation +
                 carbodywagon + 
                 enginetyperotor + cylindernumberfive + 
                 cylindernumbersix +   
                 CarCompanybmw +   
                 CarCompanyjaguar +
                 totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_33)
summary(model_33)

# enginetyperotor has highter p value than any other.let us remove this variable
# we will find that the R^2 is .7152  compared to model 32 where it was .7193 

model_34 <- lm(formula = price ~  
                 enginelocation + carbodywagon + 
                 cylindernumberfive +  cylindernumbersix +   
                 CarCompanybmw + CarCompanyjaguar +
                 totalsize, data = train)

# Let us look at the VIFs and summary now
vif(model_34)
summary(model_34)
# We can see that our R^2 is now .7152 which is pretty good and also at the same time
# we were able to idenfify few independent drivers that can predict the price of car

###################### Summary of the independent variables/factors ####################################

# These are the 5 factors/features/variables which  is quite significant in predicting the car price in US market
# 1. Engine Location   2. Car body as wagon   3.  five cylinder or six cylinder  
# 4. BMW or Jaguar car  5. Total size of car

###############################################################################################################

# predicting the results in test dataset
Predict_1 <- predict(model_34,test[,-1])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted price 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared
##################################### Findings ###############################################################

# These variables can very well describe 63.73% of car price
# Predicted values from the model are able to explain 63.73% variation in the actual outcomes.


