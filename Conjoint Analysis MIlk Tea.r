# Conjoint Analysis for Milk Tea
#By Stephanie Vu

# import conjoint library
library(conjoint)

## Step 1: experimental design

#define five attributes of milktea for the conjoint study
tea=c("oolong", "black", "green")  
price=c("5","6.5","9")
lid=c("dome", "flat")
milk=c("nondairy powder", "whole milk")
calories=c("little", "much")

#create level names <---- we want to know what attributes associate to what level
levelnames = data.frame("levels" = c(tea,price,lid,milk,calories))
print(levelnames)

# full factorial design, total 3x3x2x2x2= 72 combinations
experiment = expand.grid(tea=tea, price=price, lid=lid, milk=milk, calories=calories)
surveycards = caFactorialDesign(data=experiment,cards=12, type="fractional") #<--------This is fractional factorial design of 12 cards

#view experiment and card design
head(experiment)
print(surveycards)

# QC design cards
cor(caEncodedDesign(surveycards))
print(surveycards)
# encode the design
profiles=caEncodedDesign(design=surveycards)

## Step 2: collect survey data from csv file
preferences = read.csv(file="C:\\STEPHY\\R\\somefakepath\\MilkteaConjoint.csv")

print(profiles)

## Step 3: Find the result for the conjoint model

## find the feature importance & print
u2 = caImportance(y=preferences,x=profiles) #check the importance of the attributes
barplot(u2, names.arg = c("tea", "price","lid","milk","calories"))

## Step 4: Find the optimal price

# 1. Find the most favorable product profiled1 = prod_utility['intercept',] + prod_utility['black',] + prod_utility['flat',] + prod_utility['nondairy',] + prod_utility['little'] + prod_utility['5',]

u1 = caUtilities(y=preferences,x=profiles,z=levelnames)
prod_utility = data.frame(u1,row.names = c("intercept",as.list(levelnames)$levels))
print(prod_utility)

#From the plots, we can find the most favorable profile for tea as tea='black', price='5', lid = 'flat', milk ='nondairy', calories='little')

# 2. Find the optimal price

#price1 demand = intercept + black + flat + nondairy + little + price1
d1 = prod_utility['intercept',] + prod_utility['black',] + prod_utility['flat',] + prod_utility['nondairy',] + prod_utility['little',] + prod_utility['5',]
print(d1)
r1 = d1* 5 #revenue = demand * price

#price2 demand = intercept + black + granulated + yes + price2
d2 = prod_utility['intercept',] + prod_utility['black',] + prod_utility['flat',] + prod_utility['nondairy',] + prod_utility['little',] + prod_utility['6.5',]
print(d2)
r2 = d2* 6.5 #revenue = demand * price

#price3 demand = intercept + black + granulated + yes + price3
d3 = prod_utility['intercept',] + prod_utility['black',] + prod_utility['flat',] + prod_utility['nondairy',] + prod_utility['little',] + prod_utility['9',]
print (d3)
r3 = d3* 9 #revenue = demand * price

#create the dataframe for plot
est_demand = c(d1,d2,d3)
est_rev = c(r1,r2,r3)
price_level = c(5, 6.5, 9)

revenue = data.frame(est_demand, est_rev, price_level)

#plot demand curve
plot(price_level, est_demand, type='o',xlab="price", ylab="demand")

#plot revenue curve
plot(price_level, est_rev, type='o',xlab="price", ylab="revenue")


