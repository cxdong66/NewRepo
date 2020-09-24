setwd("I:/ese5023")
# In-class problem 1

# Install the library
install.packages('gtools')

# Load the library
library(gtools)

# Check help doc
?permutations
?combinations

Balls_In_Bag <- c( "red", "blue", "green", "black")

# Permutations
Output1      <- permutations(n=4,r=2,v=Balls_In_Bag,repeats.allowed = T)
Output2      <- permutations(n=4,r=2, v=Balls_In_Bag,repeats.allowed = F)

#====================================
# In-class problem 2
#====================================

# Read csv
Keeling_Data <- read.csv(file = "co2_mm_mlo.csv", header = T)

# Load data
Data_Year  <- Keeling_Data$year
Data_CO2   <- Keeling_Data$co2
Data_Month <- Keeling_Data$month

# Clean data
Data_CO2[which(Data_CO2 == -99.99)]  <- NA

# Annual mean
CO2_annual_mean <- c()
for(iYear in unique(Data_Year)){
  # unique() function, returns unique elements of the vector
  ThisYear      <- which(Data_Year == iYear)
  ThisYear_Mean <- mean(Data_CO2[ThisYear], na.rm=T)
  CO2_annual_mean <- c(CO2_annual_mean, ThisYear_Mean)
}

# Plot annual mean
plot(seq(1958,2020,1),CO2_annual_mean,type="l",
     xlab="Year",ylab="CO2 [ppm]")

# 
CO2_annual_growth <- c()

for(iYear in 1959:2019){
  CO2_Jan <- NA
  CO2_Dec <- NA
  
  ThisYear      <- which(Data_Year == iYear)
  for(iMonth in ThisYear){
    # We get valid value for Jan.
    if( Data_Month[iMonth] =="January" ){
      CO2_Jan <- Data_CO2[iMonth]
    }
    # We get valid value for Dec.
    if( Data_Month[iMonth] =="December" ){
      CO2_Dec <- Data_CO2[iMonth]
    }
    # If both, do the following:
    if( !is.na(CO2_Dec) && !is.na(CO2_Jan) ){
      CO2_annual_growth <- c(CO2_annual_growth, CO2_Dec - CO2_Jan)
    }
  }
}

# Plot
plot(CO2_annual_growth, type="l",xlab="Year",ylab="Growth Rate [ppm/yr]")
abline(h=0, lty=2, col="red")


#this is for test in Git 
#I am editing my file 
