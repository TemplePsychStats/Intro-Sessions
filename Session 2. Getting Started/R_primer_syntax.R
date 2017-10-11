#Getting Started with R, October 9th, 2017

#To download a free copy of R, go to https://www.r-project.org/
#To download a free copy of RStudio, go to https://www.rstudio.com/

#Objects and functions: 
#Most commands are of the form "object <- function"
	a <- 3
	a
	b = 2 #'<-' and '=' both can be used to assign values to objects. Which one to use is controversial.
	b
	c <- a + b
	c

#R is case sensitive for all objects and functions!
	avg <- -12
	Avg
	avg
	
#Concatenation: c() is used to combine multiple values into a single object
	trials <- c(1,5,3,11)
	trials
	trials[3] #Returns the third element in the object

#You can perform operations on single elements or the entire variable
	trials[3] <- 4
	trials <- trials + 10
	mean(trials)
	
#Character objects can be created by using quotation marks
	participants <- c("Karen","Dan","Jenny")
	participants
	participants <- c(participants, "Jose")

#Dataframes combine variables into a single object

	record1 <- data.frame(name = participants, trials = trials)
	record1
	record1$trials

#You can also define variables as factors
	condition <- c(0,1,0,1)
	condition <- factor(condition, labels = c("control", "treatment"))
  
	record1$condition <- condition
	record1
	levels(record1$condition)

#Saving data as a comma delimited file
	write.csv(record1,"C:/Users/tue44557/Documents/demo_data1.csv")  

#Note: When opening and saving files, you can simplify the path by setting up
#a working directory: setwd("C:/Users/tue44557/Documents/")

#Importing Data from CSV or SPSS files
	dietdata <- read.csv("C:/Users/tue44557/Documents/Diet.csv",header=TRUE)

	install.packages("foreign", dependencies = TRUE) #"dependencies = TRUE" installs packages that the main package requires to run if necessary.	
  	library(foreign)
	dietdata <- read.spss("C:/Users/tue44557/Documents/Diet.sav", use.value.labels = TRUE, to.data.frame = TRUE)
	
#"melting" dataframes into long format
	library(reshape2) #You will also need to install the first time you use reshape2
	dietdata_long <- melt(dietdata,id=c("Subject","Sex"), Measured = c("Start","Month1","Month2"))

#"casting" dataframes into narrow format	
	dietdata_wide <- dcast(dietdata_long, Subject + Sex ~ variable, value = "value")
	
#Selecting subsets of a dataframe
	dietdata_male <- subset(dietdata, Sex == "Male") #selected rows
	dietdata_baseline <- subset(dietdata, select = c("Subject","Sex","Start")) #selected columns
	dietdata_male_baseline <- subset(dietdata, Sex == "Male",select = c("Subject","Sex","Start")) #selected rows and columns

#R Commander: A GUI (graphic user interface) package for basic analyses
	install.packages("Rcmdr", dependencies = TRUE) 
	library(Rcmdr)

#final notes:
#
#help() is an easy way to get information on a function. 
#For example, help(melt) will give you information on how to use the melt function from the reshape2 package.
#
#ls() lists all objects currently loaded into memory
#
#Using a consistent system for naming files and variables will avoid confusion. Here are the most widely used conventions:
#	  alllowercase (e.g., stressdata)
#   	  period.separated (e.g., stress.data)
#	  underscore_separated (e.g., stress_data)
#	  lowerCamelCase (e.g., stressData)
#	  UpperCamelCase (e.g., StressData)
#For more info on naming conventions, go to: 
#https://google.github.io/styleguide/Rguide.xml
