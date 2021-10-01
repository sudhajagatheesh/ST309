# Week 1 R session 

# some hot keys
# Run any line of the code: Ctrl + Enter
# comments: Ctrl + Shift + c
# recall: Ctrl + z
# Assign operator <- : Alt + '-'

# basic arithmetic operator
# plus +; minus -; multiplication *;division /, power ^;
# square root sqrt(): # exponential function: exp();  ; logarithm log()

# use as a calculator

1+3.14

2^3+3.14*2-3.14*sqrt(3) + 2/3

exp(2) + log(2)


# create a vector via concatenation function c(...)
c(1,2,3)

# Quotation  mark for characters
c('Ali','Bet','Cat')
c("Ali","Bet","Cat")

# vector elements will be unified into characters
c(1,'Ali',2)

# In order to make use of vectors, we need identifiers for them

# Some rules for identifiers  
# case sensitive ; Number, number
# acceptable Punctuation 'dot' . 'underline'_, num_ber,num.ber
# don't start with numbers or punctuation, 2number _number, .number (invalid)
# as precise as possible

numbers <- c(1,2,3) 
people <-  c("Ali","Bet","Cat")

# for further operation
numbers^2

numbers2 <- c(2.2,3,4)
numbers - numbers2
numbers^numbers2

# sheep weights example 
weight <- c(84.5,72.6,75.7,94.8,71.3)

mean(weight)
var(weight)
summary(weight)

# data frame
height <- c(86.5, 71.8, 77.2, 84.9, 75.4)
sheep <- data.frame(weight, height)
sheep

# access columns
sheep$weight
sheep[,1]
sheep[,'weight']

# access rows
sheep[2,]

# add another column
sheep$backlength <- c(130.4, 100.2, 109.4, 140.6, 101.4)
sheep

# summary function is automatically applied to each columns
summary(sheep)
IQR(sheep$height) # interquartile range
sd(sheep$backlength) # standard deviation

# Import the data
# check your working directory
getwd()
# you can change it through Session -> Set Working Directory -> Choose directory ... 
# or the hot key : Ctrl + Shift + H

# put the 'sheep.txt' into your working directory

# header = TRUE: variable names in the first row of the data file 
# are used as identifiers for the columns of our data set
sheep2 <- read.table("sheep.txt", header=TRUE)
# check the structure of the data
str(sheep2)
# check descriptive statistics
summary(sheep2)
# make a scatter plot
plot(sheep2$weight,sheep2$height)

# Session management and visibility
# check the objects created in the current environment
ls()
objects()

# height and weight are now encapsulated in sheep, 
# we can tidy up our workspace by removing them 
rm(height,weight)

# remove all the object 
rm(list = ls())
rm(list = object())

# If we are going to be using the variables in the sheep data frame a lot, 
# we can make them visible from the command
# Need to first remove the weight and height in the global environment
attach(sheep)
weight

# If two data frame share the same column names
# make sure to detach the old one before using the new one
detach()
attach(sheep2)

detach()

# This will save everything happened in the session, 
# including all objects (e.g. vectors, data frames), all the commands used
save.image("intro1.Rdata") 
# This is useful when some calculation or analysis take a long time to run
# then you can save the results rather than rerun the calculation
# again when you open R some day in the future

# To save all the commands used in the current session only
savehistory("intro1.Rhistory")

# Bring back to where the previous session left
load("intro1.Rdata") 

# Execute all commands used in previous session
source("intro1.Rhistory")

# documentation of the functions
?mean

