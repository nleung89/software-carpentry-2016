#changing directory
setwd("~/Desktop/software-carpentry-2016/data-files/r-lesson/data/")
list.files()
read.csv(file = "inflammation-01.csv", header = FALSE)

#assigning variables and manipulating them
weight_kg <- 55
weight_kg
  #weight in pounds
2.2 * weight_kg
weight_kg <- 57.5
weight_kg
  #calculate weight in pounds, save to variable weight_lb
weight_lb <- 2.2 * weight_kg
weight_lb

dat <- read.csv(file = "inflammation-01.csv", header = FALSE)
head(dat)
class(dat)
dim(dat)

#first value in dat
dat[1,1]
#middle value in dat
dat[30,20]
dat[1:4, 1:10]
dat[5:10, 1:10]
dat[c(3,8,37,56), c(10,14,29)]

#all columns from row 5
dat[5,]
#all rows from row 16
dat[,16]

raptor_1 <- dat[1,]
max(raptor_1)
min(raptor_1)
mean(raptor_1)
mean(dat[,7])
median(dat[,7])
sd(dat[,7])

max(dat[2,])

?apply
avg_raptor_inflammation <- apply(dat,1,mean)
avg_raptor_inflammation
avg_day_inflammation <- apply(dat,2,mean)
avg_day_inflammation

#same as apply(dat,1,mean)
rowMeans(dat)
#same as apply(dat,2,mean
colMeans(dat)

#practice problem
animal <- c("m", "o", "n", "k", "e", "y")
animal[1:3]
animal[4:6]
animal[1:4]
animal[4:1]
animal[-1]
animal[-4]
animal[-1:-4]
#specify individual values in a list
animal[c(5,2,3)]

plot(avg_day_inflammation)
max_day_inflammation <- apply(dat, 2, max)
plot(max_day_inflammation)
min_day_inflammation <- apply(dat, 2, min)
plot(min_day_inflammation)
sd_day_inflammation <- apply(dat, 2, sd)
plot(sd_day_inflammation)

#CREATING FUNCTIONS
#defining a function: fahrenheit to kelvin
fahr_to_kelvin <-function(temp){
  kelvin <-((temp-32)*(5/9)) + 273.15
  return(kelvin)
}
fahr_to_kelvin(100)
               
#composing functions  
kelvin_to_celsius <- function(temp) {
  celsius <- temp - 273.15
  return(celsius)
}
kelvin_to_celsius(0)

fahr_to_celsius <- function(temp) {
  temp_k <- fahr_to_kelvin(temp)
  result <- kelvin_to_celsius(temp_k)
  return(result)
}
fahr_to_celsius(32.0)

original <- c("a","b","c","d","e")
wrapper <- "zzz"
fence <- function(original,wrapper) {
  return(c(wrapper,original,wrapper))
}
fence(original,wrapper)

#the call stack

#testing and documenting

#defining defaults

