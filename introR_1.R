# ANALYZING RAPTOR DATA
# loading data
setwd("~/Desktop/software-carpentry-2016/data-files/r-lesson/data/")
list.files()
read.csv(file = "inflammation-01.csv", header = FALSE)

# assigning variables
weight_kg <- 55
weight_kg
  # weight in pounds
2.2 * weight_kg
weight_kg <- 57.5
weight_kg
  # calculate weight in pounds, save to variable weight_lb
weight_lb <- 2.2 * weight_kg
weight_lb

# manipulating data
dat <- read.csv(file = "inflammation-01.csv", header = FALSE)
head(dat)
class(dat)
dim(dat)
  # first value in dat
dat[1,1]
  # middle value in dat
dat[30,20]
dat[1:4, 1:10]
dat[5:10, 1:10]
dat[c(3,8,37,56), c(10,14,29)]
  # all columns from row 5
dat[5,]
  # all rows from row 16
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

# same as apply(dat,1,mean)
rowMeans(dat)
# same as apply(dat,2,mean
colMeans(dat)

# challenge-slicing data
animal <- c("m", "o", "n", "k", "e", "y")
animal[1:3]
animal[4:6]
animal[1:4]
animal[4:1]
animal[-1]
animal[-4]
animal[-1:-4]
# specify individual values in a list
animal[c(5,2,3)]

# plotting
plot(avg_day_inflammation)
max_day_inflammation <- apply(dat, 2, max)
plot(max_day_inflammation)
min_day_inflammation <- apply(dat, 2, min)
plot(min_day_inflammation)
sd_day_inflammation <- apply(dat, 2, sd)
plot(sd_day_inflammation)


# CREATING FUNCTIONS
# defining a function: fahrenheit to kelvin
fahr_to_kelvin <-function(temp){
  kelvin <-((temp-32)*(5/9)) + 273.15
  return(kelvin)
}
fahr_to_kelvin(100)
               
# composing functions  
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


# ANALZING MULTIPLE DATA SETS
list.files()
analyze <- function(filename) {
  # Plots the average, min, and max inflammation over time.
  # Input is character string of a csv file.
  
  dat <- read.csv(file = filename, header = FALSE)
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation)
  max_day_inflammation <- apply(dat, 2, max)
  plot(max_day_inflammation)
  min_day_inflammation <- apply(dat, 2, min)
  plot(min_day_inflammation)
}

analyze("inflammation-01.csv")
analyze("inflammation-02.csv")

# For loops
# Bad approach example
best_practice <- c("Let", "the", "computer", "do", "the", "work")
print_words <- function(sentence) {
  print(sentence[1])
  print(sentence[2])
  print(sentence[3])
  print(sentence[4])
  print(sentence[5])
  print(sentence[6])
}

print_words(best_practice)
print_words(best_practice[-6])

# Good approach example
print_words <- function(sentence) {
  for (word in sentence) {
    # word <- sentence[1]
    # word <- sentence[2]
    print(word)
  }
}

print_words(best_practice)
print_words(best_practice[-6])

# for(variable in collection) {
# do things with variable
# }

# Another example
len <- 0
vowels <- c("a", "e", "i", "o", "u")
for (v in vowels) {
  print(len)
  len <- len + 1
}
# Number of vowels
len

# Challenge - using loops
## Question 1
seq(3)
print_N <- function(natural_num) {
  # Function prints natural numbers
  # Input is a number
for (x in seq(natural_num)) 
  print(x)
}
print_N(3)
print_N(7)

## Question 2
ex_vec <- c(4, 8, 15, 16, 23, 42)
total <- function(num) {
  tot = 0
  for(x in num) {
    tot = tot + x
  }
  return(tot)
}
total(ex_vec)
  
## Question 3
2^4
expo <- function(base, exponent) {
  tot = 1
  # Accounts for exponent = 0. Don't worry about this for now
  if(exponent == 0){
    return(tot)
  }
  
  for(i in 1:exponent){
    tot = tot * base
  }
  return(tot)
}

expo(3, 0)
expo(3, 2)

# Processing multiple flies - loop through files analysis
list.files()
# Allows to use the wildcard
Sys.glob("*csv")
Sys.glob("inflammation***.csv")
# This also works
list.files(pattern="inflammation")

filenames <- Sys.glob("i*.csv")
  for (file in filenames) {
    print(file)
    analyze(file)
  }
  

# MAKING CHOICES

# Saving plots to file
analyze <- function(filename) {
  # Plots the average, min, and max inflammation over time.
  # Input is character string of a csv file.
  dat <- read.csv(file = filename, header = FALSE)
  avg_day_inflammation <- apply(dat, 2, mean)
  plot(avg_day_inflammation)
  max_day_inflammation <- apply(dat, 2, max)
  plot(max_day_inflammation)
  min_day_inflammation <- apply(dat, 2, min)
  plot(min_day_inflammation)
}

analyze_all <- function(pattern) {
  # Runs the function analyze for each file in the current working directory
  # that contains the given pattern.
  filenames <- list.files(path = "data", pattern = pattern, full.names = TRUE)
  for (f in filenames) {
    analyze(f)
  }
}

analyze("inflammation-01.csv")

pdf("inflammation-01.pdf")
analyze("inflammation-01.csv")
dev.off()

# This will save it to a parent directory of current working directory
pdf("../inflammation-01.pdf)