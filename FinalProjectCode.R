##### STT 420 Final Project #####
#####   Logistic Incedence  #####
##### Authors: Thomas Billman, Danielle Chaung, James Royal #####

##### Population Simulation #####

#Population probabilities
gender  <- c(.52,.48) #f, m
age <- c(.19, .13, .26, .25, .16) #0-17, 18-24, 25-44, 45-64, 65+
race <- c(.817, .14, .016, .006) #W, B, Asian, Indian
mental <- c(.8,.2) #n, y
substance <- c(.906, .094) #n,y
perscription <- c(.8, .2) #n, y
hosp <- c(.927, .073) #n, y
rural <- c(.978, .022) #n, y

#Simulation
set.seed(420)
n <- 216430
gend <- rbernoulli(n, gender[2])
ages <- sample(1:5, n, replace = T, prob = age)
races <- sample(1:4, n, replace = T, prob = race)
mentals <- rbernoulli(n, mental[2])
substances <- rbernoulli(n, substance[2])
perscriptions <- rbernoulli(n, perscription[2])
hosps <- rbernoulli(n, hosp[2])
rurals <- rbernoulli(n,rural[2])

#Combination
dat <- data.frame(gend,
                  ages,
                  races,
                  mentals,
                  substances,
                  perscriptions,
                  hosps,
                  rurals)

##### Emergency Room Visits Simulation #####

#There were a total of 535 visits to New Hanover Medical Center in 2016
#so average p should be 535/n = .002471931
c <- 535

#gender parameter estimation
p <- .7 #proportion of cases that are male
pc <- round(p*c) #positive cases are the rounded product of these
nc <- c - pc #negative cases are the remainders
or <- pc*(n-sum(gend))/(sum(gend)*nc)
bgen <- log(or) # Beta for male gender 


#Age parameter estimation
  pops <- table(ages)/n
  risks <- c(0, .06, .55, .38, .01)
  nrisks <- round(risks * c)
  nrisks[5] <- nrisks[5] + 1 #Due to odd rounding
  
  agemat <- matrix(c(nrisks, table(ages)-nrisks), nrow = 2, byrow = T)
  agemat
#Baseline defined as second group due to the first being degenerate

  bage1 <- -Inf #This approaches negative inifinity so that the p for this group goes to 0
  bage2 <- 0
  bage3 <- log(agemat[1,3]*agemat[2,2]/agemat[1,2]/agemat[2,3]) #Remaining logs odds ratios used to
  bage4 <- log(agemat[1,4]*agemat[2,2]/agemat[1,2]/agemat[2,4]) #To esimated beta factors
  bage5 <- log(agemat[1,5]*agemat[2,2]/agemat[1,2]/agemat[2,5])


#Race parameter estimation
pops <- table(races)/n
risks <- c(.86, .10, 0, .38, .01)
nrisks <- round(risks * c)
nrisks[5] <- nrisks[5] + 1 #Due to odd rounding

agemat <- matrix(c(nrisks, table(ages)-nrisks), nrow = 2, byrow = T)
agemat
#Baseline defined as second group due to the first being degenerate

bage1 <- -Inf #This approaches negative inifinity so that the p for this group goes to 0
bage2 <- 0
bage3 <- log(agemat[1,3]*agemat[2,2]/agemat[1,2]/agemat[2,3]) #Remaining logs odds ratios used to
bage4 <- log(agemat[1,4]*agemat[2,2]/agemat[1,2]/agemat[2,4]) #To esimated beta factors
bage5 <- log(agemat[1,5]*agemat[2,2]/agemat[1,2]/agemat[2,5])
