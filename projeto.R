setwd("C:/Users/g1a9p/Documents/R/Projecto-R")

library(readxl)
CreditCard <- read_excel("CreditCard.xlsx", 
                         +     col_types = c("text", "numeric", "numeric", 
                                             +         "numeric", "numeric", "numeric", 
                                             +         "numeric", "numeric", "numeric", 
                                             +         "numeric", "numeric", "numeric", 
                                             +         "numeric", "numeric"))

summary(CreditCard)
