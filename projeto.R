install.packages("xlsx")
library(xlsx)
dados <- read.xlsx("CreditCard.xlsx",1)
dados
summary(dados)
