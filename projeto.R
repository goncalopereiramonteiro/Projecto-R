setwd("C:/Users/g1a9p/Documents/R/Projecto-R")

library(ggplot2)
library(readxl)
install.packages("factoextra")
library("factoextra")


#Leitura do Ficheiro e passagem de variáveis para formato contínuo
CreditCard <- read_excel("CreditCard.xlsx", col_types = c("text", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric"))

summary(CreditCard)
View(CreditCard)

#Transformar Colunas em variáveis
BalanceFrequency <- CreditCard$BALANCE_FREQUENCY
Purchases <- CreditCard$PURCHASES
InstallmentsPurchases <- CreditCard$INSTALLMENTS_PURCHASES
PurchasesFrequency <- CreditCard$PURCHASES_FREQUENCY
PurchasesInstallmentsFrequency <- CreditCard$PURCHASES_INSTALLMENTS_FREQUENCY
CashAdvanceFrequency <- CreditCard$CASH_ADVANCE_FREQUENCY
CashAdvanceTrx <- CreditCard$CASH_ADVANCE_TRX
PurchasesTrx <- CreditCard$PURCHASES_TRX
CreditLimit <- CreditCard$CREDIT_LIMIT



#Remover outliers CreditCard
CreditCard.no.outliers = subset(CreditCard,CreditCard$CASH_ADVANCE_FREQUENCY<1.0 & CreditCard$CREDIT_LIMIT<24000 & CreditCard$PURCHASES<30000 & CreditCard$INSTALLMENTS_PURCHASES<14000 & CreditCard$CASH_ADVANCE_TRX<56 & CreditCard$PURCHASES_TRX < 240)
boxplot(CashAdvanceFrequency.no.outliers)
summary(CashAdvanceFrequency.no.outliers)
View(CreditCard.no.outliers)



#CreditLimit -> Introduzir os NA's com a m?dia
for(i in 1:length(CreditCard.no.outliers$CREDIT_LIMIT)) {
  if(is.na(CreditCard.no.outliers$CREDIT_LIMIT[i])) {
    CreditCard.no.outliers$CREDIT_LIMIT[i] = 4494
  }
}
summary(CreditLimit)



#PurchasesInstallmentsFrequency -> Introduzir NAs com a media
for(i in 1:length(CreditCard.no.outliers$PURCHASES_INSTALLMENTS_FREQUENCY)) {
  if(is.na(CreditCard.no.outliers$PURCHASES_INSTALLMENTS_FREQUENCY[i])) {
    CreditCard.no.outliers$PURCHASES_INSTALLMENTS_FREQUENCY[i] = 0.3505
  }
}



#Criação de um novo DataSet
CreditCard_final <- cbind(CreditCard.no.outliers$PURCHASES_FREQUENCY,CreditCard.no.outliers$PURCHASES_TRX)
View(CreditCard_final)


#Scale
m = apply(CreditCard_final,2,mean) #medias das colunas/variaveis
s = apply(CreditCard_final, 2, sd) #desvio padrao das variaveis
CreditCard_scale = scale(CreditCard_final, m ,s)




#Gerar o algoritmo K-Means
CreditCardkm = kmeans(CreditCard_scale,3)
CreditCardkm


#Visualizar
fviz_cluster(CreditCardkm , geom ="point", data = as.data.frame(CreditCard_scale)) 


#testar K-Means
CreditCardkm = kmeans(CreditCard_scale,2)
CreditCardkm = kmeans(CreditCard_scale,4)
CreditCardkm = kmeans(CreditCard_scale,5)
CreditCardkm = kmeans(CreditCard_scale,6)
fviz_cluster(CreditCardkm , geom ="point", data = CreditCard_scale) 

