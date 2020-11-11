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
summary(CreditCard.no.outliers)
View(CreditCard.no.outliers)



#CreditLimit -> Introduzir os NA's com a media
for(i in 1:length(CreditCard.no.outliers$CREDIT_LIMIT)) {
  if(is.na(CreditCard.no.outliers$CREDIT_LIMIT[i])) {
    CreditCard.no.outliers$CREDIT_LIMIT[i] = 4494
  }
}
summary(CreditCard.no.outliers$CREDIT_LIMIT)



#PurchasesInstallmentsFrequency -> Introduzir NA's com a media
for(i in 1:length(CreditCard.no.outliers$PURCHASES_INSTALLMENTS_FREQUENCY)) {
  if(is.na(CreditCard.no.outliers$PURCHASES_INSTALLMENTS_FREQUENCY[i])) {
    CreditCard.no.outliers$PURCHASES_INSTALLMENTS_FREQUENCY[i] = 0.3505
  }
}
summary(CreditCard.no.outliers$PURCHASES_INSTALLMENTS_FREQUENCY)



#Remover Colunas (ONEOFF_PURCHASES,GOLD_CARD,PRC_CAR_USAGE,OLD_CARD)
CreditCard.no.outliers <- CreditCard.no.outliers[,-c(4,6,8,12)]
View(CreditCard.no.outliers)
summary(CreditCard.no.outliers)




#Criação de um novo DataSet - Objetivo: Proporcionar uma experiência superior através de um melhor conhecimento do cliente
CreditCard_final_1 <- cbind(CreditCard.no.outliers$PURCHASES_FREQUENCY,CreditCard.no.outliers$PURCHASES_TRX)
View(CreditCard_final_1)



#Scale
m = apply(CreditCard_final_1,2,mean) #medias das colunas/variaveis
s = apply(CreditCard_final_1, 2, sd) #desvio padrao das variaveis
CreditCard_scale = scale(CreditCard_final_1, m ,s)



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






#Criação de um novo DataSet - Objetivo: Reduzir as taxas de abandono para outros bancos (churn) de forma pró-ativa 
CreditCard_final_2 <- cbind(CreditCard.no.outliers$BALANCE_FREQUENCY,CreditCard.no.outliers$PURCHASES_TRX)


#Scale
m = apply(CreditCard_final_2,2,mean) #medias das colunas/variaveis
s = apply(CreditCard_final_2, 2, sd) #desvio padrao das variaveis
CreditCard_scale = scale(CreditCard_final_2, m ,s)



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
fviz_cluster(CreditCardkm , geom ="point", data = as.data.frame(CreditCard_scale)) 







#Criação de um novo DataSet - Objetivo: Fomentar o uso do cartão de crédito por parte dos seus clientes
CreditCard_final_3 <- cbind(CreditCard.no.outliers$PURCHASES,CreditCard.no.outliers$CREDIT_LIMIT)

#Scale
m = apply(CreditCard_final_3,2,mean) #medias das colunas/variaveis
s = apply(CreditCard_final_3, 2, sd) #desvio padrao das variaveis
CreditCard_scale = scale(CreditCard_final_3, m ,s)



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
fviz_cluster(CreditCardkm , geom ="point", data = as.data.frame(CreditCard_scale)) 
