setwd("C:/Users/g1a9p/Documents/R/Projecto-R")

#Bibliotecas essenciais para o algoritmo K-Means
library(ggplot2)
library("factoextra")

#Biblioteca essencial para a leitura do ficheiro XLSX
library(readxl)

#Leitura do Ficheiro e passagem de variáveis para formato contínuo
CreditCard <- read_excel("CreditCard.xlsx", col_types = c("text", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric"))
summary(CreditCard)
View(CreditCard)


#Transformar Colunas em variaveis
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
CreditCard.no.outliers = subset(CreditCard,CreditCard$CASH_ADVANCE_FREQUENCY<1.0 & CreditCard$CREDIT_LIMIT<24000 & CreditCard$PURCHASES<12000 & CreditCard$INSTALLMENTS_PURCHASES<8000 & CreditCard$CASH_ADVANCE_TRX<56 & CreditCard$PURCHASES_TRX<240)
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




#Criacao de um novo DataSet - Objetivo: Proporcionar uma experiencia superior atraves de um melhor conhecimento do cliente
CreditCard_final_1 <- cbind(CreditCard.no.outliers$PURCHASES_FREQUENCY,CreditCard.no.outliers$PURCHASES)
View(CreditCard_final_1)



#Scale
m = apply(CreditCard_final_1,2,mean) #medias das colunas/variaveis
s = apply(CreditCard_final_1, 2, sd) #desvio padrao das variaveis
CreditCard_scale = scale(CreditCard_final_1, m ,s)



#Gerar o algoritmo K-Means
CreditCardkm = kmeans(CreditCard_scale,3)
CreditCardkm


#Visualizar os Clusters
fviz_cluster(CreditCardkm ,xlab = "PURCHASES_FREQUENCY",ylab = "PURCHASES", geom ="point", data = as.data.frame(CreditCard_scale)) + ggtitle("Objetivo 1")

#Testar o K-Means
CreditCardkm = kmeans(CreditCard_scale,2)
CreditCardkm = kmeans(CreditCard_scale,4)
CreditCardkm = kmeans(CreditCard_scale,5)
CreditCardkm = kmeans(CreditCard_scale,6)
fviz_cluster(CreditCardkm , geom ="point", data = CreditCard_scale) + ggtitle("Objetivo 1")






#Criacao de um novo DataSet - Objetivo: Reduzir as taxas de abandono para outros bancos (churn) de forma pro-ativa 
CreditCard_final_2 <- cbind(CreditCard.no.outliers$BALANCE_FREQUENCY,CreditCard.no.outliers$PURCHASES_TRX)


#Scale
m = apply(CreditCard_final_2,2,mean) #medias das colunas/variaveis
s = apply(CreditCard_final_2, 2, sd) #desvio padrao das variaveis
CreditCard_scale = scale(CreditCard_final_2, m ,s)



#Gerar o algoritmo K-Means
CreditCardkm = kmeans(CreditCard_scale,3)
CreditCardkm


#Visualizar os Clusters
fviz_cluster(CreditCardkm ,xlab = "BALANCE_FREQUENCY",ylab = "PURCHASES_TRX", geom ="point", data = as.data.frame(CreditCard_scale)) + ggtitle("Objetivo 2")


#Testar o K-Means
CreditCardkm = kmeans(CreditCard_scale,2)
CreditCardkm = kmeans(CreditCard_scale,4)
CreditCardkm = kmeans(CreditCard_scale,5)
CreditCardkm = kmeans(CreditCard_scale,6)
fviz_cluster(CreditCardkm , geom ="point", data = as.data.frame(CreditCard_scale)) + ggtitle("Objetivo 2")






#Criacao de um novo DataSet - Objetivo: Fomentar o uso do cartao de credito por parte dos seus clientes
CreditCard_final_3 <- cbind(CreditCard.no.outliers$PURCHASES,CreditCard.no.outliers$CREDIT_LIMIT)

#Scale
m = apply(CreditCard_final_3,2,mean) #medias das colunas/variaveis
s = apply(CreditCard_final_3, 2, sd) #desvio padrao das variaveis
CreditCard_scale = scale(CreditCard_final_3, m ,s)



#Gerar o algoritmo K-Means
CreditCardkm = kmeans(CreditCard_scale,3)
CreditCardkm


#Visualizar os Clusters
fviz_cluster(CreditCardkm , xlab = "PURCHASES", ylab = "CREDIT_LIMIT" , geom ="point", data = as.data.frame(CreditCard_scale)) + ggtitle("Objetivo 3") 


#Testar o K-Means
CreditCardkm = kmeans(CreditCard_scale,2)
CreditCardkm = kmeans(CreditCard_scale,4)
CreditCardkm = kmeans(CreditCard_scale,5)
CreditCardkm = kmeans(CreditCard_scale,6)
fviz_cluster(CreditCardkm , geom ="point", data = as.data.frame(CreditCard_scale)) + ggtitle("Objetivo 3")
