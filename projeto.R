setwd("C:/Users/g1a9p/Documents/R/Projecto-R")


library(readxl)
CreditCard <- read_excel("CreditCard.xlsx", col_types = c("text", "numeric", "numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric", "numeric"))

summary(CreditCard)
View(CreditCard)


BalanceFrequency <- CreditCard$BALANCE_FREQUENCY
Purchases <- CreditCard$PURCHASES
InstallmentsPurchases <- CreditCard$INSTALLMENTS_PURCHASES
PurchasesFrequency <- CreditCard$PURCHASES_FREQUENCY
PurchasesInstallmentsFrequency <- CreditCard$PURCHASES_INSTALLMENTS_FREQUENCY
CashAdvanceFrequency <- CreditCard$CASH_ADVANCE_FREQUENCY
CashAdvanceTrx <- CreditCard$CASH_ADVANCE_TRX
PurchasesTrx <- CreditCard$PURCHASES_TRX
CreditLimit <- CreditCard$CREDIT_LIMIT
View(CreditLimit)
summary(InstallmentsPurchases)

#Remover outlier CashAdvanceFrequency
#CashAdvanceFrequency.outliers = subset(CashAdvanceFrequency, Mileage>1000)
#CashAdvanceFrequency.no.outliers = subset(CashAdvanceFrequency, Mileage<=1000)

#Remover outliers Purchases
#Purchases.outliers = subset(Purchases, Mileage>3000)
#Purchases.no.outliers = subset(Purchases, Mileage<=3000)

#Remover outliers InstallmentsPurchases
#InstallmentsPurchases.outliers = subset(Purchases, Mileage>15000)
#InstallmentsPurchases.outliers = subset(Purchases, Mileage<=15000)


#CreditLimit -> Introduzir os NA's com a m?dia
for(i in 1:length(CreditLimit)) {
  if(is.na(CreditLimit[i])) {
    CreditLimit[i] = 4494
  }
}
summary(CreditLimit)



#PurchasesInstallmentsFrequency -> Introduzir NAs com a media
for(i in 1:length(PurchasesInstallmentsFrequency)) {
  if(is.na(PurchasesInstallmentsFrequency[i])) {
    PurchasesInstallmentsFrequency[i] = 0.3505
  }
}
summary(PurchasesInstallmentsFrequency)


boxplot(InstallmentsPurchases)
