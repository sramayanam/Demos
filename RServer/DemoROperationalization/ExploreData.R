###Read the file###
#
Loans <- read.csv("C:/Users/srram/Desktop/CleanedOutput.csv",header = TRUE)
#         header = TRUE,
#         col.names=c('LoanID', 'CustomerID', 'Status', 'Amount', 'Term', 'CrdtScore',
#                       'YearsJob', 'HomeOwnshp', 'AnnlIncome', 'Purpose',
#                       'MthlyDebt', 'YearsCrdtHist', 'MthsLastDlqnt', 'OpenAccts', 'CrdtProbs',
#                       'CrdtBal', 'OpenCrdt', 'Bankrptcies', 'TaxLiens'),
#         stringsAsFactors = FALSE)



#library(tidyr)
#remoteLogin("http://srramrserver.southcentralus.cloudapp.azure.com:12800", session = TRUE, commandline = TRUE, prompt = "Remote->>")
library(dplyr)
library(RevoScaleR)
library(ggplot2)
#summary(Loans)
convert.factor <- function(x) {

    if (is.character(Loans[, x])) {
        # print("trying to convert ... 2..")
        as.factor(Loans[, x])
    }
}

name.list <- function(x) {
    names <- names(x)
    len <- length(names)
    names[-len]
}

lapply(c("Purpose", "HomeOwnshp", "YearsJob"), as.factor)
names(Loans)

#n <- name.list(Loans)
attach(Loans)
##Start Cleaning up and Transforming the data
#Loans <- filter(Loans, !is.na(AnnlIncome))
Loans <- filter(Loans, !is.na(CrdtScore))
Loans <- filter(Loans, Bankrptcies != "NA")
Loans <- filter(Loans, TaxLiens != "NA")
Loans$MthlyDebt <- as.double(sub(",", "", substring(Loans$MthlyDebt, 2,)))
Loans$OpenCrdt <- as.integer(Loans$OpenCrdt)
Loans <- filter(Loans, OpenCrdt > 0)
Loans <- filter(Loans, AnnlIncome > 0)
Loans$CrdtScore <- as.integer(substring(Loans$CrdtScore, 1, 3))
Loans <- mutate(Loans, MthsLastDlqntnew = ifelse(is.na(MthsLastDlqnt) & CrdtProbs == 0, 0, MthsLastDlqnt))
Loans <- mutate(Loans, CrdtProbsnew = ifelse(CrdtProbs >= 1, 1, 0))
Loans <- Loans %>% mutate(Purpose.new = ifelse(Purpose != "Debt Consolidation", ifelse(Purpose == "Business Loan", "Business Loan", "NDC Personal"), "Debt Consolidation"))
Loans <- mutate(Loans, risk =
                ifelse((CrdtProbs >= 1 | MthsLastDlqntnew > 2) & Purpose.new == "Non Debt Consolidation", 2,
                       ifelse(Purpose.new == "Debt Consolidation" & MthsLastDlqntnew > 24, 2, CrdtProbsnew)))

#summary(Loans)
Loans <- Loans %>% filter( (AnnlIncome < 2500000 | is.na(AnnlIncome)) & CrdtBal < 50000 & OpenCrdt < 2500000 )
Loans.coff <- Loans %>% filter(Amount < 40000 & Amount != 99999999 & AnnlIncome < 250000 & CrdtBal < 50000 & OpenCrdt < 150000 & AnnlIncome != -1)
#summary(Loans.coff)
#ggplot(Loans, aes(YearsJob)) +
#geom_bar(aes(fill = Status))

Loans.coff <- Loans %>% filter(Amount < 40000 & AnnlIncome < 250000 & CrdtBal < 50000 & OpenCrdt < 250000 & MthsLastDlqnt > 0)
Loans.coff <- Loans.coff %>% filter(Purpose %in% c("Take a Trip", "Educational Expenses", "Debt Consolidation"))

write.csv(Loans, "C:/Users/srram/Desktop/Demos/RServer/loansclean.csv")

#levels(as.factor(Loans$Purpose))

###Gain Insights
ggplot(Loans.coff, aes(TaxLiens, color = Status)) +
  geom_freqpoly(aes(y = ..density..)) +
  facet_grid(. ~ Purpose)

ggplot(Loans.coff, aes_string('Status', "MthsLastDlqnt")) +
  geom_boxplot(aes(color = Status))

ggplot(Loans.coff, aes(Home.Ownership)) +
  geom_bar(aes(fill = Status))

ggplot(Loans.coff, aes(YearsJob)) +
  geom_bar(aes(fill = Status))

ggplot(Loans.coff, aes(risk)) +
  geom_bar(aes(fill = Status))

ggplot(Loans.coff, aes(Purposenew)) +
  geom_bar(aes(fill = Status))

ggplot(Loans, aes(MthsLastDlqntnew, fill = Status)) +
  geom_histogram(binwidth = 5) +
 facet_wrap(~CrdtProbsnew)

ggplot(Loans.coff, aes_string('Status', "OpenAccts")) +
  geom_boxplot(aes(color = Status))

ggplot(Loans.coff, aes_string('Status', "OpenCrdt")) +
  geom_boxplot(aes(color = Status))

ggplot(Loans.coff, aes_string('Status', "CrdtBal")) +
  geom_boxplot(aes(color = Status))

ggplot(Loans.coff, aes_string('Status', "Amount")) +
  geom_boxplot(aes(color = Status))

ggplot(Loans.coff, aes_string('Status', "AnnlIncome")) +
  geom_boxplot(aes(color = Status))

ggplot(Loans.coff, aes_string('Amount', "MthlyDebt")) +
  geom_smooth(aes(color = Status))

ggplot(Loans.coff, aes_string('Amount', "AnnlIncome")) +
  geom_point(aes(color = Status))
