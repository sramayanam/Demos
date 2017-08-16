library(RevoScaleR)
inputFile <- file.path(getwd(), "loansclean.csv")
inputFile <- unique.data.frame(inputFile)
outputFilexdf <- file.path(getwd(), "loansclean.xdf")
transformedxdf <- file.path(getwd(), "transform.xdf")

rxImport(inputFile, outputFilexdf)

normalize <- function(x) {
    log(x)
}

transformamount <- function(amount, monthlydebt, term) {
    if (amount == 99999999 & term == 'Short Term') {
        log(monthlydebt * 18)
    }
    else {
        if (amount == 99999999 & term == 'Long Term') {
            log(monthlydebt * 180)
        } else {
            log(amount)
        }
    }
}

#modelparameters <- c("Status", "Amount", "Term", "CrdtScore", "YearsJob", "HomeOwnshp","AnnlIncome", "Purpose","MthlyDebt", "YearsCrdtHist","Bankrptcies","TaxLiens","Dfactor","crdtusage")
modelparameters <- c("Status", "Amount", "Term", "CrdtScore", "YearsJob", "HomeOwnshp", "AnnlIncome", "Purpose", "MthlyDebt", "YearsCrdtHist", "Bankrptcies", "TaxLiens", "Dfactor", "crdtusage")
dropfeautures <- c("LoanID","CustomerID","CrdtProbs","CrdtProbsnew","Purpose.new","risk")

rxDataStep(inData = outputFilexdf, outFile = transformedxdf,
            transforms = list(Status = as.factor(Status), CrdtScore = myTransform(CrdtScore), crdtusage = (CrdtBal / OpenCrdt), debttoincome = (MthlyDebt / AnnlIncome),
            DFactor = cut(MthsLastDlqntnew, breaks = c(-2, 0, 5, 15, 30,60,150), labels = c("Unknown", "VeryLow", "Low","Medium","High","Very High")),
            Term = as.factor(Term), YearsJob = as.factor(YearsJob), HomeOwnshp = as.factor(HomeOwnshp), Purpose = as.factor(Purpose),
            AnnlIncome = myTransform(AnnlIncome), MthlyDebt = myTransform(MthlyDebt), Amount = myTransform1(Amount, MthlyDebt, Term)), transformObjects = list(myTransform = normalize, myTransform1 = transformamount),
            removeMissings = TRUE, overwrite = TRUE, varsToDrop = dropfeautures)

rxGetVarInfo(transformedxdf)

# Set the random seed for reproducibility of randomness.

set.seed(4253, "L'Ecuyer-CMRG")

# Randomly split the data 80-20 between train and test sets.

dataProb <- c(Train = 0.8, Test = 0.2)

dataSplit <-rxSplit(transformedxdf, splitByFactor = "splitVar",transforms = list(splitVar =sample(dataFactor,size = .rxNumRows,replace = TRUE,
                                       prob = dataProb)),
                                       transformObjects =list(dataProb = dataProb,dataFactor = factor(names(dataProb),levels = names(dataProb))),outFilesBase = "loans")

# Name the train and test datasets.
dataTrain <- dataSplit[[1]]
dataTest <- dataSplit[[2]]

names(dataTrain)

model <- formula("Status ~ Amount + Term + CrdtScore + YearsJob + HomeOwnshp  + Purpose + debttoincome + YearsCrdtHist + crdtusage + DFactor")
#model <- formula("Status ~ Amount + Term + CrdtScore + YearsJob + HomeOwnshp  + Purpose + debttoincome + YearsCrdtHist + crdtusage ")

rxLogisticRegressionFit <- rxLogisticRegression(model, data = dataTrain)
rxFastLinearFit <- rxFastLinear(model, data = dataTrain)
rxFastTreesFit <- rxFastTrees(model, data = dataTrain, randomSeed = 23648)
rxFastForestFit <- rxFastForest(model, data = dataTrain, randomSeed = 23648)
rxNeuralNetFit <- rxNeuralNet(model, data = dataTrain)

fitScores <-  rxPredict(rxLogisticRegressionFit, dataTest, suffix = ".rxLogisticRegression",
              extraVarsToWrite = names(dataTest),
              outData = tempfile(fileext = ".xdf"))
fitScores <- rxPredict(rxFastLinearFit, fitScores, suffix = ".rxFastLinear",
              extraVarsToWrite = names(fitScores),
              outData = tempfile(fileext = ".xdf"))
fitScores <- rxPredict(rxFastTreesFit, fitScores, suffix = ".rxFastTrees",
              extraVarsToWrite = names(fitScores),
              outData = tempfile(fileext = ".xdf"))
fitScores <- rxPredict(rxFastForestFit, fitScores, suffix = ".rxFastForest",
              extraVarsToWrite = names(fitScores),
              outData = tempfile(fileext = ".xdf"))
fitScores <- rxPredict(rxNeuralNetFit, fitScores, suffix = ".rxNeuralNet",
              extraVarsToWrite = names(fitScores),
              outData = tempfile(fileext = ".xdf"))


scores <- file.path(getwd(), "scores.xdf")

tobinary <- function(x) {
    if (x == "FullyPaid" ) {
        as.numeric(x == "FullyPaid")
    }
    else {
        as.numeric(x == "FullyPaid")
    }
}

rxDataStep(inData = fitScores, outFile = scores,
            transforms = list(status_n = myTransform(as.character(Status))), transformObjects = list(myTransform = tobinary),
            removeMissings = TRUE, overwrite = TRUE)


fitRoc <- rxRoc("status_n", paste("Probability", c("rxLogisticRegression.FullyPaid", "rxFastLinear.FullyPaid", "rxFastTrees.FullyPaid", "rxFastForest.FullyPaid", "rxNeuralNet.FullyPaid"), sep = "."), scores)

plot(fitRoc)

fitList <-    list( rxLogisticRegression = rxLogisticRegressionFit,
                    rxFastLinear = rxFastLinearFit,
                    rxFastTrees = rxFastTreesFit,
                    rxFastForest = rxFastForestFit,
                    rxNeuralNet = rxNeuralNetFit)

# Compute the fit models's AUCs.
fitAuc <- rxAuc(fitRoc)
names(fitAuc) <- substring(names(fitAuc), nchar("Probability.") + 1)

# Find the name of the fit with the largest AUC.
bestFitName <- names(which.max(fitAuc))

# Select the fit model with the largest AUC.
bestFit <- fitList[[bestFitName]]

# Report the fit AUCs.
cat("Fit model AUCs:\n")
print(fitAuc, digits = 2)

# Report the best fit.
cat(paste0("Best fit model with ", bestFitName,
           ", AUC = ", signif(fitAuc[[bestFitName]], digits = 2),
           ".\n"))