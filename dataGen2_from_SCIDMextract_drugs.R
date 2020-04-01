setwd("~/projects/sourceData/")

# source("~/R/_workingDirectory/_perAdmissionRewriteDataTableFunctions.R")
# library(gtools)
# library(igraph)
library(data.table)
library(zoo)

id_per_location <- function(ID) {
  return(length(unique(ID)))
}

flagMove <- function(ID, charL) {
  
  charLreport <- charL
  charLnumeric <- as.numeric(factor(charL))
  
  testFrame <- data.frame(charLreport, charLnumeric)
  
  testFrame$flagMove <- 0
  testFrame$flagMove[1:nrow(testFrame)-1] <- diff(testFrame$charLnumeric)
  testFrame$nextL <- c("spacer")
  testFrame$nextL[1:(nrow(testFrame)-1)] <- charLreport[2:length(charLreport)]
  
  testFrame$charLreport <- as.character(factor(charL))
  
  outputList <- list(testFrame$charLreport, testFrame$nextL, testFrame$flagMove)
  
  return(outputList)
  
}

returnUnixDateTime<-function(date) {
  returnVal<-as.numeric(as.POSIXct(date, format="%Y-%m-%d", tz="GMT"))
  return(returnVal)
}

findSimilarDrugs <- function(inputFrame) {
  
  # inputFrame <- interestSet
  # inputFrame <- inputFrame[1:10000,]
  
  inputFrame$DrugName.original <- inputFrame$DrugName
  inputFrame$DrugNameNew <- inputFrame$DrugName
  
  inputFrame <- subset(inputFrame, DrugNameNew != "Disposable")
  
  inputFrame$DrugNameNew[grep("Glucose", inputFrame$DrugName, ignore.case = TRUE)] <- "Glucose"
  inputFrame$DrugNameNew[grep("Glucogel", inputFrame$DrugName, ignore.case = TRUE)] <- "Glucose"
  
  inputFrame$DrugNameNew[grep("Glucagen Hypokit", inputFrame$DrugName, ignore.case = TRUE)] <- "Glucagon"
  inputFrame$DrugNameNew[grep("Optium Plus", inputFrame$DrugName, ignore.case = TRUE)] <- "Test Strips"
  
  
  inputFrame$DrugNameNew[grep("Metformin", inputFrame$DrugName, ignore.case = TRUE)] <- "Metformin"
  inputFrame$DrugNameNew[grep("Glucophage", inputFrame$DrugName, ignore.case = TRUE)] <- "Metformin"
  
  inputFrame$DrugNameNew[grep("Gliclazide", inputFrame$DrugName, ignore.case = TRUE)] <- "Gliclazide"
  inputFrame$DrugNameNew[grep("Diamicron", inputFrame$DrugName, ignore.case = TRUE)] <- "Gliclazide"
  
  inputFrame$DrugNameNew[grep("Rosiglitazone", inputFrame$DrugName, ignore.case = TRUE)] <- "Rosiglitazone"
  inputFrame$DrugNameNew[grep("Avandia", inputFrame$DrugName, ignore.case = TRUE)] <- "Rosiglitazone"
  
  inputFrame$DrugNameNew[grep("Linagliptin", inputFrame$DrugName, ignore.case = TRUE)] <- "Linagliptin"
  
  inputFrame$DrugNameNew[grep("Victoza", inputFrame$DrugName, ignore.case = TRUE)] <- "Liraglutide"
  inputFrame$DrugNameNew[grep("Liraglutide", inputFrame$DrugName, ignore.case = TRUE)] <- "Liraglutide"
  
  inputFrame$DrugNameNew[grep("Pioglitazone", inputFrame$DrugName, ignore.case = TRUE)] <- "Pioglitazone"
  
  inputFrame$DrugNameNew[grep("Sitagliptin", inputFrame$DrugName, ignore.case = TRUE)] <- "Sitagliptin"
  inputFrame$DrugNameNew[grep("Januvia", inputFrame$DrugName, ignore.case = TRUE)] <- "Sitagliptin"
  
  inputFrame$DrugNameNew[grep("Dapagliflozin", inputFrame$DrugName, ignore.case = TRUE)] <- "Dapagliflozin"
  
  inputFrame$DrugNameNew[grep("Humalog Mix25", inputFrame$DrugName, ignore.case = TRUE)] <- "Humalog Mix 25"
  
  inputFrame$DrugNameNew[grep("Lantus", inputFrame$DrugName, ignore.case = TRUE)] <- "Insulin Glargine"
  inputFrame$DrugNameNew[grep("Levemir", inputFrame$DrugName, ignore.case = TRUE)] <- "Insulin Detemir"
  
  inputFrame$DrugNameNew[grep("Insulatard", inputFrame$DrugName, ignore.case = TRUE)] <- "Insulatard"
  
  inputFrame$DrugNameNew[grep("Actrapid", inputFrame$DrugName, ignore.case = TRUE)] <- "Actrapid"
  inputFrame$DrugNameNew[grep("Humalog 100units/ml solution", inputFrame$DrugName, ignore.case = TRUE)] <- "Humalog"
  
  inputFrame$DrugNameNew[grep("Novorapid", inputFrame$DrugName, ignore.case = TRUE)] <- "Novorapid"
  
  inputFrame$DrugNameNew[grep("Novomix 30", inputFrame$DrugName, ignore.case = TRUE)] <- "Novomix 30"
  
  inputFrame$DrugNameNew[grep("Mixtard 30", inputFrame$DrugName, ignore.case = TRUE)] <- "Mixtard 30"
  inputFrame$DrugNameNew[grep("Mixtard 20", inputFrame$DrugName, ignore.case = TRUE)] <- "Mixtard 20"
  
  inputFrame$DrugNameNew[grep("Humulin M3", inputFrame$DrugName, ignore.case = TRUE)] <- "Humulin M3"
  
  inputFrame$DrugNameNew[grep("Humalog Mix50", inputFrame$DrugName, ignore.case = TRUE)] <- "Humalog Mix50"
  
  inputFrame$DrugNameNew[grep("strip", inputFrame$DrugName, ignore.case = TRUE)] <- "Test Strips"
  
  inputFrame$DrugNameNew[grep("Bd-Microfine", inputFrame$DrugName, ignore.case = TRUE)] <- "Needle"
  inputFrame$DrugNameNew[grep("Needle", inputFrame$DrugName, ignore.case = TRUE)] <- "Needle"
  
  
  outputFrame <- inputFrame
  
  outputFrame$DrugName.original <- NULL
  outputFrame$DrugName <- outputFrame$DrugNameNew
  outputFrame$DrugNameNew <- NULL
  
  return(outputFrame)
}

simplifyDrugs <- function(inputFrame) {
  
  # inputFrame <- interestSet
  # inputFrame <- inputFrame[1:100000,]
  
  inputFrame$DrugName.original <- inputFrame$DrugName
  inputFrame$DrugNameNew <- inputFrame$DrugName
  
  inputFrame <- subset(inputFrame, DrugNameNew != "Disposable")
  
  inputFrame$DrugNameNew[grep("Glucose", inputFrame$DrugName, ignore.case = TRUE)] <- "Glucose"
  inputFrame$DrugNameNew[grep("Glucogel", inputFrame$DrugName, ignore.case = TRUE)] <- "Glucose"
  inputFrame$DrugNameNew[grep("Dextrogel", inputFrame$DrugName, ignore.case = TRUE)] <- "Glucose"
  inputFrame$DrugNameNew[grep("DEXTROSE", inputFrame$DrugName, ignore.case = TRUE)] <- "Glucose"
  
  
  inputFrame$DrugNameNew[grep("Glucagen Hypokit", inputFrame$DrugName, ignore.case = TRUE)] <- "Glucagon"
  inputFrame$DrugNameNew[grep("Glucagon", inputFrame$DrugName, ignore.case = TRUE)] <- "Glucagon"
  
  inputFrame$DrugNameNew[grep("Optium Plus", inputFrame$DrugName, ignore.case = TRUE)] <- "Test Strips"
  
  inputFrame$DrugNameNew[grep("Acarbose", inputFrame$DrugName, ignore.case = TRUE)] <- "Acarbose"
  inputFrame$DrugNameNew[grep("Glucobay", inputFrame$DrugName, ignore.case = TRUE)] <- "Acarbose"
  
  inputFrame$DrugNameNew[grep("REPAGLINIDE", inputFrame$DrugName, ignore.case = TRUE)] <- "meglitinide"
  inputFrame$DrugNameNew[grep("Repaglinide", inputFrame$DrugName, ignore.case = TRUE)] <- "meglitinide"
  inputFrame$DrugNameNew[grep("Nateglinide", inputFrame$DrugName, ignore.case = TRUE)] <- "meglitinide"
  
  inputFrame$DrugNameNew[grep("Metformin", inputFrame$DrugName, ignore.case = TRUE)] <- "Metformin_"
  inputFrame$DrugNameNew[grep("Glucophage", inputFrame$DrugName, ignore.case = TRUE)] <- "Metformin_"
  inputFrame$DrugNameNew[grep("Sukkarto", inputFrame$DrugName, ignore.case = TRUE)] <- "Metformin_"
  
  inputFrame$DrugNameNew[grep("Gliclazide", inputFrame$DrugName, ignore.case = TRUE)] <- "SU_"
  inputFrame$DrugNameNew[grep("Glipizide", inputFrame$DrugName, ignore.case = TRUE)] <- "SU_"
  inputFrame$DrugNameNew[grep("Glibenese", inputFrame$DrugName, ignore.case = TRUE)] <- "SU_"
  inputFrame$DrugNameNew[grep("Amaryl", inputFrame$DrugName, ignore.case = TRUE)] <- "SU_"
  inputFrame$DrugNameNew[grep("Glimepiride", inputFrame$DrugName, ignore.case = TRUE)] <- "SU_"
  inputFrame$DrugNameNew[grep("GLIMEPIRIDE", inputFrame$DrugName, ignore.case = TRUE)] <- "SU_"
  inputFrame$DrugNameNew[grep("Glibenclamide", inputFrame$DrugName, ignore.case = TRUE)] <- "SU_"
  inputFrame$DrugNameNew[grep("Daonil", inputFrame$DrugName, ignore.case = TRUE)] <- "SU_"

  inputFrame$DrugNameNew[grep("Rosiglitazone", inputFrame$DrugName, ignore.case = TRUE)] <- "TZD_"
  inputFrame$DrugNameNew[grep("Pioglitazone", inputFrame$DrugName, ignore.case = TRUE)] <- "TZD_"
  inputFrame$DrugNameNew[grep("Actos", inputFrame$DrugName, ignore.case = TRUE)] <- "TZD_"
  
  inputFrame$DrugNameNew[grep("Linagliptin", inputFrame$DrugName, ignore.case = TRUE)] <- "DPP4_"
  inputFrame$DrugNameNew[grep("Trajenta", inputFrame$DrugName, ignore.case = TRUE)] <- "DPP4_"
  inputFrame$DrugNameNew[grep("Sitagliptin", inputFrame$DrugName, ignore.case = TRUE)] <- "DPP4_"
  inputFrame$DrugNameNew[grep("Januvia", inputFrame$DrugName, ignore.case = TRUE)] <- "DPP4_"

  inputFrame$DrugNameNew[grep("Vildagliptin", inputFrame$DrugName, ignore.case = TRUE)] <- "DPP4_"
  inputFrame$DrugNameNew[grep("Galvus", inputFrame$DrugName, ignore.case = TRUE)] <- "DPP4_"
  inputFrame$DrugNameNew[grep("Saxagliptin", inputFrame$DrugName, ignore.case = TRUE)] <- "DPP4_"
  inputFrame$DrugNameNew[grep("SAXAGLIPTIN", inputFrame$DrugName, ignore.case = TRUE)] <- "DPP4_"
  inputFrame$DrugNameNew[grep("Alogliptin", inputFrame$DrugName, ignore.case = TRUE)] <- "DPP4_"
  
  inputFrame$DrugNameNew[grep("Liraglutide", inputFrame$DrugName, ignore.case = TRUE)] <- "GLP1_"
  inputFrame$DrugNameNew[grep("Victoza", inputFrame$DrugName, ignore.case = TRUE)] <- "GLP1_"
  inputFrame$DrugNameNew[grep("Exenatide", inputFrame$DrugName, ignore.case = TRUE)] <- "GLP1_"
  inputFrame$DrugNameNew[grep("Byetta", inputFrame$DrugName, ignore.case = TRUE)] <- "GLP1_"
  inputFrame$DrugNameNew[grep("BYETTA", inputFrame$DrugName, ignore.case = TRUE)] <- "GLP1_"
  inputFrame$DrugNameNew[grep("Bydureon", inputFrame$DrugName, ignore.case = TRUE)] <- "GLP1_"
  inputFrame$DrugNameNew[grep("Lixisenatide", inputFrame$DrugName, ignore.case = TRUE)] <- "GLP1_"
  inputFrame$DrugNameNew[grep("Lyxumia", inputFrame$DrugName, ignore.case = TRUE)] <- "GLP1_"
  inputFrame$DrugNameNew[grep("Dulaglutide", inputFrame$DrugName, ignore.case = TRUE)] <- "GLP1_"
  inputFrame$DrugNameNew[grep("Trulicity", inputFrame$DrugName, ignore.case = TRUE)] <- "GLP1_"
  
  inputFrame$DrugNameNew[grep("Dapagliflozin", inputFrame$DrugName, ignore.case = TRUE)] <- "SGLT2_"
  inputFrame$DrugNameNew[grep("Forxiga", inputFrame$DrugName, ignore.case = TRUE)] <- "SGLT2_"
  inputFrame$DrugNameNew[grep("Canagliflozin", inputFrame$DrugName, ignore.case = TRUE)] <- "SGLT2_"
  inputFrame$DrugNameNew[grep("Invokana", inputFrame$DrugName, ignore.case = TRUE)] <- "SGLT2_"
  inputFrame$DrugNameNew[grep("Empagliflozin", inputFrame$DrugName, ignore.case = TRUE)] <- "SGLT2_"
  inputFrame$DrugNameNew[grep("Jardiance", inputFrame$DrugName, ignore.case = TRUE)] <- "SGLT2_"
  
  # inputFrame$DrugNameNew[grep("Dapagliflozin", inputFrame$DrugName, ignore.case = TRUE)] <- "SGLT2dapa_"
  # inputFrame$DrugNameNew[grep("Forxiga", inputFrame$DrugName, ignore.case = TRUE)] <- "SGLT2dapa_"
  # inputFrame$DrugNameNew[grep("Canagliflozin", inputFrame$DrugName, ignore.case = TRUE)] <- "SGLT2cana_"
  # inputFrame$DrugNameNew[grep("Invokana", inputFrame$DrugName, ignore.case = TRUE)] <- "SGLT2cana_"
  # inputFrame$DrugNameNew[grep("Empagliflozin", inputFrame$DrugName, ignore.case = TRUE)] <- "SGLT2empa_"
  # inputFrame$DrugNameNew[grep("Jardiance", inputFrame$DrugName, ignore.case = TRUE)] <- "SGLT2empa_"
  
  # combinations
  inputFrame$DrugNameNew[grep("Avandamet", inputFrame$DrugName, ignore.case = TRUE)] <- "MetforminTZD_"
  inputFrame$DrugNameNew[grep("Competact", inputFrame$DrugName, ignore.case = TRUE)] <- "MetforminTZD_"
  inputFrame$DrugNameNew[grep("Eucreas", inputFrame$DrugName, ignore.case = TRUE)] <- "DPP4Metformin_"
  inputFrame$DrugNameNew[grep("EUCREAS", inputFrame$DrugName, ignore.case = TRUE)] <- "DPP4Metformin_"
  inputFrame$DrugNameNew[grep("Janumet", inputFrame$DrugName, ignore.case = TRUE)] <- "DPP4Metformin_"
  inputFrame$DrugNameNew[grep("JANUMET", inputFrame$DrugName, ignore.case = TRUE)] <- "DPP4Metformin_"
  
  inputFrame$DrugNameNew[grep("ideglira", inputFrame$DrugName, ignore.case = TRUE)] <- "ideglira_"
  inputFrame$DrugNameNew[grep("xultophy", inputFrame$DrugName, ignore.case = TRUE)] <- "ideglira_"

  # SSRI
  inputFrame$DrugNameNew[grep("Duloxetine", inputFrame$DrugName, ignore.case = TRUE)] <- "SSRI"
  inputFrame$DrugNameNew[grep("DULOXETINE", inputFrame$DrugName, ignore.case = TRUE)] <- "SSRI"
  
  
  # bd mix insulins
  inputFrame$DrugNameNew[grep("Humalog Mix", inputFrame$DrugName, ignore.case = TRUE)] <- "BDmixInsulin_"
  inputFrame$DrugNameNew[grep("Novomix", inputFrame$DrugName, ignore.case = TRUE)] <- "BDmixInsulin_"
  inputFrame$DrugNameNew[grep("Mixtard", inputFrame$DrugName, ignore.case = TRUE)] <- "BDmixInsulin_"
  inputFrame$DrugNameNew[grep("Humulin M4", inputFrame$DrugName, ignore.case = TRUE)] <- "BDmixInsulin_"
  
  inputFrame$DrugNameNew[grep("Humulin M3", inputFrame$DrugName, ignore.case = TRUE)] <- "BDmixInsulin_"
  inputFrame$DrugNameNew[grep("Humulin M2", inputFrame$DrugName, ignore.case = TRUE)] <- "BDmixInsulin_"
  inputFrame$DrugNameNew[grep("Humulin M1", inputFrame$DrugName, ignore.case = TRUE)] <- "BDmixInsulin_"
  
  inputFrame$DrugNameNew[grep("HUMULIN M2", inputFrame$DrugName, ignore.case = TRUE)] <- "BDmixInsulin_"
  inputFrame$DrugNameNew[grep("HUMULIN M1", inputFrame$DrugName, ignore.case = TRUE)] <- "BDmixInsulin_"
  
  inputFrame$DrugNameNew[grep("Humalog Mix", inputFrame$DrugName, ignore.case = TRUE)] <- "BDmixInsulin_"
  
  # basal insulins
  inputFrame$DrugNameNew[grep("Insulin Glargine", inputFrame$DrugName, ignore.case = TRUE)] <- "BasalInsulin_"
  inputFrame$DrugNameNew[grep("Lantus", inputFrame$DrugName, ignore.case = TRUE)] <- "BasalInsulin_"
  
  inputFrame$DrugNameNew[grep("degludec", inputFrame$DrugName, ignore.case = TRUE)] <- "BasalInsulin_"
  inputFrame$DrugNameNew[grep("Degludec", inputFrame$DrugName, ignore.case = TRUE)] <- "BasalInsulin_"
  inputFrame$DrugNameNew[grep("Tresiba", inputFrame$DrugName, ignore.case = TRUE)] <- "BasalInsulin_"
  inputFrame$DrugNameNew[grep("Insulin Detemir", inputFrame$DrugName, ignore.case = TRUE)] <- "BasalInsulin_"
  inputFrame$DrugNameNew[grep("Insulatard", inputFrame$DrugName, ignore.case = TRUE)] <- "BasalInsulin_"
  inputFrame$DrugNameNew[grep("ULTRATARD", inputFrame$DrugName, ignore.case = TRUE)] <- "BasalInsulin_"  
  inputFrame$DrugNameNew[grep("MONOTARD", inputFrame$DrugName, ignore.case = TRUE)] <- "BasalInsulin_"
  inputFrame$DrugNameNew[grep("Humulin I", inputFrame$DrugName, ignore.case = TRUE)] <- "BasalInsulin_"
  inputFrame$DrugNameNew[grep("Toujeo", inputFrame$DrugName, ignore.case = TRUE)] <- "BasalInsulin_"
  
  # prandial insulins
  inputFrame$DrugNameNew[grep("Actrapid", inputFrame$DrugName, ignore.case = TRUE)] <- "PrandialInsulin_"
  inputFrame$DrugNameNew[grep("Humalog", inputFrame$DrugName, ignore.case = TRUE)] <- "PrandialInsulin_"
  inputFrame$DrugNameNew[grep("Insulin Lispro", inputFrame$DrugName, ignore.case = TRUE)] <- "PrandialInsulin_"
  
  inputFrame$DrugNameNew[grep("Novorapid", inputFrame$DrugName, ignore.case = TRUE)] <- "PrandialInsulin_"
  inputFrame$DrugNameNew[grep("aspart", inputFrame$DrugName, ignore.case = TRUE)] <- "PrandialInsulin_"
  inputFrame$DrugNameNew[grep("fiasp", inputFrame$DrugName, ignore.case = TRUE)] <- "PrandialInsulin_"
  
  inputFrame$DrugNameNew[grep("Apidra", inputFrame$DrugName, ignore.case = TRUE)] <- "PrandialInsulin_"
  inputFrame$DrugNameNew[grep("Humulin S", inputFrame$DrugName, ignore.case = TRUE)] <- "PrandialInsulin_"
  
  # animal insulins
  inputFrame$DrugNameNew[grep("Bovine", inputFrame$DrugName, ignore.case = TRUE)] <- "animalInsulin"
  inputFrame$DrugNameNew[grep("BOVINE", inputFrame$DrugName, ignore.case = TRUE)] <- "animalInsulin"
  inputFrame$DrugNameNew[grep("Porcine", inputFrame$DrugName, ignore.case = TRUE)] <- "animalInsulin"
  
  inputFrame$DrugNameNew[grep("lancet", inputFrame$DrugName, ignore.case = TRUE)] <- "lancet"
  inputFrame$DrugNameNew[grep("Lancet", inputFrame$DrugName, ignore.case = TRUE)] <- "lancet"
  inputFrame$DrugNameNew[grep("LANCET", inputFrame$DrugName, ignore.case = TRUE)] <- "lancet"
  inputFrame$DrugNameNew[grep("Accu", inputFrame$DrugName, ignore.case = TRUE)] <- "lancet"
  inputFrame$DrugNameNew[grep("ACCU", inputFrame$DrugName, ignore.case = TRUE)] <- "lancet"
  inputFrame$DrugNameNew[grep("accu", inputFrame$DrugName, ignore.case = TRUE)] <- "lancet"
  
  inputFrame$DrugNameNew[grep("pen", inputFrame$DrugName, ignore.case = TRUE)] <- "pen"
  inputFrame$DrugNameNew[grep("Insulin Syringe", inputFrame$DrugName, ignore.case = TRUE)] <- "pen"
  
  inputFrame$DrugNameNew[grep("strip", inputFrame$DrugName, ignore.case = TRUE)] <- "TestStrips"
  
  inputFrame$DrugNameNew[grep("Bd-Microfine", inputFrame$DrugName, ignore.case = TRUE)] <- "Needle"
  inputFrame$DrugNameNew[grep("BD Micro-Fine", inputFrame$DrugName, ignore.case = TRUE)] <- "Needle"
  inputFrame$DrugNameNew[grep("Needle", inputFrame$DrugName, ignore.case = TRUE)] <- "Needle"
  inputFrame$DrugNameNew[grep("need", inputFrame$DrugName, ignore.case = TRUE)] <- "Needle"
  inputFrame$DrugNameNew[grep("ndle", inputFrame$DrugName, ignore.case = TRUE)] <- "Needle"
  
  inputFrame$DrugNameNew[grep("Bd Safe-Clip", inputFrame$DrugName, ignore.case = TRUE)] <- "ClippingDevice"
  
  x <- as.data.frame(table(inputFrame$DrugNameNew))
  x = x[order(x$Freq), ]
  
  outputFrame <- inputFrame
  
  outputFrame$DrugName.original <- NULL
  outputFrame$DrugName <- outputFrame$DrugNameNew
  outputFrame$DrugNameNew <- NULL
  
  return(outputFrame)
}

# read in config file - saved out from:
config = read.csv(paste("~/projects/TSpaper3_output/configFile.csv", sep = ""), header = TRUE, row.names = NULL)
n_bins_train = config$trainWindow_years * 12
n_bins_test = config$testWindow_years * 12
n_bins_horizon = config$horizon_years * 12

# generate node and link files
drugDataSet <- read.csv("~/projects/sourceData/Export_all_diabetes_drugs.txt",header=TRUE,row.names=NULL)
topUpDrugData <-paste("~/projects/sourceData/diabetesDrugs_nov16-nov17.txt",sep="")
topUpDrugDataSet <- read.csv(topUpDrugData)

concatDrugSet <- rbind(drugDataSet, topUpDrugDataSet)
concatDrugSetDT = data.table(concatDrugSet)

# remove duplicates in a memory efficient way
IDlist = unique(concatDrugSet$LinkId)
print(length(IDlist))
  for (ii in seq(1, length(IDlist), 1)) {
    if(ii %% 1000 == 0) {print(ii)}
    sub = concatDrugSetDT[LinkId == IDlist[ii]]
    dup = duplicated(sub)
    if(ii == 1) {exportDup = dup}
    if(ii>1) {exportDup = c(exportDup, dup)}
  }
concatDrugSetDT_ = concatDrugSetDT[exportDup == FALSE]

# save out drug data without duplicates
write.table(concatDrugSetDT_, file = paste("~/projects/sourceData/concatDrugSetDT.csv", sep=""), sep=",", row.names = FALSE)

# concatDrugSet <- read.csv(file = paste("~/projects/sourceData/concatDrugSetDT.csv", sep=""))

concatDrugSet <- concatDrugSetDT_

drugDataSet <- concatDrugSet
drugDataSet_original <- drugDataSet
# drugDataSet = drugDataSet[1:100000, ]

# load and process mortality data

diagnosisSet<-paste("~/projects/sourceData/deathData.csv",sep="")
diagnosisSetDF<-read.csv(diagnosisSet); diagnosisSetDT<-data.table(diagnosisSetDF)

deathData = diagnosisSetDF
# deathData <- read.csv("~/R/GlCoSy/SDsource/diagnosisDateDeathDate.txt", sep=",")
deathData$unix_deathDate <- returnUnixDateTime(deathData$DeathDate)
deathData$unix_deathDate[is.na(deathData$unix_deathDate)] <- 0
deathData$isDead <- ifelse(deathData$unix_deathDate > 0, 1, 0)
deathData$unix_diagnosisDate <- returnUnixDateTime(deathData$DateOfDiagnosis)

# drugDataSet <- read.csv("~/R/GlCoSy/SDsource/test_drug_out_second100kIDs_allTime.txt",header=TRUE,row.names=NULL)
drugDataSet$BNFCode <- as.character(drugDataSet$BNFCode)
drugDataSet$DrugName <- as.character(drugDataSet$DrugName)
drugDataSet$LinkId <- as.numeric(levels(drugDataSet$LinkId))[drugDataSet$LinkId]
# drugDataSet$LinkId <- as.numeric(drugDataSet$LinkId)
# drugDataSet <- read.csv("./test_drug_out_second100kIDs_allTime.txt",header=TRUE,row.names=NULL)

# restrict to diabetes drugs
interestSet <- subset(drugDataSet, substr(drugDataSet$BNFCode,1,3) == "6.1" | substr(drugDataSet$BNFCode,1,4) == "0601")

# restrict to prescriptions within dates
interestSetDT <- data.table(interestSet)
interestSetDT$prescription_dateplustime1 <- returnUnixDateTime(interestSetDT$PrescriptionDateTime)
interestSetDT <- interestSetDT[prescription_dateplustime1 < as.numeric(Sys.time())]

# set runin period of interest
startRuninPeriod <- '2000-01-01'
endRuninPeriod   <- '2019-01-01'
interestSetDT <- interestSetDT[prescription_dateplustime1 > returnUnixDateTime(startRuninPeriod) &
                                 prescription_dateplustime1 < returnUnixDateTime(endRuninPeriod)]

interestSetDF <- data.frame(interestSetDT)

# run simplifying functions
interestSetDF <- findSimilarDrugs(interestSetDF)
interestSetDF <- simplifyDrugs(interestSetDF)

write.table(interestSetDF, file = paste("~/projects/sourceData/drugsPostSimplification1.csv", sep=""), sep=",", row.names = FALSE)
interestSetDF <- read.csv("~/projects/sourceData/drugsPostSimplification1.csv")

# sample random rows from interestSetDF
# interestSetDF <- interestSetDF[sample(nrow(interestSetDF), 10000), ]

## remove unaltered drugs
## if drug name has '_' after it in simplifydrugs() it will be retained.
interestSetDF$retain <- 0
interestSetDF$retain[grep("_", interestSetDF$DrugName, ignore.case = TRUE)] <- 1
interestSetDF <- subset(interestSetDF, retain == 1)
interestSetDF$retain <- NULL

tableOfDrugs = as.data.frame(table(interestSetDF$DrugName))

# generate a top-25 etc list for merging back
# meeds a bit of data cleaning - merging synonymous drugs etc
n = 25
topNdrugs_DrugNames <- as.data.frame(table(interestSetDF$DrugName))
topNdrugs_DrugNames <- topNdrugs_DrugNames[order(topNdrugs_DrugNames$Freq), ]

topNdrugs <- tail(topNdrugs_DrugNames, n)

topNdrugs$Var1 <- gsub(" ", "", topNdrugs$Var1, fixed = TRUE)
topNdrugs$Var1 <- gsub("/", "", topNdrugs$Var1, fixed = TRUE)
topNdrugs$Var1 <- gsub("-", "", topNdrugs$Var1, fixed = TRUE)

# merge top drugs back with interestSet to generate working data frame:
interestSet_topN_merge <- merge(interestSetDF, topNdrugs, by.x="DrugName", by.y="Var1")

###############################
## start drug data manipulation
###############################
# interestSet_topN_merge <- read.csv("~/R/_workingDirectory/dataClean/generatedFileArchive/drugData_tempIntermediate.csv", header = T)

drugsetDT <- data.table(interestSet_topN_merge)
drugsetDT$prescription_dateplustime1 <- returnUnixDateTime(drugsetDT$PrescriptionDateTime)
# drugsetDT_original <-drugsetDT # preserve an original full dataset incase needed

drugsetDT <- transform(drugsetDT,id=as.numeric(factor(LinkId)))

## set up stride starting time points
    # total cycle time
    yearSeconds = (60*60*24*365.25)
    totalCycleTime = returnUnixDateTime(config$cycle_endDate[1]) - returnUnixDateTime(config$cycle_startDate[1])
    cycleTimeYears = totalCycleTime / yearSeconds
    
    # total number of steps possible
    horizon_years = config$horizon_months[1] / 12
    step_n = floor((cycleTimeYears - horizon_years) / config$stride_years[1])
    
    # main stride loop
    # returns a sequence - where the last value represents the end of the 18 month post-training period for finding the followup value
    startPoints = seq(returnUnixDateTime(config$cycle_startDate[1]),
                      (returnUnixDateTime(config$cycle_startDate[1]) + ((config$stride_years[1] * yearSeconds * step_n) + (yearSeconds * horizon_years))),
                      (config$stride_years[1] * yearSeconds))
    
    # calculate last time that a full dataset obtainable within available data
    lastDate =  (returnUnixDateTime(config$cycle_endDate[1])) - ((config$trainWindow_years[1] * yearSeconds) + (horizon_years * yearSeconds))
    
    # ensure that all start points associated with enough subsequent data
    startPoints = startPoints[startPoints < lastDate]

# time bins derived from config file
# for now. add 1 year of drug info - strictly should be startOfTargetWindow_months to prevent getting information from the future
# additionalMonths = config$startOfTargetWindow_months[1]
additionalMonths = 12
additionaYears = additionalMonths / 12
nBins = (config$trainWindow_years[1] * 12) + additionalMonths

# new approach
drugWordGen <- function(inputDrugs) {
  uniqueDrugs <- unique(inputDrugs)
  uniqueDrugs <- sort(uniqueDrugs)
  return(paste(uniqueDrugs, collapse = ''))
}

# loop through strides. form drug words per bin and write out 1 file per stride
for (stride in seq(1, length(startPoints), 1)) {
  print(stride)
  # stride = 1
  stride_sub <- drugsetDT[prescription_dateplustime1 >= startPoints[stride] &
                            prescription_dateplustime1 < (startPoints[stride] + ((config$trainWindow_years[1] + additionaYears) * yearSeconds))]

  # assign each drug to appropriate bin
  binVector <- cut(stride_sub$prescription_dateplustime1, breaks = nBins, labels = c(1:nBins))
  stride_sub$bin_number <- binVector
  stride_sub[, c("drugWord") := drugWordGen(DrugName) , by=.(id, bin_number)]
  stride_sub[, c("n_") := seq(1, .N, 1) , by=.(id, bin_number)]
  
  singleRow_perID_perBin <- stride_sub[n_ == 1]
  
  nil_frame <- as.data.frame(matrix('nil', nrow = uniqueN(singleRow_perID_perBin$LinkId), ncol = (nBins)))
  
  nil_frame <- data.frame(lapply(nil_frame, as.character), stringsAsFactors=FALSE)
  
  uniqueIDvector <- unique(singleRow_perID_perBin$LinkId)
  print(length(uniqueIDvector))
  for (j in seq(1, length(uniqueIDvector), 1)) {
    if (j%%1000 == 0) {print(j)}
    id_sub <- singleRow_perID_perBin[LinkId == uniqueIDvector[j]]
    nil_frame[j, as.numeric(id_sub$bin_number)] <- id_sub$drugWord
  }
  nil_frame <- cbind(uniqueIDvector, nil_frame)
  
  write.table(nil_frame, file = paste("~/projects/TSpaper3_output/drugWords_stride_", stride, ".csv" , sep=""), sep=",", row.names = FALSE)
  # nil_frame <- read.csv(file = paste("~/projects/TSpaper3_output/drugWords_stride_", stride, ".csv" , sep=""))
  
  }

# load merged numerical files
# pad to correct size with R hand zeros
# merge with drugWord frames to generate same size frames
# convert drugWord frames to numeric representations
# save out a 

# set up loop through file names
interest_vector <- c('HbA1c', 'SBP', 'DBP', 'BMI')
filenameStartStrings <- c("functionTest", "parameterMask_functionTest", "target_gradient_functionTest", "target_oneYvalueFromGradient_functionTest")

for (l in c(1:length(filenameStartStrings))) {
  patternPaste <- filenameStartStrings[l]
  
  files <- list.files(path = "~/projects/TSpaper3_output/", pattern = paste("^", patternPaste, sep = ""), ignore.case = TRUE)
  numberOfStrides <- length(files) / length(interest_vector)
  
  # load each stride in turn
  for (s in c(1:numberOfStrides)) {
    
    # load the drugWord set for each stride, set as the master ID list to merge other params against
    drugSet <- read.csv(paste("~/projects/TSpaper3_output/drugWords_stride_", s, ".csv", sep = ""))
    drugIDs <- drugSet$uniqueIDvector
    runningMergedID <- drugIDs
    
    # for stride s, load each parameter and merge LinkIDs
    for (iv in c(1:length(interest_vector))) {
      
      mergeTestSet <- read.csv(paste("~/projects/TSpaper3_output/", filenameStartStrings[l], "_", s, "_", interest_vector[iv], ".csv", sep = ""))
      mergeID_vector <- mergeTestSet$LinkId
      
      # if (iv == 1) {runningMergedID = mergeID_vector}
      if (iv > 0)  {
        intermediate_merge = mergeID_vector[match(runningMergedID, mergeID_vector)]
        intermediate_merge[is.na(intermediate_merge)] <- 0
        runningMergedID = intermediate_merge[intermediate_merge > 0]
      }

    }
    
    # save out common files for stride s
    for (cf in c(1:length(interest_vector))) {
      print(cf)
      commonSet <- read.csv(paste("~/projects/TSpaper3_output/", filenameStartStrings[l], "_", s, "_", interest_vector[cf], ".csv", sep = ""))
      commonSet <- merge(commonSet, as.data.frame(runningMergedID), by.x = "LinkId", by.y = "runningMergedID")
      commonSet <- commonSet[order(commonSet$LinkId), ]
      write.table(commonSet, file = paste("~/projects/TSpaper3_output/withDrugs_common_", patternPaste, "_stride_", s, "_", interest_vector[cf], ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = TRUE)
    }
    
    # save out the equivalent drugWord file (saves this out multiple times, but that doesn't matter)
    common_drug_Set <- merge(drugSet, as.data.frame(runningMergedID), by.x = "uniqueIDvector", by.y = "runningMergedID")
    colnames(common_drug_Set) <- c('LinkId', c(1:(ncol(common_drug_Set) - 1)))
    common_drug_Set <- common_drug_Set[order(common_drug_Set$LinkId), ]
    write.table(common_drug_Set, file = paste("~/projects/TSpaper3_output/withDrugs_common_", "_stride_", s, "_drugWords.csv", sep = ""), sep = ",", row.names = FALSE, col.names = TRUE)
    
  }
  
}

## reload and concatenate components into single files
concatFunction <- function(fileNameStem, max_n, interest_metric) {
  
  concat_file <- data.frame()
  # fileNameStem = "functionTest_"; max_n = length(startPoints) 
  for (i in seq(1, max_n, 1)) {
    loopSub <- read.csv(paste('~/projects/TSpaper3_output/', fileNameStem, i, '_', interest_metric,'.csv', sep = ''))
    if (i == 1) {concat_file = loopSub}
    if (i>1) {concat_file = rbind(concat_file, loopSub)}
  }
  
  concat_file <- concat_file[order(concat_file$LinkId), ]
  return(concat_file)
  
}

## concatLoop
for (c in seq(1, length(filenameStartStrings), 1)) {
  
  for (d in seq(1, length(interest_vector), 1)) {

    concatFile <- concatFunction(paste('withDrugs_common_', filenameStartStrings[c], "_stride_", sep = ''), numberOfStrides, interest_vector[d])
    write.table(concatFile, file = paste("~/projects/TSpaper3_output/withDrugs_MergeConcat_", filenameStartStrings[c], "_", interest_vector[d], ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = TRUE)

  }
  
}

# as a separate step - concat drug file
drugConcatFile <- concatFunction(paste('withDrugs_common__stride_', sep = ''), numberOfStrides, 'drugWords')
write.table(drugConcatFile, file = paste("~/projects/TSpaper3_output/withDrugs_MergeConcat_drugWords.csv", sep = ""), sep = ",", row.names = FALSE, col.names = TRUE)

# convert drug file to numerical version

drugWordFrame <- drugConcatFile[, 2:ncol(drugConcatFile)] # establish a copy of drugWordFrame

frameColN <- ncol(drugWordFrame)
vectorWords <- as.vector(as.matrix(drugWordFrame[, 1 : (frameColN - 1)]))
vectorNumbers <- as.numeric(as.factor(vectorWords))
lookup <- data.frame(vectorWords, vectorNumbers)
lookup <- unique(lookup)

numerical_drugWordFrame <- as.data.frame(matrix(0, nrow = nrow(drugWordFrame), ncol = (ncol(drugWordFrame) - 1)))
for (jj in seq(1, (ncol(numerical_drugWordFrame) - 1), 1)) {
  index <- match(lvcfFrame[,jj], lookup$vectorWords)
  numerical_lvcfFrame[,jj] <- lookup$vectorNumbers[index]
  
}




## end










nBins = nBins - 1

# set time bins
binLengthSeconds = (endDate_unix - startDate_unix) / nBins
binLengthMonths = binLengthSeconds / (60*60*24*(365.25/12))
sequence <- seq(startDate_unix, endDate_unix, binLengthSeconds)

# generate bag of drugs frame
drugWordFrame <- as.data.frame(matrix(nrow = length(unique(drugsetDT$LinkId)), ncol = (length(sequence)) ))
colnames(drugWordFrame) <- c(1:(length(sequence)))
drugWordFrame$LinkId <- 0
    
    # function to generate drugwords for each time interval
    returnIntervals <- function(LinkId, DrugName, prescription_dateplustime1, sequence, id) {
      
      # DrugName <- subset(drugsetDT, id == 2)$DrugName; prescription_dateplustime1 <- subset(drugsetDT, id == 2)$prescription_dateplustime1; id = 2; LinkId <- subset(drugsetDT, id == 2)$LinkId
      
          inputSet <- data.table(DrugName, prescription_dateplustime1)
      
          ## add nil values to fill time slots without any drugs
          nilFrame <- as.data.frame(matrix(nrow = length(sequence), ncol = ncol(inputSet)))
          colnames(nilFrame) <- colnames(inputSet)
          
          nilFrame$DrugName <- 'nil'
          nilFrame$prescription_dateplustime1 <- sequence
          
          outputSet <- rbind(nilFrame, inputSet)
          
      ## generate drug words
          
      interimSet <- outputSet
      
      interimSet <- interimSet[, interv := cut(prescription_dateplustime1, sequence)][, .(drugs = (unique(DrugName))), by = interv]
      interimSet[, drugWord := paste(drugs, collapse = ''), by = interv]
      
      interimSet <- interimSet[order(interimSet$interv), ]
      interimSet[, drugSequenceNumber := seq(1, .N, 1), by = interv]
      
      reportSet <- interimSet[drugSequenceNumber == 1]
      reportSet$drugWord <- ifelse(substr(reportSet$drugWord,1,3) == 'nil' & nchar(reportSet$drugWord) == 3, reportSet$drugWord, substr(reportSet$drugWord,4,nchar(reportSet$drugWord)))
      
      reportSet <- reportSet[1:nrow(reportSet), ]
      reportSet$intervalNumber <- c(1:nrow(reportSet))
      
#      print(reportSet$drugWord)
      
      return(c(reportSet$drugWord, LinkId[1]))
      

    }
    
    print(max(drugsetDT$id))
    for (j in seq(1, max(drugsetDT$id), )) {
    # for (j in seq(1, 100, )) {
      
      if(j%%100 == 0) {print(j)}
      
      injectionSet <- drugsetDT[id == j]
      drugWordFrame[j, ] <- returnIntervals(injectionSet$LinkId, injectionSet$DrugName, injectionSet$prescription_dateplustime1, sequence, j)
    }
  
    # write.table(drugWordFrame, file = "~/R/_workingDirectory/dataClean/drugWordFrame_withID_2011_2017_72bins.csv", sep=",")
    # drugWordFrame <- read.csv("~/R/GlCoSy/MLsource/drugWordFrame.csv", stringsAsFactors = F, row.names = NULL); drugWordFrame$row.names <- NULL
    
    # the bin frequency is 0.5 that in splineInterpolation.R (72 bins vs 144)
    # need to replicate the dataframe, and then interleave to give 2 bins for every original time bin (bringing total n back to 144)
    
    drugWordFrame_postLoop <- drugWordFrame # establish a copy of drugWordFrame
    
    frameColN <- ncol(drugWordFrame_postLoop)
    vectorWords <- as.vector(as.matrix(drugWordFrame_postLoop[, 1 : (frameColN - 1)]))
    vectorNumbers <- as.numeric(as.factor(vectorWords))
    lookup <- data.frame(vectorWords, vectorNumbers)
    lookup <- unique(lookup)
    # lookup <- data.table(lookup)
    
    # interpolate LVCF for drugs
    lvcfFrame = drugWordFrame_postLoop
    
    # vectorised lookup table use
    numerical_lvcfFrame <- as.data.frame(matrix(0, nrow = nrow(lvcfFrame), ncol = (ncol(lvcfFrame) - 1)))
    for (jj in seq(1, (ncol(lvcfFrame) - 1), 1)) {
      index <- match(lvcfFrame[,jj], lookup$vectorWords)
      numerical_lvcfFrame[,jj] <- lookup$vectorNumbers[index]
      
    }
      
    # number to replace the nils
    nilNumber = vectorNumbers[vectorWords == "nil"][1]
    
    # replace all the nil numbers with NA
    numerical_lvcfFrame[numerical_lvcfFrame == nilNumber] = NA
    
    # transpose numerical_lvcfFrame
    numerical_lvcfFrame = t(numerical_lvcfFrame)
    numerical_lvcfFrame = na.locf(numerical_lvcfFrame)
    numerical_lvcfFrame = t(numerical_lvcfFrame)
    
    # turn NAs to zeros to give option for padding in keras
    numerical_lvcfFrame[is.na(numerical_lvcfFrame)] <- 0
    numerical_lvcfFrame = as.data.frame(numerical_lvcfFrame)
    
    # add IDs back
    numerical_lvcfFrame = cbind(numerical_lvcfFrame, lvcfFrame[, ncol(lvcfFrame)])
    colnames(numerical_lvcfFrame)[ncol(numerical_lvcfFrame)] = c("LinkId")
    
    # generate a text version of the numerical frame - with lvcf interpolation
    word_lvcfFrame <- as.data.frame(matrix(0, nrow = nrow(numerical_lvcfFrame), ncol = (ncol(numerical_lvcfFrame) - 1)))
    for (ii in seq(1, (ncol(numerical_lvcfFrame) - 1), 1)) {
      index <- match(numerical_lvcfFrame[,ii], lookup$vectorNumbers)
      word_lvcfFrame[,ii] <- lookup$vectorWords[index]
      
    }
    word_lvcfFrame[is.na(word_lvcfFrame)] <- 'nil' # bring NAs back to 'nil'
    
    ## expand frames to give correct number of cols
    
    interleave_nTimes <- function(inputFrame, finalN) { # finalN = the numerical TS bin number eg 144, 288 etc
      # finalN = 144; inputFrame = numerical_lvcfFrame
      loopN = finalN / (nBins + 1)
      
      inputFrameMinusID <- inputFrame[, 1 : (ncol(inputFrame) - 1)] # remove ID
      colnames(inputFrameMinusID) = as.character(c(1 : ncol(inputFrameMinusID)))
      
      if(loopN == 2) {
        drugWordFrame_1 <- inputFrameMinusID
        drugWordFrame_2 <- inputFrameMinusID
        
        doubledFrame = cbind(drugWordFrame_1, drugWordFrame_2)
        indx <- rbind(colnames(drugWordFrame_1),colnames(drugWordFrame_2))
        
        interleavedFrame <- doubledFrame[, indx]
        colnames(interleavedFrame) <- c(1:ncol(interleavedFrame))
      }
      
      if(loopN == 4) {
        drugWordFrame_1 <- inputFrameMinusID
        drugWordFrame_2 <- inputFrameMinusID
        drugWordFrame_3 <- inputFrameMinusID
        drugWordFrame_4 <- inputFrameMinusID
        
        doubledFrame = cbind(drugWordFrame_1, drugWordFrame_2, drugWordFrame_4, drugWordFrame_4)
        indx <- rbind(colnames(drugWordFrame_1),colnames(drugWordFrame_2), colnames(drugWordFrame_3), colnames(drugWordFrame_4))
        
        interleavedFrame <- doubledFrame[, indx]
        colnames(interleavedFrame) <- c(1:ncol(interleavedFrame))
      }
      
      return(interleavedFrame)
    }
    
    interleavedFrame = interleave_nTimes(numerical_lvcfFrame, n_finalDrugBins)

    # reattach Link ID
    interleavedFrame_withID <- cbind(interleavedFrame, lvcfFrame[, (ncol(lvcfFrame))])
    colnames(interleavedFrame_withID)[ncol(interleavedFrame_withID)] <- c("LinkId")
    
    # here do analysis to select rows (IDs) for later analysis
    # replace existing drugWordFrame with new expanded version
    drugWordFrame <- interleavedFrame_withID
    
    # mortality outcome at 2017-01-01
    drugWordFrame_mortality <- merge(drugWordFrame, deathData, by.x = "LinkId", by.y= "LinkId")
    # remove those dead before end of FU
    # analysis frame = those who are not dead, or those who have died after the end of the runin period. ie all individuals in analysis alive at the end of the runin period
    drugWordFrame_mortality <- subset(drugWordFrame_mortality, isDead == 0 | (isDead == 1 & unix_deathDate > returnUnixDateTime(endRuninPeriod)) )
    # remove those diagnosed after the end of the runin period
    drugWordFrame_mortality <- subset(drugWordFrame_mortality, unix_diagnosisDate <= returnUnixDateTime(endRuninPeriod) )
    
    # set up drug sentences for analysis
    
### 010818 as far as here
    
    
    drugWordFrame_forAnalysis <- drugWordFrame_mortality
      
    numericalDrugsFrame <- drugWordFrame_forAnalysis[, 2:(1+(ncol(drugWordFrame_forAnalysis)-8)) ]
    
    ''' 
    drugSentenceFrame <- as.data.frame(matrix(nrow = nrow(drugWordFrame_forAnalysis), ncol = 1))
    colnames(drugSentenceFrame) <- c("drugSentence")
    
    vectorWords <- as.vector(as.matrix(drugWordFrame_drugNames))
    vectorNumbers <- as.numeric(as.factor(vectorWords))
    lookup <- data.frame(vectorWords, vectorNumbers)
    lookup <- unique(lookup)
    lookup <- data.table(lookup)
    '''
    
    # write out lookup table
    write.table(lookup, file = paste("~/R/_workingDirectory/dataClean/lookupTable_", startRuninPeriod, "_to_", endRuninPeriod, "_simplifiedDrugs_", round(binLengthMonths, 1),"mBins.csv", sep=""), sep=",", row.names = FALSE)
    
    '''
    # vectorised lookup table use
    numericalDrugsFrame <- as.data.frame(matrix(0, nrow = nrow(drugWordFrame_drugNames), ncol = ncol(drugWordFrame_drugNames)))
    
    for (jj in seq(1, ncol(drugWordFrame_drugNames), 1)) {
      
      index <- match(drugWordFrame_drugNames[,jj], lookup$vectorWords)
      numericalDrugsFrame[,jj] <- lookup$vectorNumbers[index]
      
    }
    
    y_vector <- drugWordFrame_forAnalysis$isDead
    '''
    

    # write out sequence for analysis
    #write.table(numericalDrugsFrame, file = paste("~/R/_workingDirectory/dataClean/numericalDrugsFrame_", startRuninPeriod, "_to_", endRuninPeriod, "_simplifiedDrugs_", binLengthMonths,"mBins.csv", sep=""), sep=",", row.names = FALSE)
    
    # write out sequence for analysis
    #write.table(drugWordFrame_mortality, file = paste("~/R/_workingDirectory/dataClean/drugWordFrame_mortality_", startRuninPeriod, "_to_", endRuninPeriod, "_simplifiedDrugs_", binLengthMonths,"mBins.csv", sep=""), sep=",", row.names = FALSE)
    
    numericalDrugsFrame_withID <- data.frame(numericalDrugsFrame, drugWordFrame_forAnalysis$LinkId)
    # write out sequence for analysis
    #write.table(numericalDrugsFrame_withID, file = paste("~/R/_workingDirectory/dataClean/numericalDrugsFrame_withID_", startRuninPeriod, "_to_", endRuninPeriod, "_simplifiedDrugs_", binLengthMonths,"mBins.csv", sep=""), sep=",", row.names = FALSE)
    
    
###
# generate 1/0 drug export files:
# NB dummy variable trap potential problem ok as nil not included
drugList = c("BasalInsulin", "BDmixInsulin", "DPP4", "GLP1", "Metformin", "PrandialInsulin", "SGLT2", "SU", "TZD")
word_lvcfFrame_ac <- data.frame(lapply(word_lvcfFrame, as.character), stringsAsFactors=FALSE)

for (t in seq(1, length(drugList), 1)) {
  
  target = drugList[t]
  print(target)

zerosFrame <- as.data.frame(matrix(0, nrow = nrow(word_lvcfFrame_ac), ncol = ncol(word_lvcfFrame_ac)))
for (j in seq(1, nrow(word_lvcfFrame_ac), 1)) {
  if (j %% 1000 == 0) {print(j)}
  logical = grepl(target, word_lvcfFrame_ac[j, ])
  zerosFrame[j, ] <- as.numeric(logical)
}

# reattach ID
zerosFrame <- cbind(zerosFrame, lvcfFrame[, (ncol(lvcfFrame))])
colnames(zerosFrame)[ncol(zerosFrame)] <- c("LinkId")

interleavedZerosFrame = interleave_nTimes(zerosFrame, n_finalDrugBins)

interleavedZerosFrame <- cbind(interleavedZerosFrame, lvcfFrame[, (ncol(lvcfFrame))])
colnames(interleavedZerosFrame)[ncol(interleavedZerosFrame)] <- c("LinkId")

write.table(interleavedZerosFrame, file = paste("./RFS_intermediateFiles_v2/numericalCateorical_drug_", t, "_",target, ".csv", sep=""), sep=",", row.names = FALSE)

}
