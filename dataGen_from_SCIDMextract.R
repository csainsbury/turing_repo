library(data.table)
library(zoo)
library(dplyr)
library(imputeTS)

returnUnixDateTime<-function(date) {
  returnVal<-as.numeric(as.POSIXct(date, format="%Y-%m-%d", tz="GMT"))
  return(returnVal)
}

lvcfInterpol_withMask <- function(inputFrame, unix_startDate, trainWindow_years, nBins, minTestsN, filename, horizon_months, startOfTargetWindow_months, endOfTargetWindow_months, interest_metric) {
  # totalData, "2000-01-01", "2005-01-01", n_bins_train, (trainWindow_years - 1), 'test')
  
  # inputFrame = timeChunk; unix_startDate = startPoints[st]; trainWindow_years = 6; nBins = n_bins_train; filename = "test_test"; minTestsN = (trainWindow_years - 1); horizon_months = 18; startOfTargetWindow_months = 6; endOfTargetWindow_months = 18
  # inputFrame = inputFrame[1:20000, ]
  
  nBins = (nBins - 1)
  
  startDate_unix <- unix_startDate
  trainWindow_seconds = trainWindow_years * (60*60*24*365.25)
  endDate_unix = startDate_unix + trainWindow_seconds
  startOfTargetWindow_seconds <- startOfTargetWindow_months * (60*60*24*(365.25/12))
  endOfTargetWindow_seconds <- endOfTargetWindow_months * (60*60*24*(365.25/12))
  
  # drop all measures outwith window
  # inputFrame <- inputFrame[dateplustime1 <= endDate_unix]
  inputFrame$unix_diagnosisDate[is.na(inputFrame$unix_diagnosisDate)] <- 0 # make sure that no missing diagnosis dates
  
  # ensure that all alive horizon_months (eg 18) months after enddate
  inputFrame[, c("alivePost") := ifelse(unix_deathDate == 0 | unix_deathDate > (endDate_unix + (horizon_months * 60*60*24 * (365.25/12))), 1, 0) , by=.(LinkId)]
  inputFrame <- inputFrame[alivePost == 1]
  
  # ensure that diagnosed before startdate
  inputFrame[, c("diagnosedPre") := ifelse(unix_diagnosisDate < startDate_unix, 1, 0) , by=.(LinkId)]
  inputFrame <- inputFrame[diagnosedPre == 1]
  
  # require that all IDs have >1 measure between the start date and start date + 1 year
  inputFrame[, c("earlyMeasure") := ifelse(min(dateplustime1) < (startDate_unix + (60*60*24*365.25)), 1, 0) , by=.(LinkId)]
  inputFrame <- inputFrame[earlyMeasure == 1]
  
  # require that all IDs have >1 measure after end date - 1 year
  inputFrame[, c("lateMeasure") := ifelse(max(dateplustime1) > (endDate_unix - (60*60*24*365.25)), 1, 0) , by=.(LinkId)]
  inputFrame <- inputFrame[lateMeasure == 1]
  
  # require that all IDs have >1 measure after end dateplus startOfTargetWindow_seconds
  inputFrame[, c("followUpMeasure") := ifelse((max(dateplustime1) > (endDate_unix + startOfTargetWindow_seconds)) & (max(dateplustime1) < (endDate_unix + endOfTargetWindow_seconds)), 1, 0) , by=.(LinkId)]
  inputFrame <- inputFrame[followUpMeasure == 1]
  
  # count individual measures
  inputFrame <- inputFrame[order(inputFrame$LinkId, inputFrame$dateplustime1), ]
  inputFrame[, c("testN") := seq(1, .N, 1) , by=.(LinkId)]
  
  # generate separate frame for producing training targets
  targetFrame <- inputFrame[dateplustime1 > endDate_unix]
  targetFrame <- targetFrame[order(targetFrame$LinkId), ]
  inputFrame <- inputFrame[dateplustime1 <= endDate_unix]
  inputFrame <- inputFrame[order(inputFrame$LinkId), ]
  
  # generate CV for all (note not managing time bins here)
  inputFrame[, c("CV") := (sd(numericValue) / mean(numericValue)) , by=.(LinkId)]
  inputFrame[, c("median") := quantile(numericValue)[3], by =.(LinkId)]
  inputFrame[, c("mean") := mean(numericValue), by =.(LinkId)]
  inputFrame[, c("totalN_measures") := .N, by =.(LinkId)]
  
  # ensure that more than n measures per ID
  inputFrame[, c("moreThanN_flag") := ifelse(max(testN) >= minTestsN, 1, 0) , by=.(LinkId)]
  inputFrame <- inputFrame[moreThanN_flag == 1]
  
  # summary output
  summaryOutputData <- inputFrame[testN == 1]
  
  write.table(summaryOutputData, file = paste("~/projects/TSpaper3_output/summaryData_", filename, "_", interest_metric, ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = TRUE)
  
  
  #setup the outputframe
  IDvec <- unique(inputFrame$LinkId)
  binLengthSeconds = (endDate_unix - startDate_unix) / nBins
  
  sequence = seq(startDate_unix, endDate_unix, binLengthSeconds)
  
  generateOutputFrame <- as.data.frame(matrix(0, nrow = length(IDvec), ncol = length(sequence)))
  generateMaskingFrame <- as.data.frame(matrix(0, nrow = length(IDvec), ncol = length(sequence)))
  
  print(length(IDvec))
  for (jj in seq(1, length(IDvec), 1)) {
    
    if(jj%%1000 == 0) {print(jj)}
    
    interestSub <- inputFrame[LinkId == IDvec[jj]]
    medianValue <- quantile(interestSub$numericValue)[3]
    
    dataBreaks <- split(interestSub$numericValue, cut(interestSub$dateplustime1, breaks = sequence))
    outputVector <- c(rep(0, length(sequence)))
    
    # returns either 0, or the median of all values in the time bin
    for (kk in seq(1, length(dataBreaks), 1)) {
      values <- dataBreaks[[kk]]
      if (length(values) == 1) { outputVector[kk] = 0}
      if (length(values) > 0) { outputVector[kk] = quantile(values[values > 0])[3]}
    }
    
    maskingSwitch <- ifelse(outputVector[1] == 0, 1, 0) # flag to replace first value of maskingVector with zero - to indicate that the value is interpolated
    outputVector[1] <- ifelse(outputVector[1] == 0, medianValue, outputVector[1])
    
    # generate masking vector to use as additional feature
    maskingVector <- outputVector
    maskingVector <- ifelse(maskingVector > 0, 1, 0)
    maskingVector[1] <- ifelse(maskingSwitch == 1, 0, 1)
    
    # add masking Vector to masking output frame
    generateMaskingFrame[jj, ] <- maskingVector
    
    # replace 0 with NA
    outputVector[outputVector == 0] <- NA
    
    generateOutputFrame[jj, ] <- na.locf(outputVector, option ="locf")
  }
  
  generateOutputFrame$LinkId <- IDvec
  generateMaskingFrame$LinkId <- IDvec
  
  print(dim(generateOutputFrame))
  
  # write out whole output frames
  write.table(generateOutputFrame, file = paste("~/projects/TSpaper3_output/", filename, "_", interest_metric, ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = TRUE)
  write.table(generateMaskingFrame, file = paste("~/projects/TSpaper3_output/parameterMask_", filename, "_", interest_metric, ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = TRUE)
  
  # generate a limited data.table of ID and last value for merging later.
  generateOutputFrame = data.table(generateOutputFrame)
  c = ncol(generateOutputFrame) - 1
  lastValSub <- data.table(generateOutputFrame$LinkId, generateOutputFrame[, ..c])
  colnames(lastValSub) <- c('LinkId', 'lastValue')
  
  ## generate targets
  targetFrame$monthsFromEndDate <- (targetFrame$dateplustime1 - endDate_unix) / (60*60*24*(365.25/12))
  
  targetFrame[, c("closestTo1y") := ifelse( ((monthsFromEndDate - 12)^2) == min((monthsFromEndDate - 12)^2), 1, 0) , by=.(LinkId)]
  targetFrame = targetFrame[closestTo1y == 1]
  
  # ensure that only one target value per ID
  targetFrame[, c("testN_target") := seq(1, .N, 1) , by=.(LinkId)]
  targetFrame = targetFrame[testN_target == 1]
  
  targetFrame = merge(targetFrame, lastValSub)
  targetFrame$gradient <- (targetFrame$numericValue - targetFrame$lastValue) / targetFrame$monthsFromEndDate
  targetFrame$oneYvalueFromGradient <- (targetFrame$gradient * 12) + targetFrame$lastValue
  
  # set up targets for saving out
  targetFrame_gradient = data.table(targetFrame$LinkId, targetFrame$gradient)
  colnames(targetFrame_gradient) <- c("LinkId", "gradient")
  targetFrame_oneYvalueFromGradient = data.table(targetFrame$LinkId, targetFrame$oneYvalueFromGradient)
  colnames(targetFrame_oneYvalueFromGradient) <- c("LinkId", "oneYvalueFromGradient")
  
  print(dim(targetFrame))  
  
  # save out targets
  write.table(targetFrame_gradient, file = paste("~/projects/TSpaper3_output/target_gradient_", filename, "_", interest_metric, ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = TRUE)
  write.table(targetFrame_oneYvalueFromGradient, file = paste("~/projects/TSpaper3_output/target_oneYvalueFromGradient_", filename, "_", interest_metric, ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = TRUE)
  
  # return(generateOutputFrame)
  
}

interest_vector <- c('HbA1c', 'SBP', 'DBP', 'BMI')
for (int in c(1:length(interest_vector))) {
  
interest_metric <- interest_vector[int]

#cleanHbA1cData <- read.csv("~/R/GlCoSy/SD_workingSource/SBPsetDTclean.csv", sep=",", header = TRUE, row.names = NULL)
#hba1cLimited <- data.frame(cleanHbA1cData$LinkId, cleanHbA1cData$sbpNumeric, cleanHbA1cData$dateplustime1); colnames(hba1cLimited) <- c("LinkId", "numericValue", "dateplustime1")

if(interest_metric == 'HbA1c') {
  cleanHbA1cData <- read.csv("~/projects/sourceData/hba1cDTclean.csv", sep=",", header = TRUE, row.names = NULL)
  hba1cLimited <- data.frame(cleanHbA1cData$LinkId, cleanHbA1cData$hba1cNumeric, cleanHbA1cData$dateplustime1); colnames(hba1cLimited) <- c("LinkId", "numericValue", "dateplustime1")
}
if(interest_metric == 'SBP') {
  cleanHbA1cData <- read.csv("~/projects/sourceData/SBPsetDTclean.csv", sep=",", header = TRUE, row.names = NULL)
  hba1cLimited <- data.frame(cleanHbA1cData$LinkId, cleanHbA1cData$sbpNumeric, cleanHbA1cData$dateplustime1); colnames(hba1cLimited) <- c("LinkId", "numericValue", "dateplustime1")
}
if(interest_metric == 'DBP') {
  cleanHbA1cData <- read.csv("~/projects/sourceData/DBPsetDTclean.csv", sep=",", header = TRUE, row.names = NULL)
  hba1cLimited <- data.frame(cleanHbA1cData$LinkId, cleanHbA1cData$dbpNumeric, cleanHbA1cData$dateplustime1); colnames(hba1cLimited) <- c("LinkId", "numericValue", "dateplustime1")
}
if(interest_metric == 'BMI') {
  cleanHbA1cData <- read.csv("~/projects/sourceData/BMISetDTclean.csv", sep=",", header = TRUE, row.names = NULL)
  hba1cLimited <- data.frame(cleanHbA1cData$LinkId, cleanHbA1cData$bmiNumeric, cleanHbA1cData$dateplustime1); colnames(hba1cLimited) <- c("LinkId", "numericValue", "dateplustime1")
}

hba1cLimited <- data.table(hba1cLimited)
hba1cLimited <- hba1cLimited[dateplustime1 < returnUnixDateTime('2018-01-01') & dateplustime1 > returnUnixDateTime('2000-01-01')]
#hba1cLimited = hba1cLimited[1:1000, ]
  
  # add in death and demographic data
  diagnosisSet<-paste("~/projects/sourceData/deathData.csv",sep="")
  diagnosisSetDF<-read.csv(diagnosisSet); diagnosisSetDT<-data.table(diagnosisSetDF)
  
  deathData = diagnosisSetDF
  deathData$unix_deathDate <- returnUnixDateTime(deathData$DeathDate)
  deathData$unix_deathDate[is.na(deathData$unix_deathDate)] <- 0
  deathData$isDead <- ifelse(deathData$unix_deathDate > 0, 1, 0)
  deathData$unix_diagnosisDate <- returnUnixDateTime(deathData$DateOfDiagnosis)

# merged dataset
totalData = merge(deathData, hba1cLimited, by.x = "LinkId", by.y = "LinkId")
totalData = data.table(totalData)

# DM type: T1 for now
filenameType <- "Type 2 Diabetes Mellitus"
totalData = totalData[DiabetesMellitusType_Mapped == filenameType] # DM type

# setup parameters
trainWindow_years <- 6
testWindow_years <- 1
stride_years <- 6
horizon_months = 18
minTestsN = trainWindow_years - 1
startOfTargetWindow_months = 6
endOfTargetWindow_months = horizon_months

# assume standard year length
yearSeconds = (60*60*24*365.25)
horizon_years = horizon_months / 12

divisor = 1 # set to 12 to turn month bins to year bins. 1 to keep month bins. 4 for 3 month bins

n_bins_train = trainWindow_years * 12 / divisor
n_bins_test = testWindow_years * 12 / divisor
n_bins_horizon = horizon_years * 12 / divisor
  
cycle_startDate = "2000-01-01"
cycle_endDate = "2017-03-24"

# generate and save out config file for drug series generation.
configFrame <- as.data.frame(matrix(0, nrow = 1, ncol = 8))
colnames(configFrame) <- c("trainWindow_years", "testWindow_years", "stride_years", "horizon_months", "startOfTargetWindow_months", "endOfTargetWindow_months", "cycle_startDate", "cycle_endDate")
configFrame$trainWindow_years[1] <- trainWindow_years; configFrame$testWindow_years[1] <- testWindow_years
configFrame$stride_years[1] <- stride_years; configFrame$horizon_months[1] <- horizon_months
configFrame$startOfTargetWindow_months[1] <- startOfTargetWindow_months; configFrame$endOfTargetWindow_months[1] <- endOfTargetWindow_months
configFrame$cycle_startDate[1] <- cycle_startDate; configFrame$cycle_endDate[1] <- cycle_endDate
write.table(configFrame, file = paste("~/projects/TSpaper3_output/configFile.csv", sep = ""), sep = ",", row.names = FALSE, col.names = TRUE)


# generate limited col set for working with
hba1cLimited = data.table(totalData$LinkId, totalData$numericValue, totalData$dateplustime1, totalData$unix_deathDate, totalData$unix_diagnosisDate)
colnames(hba1cLimited) <- c("LinkId", "numericValue", "dateplustime1", "unix_deathDate", "unix_diagnosisDate")

## loop through time ##

# downsample hba1cLimited
#hba1cLimited = hba1cLimited[1:10000, ]

# total cycle time
totalCycleTime = returnUnixDateTime(cycle_endDate) - returnUnixDateTime(cycle_startDate)
cycleTimeYears = totalCycleTime / yearSeconds

# total number of steps possible
step_n = floor((cycleTimeYears - horizon_years) / stride_years)

# main stride loop
# returns a sequence - where the last value represents the end of the 18 month post-training period for finding the followup value
startPoints = seq(returnUnixDateTime(cycle_startDate),
                  (returnUnixDateTime(cycle_startDate) + ((stride_years * yearSeconds * step_n) + (yearSeconds * horizon_years))),
                  (stride_years * yearSeconds))

# calculate last time that a full dataset obtainable within available data
lastDate =  (returnUnixDateTime(cycle_endDate)) - ((trainWindow_years * yearSeconds) + (horizon_years * yearSeconds))

# ensure that all start points associated with enough subsequent data
startPoints = startPoints[startPoints < lastDate]
       
# loop through each time step and save out 4 files:
# functiontest, mask, target gradient and target 1y interpolated value
for (st in seq(1, length(startPoints), 1)) {
  print('total steps:'); print(length(startPoints))
  print('time step:'); print(st)
  
  # extract data for time span of interest
  timeChunk <- hba1cLimited[dateplustime1 > startPoints[st] &
                              dateplustime1 <= (startPoints[st] + ((trainWindow_years + horizon_years) * yearSeconds))]

  # run interpolation function - saves out input data, masking data and targets
  lvcfInterpol_withMask(timeChunk, startPoints[st], trainWindow_years, n_bins_train, minTestsN, paste("functionTest_", st, sep = ""), horizon_months, startOfTargetWindow_months, endOfTargetWindow_months, interest_metric)
  
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
      
      return(concat_file)
      
    }

# generate concat files for all input files
main_inputData <- concatFunction('functionTest_', length(startPoints), interest_metric)
write.table(main_inputData, file = paste("~/projects/TSpaper3_output/python_mainInput", "_", interest_metric, ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = TRUE)

main_maskData <- concatFunction('parameterMask_functionTest_', length(startPoints), interest_metric)
write.table(main_maskData, file = paste("~/projects/TSpaper3_output/python_mainMask", "_", interest_metric, ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = TRUE)

target_gradient <- concatFunction('target_gradient_functionTest_', length(startPoints), interest_metric)
write.table(target_gradient, file = paste("~/projects/TSpaper3_output/python_gradient", "_", interest_metric, ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = TRUE)

target_oneYvalueFromGradient <- concatFunction('target_oneYvalueFromGradient_functionTest_', length(startPoints), interest_metric)
write.table(target_oneYvalueFromGradient, file = paste("~/projects/TSpaper3_output/python_oneYearValue", "_", interest_metric, ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = TRUE)

} # end interest_metric loop

# generate single merge set from all parameters
# int loop as above

  # set up loop through file names
  filenameStartStrings <- c("functionTest", "parameterMask_functionTest", "target_gradient_functionTest", "target_oneYvalueFromGradient_functionTest")
  
  for (l in c(1:length(filenameStartStrings))) {
    patternPaste <- filenameStartStrings[l]
    
    files <- list.files(path = "~/projects/TSpaper3_output/", pattern = paste("^", patternPaste, sep = ""), ignore.case = TRUE)
    numberOfStrides <- length(files) / length(interest_vector)
    
    # load each stride in turn
    for (s in c(1:numberOfStrides)) {
      
      # for stride s, load each parameter and merge LinkIDs
      for (iv in c(1:length(interest_vector))) {
      mergeTestSet <- read.csv(paste("~/projects/TSpaper3_output/", filenameStartStrings[l], "_", s, "_", interest_vector[iv], ".csv", sep = ""))
      mergeID_vector <- mergeTestSet$LinkId
      
      if (iv == 1) {runningMergedID = mergeID_vector}
      if (iv > 1)  {
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
        write.table(commonSet, file = paste("~/projects/TSpaper3_output/common_", patternPaste, "_stride_", s, "_", interest_vector[cf], ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = TRUE)
      }
    
    }
    
  }
  
  # concat each param
  ## reload and concatenate components into single files
  concat_byParam_Function <- function(fileNameStem, max_n, paramList) {
    
    # fileNameStem = "common_functionTest_stride_"; max_n = numberOfStrides; paramList = c("HbA1c", "SBP", "DBP", "BMI")
    for (p in c(1:length(paramList))) {
      
      concat_file <- data.frame()
    
    for (i in seq(1, max_n, 1)) {
      loopSub <- read.csv(paste('~/projects/TSpaper3_output/', fileNameStem, i, '_', paramList[p],'.csv', sep = ''))
      if (i == 1) {concat_file = loopSub}
      if (i>1) {concat_file = rbind(concat_file, loopSub)}
    }
     
      write.table(concat_file, file = paste("~/projects/TSpaper3_output/common_concat_", fileNameStem, "_", paramList[p], ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = TRUE)
      
    }
    
  }
  
  concat_byParam_Function("common_functionTest_stride_", numberOfStrides, interest_vector)
  concat_byParam_Function("common_parameterMask_functionTest_stride_", numberOfStrides, interest_vector)
  concat_byParam_Function("common_target_gradient_functionTest_stride_", numberOfStrides, interest_vector)
  concat_byParam_Function("common_target_oneYvalueFromGradient_functionTest_stride_", numberOfStrides, interest_vector)
  
# extract LinkId from each feature
# merge LinkIds to generate a common set
# merge this common set back to each parameter set in turn to generate final common dataset

##
# notes on drug vector generation- use same variables to match bins
##

# load drug data. class = data table
drugsetDT_ = read.csv(paste("~/projects/sourceData/drugsetDT.csv", sep=""), sep=",", header = TRUE)
drugsetDT_ = data.table(drugsetDT_)

# cut drug data to whole of prescription time window
working_drugset = drugsetDT_[prescription_dateplustime1.original > returnUnixDateTime(cycle_startDate) & prescription_dateplustime1.original < returnUnixDateTime(cycle_endDate)]

# random subset for development
#working_drugset = working_drugset[sample(nrow(working_drugset), 1000,)]

# list of drug possibilities
drugTable <- as.data.frame(table(working_drugset$DrugName))
drugList <- drugTable$Var1

# generate the reporting array for drug vectors
# for each stride, x = total unique IDs, y = total number of time bins for training period + the total length of the horizon + 1 for ID col, z = number of drug classes to test.
reportingArray <- array(0, dim = c(uniqueN(working_drugset$LinkId),      # x
                                   1 + (n_bins_train + n_bins_horizon),  # y
                                   length(drugList)))                    # z

# add unique ID as first column to each of the 2D drug frames - for keying the input of the drug use vector
reportingArray[, 1, ] <- rep(unique(working_drugset$LinkId), length(drugList))

    drugUseVector <- function(subject_ID, prescription_dateplustime1, startPoint, endStridePoint, interest_drug_n, reportingArray_slice) {
      
      # subject_ID = drugSpecificSubset[LinkId == 2147653203 ]$LinkId; prescription_dateplustime1 = drugSpecificSubset[LinkId == 2147653203 ]$prescription_dateplustime1; interest_drug_n = 2
      
      # concat prescription_dateplustime1 with start and end points of stride to ensure that the correct bins are generated
      prescription_dateplustime1 <- c(prescription_dateplustime1, startPoint, endStridePoint)
      subject_ID <- c(subject_ID, 0, 0)
      # print(subject_ID)
      
      match = 0
      
      if(length(subject_ID) > 0) {
        dataBreaks <- split(subject_ID, cut(prescription_dateplustime1, breaks = ((n_bins_train + n_bins_horizon))), drop = FALSE)
        #outputVector <- c(rep(0, (n_bins_train + n_bins_horizon)))
        
        # convert list to numeric vector
        dataBreak_lengths <- as.numeric(lengths(dataBreaks))
        # explicitly manage the first and last values
        # a length of 1 has previously been imposed so only keep if length > 1
        if(dataBreak_lengths[1] == 1) {dataBreak_lengths[1] = 0}
        if(dataBreak_lengths[length(dataBreak_lengths)] == 1) {dataBreak_lengths[length(dataBreak_lengths)] = 0}
        dataBreak_lengths <- ifelse(dataBreak_lengths > 0, 1, 0)
        
        ID_row_number = which(match(reportingArray[,1 ,1 ], subject_ID) == 1)
        
        # print("substitute step")
        reportingArray_slice[ID_row_number, 2:(dim(reportingArray)[2])] = dataBreak_lengths
        #print(reportingArray[ID_row_number, 2:(dim(reportingArray)[2]), interest_drug_n])
        match = 1
      }
      
      assign('reportingArray_slice',reportingArray_slice,envir=.GlobalEnv)
      return(match)
      
    }

for (st in seq(1, length(startPoints), 1)) {
  print('total steps:'); print(length(startPoints))
  print('time step:'); print(st)
  
  startPoint = startPoints[st]
  endStridePoint = startPoint + ((trainWindow_years + horizon_years) * yearSeconds)
  
    # subset by drug (loop) then populate rows in reportingArray where there are positives
      for (d in seq(1, length(drugList), 1)) {
        
        print(d)
        
        reportingArray_slice = reportingArray[ , , d]
        
        drugSpecificSubset = working_drugset[DrugName == drugList[d]]
        
        drugSpecificSubset = drugSpecificSubset[prescription_dateplustime1 >= startPoint & prescription_dateplustime1 <= endStridePoint]
        
        drugSpecificSubset[, c("drugmatch") := drugUseVector(LinkId, prescription_dateplustime1, startPoint, endStridePoint, d, reportingArray_slice) , by=.(id)]
        
        # save_out_array_subset <- reportingArray[, , d]
        
        saveName <- paste("~/R/projects/TSpaper3_output/drugVector_", drugList[d],"stride_",st, ".csv", sep = "")
        write.table(reportingArray_slice, file = saveName, sep = ",", row.names = FALSE, col.names = TRUE)
        
      }
  
}
    
  # start 1130. end stride 8 2230
 
#################################################################################
## part 3 - generate concatenated files for python input
#################################################################################
    
# load start points
# load drug list

# for each stride need to merge numerical and drug data with outputs and
    
    for (st in seq(1, length(startPoints), 1)) {
        
        # load values and mask for numerical param 1
        functionTestData <- read.csv(paste('~/R/projects/TSpaper3_output/functionTest_', st,'.csv', sep = ''))
          functionTestData <- functionTestData[order(functionTestData$LinkId), ]
        param_mask_functionTestData <- read.csv(paste('~/R/projects/TSpaper3_output/parameterMask_functionTest_', st,'.csv', sep =''))
          param_mask_functionTestData <- param_mask_functionTestData[order(param_mask_functionTestData$LinkId), ]
        # load target of interest
        target <- read.csv(paste('~/R/projects/TSpaper3_output/target_oneYvalueFromGradient_functionTest_', st,'.csv', sep = ''))
          target <- target[order(target$LinkId), ]
        # ordered IDlist
        IDlist <- data.frame(functionTestData$LinkId)
        colnames(IDlist) <- c('LinkId')
        
        # drug loop
        for (dr in seq(1, length(drugList), 1)) {
          
          # load drug files for the stride st of interest in turn
          drugVector_file <-read.csv(paste('~/R/projects/TSpaper3_output/drugVector_', drugList[dr], 'stride_', st,'.csv', sep = ''))
          drug_subset <- merge(IDlist, drugVector_file, by.x = 'LinkId', by.y = 'V1')
          drug_subset <- drug_subset[order(drug_subset$LinkId), ]
          if (dr == 1) {
            mergedIDlist <- data.frame(drug_subset$LinkId)
            colnames(mergedIDlist) <- c('LinkId')
            
            # create 3D array of ID vs cols vs drug if dr = 1, populate if dr >= 1
            drugArray <- array(0, dim = c(nrow(drug_subset), # rows = IDs
                                  ncol(drug_subset),         # cols = time bins
                                  length(drugList) + 3))     # z = n drugs + 2 for param details
            drugArray[,,dr] <- unlist(drug_subset)
          } else {
            drugArray[,,dr] <- unlist(drug_subset)
          }
        }
          
          # now merge paramter and mask data to get into same order
          merged_functionTestData <- merge(mergedIDlist, functionTestData, by.x = 'LinkId', by.y = 'LinkId')
          merged_param_mask_functionTestData <- merge(mergedIDlist, param_mask_functionTestData, by.x = 'LinkId', by.y = 'LinkId')
          merged_target <- merge(mergedIDlist, target, by.x = 'LinkId', by.y = 'LinkId')
          
          # expand merged_target to same size
          merged_target <- merged_target[rep(names(merged_target), c(1, (dim(drugArray)[2] - 1)))]
          
          # generate zero value matrix for padding
          zeroPad <- as.data.frame(matrix(0, nrow = nrow(merged_functionTestData), ncol = dim(drugArray)[2] - ncol(merged_functionTestData)))
          
          # pad parameter values
          merged_functionTestData_padded <- cbind(merged_functionTestData, zeroPad)
          merged_param_mask_functionTestData_padded <- cbind(merged_param_mask_functionTestData, zeroPad)
          
          # add padded values to drugArray
          drugArray[,,length(drugList) + 1] <- unlist(merged_functionTestData_padded)
          drugArray[,,length(drugList) + 2] <- unlist(merged_param_mask_functionTestData_padded)
          drugArray[,,length(drugList) + 3] <- unlist(merged_target)
          
          # save out each slice of drug array in turn
          saveOutnames <- c(as.character(drugList), 'paramterValue', 'parameterValueMask', 'target')
          
          for (s in seq(1, dim(drugArray)[3], 1)) {
            writeName <- paste('~/R/projects/TSpaper3_output/', saveOutnames[s],'stride_', st,'.csv', sep = '')
            write.table(drugArray[ , , s], file = writeName, sep = ",", row.names = FALSE, col.names = TRUE)
          }
    }
    
################################################################################
    ## concatatenate all strides together for each parameter and save out
################################################################################

    for (cc in seq(1, dim(drugArray)[3], 1)) {
      stem = paste(saveOutnames[cc], 'stride_', sep='')
      x <- concatFunction(stem, length(startPoints))
      
      writeName <- paste('~/R/projects/TSpaper3_output/', saveOutnames[cc], "concatOutput.csv", sep = '')
      write.table(x, file = writeName, sep = ",", row.names = FALSE, col.names = TRUE)      
    }
          
          