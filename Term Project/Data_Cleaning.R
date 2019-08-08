
rm(list = ls())

library(readxl)

# Import and clean data.
divorceData = data.frame(read.csv("data/divorce.csv", fileEncoding = "UTF-8")) # DivorceRate
colnames(divorceData) = c("area",as.character(seq(2017,2009,-1)))
str(divorceData)

marrigeData = data.frame(read.csv("data/marriage.csv", fileEncoding = "UTF-8")) # MarriageRate
colnames(marrigeData) = c("area", as.character(seq(2017, 2009, -1)))
str(marrigeData)

DLI = data.frame(read_excel("data/DLI.xlsx"))
nrows = nrow(DLI)
DLI = DLI[-c(nrows,nrows-1),]
colnames(DLI) = c("area", as.character(seq(2000, 2012)))
str(DLI)

singleOver15 = data.frame(read_excel("data/SingleOver15.xlsx"))
colnames(singleOver15) = c("area", as.character(seq(2018, 2002, -1)))
ncols = ncol(singleOver15)
nrows = nrow(singleOver15)
singleOver15 = singleOver15[-c(nrows, nrows - 1),]
singleOver15 = singleOver15[, - c(ncols, ncols - 1, ncols - 2)]
str(singleOver15)

genderRatio = data.frame(read_excel("data/GenderRatio.xlsx"))       # GenderRatio
colnames(genderRatio) = c("area", as.character(seq(2018, 2009, -1)))
str(genderRatio)

savings = data.frame(read_excel("data/Savings.xlsx"))
colnames(savings) = c("area", as.character(seq(2018, 2000, -1)))
ncols = ncol(savings)
nrows = nrow(savings)
savings = savings[-c(nrows, nrows - 1),]
savings = savings[,-c(ncols,ncols-1,ncols-2)]
str(savings)

averageHomePrice = data.frame(read.csv("data/AverageHomePrice.csv")) # AHP
averageHomePrice = averageHomePrice[,-2]
colnames(averageHomePrice) = c("area", as.character(seq(2017, 2009, -1)))
str(averageHomePrice)

CPI = data.frame(read.csv("data/ConsumerPriceIndex.csv")) # CPI
colnames(CPI) = c("area", as.character(seq(2018, 2009, -1)))
str(CPI)

consumptionPerCapita = data.frame(read.csv("data/ConsumptionPerCapita.csv"))
nrows = nrow(consumptionPerCapita)
consumptionPerCapita = consumptionPerCapita[-seq(nrows, nrows - 2, -1),]
ncols = ncol(consumptionPerCapita)
consumptionPerCapita = consumptionPerCapita[, - seq(ncols, ncols - 3, -1)]
colnames(consumptionPerCapita) = c("area", as.character(seq(2018, 2013, -1)))
str(consumptionPerCapita)

educationFunding = data.frame(read.csv("data/EducationFunding.csv")) # eduFunding
educationFunding = t(educationFunding)
area = educationFunding[1,]
educationFunding = educationFunding[-1,]
educationFunding = educationFunding[c(-1,-2),]
educationFunding = as.data.frame(t(educationFunding))
colnames(educationFunding) = c(as.character(seq(2016, 2009, -1)))
educationFunding = cbind(area,educationFunding)
str(educationFunding)


gdp = data.frame(read.csv("data/GDP.csv"))
nrows = nrow(gdp)
gdp = gdp[-seq(nrows, nrows - 2, -1),]
colnames(gdp) = c("area", as.character(seq(2018, 2009, -1)))
str(gdp)

heathTechnicainPerTenThousand = data.frame(read.csv("data/HealthTechnician.csv")) # HTper10000
nrows = nrow(heathTechnicainPerTenThousand)
heathTechnicainPerTenThousand = heathTechnicainPerTenThousand[-c(nrows-1,nrows),]
heathTechnicainPerTenThousand = heathTechnicainPerTenThousand[, -2]
colnames(heathTechnicainPerTenThousand) = c("area", as.character(seq(2017, 2009, -1)))
str(heathTechnicainPerTenThousand)


houseHoldConsumptionLevel = data.frame(read.csv("data/HouseholdConsumptionLevel.csv")) # HCL
nrows = nrow(houseHoldConsumptionLevel)
houseHoldConsumptionLevel = houseHoldConsumptionLevel[-c(nrows - 1, nrows),]
houseHoldConsumptionLevel = houseHoldConsumptionLevel[, -2]
houseHoldConsumptionLevel = houseHoldConsumptionLevel[,seq(1,10)]
colnames(houseHoldConsumptionLevel) = c("area", as.character(seq(2017, 2009, -1)))
str(houseHoldConsumptionLevel)

disPosableIncome = data.frame(read.csv("data/HouseholdPerCapitaDisposableIncome.csv")) # Income
ncols = ncol(disPosableIncome)
colnames(disPosableIncome) = c("area", as.character(seq(2018, 2013, -1)))
disPosableIncome = disPosableIncome[,c(seq(1,7))]
str(disPosableIncome)

higherEducationEnrollmentPerTenThousand = data.frame(read.csv("data/HigherEducationEnrollmentPerTenThousand.csv"))
nrows = nrow(higherEducationEnrollmentPerTenThousand)
higherEducationEnrollmentPerTenThousand = higherEducationEnrollmentPerTenThousand[-c(nrows - 2, nrows - 1, nrows),]
higherEducationEnrollmentPerTenThousand = higherEducationEnrollmentPerTenThousand[,-2]
colnames(higherEducationEnrollmentPerTenThousand) = c("area", as.character(seq(2017, 2009, -1)))


population = data.frame(read.csv("data/Population.csv")) # POP
nrows = nrow(population)
population = population[-c(nrows-2,nrows-1,nrows),]
colnames(population) = c("area", as.character(seq(2018, 2009, -1)))
str(population)

unemploymentRate = data.frame(read.csv("data/UnemploymentRate.csv")) # UR
nrows = nrow(unemploymentRate)
unemploymentRate = unemploymentRate[-c(nrows - 1, nrows),]
unemploymentRate = unemploymentRate[,-2]
colnames(unemploymentRate) = c("area", as.character(seq(2017, 2009, -1)))
str(unemploymentRate)



# Function: transpose data frame together with years.
Transpose = function(vectorData, name) {
    vectorData = vectorData[, -1]
    colNames = colnames(vectorData)
    vectorData = cbind(colNames, data.frame(t(vectorData)))
    rownames(vectorData) = seq(1, nrow(vectorData))
    colnames(vectorData) = c("year", name)
    return(vectorData)
}

# Function: merge a list of dataframes
mergeFunc = function(dataFrameList, mergeBy) {
    res = dataFrameList[[1]] %>% select(mergeBy)
    for (df in dataFrameList) {
        res = merge(res, df, by = mergeBy, all.x = TRUE)
    }
    return(res)
}

library(dplyr)
# Function: get all relative data of a certain area.
# Get data and deal with the NAs. Use "na.interp" directly won't work.
linearInterp = function(x, y, fitData) {
    model = lm(y ~ x, data = fitData)
    interpData = x[is.na(y)]
    y[is.na(y)] = predict(model, newdata = data.frame(x = interpData))
    return(y)
}

fill_NA_linear = function(dataFrame, xVar) {
    # The intepretation lines don't work and I don't know why.
    # xValues = dataFrame %>% select(xVar)
    xIndex = which(colnames(dataFrame) == xVar)
    for (i in seq(1, ncol(dataFrame))) {
        if (colnames(dataFrame)[i] != xVar) {
            # dataFrame[,i] = linearInterp(x = xValues, y = dataFrame[,i], fitData = dataFrame)
            dataFrame[, i] = linearInterp(x = dataFrame[, xIndex], y = dataFrame[, i], fitData = dataFrame)
        }
    }
    return(dataFrame)
}
getDataCertainArea = function(areaSelect) {
    
    year = data.frame(as.numeric(seq(2009, 2018)))
    province = data.frame(rep(areaSelect, length(year)))
    year = cbind(year, province)
    colnames(year) = c("year", "province")

    marSelect = Transpose(marrigeData %>% filter(area == areaSelect), "MR")
    divSelect = Transpose(divorceData %>% filter(area == areaSelect), "DR")
    popSelect = Transpose(population %>% filter(area == areaSelect), "POP")
    
    gdpSelect = Transpose(gdp %>% filter(area == areaSelect), "GDP")
    incSelect = Transpose(disPosableIncome %>% filter(area == areaSelect),"INC")
    conSelect = Transpose(consumptionPerCapita %>% filter(area == areaSelect), "CON")
    #hclSelect = Transpose(houseHoldConsumptionLevel %>% filter(area == areaSelect), "HCL")
    cpiSelect = Transpose(CPI %>% filter(area == areaSelect),"CPI")
    heetSelect = Transpose(higherEducationEnrollmentPerTenThousand %>% filter(area == areaSelect),"HEET")
    #edufSelect = Transpose(educationFunding %>% filter(area == areaSelect), "EDUF")
    #edufSelect$EDUF = as.numeric(as.character(edufSelect$EDUF))
    #heaSelect = Transpose(heathTechnicainPerTenThousand %>% filter(area == areaSelect),"HTPT")
    hopSelect = Transpose(averageHomePrice %>% filter(area == areaSelect),"HP")
    uneSelect = Transpose(unemploymentRate %>% filter(area == areaSelect), "UR")
    grSelect = Transpose(genderRatio %>% filter(area == areaSelect), "GR")
    savSelect = Transpose(savings %>% filter(area == areaSelect), "SAV")
    #sp15Select = Transpose(singleOver15 %>% filter(area == areaSelect), "SP15")
    #dliSelect = Transpose(DLI %>% filter(area == areaSelect), "DLI")

    # Merge and fill NAs
    return(fill_NA_linear(mergeFunc(list(year,marSelect, divSelect, popSelect, gdpSelect, incSelect, conSelect,
        cpiSelect, heetSelect, hopSelect, uneSelect, grSelect, savSelect), mergeBy = "year"),xVar = "year"))
}
# Function: get data of all provinces.
# Solving the problem of "degree of freedom".
getAllData = function() {
    areas = divorceData[, 1]
    res = data.frame()
    for (area in areas) {
        res = rbind(res,getDataCertainArea(areaSelect = area))
    }
    return(res)
}
# Get cleaned data set.
cleanedDataSet = getAllData()
anyNA(cleanedDataSet)

cleanedDataSet$MN = cleanedDataSet$MR       # Marriage Number
cleanedDataSet$DN = cleanedDataSet$DR       # Divorce Number
cleanedDataSet$MR = cleanedDataSet$MR * 2 / cleanedDataSet$POP      # Marriage Rate
cleanedDataSet$DR = cleanedDataSet$DR * 2 / cleanedDataSet$POP      # Divorce Rate
cleanedDataSet$GDPC = cleanedDataSet$GDP / cleanedDataSet$POP       # GDP per capita
cleanedDataSet$GR = cleanedDataSet$GR - 100
# Savings per GDP.
cleanedDataSet$SAVPG = cleanedDataSet$SAV / cleanedDataSet$GDP

# Save the cleaned data.
write.csv(cleanedDataSet, "cleaned_data.csv", row.names = F)

# Prepare training data and testing data for futher modeling.
training_rows = sample(1:nrow(cleanedDataSet), 0.8 * nrow(cleanedDataSet))
training_data = cleanedDataSet[training_rows,]
testing_data = cleanedDataSet[-training_rows,]
write.csv(training_data, "training_set.csv", row.names = F)
write.csv(testing_data, "testing_set.csv", row.names = F)

