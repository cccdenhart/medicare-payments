library(ggplot2)
library(reshape2)

# read in files
curWD <- "/Users/cccdenhart/Documents/R Workspace/"

medicarePayments15 <- read.csv(paste(curWD, "MedicarePayments15.csv", sep = ""))
medicarePayments14 <- read.csv(paste(curWD, "MedicarePayments14.csv", sep = ""))
medicarePayments13 <- read.csv(paste(curWD, "MedicarePayments13.csv", sep = ""))
medicarePayments12 <- read.csv(paste(curWD, "MedicarePayments12.csv", sep = ""))

n15 <- names(medicarePayments15)
n14 <- names(medicarePayments14)
n13 <- names(medicarePayments13)
n12 <- names(medicarePayments12)

# filter each file
medicarePayments15 <- medicarePayments15[medicarePayments15$State.Code.of.the.Provider=="MA", selectedCols]
medicarePayments14 <- medicarePayments14[medicarePayments14$State.Code.of.the.Provider=="MA", selectedCols]
medicarePayments13 <- medicarePayments13[medicarePayments13$State.Code=="MA", c("National.Provider.Identifier", "Entity.Code",
                                                                                "State.Code", "AVERAGE_MEDICARE_PAYMENT_AMT",
                                                                                "HCPCS_DESCRIPTION","LINE_SRVC_CNT" )]
medicarePayments12 <- medicarePayments12[medicarePayments12$State.Code=="MA", c("National.Provider.Identifier", "Entity.Code",
                                                                                "State.Code", "Average.Medicare.Payment.Amount",
                                                                                "HCPCS.Description")]

# global variables
allFiles <- c(medicarePayments15, medicarePayments14, medicarePayments13, medicarePayments12)
namesMed <- names(medicarePayments15)
selectedCols <- c("National.Provider.Identifier", "Entity.Type.of.the.Provider",
                  "State.Code.of.the.Provider", "Average.Medicare.Payment.Amount",
                  "HCPCS.Description", "Number.of.Services")
sepNames <- c("Provider.ID", "Provider.Entity", "Provider.State", "Average.Payment", "Description")
num <- 5
descList <- c("MRI scan brain", "MRI scan of upper spinal canal", 
              "Administration of influenza virus vaccine", 
"Influenza virus vaccine, split virus, when administered to individuals 3 years of age and older, for intramuscular use (fluzone)",
"Influenza virus vaccine, split virus, when administered to individuals 3 years of age and older, for intramuscular use (fluvirin)")
paymentType <- c("Hosp MRI Brain", "Ind MRI Brain", 
                 "Hosp MRI Spinal", "Ind MRI Spinal", 
                 "Hosp Influenza", "Ind Influenza",
                 "Hosp Vaccine1", "Ind Vaccine1",
                 "Hosp Vaccine2", "Ind Vaccine2")
sumNames <- c("Category","Mean", "Standard Deviation")

# returns a list of hospital and individual data
sepByType <- function(file, type) {
  
  names(file) <- sepNames
  
  result <- file[file$Provider.Entity==type,]
  
  return (result)
  
}

# adds a description to the list of data to be summarized

addDesc <- function(file, desc, type) {
  l <- sepByType(file, type)
  df <- l[l$Description==desc,]
  return (df)
}

# returns a final data frame with the means and sds of all desired from the given list
summaryData <- function(file) {
  
  totalPayments <- vector("list", length(descList) * 2)
  i <- 1
  for(item in descList) {
    totalPayments[[i]] <- addDesc(file, item, "O")$Average.Payment
    totalPayments[[i+1]] <- addDesc(file, item, "I")$Average.Payment
    i <- i+2
  }
  
  sds <- sapply(totalPayments, sd)
  means <- sapply(totalPayments, mean)
  
  finalData <- data.frame(paymentType, means, sds)
  names(finalData) <- sumNames
  
  return (finalData)
  
}

# create instances of summaries
summary15 <- summaryData(medicarePayments15)
summary14 <- summaryData(medicarePayments14)
summary13 <- summaryData(medicarePayments13)
summary12 <- summaryData(medicarePayments12)

# store the most significant elements of the dataset
mostFrequent <- function(file) {
  
  lI <- sepByType(file, "I")
  lH <- sepByType(file, "O")
  
  freqI <- sort(table(lI$Description),decreasing=TRUE)[1:num]
  freqH <- sort(table(lH$Description),decreasing=TRUE)[1:num]
  descFreq <- data.frame(freqI, freqH)
  names(descFreq) <- c("Top Individual Occ", "Ind Count", "Top Hospital Occ", "Hosp Count")
  
  return (descFreq)
  
}

# create instances of frequencies
freq15 <- mostFrequent(medicarePayments15)
freq14 <- mostFrequent(medicarePayments14)
freq13 <- mostFrequent(medicarePayments13)
freq12 <- mostFrequent(medicarePayments12)

# find counts of the descriptions in freq12 for every year
desc12T <- sort(table(sepByType(medicarePayments12, "I")$Description),decreasing=TRUE)[1:num]
desc12 <- names(desc12T)
desc12TH <- sort(table(sepByType(medicarePayments12, "O")$Description),decreasing=TRUE)[1:num]
desc12H <- names(desc12TH)

countDesc <- function(file, type) {
  count <- c()
  for (desc in desc12H) {
    count <- c(count, sum(sepByType(file, type)==desc))
  }
  return (count)
}

count12 <- countDesc(medicarePayments12, "O")
count13 <- countDesc(medicarePayments13, "O")
count14 <- countDesc(medicarePayments14, "O")
count15 <- countDesc(medicarePayments15, "O")

allCounts <- data.frame(A=numeric(0), B=numeric(0), C=numeric(0), D=numeric(0), E=numeric(0))
allCounts[nrow(allCounts)+1,] <- as.list(count12)
allCounts <- rbind(allCounts,count13)
allCounts <- rbind(allCounts,count14)
allCounts <- rbind(allCounts,count15)
allCounts <- cbind(c("2012", "2013", "2014", "2015"), allCounts)
names(allCounts) <- c("Years", desc12H)
allCountsMelt <- melt(allCounts, id.vars = "Years")

ggplot(data=allCountsMelt, aes(x = Years, y = value, group=variable, color=variable)) +
  geom_line() +
  labs(y="Number of Occurrences")
ggsave("Variable Occurrences.png",plot=last_plot(), height = 15, width = 15)


# find highest prices in 

# finds the mode of a vector
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# create tables from the hospital and individual payment data for occurence purposes
indTable <- table(sepByType(medicarePayments15, "I")$Description)
hospTable <- table(sepByType(medicarePayments15, "O")$Description)
