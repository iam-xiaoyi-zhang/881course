# read CHSI data, and put all spreadsheets in one list
library(readxl)
chsi <- list()
for(i in c(1:11)){
  chsi[[i]] <- read_excel("CHSI DataSet.xls", sheet=i)
}

# use this variable to save names of spreadsheets.
sheetIndex <- c("DATAELEMENTDESCRIPTION",
                "DEFINEDDATAVALUE",
                "HEALTHYPEOPLE",
                "DEMOGRAPHICS",
                "LEADINGCAUSEOFDEATH",
                "SUMMARYMEASURESOFHEALTH",
                "MEASURESOFBIRTHANDDEATH",
                "RELATIVEHEALTHIMPORTANT",
                "VUNERABLEPOPSANDENVHEALTH",
                "PREVENTIVESERVICESUSE",
                "RISKFACTORSANDACCESSTOCARE")

# get data in MA, AKA Worcestor's neighborhoods
MA <- list()
for(i in c(4:11)){
  MA[[i-3]] <- subset(chsi[[i]], chsi[[i]]$CHSI_State_Name=="Massachusetts")
}

MA0 <- MA[[1]]
for(i in c(2:8)){
  MA0 <- merge(MA0, MA[[i]])
}


# store all spreadsheets in one data.frame
chsi0 <- chsi[[4]]
for(i in c(5:11)){
  chsi0 <- merge(chsi0, chsi[[i]])
}

# So, to sum up:
# list    data.frame
# chsi    chsi0        all data
# MA      MA0          Massachusetts data

#---------------------------------------------------------------------

library(jsonlite)
x <- fromJSON()


