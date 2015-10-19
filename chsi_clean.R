# After running read_data.R
# Clean the list chsi

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

# All values that should be NA or 0(no report)
NAs <- c(-9999,-2222,-2222.2,-2,-9998.9)
ZEROs <- c(-1111.1,-1111,-1)
# clean all missing values, set to NA
for(i in c(3:11)){
  tmp <- chsi[[i]]
  for(j in c(1:length(tmp))){
    tmp[,j][tmp[,j] %in% NAs] <- NA
    tmp[,j][tmp[,j] %in% ZEROs] <- 0
    chsi[[i]] <- tmp
  }
}

View(chsi[[5]])
