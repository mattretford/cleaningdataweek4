question1 <- function() {
  
  dataset <- read.csv('getdata_data_ss06hid.csv',na.strings=c("", "NA"));
  
  splitlist <- strsplit(names(dataset),"wgtp");
  
  #View(splitlist)
  
  View (splitlist[[123]])
  
}

question2 <- function(){
  
  GDP <- read.csv('getdata_data_GDP.csv',na.strings=c("", "NA"));
  
  col_conv <- c("X.3")
  
  GDP[,col_conv] <- as.numeric(gsub(",","",GDP[,col_conv]));
  
  GDPSliced <- GDP[5:194,]
  
  startedUnited <- grep("^United",GDPSliced$X.2 )

 # meanGDP <- mean(GDPSliced$X.3)
  View(startedUnited)
}

question3 <- function(){
  
  GDP <- read.csv('getdata_data_GDP.csv',na.strings=c("", "NA"));
  FEDSTATS <- read.csv("getdata_data_EDSTATS_Country.csv");
  
  
  col_conv <- c("X.3")
  
  GDP[,col_conv] <- as.numeric(gsub(",","",GDP[,col_conv]));
  
  GDPSliced <- GDP[5:194,]
  
  mergedDataCode = merge(FEDSTATS, GDPSliced, by.x = "CountryCode", by.y = "X", all = FALSE, no.dups = TRUE);
  
  numberOfJune <- table(grepl("Fiscal year end: June",mergedDataCode$Specialnotes))
  
  View(numberOfJune);
  
}