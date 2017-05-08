
# 4. Read data
## 4.1 Basic package

read.csv("filename" , header = TRUE, sep = "," )
read.csv(link, header = TRUE, stringsAsFactors = TRUE)
read.table()

## 4.2 Read from html
url <- "http://www.nyc.gov/html/tlc/html/about/trip_record_data.shtml"
library(RCurl)
library(XML)
doc <- htmlParse(url)
links <- xpathSApply(doc, "//a/@href")
free(doc)
class(links)
names(links) <- NULL

links <- links[grep("green",links)]
link <- links[grep("2015-09",links)]


## 4.3 Read from excel

# read in the first worksheet from the workbook myexcel.xlsx
# first row contains variable names
library(xlsx)
mydata <- read.xlsx("c:/myexcel.xlsx", 1)

# read in the worksheet named mysheet
mydata <- read.xlsx("c:/myexcel.xlsx", sheetName = "mysheet")

# fast reading read.xlsx2
Data <- read.xlsx2("Analytics Challenge Data 2.xlsx", sheetName = 1, colIndex = c(1:12), 
                   header = TRUE, colClasses=c( "POSIXct", "character", "integer", "character", rep("integer", 8)))
Data2 <- read.xlsx("Analytics Challenge Data 2.xlsx", sheetName = 1, colIndex = 1,
                   header = TRUE, colClasses= "POSIXct")
Data[,1]<-Data2

```

## 4.4 Read from SPSS & SAS
# in R 
library(Hmisc)
mydata <- spss.get("c:/mydata.por", use.value.labels=TRUE)
# last option converts value labels to R factors

# save SAS dataset in trasport format
#libname out xport 'c:/mydata.xpt';
#data out.mydata;
#set sasuser.mydata;
#run;

mydata <- sasxport.get("c:/mydata.xpt")
# character variables are converted to R factors
```

## 4.3 Fast Reading
# Fast data reading
library(data.table)
taxiData <- fread("green_tripdata.csv",header = TRUE)
read.table("green_tripdata.csv",header = TRUE)
head(taxiData)
dim(taxiData)

