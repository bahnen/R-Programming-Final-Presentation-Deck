rm(list = ls())
#install.packages("ggpubr")
#install.packages("wordcloud")
library(wordcloud)
#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("wordcloud2")
library(wordcloud2)
library(readr)
library(dplyr)		
library(reshape2) 	
library(foreign)
library(numbers)	
library(ggplot2)		
library(zoo) 		
library(tseries)
library(choroplethr)
library(choroplethrMaps)
library(leaflet)
#install.packages("tm")
library(tm)
library(knitr)    # For knitting document and include_graphics function
library(png)      # For grabbing the dimensions of png files
library(ggpubr)
library(plotly)
library(zoo)
library(tidyverse)

df <- read_csv("Iowa_Liquor_Sales (2).csv")
head(df)


#Old method
#df <- read_csv("Iowa_Liquor_Sales.csv") 

names(df)[names(df) == "Invoice/Item Number"] <- "Invoice_ID"
names(df)[names(df) == "Store Number"] <- "Store_ID"
names(df)[names(df) == "Store Name"] <- "Store_Name"
names(df)[names(df) == "Zip Code"] <- "Zip"
names(df)[names(df) == "Store Location"] <- "Coordinates"
names(df)[names(df) == "County Number"] <- "County_ID"
names(df)[names(df) == "Category"] <- "Category_ID"
names(df)[names(df) == "Category Name"] <- "Category_Name"
names(df)[names(df) == "Vendor Name"] <- "Vendor"
names(df)[names(df) == "Vendor Number"] <- "Vendor_ID"
names(df)[names(df) == "Item Number"] <- "Item_ID"
names(df)[names(df) == "Item Description"] <- "Item_Description"
names(df)[names(df) == "Bottle Volume (ml)"] <- "Bottle_Vol_ML"
names(df)[names(df) == "State Bottle Cost"] <- "Bottle_Cost"
names(df)[names(df) == "State Bottle Retail"] <- "Bottle_Retail"
names(df)[names(df) == "Bottles Sold"] <- "Bottles_Sold"
names(df)[names(df) == "Sale (Dollars)"] <- "Sales_Dollar"
names(df)[names(df) == "Volume Sold (Gallons)"] <- "Volume_Sold_G"
df

#These factors need to be cleaned to all be lower case, 
# to eliminate redundant factor levels
df$County = tolower(df$County)
df$City = tolower(df$City)

df$County <- factor(df$County)
levels(df$County)
df$City <- factor(df$City)
levels(df$City)

#Changing Date format
df$Date <- as.Date(df$Date,"%m/%d/%Y")

#Not sure if these fields will be necessary for our analysis

df$Bottle_Vol_ML <- NULL

# i think we should keep this one for comparison purposes since most people think in gallons -Lucas df$Volume_Sold_G <- NULL

df <- df[complete.cases(df), ]
head(df)


# Brainstorming an idea for a function we can use for the project
#This will search for items within a certain type of alcohol, such as whisky (there are multiple types of whiskies in this data set)
#Returns the item desacription and the category name for any product that is a type of whisky for example

productsearch <- function(title = "") {
  
  rows <- grep(pattern = tolower(title), x = tolower(df$Category_Name), fixed = TRUE)
  df <- df[rows, ]
  
  df[, c("Category_Name", "Release")] }

#Might need to use this SQL code to find categories that include a specific word within their name
#SELECT *
#  FROM df
#WHERE Category_Name LIKE '%Whisky%';


productsearch("Whisky")


###Choreopleth Start###
# Load detailed county info
data(county.regions)

# Store in a shorter name
cr <- county.regions


cr$county.name = tolower(cr$county.name)
levels(df$County)[levels(df$County) == "buena vist"] = "buena vista"
levels(df$County)[levels(df$County) == "cerro gord"] = "cerro gordo"
levels(df$County)[levels(df$County) == "pottawatta"] = "pottawattamie"

levels(df$County)
cr$county.name <- factor(cr$county.name)

#Focus on only Iowa Data
cr <- subset(cr, state.abb == "IA")
cr <- cr[, c("region", "county.name")]
head(cr)

indata <- c("adams","allamakee","benton","black hawk","bremer","buchanan","buena vista","carroll","cedar", "cerro gordo","clay",
            "clayton","clinton","crawford","dallas","delaware","des moines","dickinson","dubuque","emmet","fayette","guthrie",
            "hamilton","hardin","howard","ida","iowa","jefferson","johnson","jones","lee","linn","lucas","lyon","marion",
            "mills","monroe","montgomery","muscatine","page","palo alto","pocahontas","polk","pottawattamie","poweshiek","sac",
            "scott","sioux","story","tama","van buren","wapello","warren","webster","woodbury")
notin <- factor(c("adair","appanoose","audubon","boone","butler","calhoun","cass","cherokee","chickasaw","clarke","davis","decatur",                                     
                  "floyd","franklin","fremont","greene","grundy","hancock","harrison","henry","humboldt","jackson","jasper","keokuk",
                  "kossuth","louisa","madison","mahaska","marshall","mitchell","monona","o'brien","osceola","plymouth","ringgold","shelby",
                  "taylor","union","washington","wayne","winnebago","winneshiek","worth","wright"))  



levels(cr$county.name)
cr$county.name <- factor(cr$county.name)

cr <- cr[(-c(1, 4, 5, 8, 12, 13, 15, 18, 19, 20, 26,27,34, 35, 36, 37, 38, 41, 43, 44, 46, 49, 50, 54, 55, 58, 61, 62,64,66,67,71,72,75,80,83,
             87, 88, 92, 93,95, 96,98,99)),]



#Group sales by county
countytrans <- group_by(df, County)


# Summarize by count in group
#Gives County with highest transactions
summtrans <- summarize(countytrans, value = n())
head(summtrans)
summsales <- summarize(countytrans, value = sum(Sales_Dollar))
summgal <- summarize(countytrans, value = sum(Volume_Sold_G))
# Reorder summ
summtrans <- summtrans[order(tolower(summtrans$County)), ]
summsales <- summsales[order(tolower(summsales$County)), ]
summgal <-summgal[order(tolower(summgal$County)), ]
# Reorder cr
cr <- cr[order(cr$county.name), ]

summtrans$region <- cr$region
summsales$region <- cr$region
summgal$region <- cr$region

#Transactional Choropleth
t <- county_choropleth(summtrans, state_zoom = "iowa")
t
t <- t + geom_path()
t
t +    scale_fill_brewer(palette="YlOrBr") +
  labs(title = "Transaction Count by County",
       subtitle = "Iowa Alcohol Sales",
       caption = "source: https://data.iowa.gov/Sales-Distribution/Iowa-Liquor-Sales/m3tr-qhgy",
       fill = "Total Transactions"
  ) 


#Sales Choropleth
s <- county_choropleth(summsales, state_zoom = "iowa")
s
s <- s + geom_path()
s
s +    scale_fill_brewer(palette="RdPu") +
  labs(title = "Sales in Dollars by County",
       subtitle = "Iowa Alcohol Sales",
       caption = "source: https://data.iowa.gov/Sales-Distribution/Iowa-Liquor-Sales/m3tr-qhgy",
       fill = "Total Transactions")

#Gallons Choropleth
g <- county_choropleth(summgal, state_zoom = "iowa")
g
g <- g + geom_path()
g
g +    scale_fill_brewer(palette="BuGn") +
  labs(title = "Total Gallons Sold by County",
       subtitle = "Iowa Alcohol Sales",
       caption = "source: https://data.iowa.gov/Sales-Distribution/Iowa-Liquor-Sales/m3tr-qhgy",
       fill = "Total Transactions")

# Interactive Word Cloud Creation#
cloud <- group_by(df, Category_Name)
summcloud <- summarise(cloud, value = n())

set.seed(1234) # for reproducibility 

c <- wordcloud2(data=summcloud, size=.35, color = "Gold", backgroundColor = "Black")
c

#Donut Chart#
donut <- group_by(df, County) 
summdonut <- summarize(donut, value = sum(Sales_Dollar))

donutsales <- plot_ly(data = summdonut, labels =~County, values = ~value, sort = FALSE, size = 1, sizes = 8,
                      colors = "YlOrRd") %>%
  add_pie(hole = .6) 

donutsales

# Grouping the data by month
dfdate = df

dfdate$Date = as.factor(dfdate$Date)
levels(dfdate$Date)[which(levels(dfdate$Date) >= "2019-01-01" & levels(dfdate$Date) <= "2019-01-31")] = "Jan2019"
levels(dfdate$Date)[which(levels(dfdate$Date) >= "2019-02-01" & levels(dfdate$Date) <= "2019-02-31")] = "Feb2019"
levels(dfdate$Date)[which(levels(dfdate$Date) >= "2019-03-01" & levels(dfdate$Date) <= "2019-03-31")] = "Mar2019"
levels(dfdate$Date)[which(levels(dfdate$Date) >= "2019-04-01" & levels(dfdate$Date) <= "2019-04-31")] = "Apr2019"
levels(dfdate$Date)[which(levels(dfdate$Date) >= "2019-05-01" & levels(dfdate$Date) <= "2019-05-31")] = "May2019"
levels(dfdate$Date)[which(levels(dfdate$Date) >= "2019-06-01" & levels(dfdate$Date) <= "2019-06-31")] = "Jun2019"
levels(dfdate$Date)[which(levels(dfdate$Date) >= "2019-07-01" & levels(dfdate$Date) <= "2019-07-31")] = "Jul2019"
levels(dfdate$Date)[which(levels(dfdate$Date) >= "2019-08-01" & levels(dfdate$Date) <= "2019-08-31")] = "Aug2019"
levels(dfdate$Date)[which(levels(dfdate$Date) >= "2019-09-01" & levels(dfdate$Date) <= "2019-09-31")] = "Sep2019"
levels(dfdate$Date)[which(levels(dfdate$Date) >= "2019-10-01" & levels(dfdate$Date) <= "2019-10-31")] = "Oct2019"
levels(dfdate$Date)[which(levels(dfdate$Date) >= "2019-11-01" & levels(dfdate$Date) <= "2019-11-31")] = "Nov2019"
levels(dfdate$Date)[which(levels(dfdate$Date) >= "2019-12-01" & levels(dfdate$Date) <= "2019-12-31")] = "Dec2019"
levels(dfdate$Date)[which(levels(dfdate$Date) >= "2020-01-01" & levels(dfdate$Date) <= "2020-01-31")] = "Jan2020"
levels(dfdate$Date)[which(levels(dfdate$Date) >= "2020-02-01" & levels(dfdate$Date) <= "2020-02-31")] = "Feb2020"
levels(dfdate$Date)[which(levels(dfdate$Date) >= "2020-03-01" & levels(dfdate$Date) <= "2020-03-31")] = "Mar2020"
levels(dfdate$Date)[which(levels(dfdate$Date) >= "2020-04-01" & levels(dfdate$Date) <= "2020-04-31")] = "Apr2020"
levels(dfdate$Date)[which(levels(dfdate$Date) >= "2020-05-01" & levels(dfdate$Date) <= "2020-05-31")] = "May2020"
levels(dfdate$Date)[which(levels(dfdate$Date) >= "2020-06-01" & levels(dfdate$Date) <= "2020-06-31")] = "Jun2020"
levels(dfdate$Date)[which(levels(dfdate$Date) >= "2020-07-01" & levels(dfdate$Date) <= "2020-07-31")] = "Jul2020"
levels(dfdate$Date)[which(levels(dfdate$Date) >= "2020-08-01" & levels(dfdate$Date) <= "2020-08-31")] = "Aug2020"
levels(dfdate$Date)[which(levels(dfdate$Date) >= "2020-09-01" & levels(dfdate$Date) <= "2020-09-31")] = "Sep2020"
levels(dfdate$Date)[which(levels(dfdate$Date) >= "2020-10-01" & levels(dfdate$Date) <= "2020-10-31")] = "Oct2020"
levels(dfdate$Date)

#summary of the data by month
dfdate = group_by(dfdate, Date)
dfdateSummary = summarize(dfdate,
                          TotalSale = sum(Sales_Dollar),
                          TotalBottleSold = sum(Bottles_Sold),
                          TotalVolumeSold = sum(Volume_Sold_G))

# Total sales by day (Transactions)
plot(df$Sales_Dollar~as.factor(dfdate$Date),type="l",
     xlab="Date", ylab="Sales Dollars",
     main="Time trend of sales")

#Had to manually save

# Total sales by month
qplot(Date,
      TotalSale,
      data = dfdateSummary,
      geom = "col",
      fill = I("gold"),
      colour = I("black"),
      main = "Total Sales ($) By Month",
      ylim = c(0, 4000000)) +
  theme(axis.text.x = element_text(
    angle = 90,
    vjust = .5)) + 
  scale_y_continuous(name = "TotalSale", breaks = c(0, 1000000, 2000000, 3000000, 3600000))

ggsave("Bar Month.png", dpi = 600)

# Broken into Quarters
dfdate2 = dfdate
dfdate2

levels(dfdate2$Date)[which(levels(dfdate2$Date) == "Jan2019" | levels(dfdate2$Date) == "Feb2019" | levels(dfdate2$Date) == "Mar2019")] = "Q1 2019"
levels(dfdate2$Date)[which(levels(dfdate2$Date) == "Apr2019" | levels(dfdate2$Date) == "May2019" | levels(dfdate2$Date) == "Jun2019")] = "Q2 2019"
levels(dfdate2$Date)[which(levels(dfdate2$Date) == "Jul2019" | levels(dfdate2$Date) == "Aug2019" | levels(dfdate2$Date) == "Sep2019")] = "Q3 2019"
levels(dfdate2$Date)[which(levels(dfdate2$Date) == "Oct2019" | levels(dfdate2$Date) == "Nov2019" | levels(dfdate2$Date) == "Dec2019")] = "Q4 2019"
levels(dfdate2$Date)[which(levels(dfdate2$Date) == "Jan2020" | levels(dfdate2$Date) == "Feb2020" | levels(dfdate2$Date) == "Mar2020")] = "Q1 2020"
levels(dfdate2$Date)[which(levels(dfdate2$Date) == "Apr2020" | levels(dfdate2$Date) == "May2020" | levels(dfdate2$Date) == "Jun2020")] = "Q2 2020"
levels(dfdate2$Date)[which(levels(dfdate2$Date) == "Jul2020" | levels(dfdate2$Date) == "Aug2020" | levels(dfdate2$Date) == "Sep2020")] = "Q3 2020"
levels(dfdate2$Date)[which(levels(dfdate2$Date) == "Oct2020")] = "Q4 2020"
levels(dfdate2$Date)

# Grouping by Quarter
dfdate2 = group_by(dfdate2, Date)
dfdate2Summary = summarize(dfdate2,
                           TotalSale = sum(Sales_Dollar),
                           TotalBottleSold = sum(Bottles_Sold),
                           TotalVolumeSold = sum(Volume_Sold_G))

# TOtal sales by Quarter (transactions)
plot(dfdate2$Sales_Dollar~as.factor(dfdate2$Date),type="l",
     xlab="Date", ylab="Sales Dollars",
     main="Time trend of sales")

# Had to manually save


# Bar graph, total sales by quarter
qplot(Date,
      TotalSale,
      data = dfdate2Summary,
      geom = "col",
      fill = I("gold"),
      colour = I("black"),
      main = "Total Sales By Quarter")

ggsave("Bar Quarter.png", dpi = 600)
