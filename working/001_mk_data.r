#####################################################################################
#	Engagement: COVID-19 Analysis       	                          							    #
#	Program: 	001_mk_data.r                                						          	    #		
#	Last Update Date: 3/29/2020                                          							#
#																	                                        			    #
#	Purpose: Load and prepare data                                                    #
#																                                          					#
#	Notes: 																	                                          #
#####################################################################################


#I. Setup ----------------------------------------------------------------------------
# A. Clear environment
rm(list = ls())

# B. Set working directory
setwd("C:/users/jguinta/desktop/")

# C. Import custom functions 

# D. Import packages
require(tidyverse)
require(data.table)
require(dtplyr)
require(stringr)
require(lubridate)
require(openxlsx)
require(stringdist)
require(sqldf)


#II. Data Loading -------------------------------------------------------------------
start.date<-c("01-22-2020")
start.date<-as.Date(start.date, "%m-%d-%Y")
end.date<-c("05-03-2020")
end.date<-as.Date(end.date, "%m-%d-%Y")

k<-1
for (i in c(start.date:end.date)) {
  j<-as.Date(i, origin="1970-01-01")
  j<-format(j, "%m-%d-%Y")
  print(j)
  
  filename<-paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/", j, ".csv", sep="")
   
  tmp<-fread(filename)
  tmp[, src:=j]

  if (k==1) {
    out<-tmp
  }
    out<-bind_rows(out,tmp)
    k<-k+1
}

#III. Data Processing ---------------------------------------------------------------

#A. Standardize fields due to change in format
out[is.na(Latitude) & is.na(Lat)==FALSE, Latitude:=Lat]
out[is.na(Longitude) & is.na(Long_)==FALSE, Longitude:=Long_]
out[is.na(`Province/State`) & is.na(Province_State)==FALSE, `Province/State`:=Province_State]
out[is.na(`Last Update`) & is.na(Last_Update)==FALSE, `Last Update`:=Last_Update]
out[is.na(`Province/State`) & is.na(Province_State)==FALSE, `Province/State`:=Province_State]
out[is.na(`Country/Region`) & is.na(Country_Region)==FALSE, `Country/Region`:=Country_Region]
out[is.na(Combined_Key), Combined_Key:=paste(`Province/State`, `Country/Region`, sep=" ")]

#B. Select what we think is useful
out<-out[, .(Combined_Key, `Province/State`, `Country/Region`, `Last Update`, Confirmed, Deaths, Recovered, Active, src, Latitude, Longitude, FIPS, Admin2)]
out[Admin2=="", Admin2:=NA]

#Rename
names(out)<-c("combined_key", "province_state", "country_region", "last_update", "confirmed", "deaths", "recovered", "active", "src", "lat", "long", "fips", "admin2")

#C. Further standardization to the contents of the data
col.list<-c("combined_key", "province_state", "country_region", "src", "admin2") 

for (i in c(col.list)) {
  out[, (i):=str_trim(gsub("  ", "", tolower(get(i))))]
  
}

num.list<-c("confirmed", "deaths", "recovered", "active")
for (i in c(num.list)) {
  out[is.na(get(i))==TRUE, (i):=0]

}


#IV. Data Output ---------------------------------------------------------------
write.csv(file="./001_covid_daily.csv", out)



