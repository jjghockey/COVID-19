#####################################################################################
#	Engagement: COVID-19 Analysis       	                          							    #
#	Program: 	002_an_data.r                                						          	    #		
#	Last Update Date: 3/29/2020                                          							#
#																	                                        			    #
#	Purpose: Build Visuals                                                    #
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
require(RCurl)
require(rvest)
require(jsonlite)


#II. Data Loading -------------------------------------------------------------------
covid<-fread("./001_covid_daily.csv")

#III. Data Processing ---------------------------------------------------------------
#A. Filter to US

covid[, us_flg:="not us"]
covid[grepl(",us", combined_key)==TRUE, us_flg:="us"]
covid[grepl(" us", combined_key)==TRUE, us_flg:="us"]
covid<-covid[us_flg=="us"]

#B. Transform variables
covid[, src:=as.Date(src, "%m-%d-%Y")]

#C. Clean Region / State

#1. Cleaning 
covid[grepl(",", province_state)==TRUE, .N, province_state]
covid[grepl(",", province_state)==TRUE, state_cln:=substr(province_state,regexpr(",", province_state)+1,255)]
covid[is.na(state_cln), state_cln:=province_state]
covid[, state_cln:=gsub("\\(from diamond princess\\)", "", state_cln )]
covid[state_cln=="chicago", state_cln:="il"]  #Chicago is in Illinois
covid[, state_cln:=str_trim(state_cln)]

for (i in 1:10) {
  covid[, state_cln:=gsub("  ", " ", state_cln)]
}
covid[, state_cln:=str_trim(state_cln)]

#2. Build state-by-abb file
st<-cbind(state.name, state.abb) %>% as.data.table()
st[, state.name:=tolower(state.name)]
st[, state.abb:=tolower(state.abb)]

covid<-merge(covid, st, by.x=c("state_cln"), by.y=c("state.name"), all.x=TRUE)
covid[is.na(state.abb)==FALSE, state_cln:=state.abb]

#3. Specific coding
covid[state_cln %in% c("d.c.", "district of columbia"), state_cln:="dc", ]
covid[state_cln %in% c("u.s.", "diamond princess", "grand princess", "unassigned location", "recovered", "us", "grand princess cruise ship","wuhan evacuee" ), state_cln:="unk"]
covid[state_cln %in% c("puerto rico"), state_cln:="pr", ]
covid[state_cln %in% c("american samoa", "guam", "northern mariana islands", "united states virgin islands", "virgin islands"), state_cln:="ter"]

#C. State Population
url <- "https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States_by_population"

#1. Scrape Webpage
wp<- read_html(url) %>% html_nodes("table.wikitable") %>% html_table(header=T, fill=TRUE) 

wp_alt<-wp[[3]] #Gives region codes by state
wp_alt<-wp_alt[, c(1,10)] #Gives region codes by state
names(wp_alt)<-c("state.name", "region")
wp_alt<-as.data.table(wp_alt)
wp_alt[, state.name:=tolower(state.name)]

wp<-wp[[1]]
wp<-wp[2:57, c(3,4)]
names(wp)<-c("state.name", "pop2019")
wp<-as.data.table(wp)
wp[, pop2019:=as.numeric(gsub(",", "", pop2019))]
wp[, state.name:=tolower(state.name)]

wp<-merge(wp, wp_alt, by=c("state.name"), all.x=TRUE)

wp<-merge(wp, st, by=c("state.name"), all.x=TRUE)
wp[state.name=="district of columbia", state.abb:="dc"]
wp[state.name=="puerto rico", state.abb:="pr"]
wp[is.na(state.abb)==TRUE, state.abb:="ter"]
wp<-wp[, list(pop2019=sum(pop2019)), by=list(state_cln=state.abb, region=tolower(region))]


#2. Add to covid data
covid<-merge(covid, wp, by=c("state_cln"), all.x=TRUE)

#D. Build Daily Cases / State
covid_st_d<-covid[, list(confirmed=sum(confirmed, na.rm=TRUE)
                         , deaths=sum(deaths, na.rm=TRUE)
                         , recovered=sum(recovered, na.rm=TRUE)
                         , active=sum(active, na.rm=TRUE)
                         , pop2019=max(pop2019, na.rm=TRUE)
                    ), by=list(src, state_cln, region)]

covid_st_d[, infections_pp100:=(confirmed/pop2019)*100000]  #http://health.utah.gov/epi/diseases/HAI/resources/Cal_Inf_Rates.pdf


#IV. Data Analysis

#A. Graphs
p<-ggplot(covid_st_d[state_cln!="unk" & state_cln!="ter"], aes(x=src, y=infections_pp100, color="red"))+geom_line()+facet_wrap(~state_cln, ncol=6, nrow=9)
p<-p+theme_classic()+theme(legend.position = "none")
p1<-p+labs(title="Infection Rate per 100,000", subtitle="United States by State Code \n (Removing Territories)", x="Date", y="Infections Per 100,000")


p<-ggplot(covid_st_d[state_cln!="unk" & state_cln!="ter" & region=="pac"], aes(x=src, y=infections_pp100, color="red"))+geom_line()+facet_wrap(~state_cln)
p<-p+theme_classic()+theme(legend.position = "none")
p2<-p+labs(title="Infection Rate per 100,000", subtitle="Pacific Region by State Code \n (Removing Territories)", x="Date", y="Infections Per 100,000")

p<-ggplot(covid_st_d[state_cln!="unk" & state_cln!="ter" & region=="matl"], aes(x=src, y=infections_pp100, color="red"))+geom_line()+facet_wrap(~state_cln)
p<-p+theme_classic()+theme(legend.position = "none")
p3<-p+labs(title="Infection Rate per 100,000", subtitle="Mid-Atlantic Region by State Code \n (Removing Territories)", x="Date", y="Infections Per 100,000")

#B. Many-By-Many (Heatmap)
tbl<-covid_st_d[state_cln!="unk" & state_cln!="ter", list(infection_pp100=(sum(confirmed)/pop2019)*100000), by=list(src, state_cln)]
tbl<-melt(tbl, id.vars = c("state_cln", "src"))

tbl<-tbl %>% mutate(rescale=scale(value)) %>% as.data.table()
tbl[, src2:=as.factor(src)]
ord<-tbl[order(-rescale, state_cln)][, .(state_cln)] %>% unique() %>% pull()
tbl[, state_cln:=factor(state_cln, levels=c(ord))]

p<-ggplot(tbl[src>=as.Date("2020-03-01")], aes(x=src2, y=state_cln))+geom_tile(aes(fill=rescale), colour="white")
p<-p+scale_fill_gradient(low="white", high="red")+theme_classic()
p<-p+theme(legend.position="none")
p<-p+labs(title="Infection Rate per 100,000", subtitle="United States by State Code \n (Removing Territories)", x="Date (Since 3/1/2020)", y="State")
p<-p+theme(axis.text.x = element_text(angle = 90, hjust = 1))
p4<-p
