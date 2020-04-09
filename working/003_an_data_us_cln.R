#####################################################################################
#	Engagement: COVID-19 Analysis       	                          							    #
#	Program: 	003_an_data.r                                						          	    #		
#	Last Update Date: 3/29/2020                                          							#
#																	                                        			    #
#	Purpose: Build Visuals                                                            #
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
covid_st_d<-fread("./002_covid_daily_us_cln.csv")

#III. Data Processing ---------------------------------------------------------------
#covid_st_d[, state_cln:=as.factor(state_cln)]
covid_st_d[, region:=as.factor(region)]
covid_st_d[, src:=as.Date(src)]

#IV. Data Analysis ------------------------------------------------------------------

#A. Graphs
p<-ggplot(covid_st_d[state_cln!="unk" & state_cln!="ter"], aes(x=src, y=infections_pp100, color="red"))+geom_line()+facet_wrap(~state_cln, ncol=6, nrow=9)
p<-p+theme_classic()+theme(legend.position = "none")
p1<-p+labs(title="Infection Rate per 100,000", subtitle="United States by State Code \n (Removing Territories)", x="Date", y="Infections Per 100,000")

p<-ggplot(covid_st_d[state_cln!="unk" & state_cln!="ter"], aes(x=src, y=infections_pp100_den, color="red"))+geom_line()+facet_wrap(~state_cln, ncol=6, nrow=9)
p<-p+theme_classic()+theme(legend.position = "none")
p1_alt<-p+labs(title="Infection Rate per 100,000 \n Weighted by Population Density", subtitle="United States by State Code \n (Removing Territories)", x="Date", y="Infections Per 100,000 Per Population Density")


p<-ggplot(covid_st_d[state_cln!="unk" & state_cln!="ter" & region=="pac"], aes(x=src, y=infections_pp100, color="red"))+geom_line()+facet_wrap(~state_cln)
p<-p+theme_classic()+theme(legend.position = "none")
p2<-p+labs(title="Infection Rate per 100,000", subtitle="Pacific Region by State Code \n (Removing Territories)", x="Date", y="Infections Per 100,000")

p<-ggplot(covid_st_d[state_cln!="unk" & state_cln!="ter" & region=="matl"], aes(x=src, y=infections_pp100, color="red"))+geom_line()+facet_wrap(~state_cln)
p<-p+theme_classic()+theme(legend.position = "none")
p3<-p+labs(title="Infection Rate per 100,000", subtitle="Mid-Atlantic Region by State Code \n (Removing Territories)", x="Date", y="Infections Per 100,000")

p<-ggplot(covid_st_d[state_cln!="unk" & state_cln!="ter" & region=="satl"], aes(x=src, y=infections_pp100, color="red"))+geom_line()+facet_wrap(~state_cln)
p<-p+theme_classic()+theme(legend.position = "none")
p4<-p+labs(title="Infection Rate per 100,000", subtitle="South-Atlantic Region by State Code \n (Removing Territories)", x="Date", y="Infections Per 100,000")


#B. Many-By-Many (Heatmap)
#Infections per 100,000
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
p5<-p

#Infections per 100,000 per Population Density
tbl<-covid_st_d[state_cln!="unk" & state_cln!="ter", list(infection_pp100=(sum(confirmed)/pop2019)*100000/den2013), by=list(src, state_cln)]
tbl<-melt(tbl, id.vars = c("state_cln", "src"))

tbl<-tbl %>% mutate(rescale=scale(value)) %>% as.data.table()
tbl[, src2:=as.factor(src)]
ord<-tbl[order(-rescale, state_cln)][, .(state_cln)] %>% unique() %>% pull()
tbl[, state_cln:=factor(state_cln, levels=c(ord))]

p<-ggplot(tbl[src>=as.Date("2020-03-01")], aes(x=src2, y=state_cln))+geom_tile(aes(fill=rescale), colour="white")
p<-p+scale_fill_gradient(low="white", high="red")+theme_classic()
p<-p+theme(legend.position="none")
p<-p+labs(title="Infection Rate per 100,000 \n Weighted by Population Density", subtitle="United States by State Code \n (Removing Territories)", x="Date (Since 3/1/2020)", y="State")
p<-p+theme(axis.text.x = element_text(angle = 90, hjust = 1))
p5_alt<-p

tbl<-covid_st_d[state_cln!="unk" & state_cln!="ter", list(infection_pp100=(sum(confirmed)/reg_pop2019)*100000), by=list(src, region)]
tbl<-melt(tbl, id.vars = c("region", "src"))

tbl<-tbl %>% mutate(rescale=scale(value)) %>% as.data.table()
tbl[, src2:=as.factor(src)]
ord<-tbl[order(-rescale, region)][, .(region)] %>% unique() %>% pull()
tbl[, state_cln:=factor(region, levels=c(ord))]

p<-ggplot(tbl[src>=as.Date("2020-03-01")], aes(x=src2, y=state_cln))+geom_tile(aes(fill=rescale), colour="white")
p<-p+scale_fill_gradient(low="white", high="red")+theme_classic()
p<-p+theme(legend.position="none")
p<-p+labs(title="Infection Rate per 100,000", subtitle="United States by Region \n (Removing Territories)", x="Date (Since 3/1/2020)", y="Region")
p<-p+theme(axis.text.x = element_text(angle = 90, hjust = 1))
p6<-p
