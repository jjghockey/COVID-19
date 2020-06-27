#####################################################################################
#	Engagement: COVID-19 Analysis       	                          							    #
#	Program: 	003_an_data.r                                						          	    #		
#	Last Update Date: 6/26/2020                                          							#
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
require(TTR)


#II. Data Loading -------------------------------------------------------------------
covid_st_d<-fread("./002_covid_daily_us_cln.csv")

#III. Data Processing ---------------------------------------------------------------
#A. Variable transformations for plotting
covid_st_d[, region:=as.factor(region)]
covid_st_d[, src:=as.Date(src)]

#B. Filtering to Contiental/Contiguous 48
covid_st_d<-covid_st_d[state_cln!="unk" & state_cln!="ter" & state_cln!="pr" & state_cln!="dc" & state_cln!="hi" & state_cln!="ak", ]

#C. Filtering to March 2020 forward
covid_st_d<-covid_st_d[src>=as.Date("2020-03-01"), ]

#B. Tiers for Density
den_rank<-covid_st_d[, .N, by=list(den2013, state_cln)][order(den2013)]
den_rank[, ord:=1]
den_rank[, ord:=cumsum(ord)]
den_rank[ord>=1 & ord<=9, rank:=1]
den_rank[ord>=10 & ord<=18, rank:=2]
den_rank[ord>=19 & ord<=27, rank:=3]
den_rank[ord>=28 & ord<=36, rank:=4]
den_rank[ord>=37 & ord<=45, rank:=5]
den_rank[ord>=46 & ord<=48, rank:=6]
den_rank[, rank:=as.factor(rank)]

covid_st_d<-merge(covid_st_d, den_rank[, .(state_cln, rank)], by=c("state_cln"), all=FALSE)
covid_st_d[, rank:=as.factor(rank)]

#IV. Data Analysis ------------------------------------------------------------------
#A. Data Transformation 

#1. Percentage Daily Change
covid_st_d<-covid_st_d[order(-state_cln, src)]
covid_st_d[, pct_chg:=0]
covid_st_d[, pct_chg:=(confirmed-lag(confirmed))/confirmed, by=list(state_cln)]
covid_st_d[is.nan(pct_chg), pct_chg:=0]
covid_st_d[is.na(pct_chg), pct_chg:=0]
covid_st_d[, SMA_pct_chg:=SMA(pct_chg,7)]

#2. Log Returns
covid_st_d[, ret:=log(infections_pp100)-log(lag(infections_pp100)), by=list(state_cln)]
covid_st_d[is.nan(ret), ret:=0]
covid_st_d[is.na(ret), ret:=0]


#A. Graphs
p<-ggplot(covid_st_d[state_cln!="unk" & state_cln!="ter"], aes(x=src, y=infections_pp100, color="red"))+geom_line()+facet_wrap(~state_cln, ncol=6, nrow=9)
p<-p+theme_classic()+theme(legend.position = "none")
p1<-p+labs(title="Infection Rate per 100,000", subtitle="United States by State Code \n (Removing Territories)", x="Date", y="Infections Per 100,000")

p<-ggplot(covid_st_d[state_cln!="unk" & state_cln!="ter"], aes(x=src, y=infections_pp100_den, color="red"))+geom_line()+facet_wrap(~state_cln, ncol=6, nrow=9)
p<-p+theme_classic()+theme(legend.position = "none")
p2<-p+labs(title="Infection Rate per 100,000 \n Weighted by Population Density", subtitle="United States by State Code \n (Removing Territories)", x="Date", y="Infections Per 100,000 Per Population Density")

p<-ggplot(covid_st_d[state_cln!="unk" & state_cln!="ter"], aes(x=src, y=infections_pp100, color="red"))+geom_line()+facet_wrap(~state_cln, ncol=6, nrow=9, scales="free")
p<-p+theme_classic()+theme(legend.position = "none")
p3<-p+labs(title="Infection Rate per 100,000", subtitle="United States by State Code \n (Removing Territories)", x="Date", y="Infections Per 100,000")

p<-ggplot(covid_st_d, aes(x=src, y=ret, color=rank))+geom_line()+facet_wrap(~state_cln, ncol=8, nrow=6)
p<-p+theme_classic()+theme(legend.position = "none")
p4<-p+labs(title="Rate of Infections Rate per 100,000", subtitle="United States by State Code", x="Date", y="Daily Rate of Infections Per 100,000")

p<-ggplot(covid_st_d[state_cln %in% c("ca", "ny", "il", "az", "fl", "tx")], aes(x=src, y=SMA_pct_chg))+geom_line()+facet_wrap(~state_cln, ncol=3, nrow=3)
p<-p+theme_classic()+theme(legend.position = "none")
p4<-p+labs(title="Percent Change in Infections Rate per 100,000", subtitle="United States by State Code", x="Date", y="7 Day Moving Average Percentage Change in the Rate of Infections Per 100,000")

p<-ggplot(covid_st_d[state_cln %in% c("ca", "ny", "il", "az", "fl", "tx")], aes(x=src, y=log(infections_pp100)))+geom_line()+facet_wrap(~state_cln, ncol=3, nrow=3)
p<-p+theme_classic()+theme(legend.position = "none")
p5<-p+labs(title="Infections Rate per 100,000", subtitle="United States by State Code", x="Date", y="LN(Infections Per 100,000)")


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
