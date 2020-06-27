#####################################################################################
#	Engagement: COVID-19 Analysis       	                          							    #
#	Program: 	004_an_data_us_cln.r                           						          	    #		
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
require(directlabels)


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
  covid_st_d[, SMA_pct_chg:=SMA(pct_chg,14), by=list(state_cln)]
  
  #2. Log Returns
  covid_st_d[, ret:=log(infections_pp100)-log(lag(infections_pp100)), by=list(state_cln)]
  covid_st_d[is.nan(ret), ret:=0]
  covid_st_d[is.na(ret), ret:=0]
  
  #3. Stay-at-Home flag
  covid_st_d[, `Stay-at-Home`:="Early"]
  covid_st_d[state_cln %in% c("tx", "fl", "az"), `Stay-at-Home`:="Late"]
  covid_st_d[state_cln %in% c("mi"), `Stat-at-Home`:="Protest"]

#B. Graphs
  
#Infections 
p<-ggplot(covid_st_d[state_cln %in% c("ca", "ny", "il", "az", "fl", "tx","mi")], aes(x=src, y=infections_pp100, color=state_cln))+geom_line()+facet_wrap(~state_cln, ncol=3, nrow=3, scales="free")
p<-p+theme_classic()+theme(legend.position = "none")
p<-p+scale_color_manual(values=c("red", "dark red", "grey", "blue", "black", "light blue", "black"))
p1<-p+labs(title="Infection Rate per 100,000", subtitle="United States by State Code", x="Date", y="Infections Per 100,000")

p<-ggplot(covid_st_d[state_cln %in% c("ca", "ny", "il", "az", "fl", "tx","mi")], aes(x=src, y=infections_pp100, color=state_cln))+geom_line()+facet_wrap(~state_cln, ncol=3, nrow=3)
p<-p+theme_classic()+theme(legend.position = "none")
p<-p+scale_color_manual(values=c("red", "dark red", "grey", "blue", "black", "light blue", "black"))
p2<-p+labs(title="Infection Rate per 100,000", subtitle="United States by State Code", x="Date", y="Infections Per 100,000")

p<-ggplot(covid_st_d[state_cln %in% c("ca", "ny", "il", "az", "fl", "tx","mi") & src>=as.Date("2020-03-15")], aes(x=src, y=SMA_pct_chg, color=state_cln))+geom_line()+facet_wrap(~state_cln, ncol=3, nrow=3)
p<-p+theme_classic()+theme(legend.position = "none")
p<-p+scale_color_manual(values=c("red", "dark red", "grey", "blue", "black", "light blue", "black"))
p3<-p+labs(title="Percent Change in Infections Rate per 100,000", subtitle="United States by State Code", x="Date", y="14 Day Moving Average Percentage Change in the Rate of Infections Per 100,000")

p<-ggplot(covid_st_d[state_cln %in% c("ca", "ny", "il", "az", "fl", "tx","mi")], aes(x=src, y=log(infections_pp100), color=state_cln))+geom_line()+facet_wrap(~state_cln, ncol=3, nrow=3)
p<-p+theme_classic()+theme(legend.position = "none")
p<-p+scale_color_manual(values=c("red", "dark red", "grey", "blue", "black", "light blue", "black"))
p4<-p+labs(title="Infections Rate per 100,000", subtitle="United States by State Code", x="Date", y="LN(Infections Per 100,000)")

p<-ggplot(covid_st_d[state_cln %in% c("ca", "ny", "il", "az", "fl", "tx","mi")], aes(x=src, y=log(infections_pp100),group=state_cln, color=state_cln))+geom_line()
p<-p+theme_classic()+theme(legend.position = "bottom")
p<-p+scale_color_manual(values=c("red", "dark red", "grey", "blue", "black", "light blue", "black"))
p<-p+labs(title="Infections Rate per 100,000", subtitle="United States by State Code", x="Date", y="LN(Infections Per 100,000)")
p5<-p+geom_dl(aes(label=state_cln), method = "last.bumpup")

p<-ggplot(covid_st_d[state_cln %in% c("ca", "ny", "il", "az", "fl", "tx","mi") & src>=as.Date("2020-03-15")], aes(x=src, y=SMA_pct_chg,group=state_cln, color=`Stay-at-Home`))+geom_line()
p<-p+theme_classic()+theme(legend.position = "bottom")
p<-p+scale_color_manual(values=c("red", "grey", "black"))
p<-p+labs(title="Infections Rate per 100,000", subtitle="United States by State Code", x="Date", y="14 Day Moving Average Percentage Change in the Rate of Infections Per 100,000")
p6<-p+geom_dl(aes(label=state_cln), method = "last.bumpup")

#Deaths
#Infections 
p<-ggplot(covid_st_d[state_cln %in% c("ca", "ny", "il", "az", "fl", "tx","mi")], aes(x=src, y=infections_pp100, color=state_cln))+geom_line()+facet_wrap(~state_cln, ncol=3, nrow=3, scales="free")
p<-p+theme_classic()+theme(legend.position = "none")
p<-p+scale_color_manual(values=c("red", "dark red", "grey", "blue", "black", "light blue", "black"))
p1<-p+labs(title="Infection Rate per 100,000", subtitle="United States by State Code", x="Date", y="Infections Per 100,000")

p<-ggplot(covid_st_d[state_cln %in% c("ca", "ny", "il", "az", "fl", "tx","mi")], aes(x=src, y=infections_pp100, color=state_cln))+geom_line()+facet_wrap(~state_cln, ncol=3, nrow=3)
p<-p+theme_classic()+theme(legend.position = "none")
p<-p+scale_color_manual(values=c("red", "dark red", "grey", "blue", "black", "light blue", "black"))
p2<-p+labs(title="Infection Rate per 100,000", subtitle="United States by State Code", x="Date", y="Infections Per 100,000")

p<-ggplot(covid_st_d[state_cln %in% c("ca", "ny", "il", "az", "fl", "tx","mi") & src>=as.Date("2020-03-15")], aes(x=src, y=SMA_pct_chg, color=state_cln))+geom_line()+facet_wrap(~state_cln, ncol=3, nrow=3)
p<-p+theme_classic()+theme(legend.position = "none")
p<-p+scale_color_manual(values=c("red", "dark red", "grey", "blue", "black", "light blue", "black"))
p3<-p+labs(title="Percent Change in Infections Rate per 100,000", subtitle="United States by State Code", x="Date", y="14 Day Moving Average Percentage Change in the Rate of Infections Per 100,000")

p<-ggplot(covid_st_d[state_cln %in% c("ca", "ny", "il", "az", "fl", "tx","mi")], aes(x=src, y=log(infections_pp100), color=state_cln))+geom_line()+facet_wrap(~state_cln, ncol=3, nrow=3)
p<-p+theme_classic()+theme(legend.position = "none")
p<-p+scale_color_manual(values=c("red", "dark red", "grey", "blue", "black", "light blue", "black"))
p4<-p+labs(title="Infections Rate per 100,000", subtitle="United States by State Code", x="Date", y="LN(Infections Per 100,000)")

p<-ggplot(covid_st_d[state_cln %in% c("ca", "ny", "il", "az", "fl", "tx","mi")], aes(x=src, y=log(infections_pp100),group=state_cln, color=state_cln))+geom_line()
p<-p+theme_classic()+theme(legend.position = "bottom")
p<-p+scale_color_manual(values=c("red", "dark red", "grey", "blue", "black", "light blue", "black"))
p<-p+labs(title="Infections Rate per 100,000", subtitle="United States by State Code", x="Date", y="LN(Infections Per 100,000)")
p5<-p+geom_dl(aes(label=state_cln), method = "last.bumpup")

p<-ggplot(covid_st_d[state_cln %in% c("ca", "ny", "il", "az", "fl", "tx","mi") & src>=as.Date("2020-03-15")], aes(x=src, y=SMA_pct_chg,group=state_cln, color=`Stay-at-Home`))+geom_line()
p<-p+theme_classic()+theme(legend.position = "bottom")
p<-p+scale_color_manual(values=c("red", "grey", "black"))
p<-p+labs(title="Infections Rate per 100,000", subtitle="United States by State Code", x="Date", y="14 Day Moving Average Percentage Change in the Rate of Infections Per 100,000")
p6<-p+geom_dl(aes(label=state_cln), method = "last.bumpup")


#Infection Rate
dta<-covid_st_d[src==max(covid_st_d$src),.(state_cln, infections_pp100, infections_pp100_den)]
ord<-dta[, .(state_cln, infections_pp100_den)][order(-infections_pp100_den)][, .(state_cln)] %>% pull()
dta[, state_cln:=as.character(state_cln)]
dta[, state_cln:=factor(state_cln, levels=c(ord))]

p<-ggplot(dta, aes(x=state_cln, y=infections_pp100_den))+geom_bar(stat="identity")
p<-p+theme_classic()
p<-p+labs(title="Population Density Weighted Infection Rate per 100,000", subtitle="United States by State Code", x="State", y=paste("Infection Rate as of", max(covid_st_d$src), sep=" " ))
p7<-p+coord_flip()

dta<-covid_st_d[src==max(covid_st_d$src),.(state_cln, infections_pp100, infections_pp100_den)]
ord<-dta[, .(state_cln, infections_pp100)][order(-infections_pp100)][, .(state_cln)] %>% pull()
dta[, state_cln:=as.character(state_cln)]
dta[, state_cln:=factor(state_cln, levels=c(ord))]

p<-ggplot(dta, aes(x=state_cln, y=infections_pp100))+geom_bar(stat="identity")
p<-p+theme_classic()
p<-p+labs(title="Infection Rate per 100,000", subtitle="United States by State Code", x="State", y=paste("Infection Rate as of", max(covid_st_d$src), sep=" " ))
p8<-p+coord_flip()

ggsave(file="./p1.png", p1)
ggsave(file="./p2.png", p2)
ggsave(file="./p3.png", p3)
ggsave(file="./p4.png", p4)
ggsave(file="./p5.png", p5)
ggsave(file="./p6.png", p6)
ggsave(file="./p7.png", p7)
ggsave(file="./p8.png", p8)
