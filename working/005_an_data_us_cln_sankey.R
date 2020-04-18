#####################################################################################
#	Engagement: COVID-19 Analysis       	                          			    #
#	Program: 	005_an_data_us_cln_sankey.r                    			       	    #		
#	Last Update Date: 3/29/2020                                         			#
#																	                #
#	Purpose: Build Visuals                                                          #
#																                 	#
#	Notes: 																	        #
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
require(ggalluvial)


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
  
#B. Set up data for Sankey Diagrams
  #1. Limit variables
  covid_cate<-covid_st_d[, .(state_cln, src, confirmed, deaths, recovered)]

  #2. Reformat counts as change of value instead of cumlative value
  setkey(covid_cate, state_cln, src)
  covid_cate[, confirmed_chg:=confirmed-lag(confirmed), by=state_cln]
  covid_cate[is.na(confirmed_chg)==TRUE, confirmed_chg:=0]

  covid_cate[, deaths_chg:=deaths-lag(deaths), by=state_cln]
  covid_cate[is.na(deaths_chg)==TRUE, deaths_chg:=0]
  
  covid_cate[, recovered_chg:=recovered-lag(recovered), by=state_cln]
  covid_cate[is.na(recovered_chg)==TRUE, recovered_chg:=0]
  
  covid_cate<-covid_cate[, .(state_cln, src, confirmed, confirmed_chg, deaths, deaths_chg, recovered, recovered_chg)]
  covid_cate_m<-melt(covid_cate[, .(state_cln, src, confirmed=confirmed_chg, deaths=deaths_chg, recovered=recovered_chg)], id.vars=c("state_cln", "src"))  #Change over time
  covid_cate_m_pp<-melt(covid_cate[, .(state_cln, src, confirmed, deaths, recovered)], id.vars=c("state_cln", "src")) #Point in time
    
#C. Graphs
A_col <- "deepskyblue3" 
B_col <- "firebrick3"
alpha <- 0.5
  
dta<-as.data.frame(covid_cate_m_pp[as.Date(as.character(src))==max(as.Date("2020-04-16")) & variable!="recovered"])

p9<-ggplot(dta,
       aes(y = value, axis1 = state_cln, axis2 = variable)) +
  geom_alluvium(aes(fill = variable, color = variable), 
                width = 1/12, alpha = alpha, knot.pos = 0.4) +
  geom_stratum(width = 1/6, color = "grey") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  scale_x_continuous(breaks = 1:2, labels = c("state_cln", "variable"))     +
  scale_fill_manual(values  = c(A_col, B_col)) +
  scale_color_manual(values = c(A_col, B_col)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold")
  ) + labs(title="Confirmed Cases and Deaths By State",subtitle=paste("All Cases as of", max(dta$src), sep=" "))+ theme(legend.position = "none")

dta<-covid_cate_m[state_cln=="ca" & variable!="recovered"]
dta<-dta[order(src)]
dta[variable=="confirmed", day:=1]
dta[variable=="deaths", day:=0]
dta[, day:=cumsum(day)]
dta[, src:=day]

p10<-ggplot(dta,
       aes(y = value, axis1 = src, axis2 = variable)) +
  geom_alluvium(aes(fill = variable, color = variable), 
                width = 1/12, alpha = alpha, knot.pos = 0.4) +
  geom_stratum(width = 1/6, color = "grey") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  #scale_x_continuous(breaks = 1:2, labels = c("src", "variable"))     +
  scale_fill_manual(values  = c(A_col, B_col)) +
  scale_color_manual(values = c(A_col, B_col)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold")
  ) + coord_flip()+theme(legend.position = "none")+ theme(axis.text.y = element_blank()) + 
  labs(title="California Confirmed Cases and Deaths over Time")


dta<-covid_cate_m[state_cln=="ny" & variable!="recovered"]
dta<-dta[order(src)]
dta[variable=="confirmed", day:=1]
dta[variable=="deaths", day:=0]
dta[, day:=cumsum(day)]
dta[, src:=day]

p11<-ggplot(dta,
          aes(y = value, axis1 = src, axis2 = variable)) +
  geom_alluvium(aes(fill = variable, color = variable), 
                width = 1/12, alpha = alpha, knot.pos = 0.4) +
  geom_stratum(width = 1/6, color = "grey") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  #scale_x_continuous(breaks = 1:2, labels = c("src", "variable"))     +
  scale_fill_manual(values  = c(A_col, B_col)) +
  scale_color_manual(values = c(A_col, B_col)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 12, face = "bold")
  ) + coord_flip()+theme(legend.position = "none")+ theme(axis.text.y = element_blank()) + 
  labs(title="New York Confirmed Cases and Deaths over Time")

ggsave(file="./p9.png", p9)
ggsave(file="./p10.png", p10)
ggsave(file="./p11.png", p11)