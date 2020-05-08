#####################################################################################
#	Engagement: COVID-19 Analysis       	                          				#
#	Program: 	007_an_data_us_cln_Pres.r                      						#		
#	Last Update Date:5/6/2020                                           			#
#																	                #
#	Purpose: Build Visuals                                                          #
#																                    #
#	Notes: 	 Sample Graphics for Presentation										#
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

#IV. Data Analysis ------------------------------------------------------------------

#A. Graphs

#Infection Rate
dta<-covid_st_d[src==max(covid_st_d$src),.(state_cln, infections_pp100)]
ord<-dta[, .(state_cln, infections_pp100)][order(-infections_pp100)][, .(state_cln)] %>% pull()
dta[, state_cln:=as.character(state_cln)]
dta[, state_cln:=factor(state_cln, levels=c(ord))]

p<-ggplot(dta, aes(x=state_cln, y=infections_pp100))+geom_bar(stat="identity")
p<-p+theme_classic()
p<-p+labs(title="Infection Rate per 100,000", subtitle="United States by State Code", x="State", y=paste("Infection Rate as of", max(covid_st_d$src), sep=" " ))
p1<-p+coord_flip()
p1

p<-ggplot(covid_st_d[state_cln %in% c("ca", "ny", "il", "az", "fl", "tx")], aes(x=src, y=infections_pp100, color=state_cln))+geom_line()+facet_wrap(~state_cln, ncol=3, nrow=3, scales="free")
p<-p+theme_classic()+theme(legend.position = "none")
p<-p+scale_color_manual(values=c("red", "dark red", "grey", "blue", "black", "light blue", "black"))
p2<-p+labs(title="Infection Rate per 100,000", subtitle="United States by State Code", x="Date", y="Infections Per 100,000")
p2
p2+scale_y_continuous(trans='log')

p<-ggplot(covid_st_d[state_cln %in% c("ca", "ny", "il", "az", "fl", "tx")], aes(x=src, y=infections_pp100, color=state_cln))+geom_line()
p<-p+theme_classic()+theme(legend.position = "none")
p<-p+scale_color_manual(values=c("red", "dark red", "grey", "blue", "black", "light blue", "black"))
p3<-p+labs(title="Infection Rate per 100,000", subtitle="United States by State Code", x="Date", y="Infections Per 100,000")
p3
p3+scale_y_continuous(trans='log')


