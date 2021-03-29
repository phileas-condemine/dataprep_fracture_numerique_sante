library(data.table)
library(readxl)
library(dplyr)
library(plotly)
library(sf)
library(ggplot2)
library(pbapply)
library(rgdal)
library(rgeos)
library(sp)

# http://www.data.drees.sante.gouv.fr/ReportFolders/reportFolders.aspx?IF_ActivePath=P,490,530 => javascript:OnDocumentClick(4476,111,4476);
APL = read_excel("data/APL2018MG.xlsx",sheet="APL_2018",skip = 7)
APL = data.table(APL)
APL = APL[-1]
names(APL) <- c("cod_com","lib_com","apl_mg2018","apl_mg2018_65","pop2016")
APL = APL %>% mutate_at(c(3,4,5),as.numeric)%>%data.table
APLs = APL

APL = read_excel("data/APL2016MG.xls",skip = 7,sheet="APL")
APL = data.table(APL)
APL = APL[-1]
names(APL) <- c("cod_com","lib_com","apl_mg2016","apl_mg2016_65","pop2014")
APL$lib_com = NULL
APL = APL %>% mutate_at(c(2,3,4),as.numeric)%>%data.table
APLs = merge(APLs,APL,by="cod_com",all.x=T,all.y=T)

APL = read_excel("data/APL2016INF.xls",skip = 7,sheet="APL")
APL = data.table(APL)
APL = APL[-1]
names(APL) <- c("cod_com","lib_com","apl_inf2016","pop2014")
APL$pop2014 = NULL
APL$lib_com = NULL
APL = APL %>% mutate_at(2,as.numeric)%>%data.table
APLs = merge(APLs,APL,by="cod_com",all.x=T,all.y=T)

APL = read_excel("data/APL2016SF.xls",skip = 7,sheet="APL")
APL = data.table(APL)
APL = APL[-1]
names(APL) <- c("cod_com","lib_com","apl_sf2016","pop2014")
APL$pop2014 = NULL
APL$lib_com = NULL
APL = APL %>% mutate_at(2,as.numeric)%>%data.table
APLs = merge(APLs,APL,by="cod_com",all.x=T,all.y=T)

APL = read_excel("data/APL2016MK.xls",skip = 7,sheet="APL")
APL = data.table(APL)
APL = APL[-1]
names(APL) <- c("cod_com","lib_com","apl_mk2016","pop2014")
APL$pop2014 = NULL
APL$lib_com = NULL
APL = APL %>% mutate_at(2,as.numeric)%>%data.table
APLs = merge(APLs,APL,by="cod_com",all.x=T,all.y=T)

save(APLs,file="data/APLs.RData")

