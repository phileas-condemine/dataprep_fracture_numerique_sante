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


#### SENSIBILITE A LAMBDA TRADEOFF SOIN-NUMERIQUE ####
load("data/communes_scored.RData")
communes[,dep:=substr(code,1,2)]
lambdas = 0:40/40
lambda= 0
lambda=lambdas[2]
diffs_per_dep = NULL
for (lambda in lambdas){
  communes[,combo_couvertures := (lambda*(apl_mg2018/3+apl_mk2016/50+apl_inf2016/90+apl_sf2016/9)/4+(1-lambda)*(tx_4G_max+tx_3G_max)/2)]
  summary(communes$combo_couvertures)
  communes[,couvertures_tranches := floor(10*rank(combo_couvertures)/(.N+1)),by=dep]
  if (lambda >0){
    diff_per_dep = communes[,.(diff=sum(couvertures_tranches!=ranking_seuil_precedent)),by="dep"]
    diff_per_dep$lambda = lambda
    print(sum(diff_per_dep$diff))
    diffs_per_dep = rbind(diffs_per_dep,diff_per_dep)
  }
  
  communes[,ranking_seuil_precedent := couvertures_tranches]
}
save(diffs_per_dep,file="data/useful_thresholds.RData")

