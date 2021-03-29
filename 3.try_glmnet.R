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
# https://www.arcep.fr/fileadmin/cru-1582035919/user_upload/Open_Data/liste-communes-bande_700_MHz-dec2015.xlsx 
# destfile="data/calendrierBande700MHz.xlsx"
arcep = read_excel("data/calendrierBande700MHz.xlsx",col_types = "text")[,1:5]%>%data.table
names(arcep) <- c("cod_com","lib_com","calendrier","ZDP","ZB")
arcep[,'cod_com':=stringi::stri_pad_left(cod_com,5,"0")]
arcep[ZB=="ZB*",ZB:="ZB"]
table(arcep$ZDP)
table(arcep$ZB)
arcep[,zone:="OK"]
arcep[ZDP=="ZDP",zone:="ZDP"]#d'abord ZDP
arcep[ZB=="ZB",zone:="ZB"]#puis on raffine en ZB qui sont bien sûr ZDP
table(arcep$zone)
arcep[,zone:=factor(zone,levels=c("ZB","ZDP","OK"))]
# http://www.data.drees.sante.gouv.fr/ReportFolders/reportFolders.aspx?IF_ActivePath=P,490,530 => javascript:OnDocumentClick(4476,111,4476);

APL = read_excel("data/APL2018MG.xlsx",sheet="APL_2018",skip = 7)
APL = data.table(APL)
APL = APL[-1]
names(APL) <- c("cod_com","lib_com","apl_mg","apl_mg65","pop2016")
APL = APL %>% mutate_at(c(3,4,5),as.numeric)%>%data.table


comp = merge(arcep[,.(cod_com,lib_com,zone)],APL[,.(cod_com,apl_mg,pop2016)],by="cod_com")
stats = comp[,.(factor(paste0("q",0:10),levels=paste0("q",0:10)),quantile(apl_mg,0:10/10)),by="zone"]
# comp[,lapply(0:10/10,function(x)quantile(apl_mg,x)),by="zone"]

plot_ly(data = stats,x=~V1,y=~V2,color=~zone)



stats = comp[,.(factor(paste0("q",0:10),levels=paste0("q",0:10)),quantile(log(pop2016),0:10/10)),by="zone"]
# comp[,lapply(0:10/10,function(x)quantile(apl_mg,x)),by="zone"]

plot_ly(data = stats,x=~V1,y=~V2,color=~zone)


cor(comp$apl_mg,comp$pop2016)
cor(comp$apl_mg,as.numeric(comp$zone))
cor(comp$pop2016,as.numeric(comp$zone))


library(glmnet)
train_id = sample(nrow(comp),.7*nrow(comp))
train = as.matrix(comp[train_id,.(apl_mg,pop2016)])
test = as.matrix(comp[-train_id,.(apl_mg,pop2016)])

y_train = as.numeric(comp[train_id]$zone)
y_test = as.numeric(comp[-train_id]$zone)
model = glmnet::glmnet(x=train,y=y_train,family="multinomial")

coefs = glmnet::coef.glmnet(model)

coefs = data.table(as.matrix(t(coefs[['3']])))
setnames(coefs,"V1","intercept")

pred = predict(model,newx = test,type="response")
perf = lapply(1:dim(pred)[3],function(i){
  data.table(ZB = auc(y_test==1,pred[,,i][,1]),
             ZDP = auc(y_test==2,pred[,,i][,2]),
             OK = auc(y_test==3,pred[,,i][,3]))
})


coefs[which.max(rowSums(rbindlist(perf))),]

# On pressent le problème du lien entre couverture numérique qui coûte $$$ et nombre d'habitants qui rapportent $. 
# Le nombre d'habitant au m² doit être encore plus pertinent.
# L'explication serait clairement économique.

