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
load("data/APLs.RData")

######################## 3G #########################
communes = pbapply::pblapply(list.files('data/tx_couverture_3G/'),function(x){
  load(paste0('data/tx_couverture_3G/',x))
  communes$op_dep = x
  communes%>%st_set_geometry(NULL)%>%data.table()
})
communes = rbindlist(communes,use.names = T)
communes[,op_dep:=gsub('.RData','',op_dep)]
communes[,operateur := stringr::str_extract(op_dep,'(Free)|(Bouygues)|(Orange)|(SFR)')]
communes[,dep:=stringr::str_extract(op_dep,"[0-9]{2}")]
communes[,op_dep:=NULL]
communes[!(has_intersection),tx_couverture_3G:= 0]
sum(is.na(communes$tx_couverture_3G))
communes[is.na(tx_couverture_3G),tx_couverture_3G:=0]
communes = communes[,.(
  tx_3G_max = max(tx_couverture_3G),
  best_op_3G = operateur[which.max(tx_couverture_3G)],
  tx_3G_min = min(tx_couverture_3G),
  worst_op_3G = operateur[which.min(tx_couverture_3G)],
  area = area[1]
),by="code"]
summary(communes$area)
communes3G = communes
######################################################


######################## 4G #########################
communes = pbapply::pblapply(list.files('data/tx_couverture_4G/'),function(x){
  load(paste0('data/tx_couverture_4G/',x))
  communes$op_dep = x
  communes%>%st_set_geometry(NULL)%>%data.table()
})
communes = rbindlist(communes,use.names = T)
communes[,op_dep:=gsub('.RData','',op_dep)]
communes[,operateur := stringr::str_extract(op_dep,'(Free)|(Bouygues)|(Orange)|(SFR)')]
communes[,dep:=stringr::str_extract(op_dep,"[0-9]{2}")]
communes[,op_dep:=NULL]
communes[!(has_intersection),tx_couverture_4G:= 0]
sum(is.na(communes$tx_couverture_4G))
communes[is.na(tx_couverture_4G),tx_couverture_4G:=0]
communes = communes[,.(
  tx_4G_max = max(tx_couverture_4G),
  best_op_4G = operateur[which.max(tx_couverture_4G)],
  tx_4G_min = min(tx_couverture_4G),
  worst_op_4G = operateur[which.min(tx_couverture_4G)],
  area = area[1]
),by="code"]
summary(communes$area)
communes = merge(communes,communes3G,by="code")
sum(communes$area.x==communes$area.y)==nrow(communes)
communes[,area.x:=NULL]
setnames(communes,"area.y","area")
######################################################

communes = merge(communes,APLs,by.x="code",by.y="cod_com",all.x=F,all.y=F)

communes[,densite_pop := pop2016/area]
summary(communes$densite_pop)
corrs = cor(communes[,c("apl_mg","tx_4G_max","tx_4G_min","tx_3G_max","tx_3G_min","pop2016","area","densite_pop")],use = "pairwise.complete.obs",method = "spearman")
heatmaply::heatmaply_cor(corrs)
# 1er quintile ou médiane ?
quantile(communes$apl_mg2018,c(0.2,0.5),na.rm=T)
quantile(communes$apl_mk2016,c(0.2,0.5),na.rm=T)
quantile(communes$apl_inf2016,c(0.2,0.5),na.rm=T)
quantile(communes$apl_sf2016,c(0.2,0.5),na.rm=T)
lambda=.5
communes[,combo_couvertures := (lambda*(apl_mg2018/3+apl_mk2016/50+apl_inf2016/90+apl_sf2016/9)/4+(1-lambda)*(tx_4G_max+tx_3G_max)/2)]
setorder(communes,combo_couvertures)
save(communes,file="data/communes_scored.RData")








# sub_com = communes[sample(nrow(communes),100),]
nrow(communes[combo_couvertures<.4,])
sub_com = head(communes[combo_couvertures<.4,],100)
plot_ly(sub_com,
        x=~apl_mg2018,y=~tx_4G_max,
        size=~sqrt(pop2016),
        text=~interaction(paste0("COM:",code),
                          paste0(round(area*1000),"ua"),
                          paste0("3G",round(100*tx_3G_max),"%"),
                          paste0("pop:",round(pop2016))))%>%
  layout(title="Commune les plus mal couvertes en médecins, 3G et 4G",
         xaxis=list(title="APL médecins généralistes"),yaxis=list(title="Meilleure taux couverture 4G"))



dep = "05"
dep = "2B"
dep = "06"

# https://www.zone5g.com/couverture-mobile/hautes-alpes/montgenevre-05100.html
sub = couverture[couverture$dept==dep,]
ggplot()+ggspatial::annotation_map_tile(zoomin = 0)+geom_sf(data = sub)

read_contours = function(my_dep){
  print(my_dep)
  info_com_sf=geojsonsf::geojson_sf(paste0("data/geojson/",my_dep,".json"))
  info_com_sf$nom=iconv(info_com_sf$nom,"UTF-8","latin1")
  info_com_sf$nom=iconv(info_com_sf$nom,"latin1","UTF-8")
  info_com_sf
}
dep_countours = read_contours(dep)
dep_countours = rmapshaper::ms_simplify(input = dep_countours) %>%
  st_as_sf()
ggplot()+ggspatial::annotation_map_tile(zoomin = 0)+geom_sf(data = dep_countours)


library(leafgl)
if (!"fond_de_carte.RData"%in%list.files("data")){
  fonds_de_carte = lapply(unique(substr(communes$code,1,2)),function(dep){
    dep_countours = read_contours(dep)
    dep_countours$geometry = rmapshaper::ms_simplify(input = dep_countours[,'geometry'],keep=.1) %>%
      st_as_sf()%>%.$geometry
    dep_countours
  })
  fonds_de_carte = do.call("rbind",fonds_de_carte)
  save(fonds_de_carte,file="data/fond_de_carte.RData")
}
load("data/fond_de_carte.RData")
fonds_de_carte = fonds_de_carte%>%st_cast("MULTIPOLYGON")%>%st_cast("POLYGON")
library(leafgl)
library(leaflet)
library(colourvalues)

fonds_de_carte = merge(fonds_de_carte,na.omit(communes[,.(combo_couvertures,code)]),by="code")
dep = "2B"
sub = fonds_de_carte[substr(fonds_de_carte$code,1,2)==dep,]

# ggplot()+ggspatial::annotation_map_tile(zoomin = 0)+geom_sf(data = sub)
ggplot()+ggspatial::annotation_map_tile(zoomin = 0)+geom_sf(data = sub,aes(fill=combo_couvertures))

# https://riptutorial.com/fr/r/example/28354/des-palettes-aux-couleurs-daltoniennes
# cols = colour_values_rgb(Hmisc::cut2(fonds_de_carte$combo_couvertures,g = 5)%>%as.numeric,palette = "blue2red",include_alpha = FALSE) / 255
cols = colour_values_rgb(Hmisc::cut2(fonds_de_carte$combo_couvertures,g = 10)%>%as.numeric,
                         palette = "green2red",include_alpha = FALSE) / 255
m = leaflet()%>%
  addTiles()%>%
  setView(lng = 2, lat = 47, zoom = 6) %>%
  addGlPolygons(data = fonds_de_carte,
                color=cols,
                popup = "nom",group="APL") %>%
  addLayersControl(overlayGroups = "APL")%>%
  leafem::addMouseCoordinates()
m



