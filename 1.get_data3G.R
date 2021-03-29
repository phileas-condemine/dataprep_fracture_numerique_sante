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

APL = read_excel("data/APL2018MG.xlsx",sheet="APL_2018",skip = 7)
APL = data.table(APL)
APL = APL[-1]
names(APL) <- c("cod_com","lib_com","apl_mg","apl_mg65","pop2016")
APL = APL %>% mutate_at(c(3,4,5),as.numeric)%>%data.table

##### GET DEP CONTOURS #####

download_contours = function(my_dep){
  print(my_dep)
  URL=sprintf(
    "https://geo.api.gouv.fr/departements/%s/communes?fields=nom,code,population,contour&format=geojson&geometry=contour"
    ,my_dep)
  if(!paste0(my_dep,".json")%in%list.files("data/geojson"))
    download.file(URL,destfile = paste0("data/geojson/",my_dep,".json"),mode = "wb")
}

read_contours = function(my_dep){
  print(my_dep)
  info_com_sf=geojsonsf::geojson_sf(paste0("data/geojson/",my_dep,".json"))
  info_com_sf$nom=iconv(info_com_sf$nom,"UTF-8","latin1")
  info_com_sf$nom=iconv(info_com_sf$nom,"latin1","UTF-8")
  info_com_sf
}
operateurs = c("SFR","Orange","Bouygues","Free")
operateur = "SFR"
operateur = "Orange"
operateur = "Bouygues"
operateur = "Free"


for (operateur in c("Orange","Bouygues","Free")){
  
  if (operateur=="SFR"){
    couverture = sf::read_sf("data/3G/SFR/METRO_SFR_couv_3G_data_2019_T2.shp")
  } 
  if (operateur=="Orange"){
    couverture = sf::read_sf("data/3G/Orange/METRO_OF_couv_3G_data_2019_T2.shp")
  }
  if (operateur=="Bouygues"){
    couverture = sf::read_sf("data/3G/Bouygues Telecom/METRO_Bouygues_Telecom_couv_3G_data_2019_T2.shp")
  }
  if (operateur=="Free"){
    couverture = sf::read_sf("data/3G/Free Mobile/METRO_Free_Mobile_couv_3G_data_2019_T2.shp")
  }
  
  dep = "2A"
  download_contours(dep)
  communes = read_contours(dep)
  couverture = st_transform(couverture,sf::st_crs(communes))
  names(couverture) <- tolower(names(couverture))
  pbapply::pblapply(couverture$dept,function(dep){
    download_contours(dep)
    if (!paste0(operateur,dep,".RData")%in%list.files("data/tx_couverture_3G/")){
      communes = read_contours(dep)
      # communes_p = communes %>%st_cast("MULTIPOLYGON") %>% st_cast("POLYGON") %>% st_buffer(0)
      communes$area =  sapply(communes$geometry,sf::st_area)
      communes = communes %>% st_buffer(0)
      sub = couverture[couverture$dept==dep,]
      sub = st_cast(sub, "MULTIPOLYGON") %>% st_cast("POLYGON") %>% st_buffer(0)
      print(paste("nb communes:",nrow(communes),"nb poly reseau mobile:",nrow(sub)))
      
      is_intersect = sf::st_intersects(sub,communes,sparse=F)
      is_intersect = colSums(is_intersect)>0
      communes$has_intersection = is_intersect
      # intersect = sf::st_intersection(st_buffer(sub,0),st_buffer(communes[is_intersect,],0))
      time <- system.time({intersect <- sf::st_intersection(sub,communes)})
      print(time)
      intersect$area = sapply(intersect$geometry,sf::st_area)
      intersect = intersect%>%st_set_geometry(NULL)
      area3G = intersect%>%group_by(code)%>%summarize(area3G=sum(area))
      communes = merge(communes,area3G,by="code",all.x=T)
      # communes$area = sapply(communes$geometry,sf::st_area)
      # communes$area3G[communes$has_intersection] = sapply(intersect$geometry,sf::st_area)
      communes$tx_couverture_3G = communes$area3G/communes$area
      summary(communes$tx_couverture_3G)
      save(communes,file=paste0("data/tx_couverture_3G/",operateur,dep,".RData"))
    }
  })
}


############################

communes = pbapply::pblapply(list.files('data/tx_couverture_3G/'),function(x){
  load(paste0('data/tx_couverture_3G/',x))
  communes$op_dep = x
  communes
})

communes = do.call("rbind",communes)

table(communes$op_dep)
table(communes$has_intersection)
summary(communes$tx_couverture_3G)
communes$tx_couverture_3G[!communes$has_intersection] <- 0
summary(communes$tx_couverture_3G)

communes = merge(communes,APL,by.x="code",by.y="cod_com",all.x=T,all.y=T)

communes$densite_pop = communes$population/communes$area
communes = st_set_geometry(communes,NULL)
corrs = cor(communes[,c("apl_mg","tx_couverture_3G","pop2016","area","densite_pop")],use = "pairwise.complete.obs",method = "spearman")
heatmaply::heatmaply_cor(corrs)

plot_ly(communes[sample(nrow(communes),1000),],x=~apl_mg,y=~tx_couverture_3G,text=~interaction(code,area*1000))

dep = "05"
dep = "2B"
dep = "06"

# https://www.zone5g.com/couverture-mobile/hautes-alpes/montgenevre-05100.html
sub = couverture[couverture$dept==dep,]
ggplot()+ggspatial::annotation_map_tile(zoomin = 0)+geom_sf(data = sub)
