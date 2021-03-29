library(rdrop2)
# token <- drop_auth(new_user = T)
# saveRDS(token, "data/droptoken.rds")
token <- readRDS("data/droptoken.rds")
drop_auth(rdstoken = "data/droptoken.rds")

my_files = list.files('data')
if (!"communes_scored.RData"%in%my_files){
  drop_download(path="fracture_numerique/communes_scored.RData",local_path =  = "data/communes_scored.RData",overwrite = F,dtoken=token)}
if (!"fond_de_carte.RData"%in%my_files){
  drop_download(path="fracture_numerique/fond_de_carte.RData",local_path = "data/fond_de_carte.RData",overwrite = F,dtoken=token)}
if (!"gadm36_FRA_2_sf.rds"%in%my_files){
  drop_download(path="fracture_numerique/gadm36_FRA_2_sf.rds",local_path = "data/gadm36_FRA_2_sf.rds",overwrite = F,dtoken=token)}
if (!"useful_thresholds.RData"%in%my_files){
  drop_download(path="fracture_numerique/useful_thresholds.RData",local_path = "data/useful_thresholds.RData",overwrite = F,dtoken=token)}
