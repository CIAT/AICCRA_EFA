# Directory of Herrero Data
DataDir<-"C:\Datasets\Livestock Production Systems (Herrero)"


CGIAR_countries_sf<-sf::read_sf("Data/CGIAR_region/CGIAR_countries_simplified.shp",options = "ENCODING=UTF8")
CGIAR_countries_sf<-CGIAR_countries_sf[CGIAR_countries_sf$CG_REG %in% c("ESA","WCA") |
                                         CGIAR_countries_sf$ADMIN %in% c("Morocco","Western Sahara","Libya","Egypt","Algeria","Tunisia","Sudan"),]
sf::st_crs(CGIAR_countries_sf)<-4326
CGIAR_countries_sf$ADMIN[CGIAR_countries_sf$ADMIN=="United Republic of Tanzania"]<-"Tanzania"
CGIAR_countries_sf$ADMIN[CGIAR_countries_sf$ADMIN=="Democratic Republic of the Congo"]<-"DRC"
CGIAR_countries_sf$ADMIN[CGIAR_countries_sf$ADMIN=="Central African Republic"]<-"CAR"
CGIAR_countries<-terra::vect(CGIAR_countries_sf)

BaseRaster<-terra::rast("Data/cell5m_livestock_vop.tif")
BaseRaster<-terra::crop(BaseRaster,CGIAR_countries)

Files<-list.files(DataDir,".zip",recursive = T,full.names = T)

if(!dir.exists("Data/Herrero/Resampled")){
  dir.create("Data/Herrero/Resampled")
}

for(i in 1:length(Files)){
  print(i)
  FILE<-grep(".tif",unzip(Files[i],list=T)$Name,value=T)
  SaveName<-paste0("Data/Herrero/Resampled/",FILE)

  if(!file.exists(SaveName)){
  unzip(Files[i],files=FILE,exdir = "Data/Herrero" )

  X<-terra::rast(paste0("Data/Herrero/",FILE))
  X<-terra::resample(X,BaseRaster,method="average")
  X<-terra::mask(X,CGIAR_countries)

  terra::writeRaster(X,SaveName,overwrite=T)
  unlink(paste0("Data/Herrero/",FILE))
  }
}
