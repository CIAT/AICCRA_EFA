if(!require("pacman", character.only = TRUE)){install.packages("pacman",dependencies = T)}

required.packages <- c("Cairo",
                       "colourpicker",
                       "data.table",
                       "DT",
                       "ggplot2",
                       "sf",
                       "terra")

pacman::p_load(char=required.packages,install = T,character.only = T)

options(scipen = 999)

# Load Geography Polygons ####

# Using raster::shapefile to avoid encoding issues found with terra::rast
CGIAR_countries_sf<-sf::read_sf("Data/CGIAR_region/CGIAR_countries_simplified.shp",options = "ENCODING=UTF8")

CGIAR_countries_sf<-CGIAR_countries_sf[CGIAR_countries_sf$CG_REG %in% c("ESA","WCA") |
                                         CGIAR_countries_sf$ADMIN %in% c("Morocco","Western Sahara","Libya","Egypt","Algeria","Tunisia","Sudan"),]
sf::st_crs(CGIAR_countries_sf)<-4326
CGIAR_countries_sf$ADMIN[CGIAR_countries_sf$ADMIN=="United Republic of Tanzania"]<-"Tanzania"
#CGIAR_countries_sf$ADMIN[CGIAR_countries_sf$ADMIN=="Democratic Republic of the Congo"]<-"DRC"
#CGIAR_countries_sf$ADMIN[CGIAR_countries_sf$ADMIN=="Central African Republic"]<-"CAR"
CGIAR_countries_sf$ADMIN[CGIAR_countries_sf$ADMIN=="Ivory Coast"]<-"Cote d'Ivoire"
CGIAR_countries<-terra::vect(CGIAR_countries_sf)


CGIAR_regions<-terra::aggregate(CGIAR_countries,by="CG_REG",dissolve=T)
CGIAR_regions$ADMIN<-CGIAR_regions$CG_REG
CGIAR_regions_sf<-sf::st_as_sf(CGIAR_regions,na.fail=T)

# Set Base Raster
BaseRaster<-terra::rast("Data/cell5m_livestock_vop.tif")
BaseRaster<-terra::crop(BaseRaster,CGIAR_countries)
CellSize.km<-terra::cellSize(BaseRaster,mask=T, unit="km")

# GLPS
LPS<-terra::resample(terra::rast("Data/GLPS/glps_gleam_61113_10km.tif"),BaseRaster,method="near")
LPS<-terra::mask(LPS,CGIAR_regions)
GPLS_Legend<-data.table::fread("Data/GLPS/LPS_legend_RGB.csv")
GPLS_Legend[,!c("System","Irrigation","Aridity")]


LPS_Suitable<-copy(LPS)
LPS_Suitable[which(LPS_Suitable[] %in% c(13,14,15))]<-NA
LPS_Suitable<-LPS_Suitable-1
levels(LPS_Suitable)<-GPLS_Legend[1:12,System_Full]
names(LPS_Suitable)<-"LPS_Suitable"

# Livestock value of production
# Values are USD/km2/yr
# What year?

LSvop_options<-data.table::fread("Data/Herrero/LS Files.csv")
LSChoice<-c("Bovine_meat","Shoat_meat")

# LS VOP
LSvop<-terra::rast(paste0("Data/Herrero/Resampled/",LSvop_options[ShortName %in% LSChoice & Variable=="VOP",Filename]))
# Convert to value per pixel
LSvop<-LSvop*CellSize.km

names(LSvop)<-paste0(LSChoice,"_vop")

if(length(LSChoice)>1){
  LSvop<- c(LSvop,sum(LSvop))
  names(LSvop)<-c(paste0(LSChoice,"_vop"),"LS_vop")
}

# LS Yield/Production
LSyield<-terra::rast(paste0("Data/Herrero/Resampled/",LSvop_options[ShortName %in% LSChoice & Variable=="MeatYield",Filename]))
# Convert to value per pixel
LSprod<-LSyield*CellSize.km

names(LSprod)<-paste0(LSChoice,"_prod")

if(length(LSChoice)>1){
  LSprod<- c(LSprod,sum(LSprod))
  names(LSprod)<-c(paste0(LSChoice,"_prod"),"LS_prod")
}

# LS Area
LSarea<-CellSize.km

if(length(LSChoice)>1){
  for(i in 1:length(LSChoice)){
    LSarea<-c(LSarea,CellSize.km)
  }
  names(LSarea)<-c(paste0(LSChoice,"_area"),"LS_area")

}else{
  names(LSarea)<-paste0(LSChoice,"_area")

}

# Crop value of production (MapSPAM)
# Value of production by pixel
MS_options<-data.table::fread("Data/MapSPAM/MS Files.csv")
MSTechCode<-"A"

CropChoice<-c("maize","bean","cassava")
FILES<-MS_options[Variable == "VOP" & TechCode == MSTechCode & Crop %in% CropChoice]

MSvop<-terra::resample(terra::rast(paste0("Data/MapSPAM/",FILES$Filename)),BaseRaster,method="near")
MSvop<-terra::mask(MSvop,CGIAR_regions)
names(MSvop)<-paste0(FILES$Crop,"_vop")

if(length(CropChoice)>1){
  X<-sum(MSvop)
  MSvop<- c(MSvop,X)
  names(MSvop)<-c(paste0(FILES$Crop,"_vop"),"crops_vop")
}

# Crop area by pixel
FILES<-MS_options[Variable == "PhysArea" & TechCode == MSTechCode & Crop %in% CropChoice]
MSarea<-terra::resample(terra::rast(paste0("Data/MapSPAM/",FILES$Filename)),BaseRaster,method="near")
MSarea<-terra::mask(MSarea,CGIAR_regions)
names(MSarea)<-paste0(FILES$Crop,"_area")

if(length(CropChoice)>1){
  MSarea<- c(MSarea,sum(MSarea))
  names(MSarea)<-c(paste0(FILES$Crop,"_area"),"crops_area")
}

# Harvested area by pixel
FILES<-MS_options[Variable == "HarvArea" & TechCode == MSTechCode & Crop %in% CropChoice]
MSharea<-terra::resample(terra::rast(paste0("Data/MapSPAM/",FILES$Filename)),BaseRaster,method="near")
MSharea<-terra::mask(MSharea,CGIAR_regions)
names(MSharea)<-paste0(FILES$Crop,"_harea")

if(length(CropChoice)>1){
  MSharea<- c(MSharea,sum(MSharea))
  names(MSharea)<-c(paste0(FILES$Crop,"_harea"),"crops_harea")
}

# Production by pixel
FILES<-MS_options[Variable == "Production" & TechCode == MSTechCode & Crop %in% CropChoice]

MSprod<-terra::resample(terra::rast(paste0("Data/MapSPAM/",FILES$Filename)),BaseRaster,method="near")
MSprod<-terra::mask(MSprod,CGIAR_regions)
names(MSprod)<-paste0(FILES$Crop,"_prod")

if(length(CropChoice)>1){
  MSprod<- c(MSprod,sum(MSprod))
  names(MSprod)<-c(paste0(FILES$Crop,"_prod"),"crops_prod")
}

# Human population
Hpop<-terra::resample(terra::rast("Data/Exposure/cell5m_ruralpop_2020_v3.tif"),BaseRaster,method="near")
Hpop<-terra::mask(Hpop,CGIAR_regions)
names(Hpop)<-"Hpop"

# Climate Hazards
RainCV<-terra::resample(terra::rast("Data/Hazards/chirps_cv.tif"),BaseRaster,method="near")
RainCV<-terra::mask(RainCV,CGIAR_regions)
names(RainCV)<-"RainCV"

# Get FAOstat data
FAODir<-"Data/FAO"
FAOFile<-paste0(FAODir,"/FAO_prod.csv")

if(!file.exists(FAOFile)){
if(!dir.exists(FAODir)){
  dir.create(FAODir)
}
fao_metadata<-FAOSTAT::FAOsearch()
FAOsearch(dataset="crop",full=FALSE)
FAO_prod<-data.table(get_faostat_bulk(code="QCL",data_folder=FAODir))

UpdateFAOCountries<-data.table(
  FAO=c("Bolivia (Plurinational State of)","Iran (Islamic Republic of)","Syrian Arab Republic","Congo","Viet Nam","Lao People's Democratic Republic","Côte d'Ivoire","Timor-Leste","Brunei Darussalam","Eswatini","Venezuela (Bolivarian Republic of)","Bahamas","United Republic of Tanzania"),
  CGIAR=c("Bolivia","Iran","Syria","Republic of the Congo","Vietnam","Laos","Cote d'Ivoire","East Timor","Brunei","eSwatini","Venezuela","The Bahamas","Tanzania")
)

# Match then update FAO names with CGIAR names
N<-match(FAO_prod[,area],UpdateFAOCountries[,FAO])

FAO_prod[which(!is.na(N)),area:=UpdateFAOCountries[N[!is.na(N)],CGIAR]]
FAO_prod<-FAO_prod[area %in% CGIAR_countries$ADMIN]

fwrite(FAO_prod,file=FAOFile)
}else{
  FAO_prod<-fread(FAOFile)
}

FAO_prod<-FAO_prod[element %in% c("yield","milk_animals","yield_carcass_weight") & !is.na(value) & year %in% 2001:2020]

# Convert hg/ha to t/ha
FAO_prod[,value:=as.numeric(value)
         ][unit=="hg/ha",value:=value*0.0001
           ][unit=="hg/ha",unit:="t/ha"]

FAO_CV<-FAO_prod[,list(Mean=mean(value),
                       SD=sd(value),
                       CV=100*(sd(value)/mean(value)),
                       N.Years=.N),by=list(area,item,element,unit)]

# TO DO: Link FAO crops to MapSPAM crops
# TO DO: Work out AUC for left tails with reduced CV

MSCrops<-MS_options[,unique(Crop)]
FAOCrops<-FAO_CV[,tolower(unique(item))]

CropMatch<-MSCrops[MSCrops %in% FAOCrops]
CropNoMatch<-MSCrops[!MSCrops %in% FAOCrops]

CropMappings<-data.table(MapSPAM=MSCrops,FAO=as.character(NA))
CropMappings[MapSPAM %in% FAOCrops,FAO:=MapSPAM]

CropMappings[MapSPAM=="arabica coffee",FAO:="coffee, green"]
CropMappings[MapSPAM=="banana",FAO:="bananas"]
CropMappings[MapSPAM=="bean",FAO:="beans, dry"]
CropMappings[MapSPAM=="chickpea",FAO:="chick peas"]
CropMappings[MapSPAM=="coconut",FAO:="coconuts"]
CropMappings[MapSPAM=="cocoa",FAO:="cocoa, beans"]
CropMappings[MapSPAM=="cotton",FAO:="seed cotton"]
CropMappings[MapSPAM=="cowpea",FAO:="cow peas, dry" ]
CropMappings[MapSPAM=="groundnut",FAO:="groundnuts, with shell"]
CropMappings[MapSPAM=="lentil",FAO:="lentils"]
CropMappings[MapSPAM=="other cereals",FAO:="cereals nes"]
CropMappings[MapSPAM=="other fibre crops",FAO:="fibre crops nes"]
CropMappings[MapSPAM=="oilpalm",FAO:="oil palm fruit"]
CropMappings[MapSPAM=="other oil crops",FAO:="oilseeds nes"]
CropMappings[MapSPAM=="other pulses",FAO:="pulses nes"]
CropMappings[MapSPAM=="other roots",FAO:="roots and tubers nes"]
CropMappings[MapSPAM=="pigeonpea",FAO:="pigeons peas"]
CropMappings[MapSPAM=="plantain",FAO:="plantains and others"]
CropMappings[MapSPAM=="pearl millet",FAO:="millet"]
CropMappings[MapSPAM=="potato",FAO:="potatoes"]
CropMappings[MapSPAM=="robusta coffee",FAO:=NA]
CropMappings[MapSPAM=="rest of crops",FAO:=NA]
CropMappings[MapSPAM=="rice",FAO:="rice, paddy"]
CropMappings[MapSPAM=="sesameseed",FAO:="sesame seed"]
CropMappings[MapSPAM=="small millet",FAO:=NA]
CropMappings[MapSPAM=="soybean",FAO:="soybeans"]
CropMappings[MapSPAM=="sugarbeet",FAO:="sugar beet"]
CropMappings[MapSPAM=="sugarcane",FAO:="sugar cane"]
CropMappings[MapSPAM=="sunflower",FAO:="sunflower seed"]
CropMappings[MapSPAM=="sweet potato",FAO:="sweet potatoes"]
CropMappings[MapSPAM=="temperate fruit",FAO:=NA]
CropMappings[MapSPAM=="tropical fruit",FAO:="fruit, tropical fresh nes"]
CropMappings[MapSPAM=="vegetables",FAO:="vegetables primary"]
CropMappings[MapSPAM=="tobacco",FAO:="tobacco, unmanufactured"]

FAO_CV[,SpamName:=CropMappings[match(tolower(FAO_CV$item),CropMappings$FAO),MapSPAM]]
FAO_CV<-FAO_CV[!is.na(SpamName)]

# Raster stacked
Stacked<-c(LPS_Suitable,
           LSvop,
           LSprod,
           MSvop,
           MSarea,
           MSprod,
           Hpop,
           RainCV)

# Choose Geography
CountriesChoice<-c("Kenya","Ethiopia","Ghana","Mali","Senegal","Zambia")
ExtractBy.Core<-CGIAR_countries[CGIAR_countries$ADMIN %in% CountriesChoice]

SpillCountriesChoice<- c("Burundi","Rwanda","Tanzania","Uganda",
                         "Benin","Burkina Faso","Chad","Cote d'Ivoire",
                         "Cameroon","Guinea","Gambia","Guinea-Bissau","Mauritania",
                         "Niger","Nigeria","Togo","Botswana","Malawi","Mozambique",
                         "Zimbabwe")

ExtractBy.Spillover<-CGIAR_countries[CGIAR_countries$ADMIN %in% SpillCountriesChoice]

# Create function to extract and summarize variables by LPS x Country
StackExtractor1<-function(Data,ExtractBy,SumCols){

  Stacked<-lapply(1:length(ExtractBy),FUN=function(i){
    X<-ExtractBy[i]
    Y<-terra::mask(terra::crop(Data,X),X)
    Y$CellSize.km<-terra::cellSize(Y[[1]],mask=T, unit="km")
    Y
  })
  names(Stacked)<-ExtractBy$ADMIN

  # Extract other variables by LPS
  Stats<-rbindlist(lapply(1:length(Stacked),FUN=function(i){
    X<-Stacked[[i]]
    Stats<-terra::zonal(X[[SumCols]],c(X[["LPS_Suitable"]]),fun=sum,na.rm=T)
    colnames(Stats)[colnames(Stats)=="CellSize.km"]<-"Area.km2"
    Stats$RainCV.ln<-log(zonal(X[[c("RainCV")]],X[["LPS_Suitable"]],fun=mean,na.rm=T)$RainCV)
    Stats$Country<-names(Stacked)[i]
    Stats
    }))

  return(Stats)
}

SumCols<-c(names(Stacked)[!names(Stacked) %in% c("LPS_Suitable","CellSize","RainCV")],"CellSize.km")

# 1% annual increment
AnnualIncrement<-0.01

# Set value columns
prod_cols<-names(MSprod)
area_cols<-names(MSarea)
vop_cols<-names(MSvop)
vop_cols_LS<-names(LSvop)

# Core
Stats.Core<-StackExtractor1(Data=Stacked, ExtractBy = ExtractBy.Core, SumCols=SumCols)

yields<-Stats.Core[,..prod_cols]/Stats.Core[,..area_cols]
colnames(yields)<-gsub("_prod","_yield",colnames(yields))

prices<-Stats.Core[,..vop_cols]/Stats.Core[,..prod_cols]
colnames(prices)<-gsub("_vop","_price",colnames(prices))

Stats.Core<-cbind(Stats.Core,yields,prices)

# Spillover
Stats.Spillover<-StackExtractor1(Data=Stacked, ExtractBy = ExtractBy.Spillover, SumCols=SumCols)

yields<-Stats.Spillover[,..prod_cols]/Stats.Spillover[,..area_cols]
colnames(yields)<-gsub("_prod","_yield",colnames(yields))

prices<-Stats.Spillover[,..vop_cols]/Stats.Spillover[,..prod_cols]
colnames(prices)<-gsub("_vop","_price",colnames(prices))

Stats.Spillover<-cbind(Stats.Spillover,yields,prices)

# Annual Increase
AnnIncFun<-function(Stats.Core,vop_cols,vop_cols_LS,Pr.Inc){
Cols<-c("LPS_Suitable","Country",vop_cols,vop_cols_LS)
AnnualInc.Core<-Stats.Core[,..Cols]

AnnualInc.Core<-melt(AnnualInc.Core,id.vars = c("LPS_Suitable","Country"),variable.name = "Crop",value.name = "VOP")

AnnualInc.Core<-AnnualInc.Core[,LPS_Code:=paste(substr(unlist(strsplit(LPS_Suitable[1] ," ")),1,1),collapse=""),by=LPS_Suitable]

AnnualInc.Core<-AnnualInc.Core[,list(LPS=paste(sort(unique(LPS_Code)),collapse=", "),VOP=sum(VOP,na.rm=T)),by=list(Country,Crop)
               ][,Crop:=gsub("_vop","",Crop)
                 ][,Country_LPS:=paste0(Country,"\n(",LPS,")")]

AnnualInc.Core<-dcast(AnnualInc.Core,Crop~Country_LPS,value.var = "VOP")
AnnualInc.Core[,Total:=rowSums(AnnualInc.Core[,-1])]
AnnualInc.Core<-rbind(AnnualInc.Core,data.table("Total (ex crops or LS)",t(colSums(AnnualInc.Core[!Crop %in% c("LS","crops"),-1]))),use.names=F)

AnnualInc.Core<-data.table(AnnualInc.Core[,Crop],AnnualInc.Core[,-1]*Pr.Inc,Increment=Pr.Inc)

return(AnnualInc.Core)
}

AnnInc<-AnnIncFun(Stats.Core,
                  vop_cols,
                  vop_cols_LS,
                  Pr.Inc=0.01)

dplyr::mutate_if(AnnInc,is.numeric,~round(./1000,3))

# Marginal increases in value over 10 years
MarginalInc<-function(Ad.Rate,Pr.Inc,Years,Yield,Area,Price,VOP,CostFactor){

  Data<-data.table(Y_non_adopt=Yield,
             Y_adopt=Yield*(1+Pr.Inc),
             Year=1:Years
             )

  Total_Adoption<-Ad.Rate

  for(i in 2:Years){
    Total_Adoption[i]<-(1-sum(Total_Adoption))*Ad.Rate
  }

  Data[,Price:=Price][,Total_Adoption:=cumsum(Total_Adoption)
       ][,A_non_adopt:=Area*(1-Total_Adoption)
         ][,A_adopt:=Area*Total_Adoption
           ][,VOP_non:=A_non_adopt*Y_non_adopt*Price
             ][,VOP_adopt:=A_adopt*Y_adopt*Price
               ][,Marginal_VOP:=VOP_adopt*(Y_adopt-Y_non_adopt)/CostFactor
                 ][,Total_VOP:=VOP_non+VOP_adopt
                   ]

  return(Data)

}

MIwrapper<-function(Data,Ad.Rates,Pr.Inc,Years,System,CostFactor){

  ValCols<-paste0(rep(System,each=4),c("_vop","_yield","_price","_area"))

  Cols<-c(ValCols,"LPS_Suitable","Country","Area.km2")
  Stats2<-melt(Data[,..Cols],id.vars = c("Country","LPS_Suitable","Area.km2"),value.name = "StartVal",variable.name = "System")
  Stats2[,Crop:=tstrsplit(System,"_",keep=1)
         ][,Variable:=tstrsplit(System,"_",keep=2)][,System:=NULL]

  Stats2<-dcast(Stats2,Country+LPS_Suitable+Area.km2+Crop~Variable,value.var = "StartVal")

  MI.args<-data.table(expand.grid(Ad.Rate=Ad.Rates,Pr.Inc=Pr.Inc))

  N1<-rep(1:nrow(MI.args),each=nrow(Stats2))
  N2<-rep(1:nrow(Stats2),nrow(MI.args))

  Stats2<-data.table(Stats2[N2],MI.args[N1])

  rm(N1,N2,MI.args)

  Stats.Marg<-Stats2[,MarginalInc(Ad.Rate=Ad.Rate,
                                  Pr.Inc=Pr.Inc,
                                  Years=10,
                                  Yield=yield,
                                  Area=area,
                                  Price=price,
                                  VOP=vop,
                                  CostFactor=CostFactor),
                     by=list(LPS_Suitable,Country,Crop,Area.km2,Ad.Rate,Pr.Inc)]
  return(Stats.Marg)
}

Pr.Inc<-c(0.05,0.1)
Years<-10
CostFactor<-2.5

# Core
Ad.Rates.Core<-c(0.01,0.02,0.03)
MI.Core<-MIwrapper(Data=Stats.Core,
          Ad.Rates=Ad.Rates.Core,
          Pr.Inc=Pr.Inc,
          Years=Years,
          System=CropChoice,
          CostFactor=CostFactor)

# Spillover
Ad.Rates.Spillover<-Ad.Rates.Core/10

MI.Spillover<-MIwrapper(Data=Stats.Spillover,
                         Ad.Rates=Ad.Rates.Spillover,
                         Pr.Inc=Pr.Inc,
                         Years=Years,
                         System=CropChoice,
                         CostFactor=CostFactor)

# Benefits of yield losses avoided through use of CIS
CV_Subset_Core<-FAO_CV[area %in% CountriesChoice & SpamName %in% CropChoice]
CV_Subset_Spill<-FAO_CV[area %in% SpillCountriesChoice & SpamName %in% CropChoice]

# The reduction in CV due to CIS
CV_Change<-5

AvLoss<-function(Mean,SD,Change,Fixed,Reps=100000){
  #Calculate co-efficient of variation

  # Calculate new standard deviation based on changed CV
  if(Fixed){
   SDcis<-((SD/Mean)-(Change/100))*Mean
  }else{
    SDcis<-((SD/Mean)*(1-(Change/100)))*Mean
  }

  if(!SDcis<=0){

  # Create normal distribution of values
  X<-rnorm(n=Reps,mean=Mean,sd=SD)
  # Calculate probabilities
  Pnorm<-pnorm(X,mean=Mean,sd=SD)
  PnormCIS<-pnorm(X,mean=Mean,sd=SDcis)

  # Calculate differences in probabilities
  PnormDiff<-PnormCIS-Pnorm

  # Sum negative differences and divide by total probability for normal CV
  AVLoss<-100*(sum(PnormDiff[PnormDiff<0])/sum(Pnorm))
  }else{
    AVLoss<-NA
  }

return(AVLoss)
}

# Calculate core CIS values
CV_Subset_Core[,CVfixed:=CV-CV_Change
          ][,AvoidedLossFixed:=AvLoss(Mean=Mean,SD=SD,Change=CV_Change,Fixed=T),by=list(area,item)
            ][,AvoidedLossProp:=AvLoss(Mean=Mean,SD=SD,Change=CV_Change,Fixed=F),by=list(area,item)]
N<-match(
  MI.Core[,paste(Country,Crop)],
  CV_Subset_Core[,paste(area,SpamName)]
)

MI.Core[,AvoidedLossFixed:=CV_Subset_Core[N,AvoidedLossFixed]
        ][,Marginal_VOP_CIS:=(((100-AvoidedLossFixed)/100)*Y_adopt*A_adopt*Price-VOP_adopt)/CostFactor
          ][,Marginal_VOP_Both:=Marginal_VOP_CIS+Marginal_VOP]

# Calculate spillover CIS values
CV_Subset_Spill[,CVfixed:=CV-CV_Change
                ][,AvoidedLossFixed:=AvLoss(Mean=Mean,SD=SD,Change=CV_Change,Fixed=T),by=list(area,item)
                  ][,AvoidedLossProp:=AvLoss(Mean=Mean,SD=SD,Change=CV_Change,Fixed=F),by=list(area,item)]
N<-match(
  MI.Spillover[,paste(Country,gsub("_vop","",System))],
  CV_Subset_Spill[,paste(area,SpamName)]
)

MI.Spillover[,AvoidedLossFixed:=CV_Subset_Spill[N,AvoidedLossFixed]
             ][,Marginal_VOP_CIS:=(((100-AvoidedLossFixed)/100)*Y_adopt*A_adopt*Price-VOP_adopt)/CostFactor
               ][,Marginal_VOP_Both:=Marginal_VOP_CIS+Marginal_VOP]

# NPV & BCR
Value<-MI.Core[1:10,Value]
Time<-MI.Core[1:10,Year]
DiscountRate<-0.05
ProjectCost<-60000000
InvestStatCrops<-"maize"

InvestData<-MI.Core[Crop %in% InvestStatCrops,list(Ad.Rate,Pr.Inc,Marginal_VOP_Both,Marginal_VOP,Year,Total_Adoption,AvoidedLossFixed)
                    ][,NPVboth:=Marginal_VOP_Both/(1+DiscountRate)^Year
                      ][,NPVcsa:=Marginal_VOP/(1+DiscountRate)^Year]

InvestData<-melt(InvestData,
     id.vars = c("Ad.Rate","Pr.Inc","Year","Total_Adoption","AvoidedLossFixed","Marginal_VOP_Both","Marginal_VOP"),
     variable.name = "NPVtype",
     value.name = "NPV")[,VOP:=Marginal_VOP
                         ][,Marginal_VOP:=NULL
                           ][NPVtype=="NPVboth",VOP:=Marginal_VOP_Both
                             ][,Marginal_VOP_Both:=NULL]

NPV<-InvestData[,list(Avoided_loss_mean=-mean(AvoidedLossFixed,na.rm=T)/100,
                      Value=sum(NPV,na.rm=T),
                      Years=length(unique(Year)),
                      Implied_Adoption=max(Total_Adoption)
                      ),by=list(Ad.Rate,Pr.Inc,NPVtype)
             ][,NPV:=Value-ProjectCost
               ][,BCR:=NPV/ProjectCost
                 ][,Value:=NULL]



# IRR
# https://stackoverflow.com/questions/29781785/computing-irr-using-optim
irr <- function(x, period = 1, starting.value = .1){

  ### This should detect the number of sign changes.  Should correctly not warn if there are many negative cash flows (so long as there is only 1 change in sign).

  irr.func <- function(r){ ( sum(x / (1 + r)^{0:(length(x)-1)}) )^2 }
  result <- optim(par = starting.value, fn = irr.func, method = "Brent", lower = -1000000, upper = 1000000)

  ## detecting number of sign changes
  x.ge.0 <- 1 * (x >= 0)
  changes <- diff(x.ge.0)
  changes <- changes * changes
  num.changes <- sum(changes)

  if( num.changes > 1) {

    statement <- "Your cash flows change more than once -- so you may have multiple IRRs. This function will only return the first IRR it finds. To find the others, you can try different starting values.  However, note the IRR does not make sense if the signs change more than once (try Modified IRR or NPV)."
    value <- period * result$par
    return(list(beware = statement, IRR = value))

  } else {

    return(list(IRR = period * result$par))

  }
}

Y<-InvestData[,list(Value=sum(VOP,na.rm=T)),by=list(Ad.Rate,Pr.Inc,Year,NPVtype)]
Y[,list(IRR=irr(c(-ProjectCost,Value),starting.value=0.1)),by=list(Ad.Rate,Pr.Inc,NPVtype)]

  InvestStats<-cbind(NPV,InvestData[,list(Value=sum(VOP,na.rm=T)),by=list(Ad.Rate,Pr.Inc,Year,NPVtype)
             ][,list(IRR=irr(c(-ProjectCost,Value),starting.value=0.1)),by=list(Ad.Rate,Pr.Inc,NPVtype)][,"IRR"])

