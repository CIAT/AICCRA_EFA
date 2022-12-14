source("EFA functions.R")


CountriesChoice<-c("Kenya","Burundi","Tanzania","Uganda","Rwanda")
SpillCountriesChoice<-c("Democratic Republic of the Congo","Ethiopia","South Sudan","Somalia","Malawi","Zambia","Mozambique")

CountriesChoice<-"Kenya"
SpillCountriesChoice<-"Tanzania"

IncTot<-"No"
BorderColCore<-"#000000"
BorderColSpill<-"#000000"
MSTechCode<-MS_options[Tech=="all technologies",unique(TechCode)]
FS_Choice<-"GLPS"
MSAreaType<-"Physical"

# Analysis params
PI_1<-5
PI_2<-10
PI_3<-3

CAAR_1<-1
CAAR_2<-2
CAAR_3<-3

SpillFactor<-1

Non_BCR<-1.62

CV_Change<-5

Years<-10
StartYear<-1
DiscountRate1<-5
ProjectCost1<-60
USD_Units<-1
Area_Name<-"ha"
Pop_Unit<-1
Prod_Unit<-1

AI_PrInc1<-1
AI_PrInc<-AI_PrInc1/100

Pr.Inc<-c(PI_1,PI_2,PI_3)[!is.na(c(PI_1,PI_2,PI_3))]/100
Ad.Rates.Core<-c(CAAR_1,CAAR_2,CAAR_3)[!is.na(c(CAAR_1,CAAR_2,CAAR_3))]/100
Ad.Rates.Spillover<-Ad.Rates.Core*(SpillFactor/100)

DiscountRate<-DiscountRate1/100

ProjectCost<-ProjectCost1*10^6

Area_Unit<-if(Area_Name=="ha"){
    1
  }else{
    if(Area_Name=="km2"){
      100
    }else{
      if(Area_Name=="10km2"){
        1000
      }else{
        if(Area_Name=="100km2"){
          10000
        }}}}

# GLPS
GPLS_Legend<-data.table::fread("Data/GLPS/LPS_legend_RGB.csv")
GPLS_Legend$Code<-0:14
colnames(GPLS_Legend)[1]<-"ID"

LPS<-terra::crop(terra::resample(terra::rast("Data/GLPS/glps_gleam_61113_10km.tif"),BaseRaster,method="near"),CGIAR_countries)
LPS<-terra::mask(LPS,CGIAR_countries)
LPS<-LPS-1
LPS<-terra::categories(LPS,layer=1,value=GPLS_Legend,index=9)

FS_Selected<-if(FS_Choice=="GLPS"){
      LPS
    }else{
      if(FS_Choice=="Adaptation Atlas L1"){
        FS1
      }else{
        FS2
      }
    }

FS_Options<-FS_Options_Fun(FS_Selected=FS_Selected,
                 ExtractBy.All=ExtractBy.All)


ExtractBy.Core<-CGIAR_countries[CGIAR_countries$ADMIN %in% CountriesChoice]

ExtractBy.Spillover<-CGIAR_countries[CGIAR_countries$ADMIN %in% SpillCountriesChoice]

ExtractBy.All<-CGIAR_countries[CGIAR_countries$ADMIN %in% c(SpillCountriesChoice,CountriesChoice)]

CGIAR_countries2<-
  CGCountriesFun(CGIAR_countries=CGIAR_countries,
                 CountriesChoice=CountriesChoice,
                 SpillCountriesChoice=SpillCountriesChoice,
                 CoreColour=BorderColCore,
                 SpillColour=BorderColSpill)

AddBorders<-function() {
    terra::plot(CGIAR_countries2,add=T,border=CGIAR_countries2$Colour,lwd=input$borderwidth,lty=CGIAR_countries2$Linetype)
  }


Farming_System<-FSsub_Fun(FS=FS_Selected,
            Choice = FS_Systems,
            Mask = ExtractBy.All)


LSvop1<-  terra::mask(terra::crop(
    terra::rast(paste0("Data/Herrero/Resampled/",LSvop_options[System %in% LSChoice & Variable=="VOP",Filename]))*CellSize.km
    ,Farming_System),Farming_System)

LSvop<-LSfun(Data=LSvop1,Choice=LSChoice,Extension="_vop",TotalName="LS",IncTot=IncTot)


# Divide by 1000 to convert kg to t
LSyield<-  terra::mask(terra::crop(
    terra::rast(paste0("Data/Herrero/Resampled/",LSvop_options[System %in% LSChoice & Variable=="MeatYield",Filename]))/1000
    ,Farming_System),Farming_System)

# *CellSize.km converts yield t/km2/year to production per pixel t/pixel/year
LSprod2<-  terra::mask(terra::crop(
    terra::rast(paste0("Data/Herrero/Resampled/",LSvop_options[System %in% LSChoice & Variable=="MeatYield",Filename]))*CellSize.km/1000
    ,Farming_System),Farming_System)

# Rename layers
LSprod<-LSfun(Data=LSprod2,Choice=LSChoice,Extension="_prod",TotalName="LS",IncTot=IncTot)


LSarea1<-LSareaFun(Data=LSprod)


# Rename layers
LSarea<-LSfun(Data=LSarea1,Choice=LSChoice,Extension="_area",TotalName="LS",IncTot=IncTot)


MSvop<-MSfun(VAR="VOP",
        TECH=MSTechCode,
        CROPS=CropChoice,
        BaseRaster=BaseRaster,
        MASK=Farming_System,
        MS_options=MS_options,
        Name="_vop",
        IncTot=IncTot)

MShparea<-MSfun(VAR="PhysArea",
        TECH=MSTechCode,
        CROPS=CropChoice,
        BaseRaster=BaseRaster,
        MASK=Farming_System,
        MS_options=MS_options,
        Name="_area",
        IncTot=IncTot)

MSharea<-MSfun(VAR="HarvArea",
                TECH=MSTechCode,
                CROPS=CropChoice,
                BaseRaster=BaseRaster,
                MASK=Farming_System,
                MS_options=MS_options,
                Name="_area",
                IncTot=IncTot)

MSarea<-if(MSAreaType=="Physical"){
    MShparea
  }else{
    MSharea
  }

MSprod<-MSfun(VAR="Production",
        TECH=MSTechCode,
        CROPS=CropChoice,
        BaseRaster=BaseRaster,
        MASK=Farming_System,
        MS_options=MS_options,
        Name="_prod",
        IncTot=IncTot)


FAODir<-"Data/FAO"
FAOFile<-paste0(FAODir,"/FAO_prod.RData")

if(!file.exists(FAOFile)){
  if(!dir.exists(FAODir)){
    dir.create(FAODir)
  }

  fao_metadata<-FAOSTAT::FAOsearch()
  #FAOSTAT::FAOsearch(dataset="crop",full=FALSE)
  FAO_prod<-data.table(FAOSTAT::get_faostat_bulk(code="QCL",data_folder=FAODir))

  UpdateFAOCountries<-data.table(
    FAO=c("Bolivia (Plurinational State of)","Iran (Islamic Republic of)","Syrian Arab Republic","Congo","Viet Nam","Lao People's Democratic Republic","Côte d'Ivoire","Timor-Leste","Brunei Darussalam","Eswatini","Venezuela (Bolivarian Republic of)","Bahamas","United Republic of Tanzania"),
    CGIAR=c("Bolivia","Iran","Syria","Republic of the Congo","Vietnam","Laos","Cote d'Ivoire","East Timor","Brunei","eSwatini","Venezuela","The Bahamas","Tanzania")
  )

  # Match then update FAO names with CGIAR names
  N<-match(FAO_prod[,area],UpdateFAOCountries[,FAO])

  FAO_prod[which(!is.na(N)),area:=UpdateFAOCountries[N[!is.na(N)],CGIAR]]
  FAO_prod<-FAO_prod[area %in% CGIAR_countries$ADMIN]

  save(FAO_prod,file=FAOFile)

  # Remove downloaded zip file
  unlink("Data/FAO/Production_Crops_Livestock_E_All_Data_(Normalized).zip")

}else{
  FAO_prod<-miceadds::load.Rdata2(FAOFile)
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

FAO_prod[,SpamName:=CropMappings[match(tolower(FAO_prod$item),CropMappings$FAO),MapSPAM]]
FAO_prod<-FAO_prod[!is.na(SpamName)]

FAOCountries<-FAOPlot(Map=CGIAR_countries2,FAO_CV=FAO_CV,CropChoice=CropChoice)

# Load and resample raster
Hpop2<-terra::resample(terra::rast(unzip("Data/Exposure/cell5m_ruralpop_2020_v3.zip")),BaseRaster,method="near")

RainCV2<-terra::resample(terra::rast("Data/Hazards/chirps_cv.tif"),BaseRaster,method="near")
names(RainCV2)<-"RainCV"

RainCV<-terra::mask(terra::crop(RainCV2,Farming_System),Farming_System)

# Remove unzipped file
unlink("cell5m_ruralpop_2020_v3.tif")

# Name raster
names(Hpop2)<-"Hpop"
Hpop<-terra::mask(terra::crop(Hpop2,Farming_System),Farming_System)


# Analysis ####
names(CellSize.ha)<-"Area.ha"

Stacked<-c(Farming_System,
    LSvop,
    LSarea,
    LSprod,
    MSvop,
    MSarea,
    MSprod,
    Hpop,
    RainCV
  )

# Columns to sum
SumCols<-c(names(Stacked)[!names(Stacked) %in% c("Farming_System","CellSize.ha","RainCV")],"CellSize.ha")


# Columns to analyse
prod_cols<-c(names(MSprod),names(LSprod))
area_cols<-c(names(MSarea),names(LSarea))
vop_cols<-c(names(MSvop),names(LSvop))


Stats.Core<-StackExtractor1(Data=Stacked,
                  ExtractBy = ExtractBy.Core,
                  SumCols=SumCols,
                  prod_cols=prod_cols,
                  area_cols=area_cols,
                  vop_cols=vop_cols)

CoreAreas<-AreaCalc(Stats=Stats.Core,CountryAreas=CountryAreas)


Stats.Core.Disp<-FormatExtraction(Data=data.table::copy(Stats.Core),
                 vop_cols=vop_cols,
                 area_cols=area_cols,
                 prod_cols=prod_cols,
                 USD_Units=as.numeric(USD_Units),
                 Area_Unit=as.numeric(Area_Unit),
                 Prod_Unit=as.numeric(Prod_Unit),
                 Pop_Unit=as.numeric(Pop_Unit),
                 Area_Name=Area_Name)

Stats.Spillover<-StackExtractor1(Data=Stacked,
                  ExtractBy = ExtractBy.Spillover,
                  SumCols=SumCols,
                  prod_cols=prod_cols,
                  area_cols=area_cols,
                  vop_cols=vop_cols)


CoreAreasSpill<-AreaCalc(Stats=Stats.Spillover,CountryAreas=CountryAreas)


Stats.Spill.Disp<-FormatExtraction(Data=data.table::copy(Stats.Spillover),
                   vop_cols=vop_cols,
                   area_cols=area_cols,
                   prod_cols=prod_cols,
                   USD_Units=as.numeric(USD_Units),
                   Area_Unit=as.numeric(Area_Unit),
                   Prod_Unit=as.numeric(Prod_Unit),
                   Pop_Unit=as.numeric(Pop_Unit),
                   Area_Name=Area_Name)

AnnInc<-AnnIncFun(Stats.Core=Stats.Core,
            vop_cols=vop_cols,
            Pr.Inc=AI_PrInc,
            AddFS=F)

AnnIncSpill<-AnnIncFun(Stats.Core=Stats.Spillover,
            vop_cols=vop_cols,
            Pr.Inc=AI_PrInc*SpillFactor/100,
            AddFS=F)


MI.Core<-MIwrapper(Data=Stats.Core,
            Ad.Rates=Ad.Rates.Core,
            Pr.Inc=Pr.Inc,
            Years=Years,
            Commodity=c(CropChoice,LSChoice),
            Non_BCR=Non_BCR)

MI.Core.Total<-MI.Core[Year==max(Year),list(Area.ha=sum(Area.ha,na.rm=T),
                                            A_non_adopt=sum(A_non_adopt,na.rm=T),
                                            A_adopt=sum(A_adopt,na.rm=T),
                                            VOP_non_adopt=sum(VOP_non_adopt,na.rm=T)/as.numeric(USD_Units),
                                            VOP_adopt=sum(VOP_adopt,na.rm=T)/as.numeric(USD_Units),
                                            NR_non_adopt=sum(NR_non_adopt,na.rm=T)/as.numeric(USD_Units),
                                            NR_adopt=sum(NR_adopt,na.rm=T)/as.numeric(USD_Units),
                                            Project_benefits=sum(Project_benefits,na.rm=T)/as.numeric(USD_Units)),by=list(Crop,Ad.Rate,Pr.Inc)
][,VOP_Total:=VOP_non_adopt+VOP_adopt]

MI.Spillover<-MIwrapper(Data=Stats.Spillover,
            Ad.Rates=Ad.Rates.Spillover,
            Pr.Inc=Pr.Inc,
            Years=Years,
            Commodity=c(CropChoice,LSChoice),
            Non_BCR=Non_BCR)

MI.Spill.Total<-MI.Spillover[Year==max(Year),list(Area.ha=sum(Area.ha,na.rm=T),
                                      A_non_adopt=sum(A_non_adopt,na.rm=T),
                                      A_adopt=sum(A_adopt,na.rm=T),
                                      VOP_non_adopt=sum(VOP_non_adopt,na.rm=T)/as.numeric(USD_Units),
                                      VOP_adopt=sum(VOP_adopt,na.rm=T)/as.numeric(USD_Units),
                                      NR_non_adopt=sum(NR_non_adopt,na.rm=T)/as.numeric(USD_Units),
                                      NR_adopt=sum(NR_adopt,na.rm=T)/as.numeric(USD_Units),
                                      Project_benefits=sum(Project_benefits,na.rm=T)/as.numeric(USD_Units)),by=list(Crop,Ad.Rate,Pr.Inc)
  ][,VOP_Total:=VOP_non_adopt+VOP_adopt]


CV_Subset_Core<-FAO_CV[area %in% CountriesChoice & SpamName %in% CropChoice]

CV_Subset_Spill<-FAO_CV[area %in% SpillCountriesChoice & SpamName %in% CropChoice]

CoreCV<-AvLossWrapper(CV_Data=CV_Subset_Core,
                Stats=MI.Core,
                CV_Change=CV_Change)

CoreCVSummary<-CoreCV$Stats[Year==max(Year),
                 list(Area.ha=sum(Area.ha,na.rm=T),
                      A_non_adopt=sum(A_non_adopt,na.rm=T),
                      A_adopt=sum(A_adopt,na.rm=T),
                      VOP_non_adopt=sum(VOP_non_adopt,na.rm=T)/as.numeric(USD_Units),
                      VOP_adopt=sum(VOP_adopt,na.rm=T)/as.numeric(USD_Units),
                      VOP_CIS=sum(VOP_CIS,na.rm = T)/as.numeric(USD_Units),
                      VOP_both=sum(VOP_both,na.rm = T)/as.numeric(USD_Units)),
                 by=list(Crop,Ad.Rate,Pr.Inc)
  ][,Ad.Rate:=as.character(Ad.Rate)][,VOP_Total:=VOP_non_adopt+VOP_adopt]


SpilloverCV<-AvLossWrapper(CV_Data=CV_Subset_Spill,
                Stats=MI.Spillover,
                CV_Change=CV_Change)

SpillCVSummary<-SpilloverCV$Stats[Year==max(Year),
                      list(Area.ha=sum(Area.ha,na.rm=T),
                           A_non_adopt=sum(A_non_adopt,na.rm=T),
                           A_adopt=sum(A_adopt,na.rm=T),
                           VOP_non_adopt=sum(VOP_non_adopt,na.rm=T)/as.numeric(USD_Units),
                           VOP_adopt=sum(VOP_adopt,na.rm=T)/as.numeric(USD_Units),
                           VOP_CIS=sum(VOP_CIS,na.rm = T)/as.numeric(USD_Units),
                           VOP_both=sum(VOP_both,na.rm = T)/as.numeric(USD_Units)),
                      by=list(Crop,Ad.Rate,Pr.Inc)
  ][,Ad.Rate:=as.character(Ad.Rate)][,VOP_Total:=VOP_non_adopt+VOP_adopt]

InvestStatCrops<-"maize"
InvestDataCore<-InvestDataWrapper(Data=CoreCV$Stats,
                    InvestStatCrops=InvestStatCrops,
                    ProjectCost=ProjectCost,
                    DiscountRate=DiscountRate,
                    StartPeriod=StartYear)


InvestDataCS<-InvestDataWrapper(Data= rbind(CoreCV$Stats,data.table::copy(SpilloverCV$Stats)[,Ad.Rate:=Ad.Rate*(100/SpillFactor)]),
                    InvestStatCrops=InvestStatCrops,
                    ProjectCost=ProjectCost,
                    DiscountRate=DiscountRate,
                    StartPeriod=StartYear)

X<-InvestDataCore$Indicators[,NPV:=round(NPV/as.numeric(USD_Units),3)
][,BCR:=round(BCR,3)
][,IRR:=round(IRR,3)
][,Implied_Adoption:=round(Implied_Adoption,3)
][,Avoided_loss_mean:=abs(round(Avoided_loss_mean,3))]
X

X<-InvestDataCS$Indicators[,NPV:=round(NPV/as.numeric(USD_Units),3)
][,BCR:=round(BCR,3)
][,IRR:=round(IRR,3)
][,Implied_Adoption:=round(Implied_Adoption,3)
][,Avoided_loss_mean:=abs(round(Avoided_loss_mean,3))]
X
