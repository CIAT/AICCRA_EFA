#' FS_Options_Fun
#' @export
FS_Options_Fun<-function(FS_Selected,ExtractBy.All){
  Mapped<-data.table(terra::levels(FS_Selected)[[1]])
  FS<-terra::mask(FS_Selected,ExtractBy.All)
  FSVals<-unique(FS[!is.na(FS)][])
  return(as.vector(sort(unlist(Mapped[ID %in% FSVals,2]))))
}
#' FSsub_Fun
#' @export
FSsub_Fun<-function(FS,Choice,Mask){
  Mapped<-data.table(terra::levels(FS)[[1]])
  colnames(Mapped)[2]<-"Level"
  ID<-Mapped[Level %in% Choice,ID]

  FS[which(!FS[] %in% ID)]<-NA

  FS<-terra::mask(terra::crop(FS,Mask),Mask)

  names(FS)<-"Farming_System"
  return(FS)
}
#' LSfun
#' @export
LSfun<-function(Data,Choice,Extension,TotalName,DoSum=T,IncTot){
  names(Data)<-paste0(Choice,Extension)

  if(length(Choice)>1 & IncTot=="Yes"){
    if(DoSum){
      Data<- c(Data,sum(Data))
    }
    names(Data)<-c(paste0(Choice,Extension),paste0(TotalName,Extension))
  }
  return(Data)
}
#' LSareaFun
#' @export
LSareaFun<-function(Data){

  if(terra::nlyr(Data)>1){
    for(i in 1:terra::nlyr(Data)){
      Data[[i]]<-terra::cellSize(Data[[i]],unit="ha")
    }
  }else{
    Data<-terra::cellSize(Data,unit="ha")
  }

  return(Data)
}
#' MSfun
#' @export
MSfun<-function(VAR,TECH,CROPS,BaseRaster,MASK,MS_options,Name,IncTot){
  FILES<-MS_options[Variable == VAR & TechCode == TECH & Crop %in% CROPS,]

  X<-terra::rast(
    raster::readAll(
      raster::stack(
        terra::rast(
          unzip(paste0("Data/MAPSPAM/",FILES[1,Archive],".zip"),files =FILES[,Filename],overwrite = T)
        ))))
  unlink(FILES[,Filename])

  X<-terra::resample(X,BaseRaster,method="near")
  X<-terra::mask(terra::crop(X,MASK),MASK)
  names(X)<-FILES[,paste0(CROPS,Name)]

  if(length(CROPS)>1 & IncTot=="Yes"){
    X<-c(X,sum(X))
    names(X)<-c(FILES[,paste0(CROPS,Name)],paste0("crops",Name))
  }
  return(X)
}
#' FAOPlot
#' @export
FAOPlot<-function(Map,FAO_CV,CropChoice){

  for(CROP in CropChoice){
    Data<-FAO_CV[SpamName == CROP]
    N<-match(Map$ADMIN,Data$area)
    Map[,paste0(CROP,"_CV")]<-Data[N,CV]
    Map[,paste0(CROP,"_Mean")]<-Data[N,Mean]

    Map[is.na(Map$Choice),paste0(CROP,"_CV")]<-NA
    Map[is.na(Map$Choice),paste0(CROP,"_Mean")]<-NA

    # N1<-which(!is.na(Map$Choice))
    #N2<-which(is.na(values(Map[N1,paste0(CROP,"_CV")])))

    #  if(length(N2>0)){
    #   Map[N1[N2],paste0(CROP,"_CV")]<-0
    # }

  }

  return(Map)
}
#' CGCountriesFun
#' @export
CGCountriesFun<-function(CGIAR_countries,CountriesChoice,SpillCountriesChoice,CoreColour,SpillColour){
  CGIAR_countries$Choice<-NA
  CGIAR_countries$Colour<-NA
  CGIAR_countries$Linetype<-0

  CGIAR_countries[CGIAR_countries$ADMIN %in% CountriesChoice]$Choice<-"Core"
  CGIAR_countries[CGIAR_countries$ADMIN %in% SpillCountriesChoice]$Choice<-"Spillover"

  CGIAR_countries[CGIAR_countries$ADMIN %in% CountriesChoice]$Colour<-CoreColour
  CGIAR_countries[CGIAR_countries$ADMIN %in% SpillCountriesChoice]$Colour<-SpillColour

  CGIAR_countries[CGIAR_countries$ADMIN %in% CountriesChoice]$Linetype<-1 #"solid"
  CGIAR_countries[CGIAR_countries$ADMIN %in% SpillCountriesChoice]$Linetype<-2 #"dashed"

  return(CGIAR_countries)
}
#' StackExtractor1
#' Function to extract and summarize variables by Farming System x Country
#' @export
StackExtractor1<-function(Data,ExtractBy,SumCols,prod_cols,area_cols,vop_cols){

  Stacked<-lapply(1:length(ExtractBy),FUN=function(i){
    X<-ExtractBy[i]
    Y<-terra::mask(terra::crop(Data,X),X)
    Y$CellSize.ha<-terra::cellSize(Y[[1]],mask=T, unit="ha")
    Y
  })
  names(Stacked)<-ExtractBy$ADMIN

  # Extract other variables by LPS
  Stats<-rbindlist(lapply(1:length(Stacked),FUN=function(i){
    X<-Stacked[[i]]
    Stats<-terra::zonal(X[[SumCols]],X[["Farming_System"]],fun=sum,na.rm=T)
    colnames(Stats)[colnames(Stats)=="CellSize.ha"]<-"Area.ha"
    Stats$RainCV.ln<-log(zonal(X[["RainCV"]],X[["Farming_System"]],fun=mean,na.rm=T)$RainCV)
    Stats$Country<-names(Stacked)[i]
    Stats
  }))

  yields<-Stats[,..prod_cols]/Stats[,..area_cols]
  colnames(yields)<-gsub("_prod","_yield",colnames(yields))

  prices<-Stats[,..vop_cols]/Stats[,..prod_cols]
  colnames(prices)<-gsub("_vop","_price",colnames(prices))

  Stats<-cbind(Stats,yields,prices)

  ColOrder<-c(c("Country","Farming_System","Area.ha","Hpop","RainCV.ln"),
              colnames(Stats)[!colnames(Stats) %in% c("Country","Farming_System","Area.ha","Hpop","RainCV.ln")]
  )

  Stats<-Stats[,..ColOrder]

  return(Stats)
}
#' AnnIncFun
#' Function to calculate value of an annual increment in productivity:
#' @export
AnnIncFun<-function(Stats.Core,vop_cols,Pr.Inc,AddFS){
  Cols<-c("Farming_System","Country",vop_cols)
  AnnualInc.Core<-Stats.Core[,..Cols]

  AnnualInc.Core<-melt(AnnualInc.Core,id.vars = c("Farming_System","Country"),variable.name = "Crop",value.name = "VOP")

  AnnualInc.Core[,LPS_Code:=paste(substr(unlist(strsplit(Farming_System[1] ," ")),1,1),collapse=""),by=Farming_System]

  AnnualInc.Core<-AnnualInc.Core[,list(LPS=paste(sort(unique(LPS_Code)),collapse=", "),VOP=sum(VOP,na.rm=T)),by=list(Country,Crop)
  ][,Crop:=gsub("_vop","",Crop)
  ][,Country_LPS:=if(AddFS){paste0(Country,"\n(",LPS,")")}else{Country}]

  AnnualInc.Core<-dcast(AnnualInc.Core,Crop~Country_LPS,value.var = "VOP")
  AnnualInc.Core[,Total:=rowSums(AnnualInc.Core[,-1])]
  AnnualInc.Core<-rbind(AnnualInc.Core,data.table("Total",t(colSums(AnnualInc.Core[!Crop %in% c("LS","crops"),-1]))),use.names=F)

  AnnualInc.Core<-data.table(System=AnnualInc.Core[,Crop],AnnualInc.Core[,-1]*Pr.Inc)

  return(AnnualInc.Core)
}
#' MarginalInc
#' Function to calculate marginal increase in value of production (VoP):
#' @export
MarginalInc<-function(Ad.Rate,Pr.Inc,Years,Yield,Area,Price,VOP,Non_BCR){


  CostAdopt<-(Yield*Price)/Non_BCR
  CostNonAdopt<-((1+Pr.Inc)*Yield*Price)/Non_BCR

  Data<-data.table(Y_non_adopt=Yield,
                   Y_adopt=Yield*(1+Pr.Inc),
                   Year=1:Years
  )

  Total_adoption<-Ad.Rate

  for(i in 2:Years){
    Total_adoption[i]<-(1-sum(Total_adoption))*Ad.Rate
  }

  Data[,Price:=Price
  ][,Cost_adopt:=CostAdopt
  ][,Cost_non_adopt:=CostNonAdopt
  ][,Total_adoption:=cumsum(Total_adoption)
  ][,A_non_adopt:=Area*(1-Total_adoption)
  ][,A_adopt:=Area*Total_adoption
  ][,VOP_non_adopt:=A_non_adopt*Y_non_adopt*Price
  ][,VOP_adopt:=A_adopt*Y_adopt*Price
  ][,NR_non_adopt:=A_non_adopt*(Y_non_adopt*Price-Cost_non_adopt)
  ][,NR_adopt:=A_adopt*(Y_adopt*Price-Cost_adopt)]


  NR_non_adopt_Year0<-Area*(Yield*Price-CostNonAdopt)
  Data[,Project_benefits:=NR_non_adopt+NR_adopt-NR_non_adopt_Year0]

  return(Data)

}
#' MIwrapper
#' Wrapper function for marginal increase in VoP which restructures data for analysis
#' @export
MIwrapper<-function(Data,Ad.Rates,Pr.Inc,Years,Commodity,Non_BCR){

  ValCols<-paste0(rep(Commodity,each=4),c("_vop","_yield","_price","_area"))

  Cols<-c(ValCols,"Farming_System","Country","Area.ha")
  Stats2<-melt(Data[,..Cols],id.vars = c("Country","Farming_System","Area.ha"),value.name = "StartVal",variable.name = "Commodity")
  Stats2[,Crop:=tstrsplit(Commodity,"_",keep=1)
  ][,Variable:=tstrsplit(Commodity,"_",keep=2)][,Commodity:=NULL]

  Stats2<-dcast(Stats2,Country+Farming_System+Area.ha+Crop~Variable,value.var = "StartVal")

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
                                  Non_BCR=Non_BCR),
                     by=list(Farming_System,Country,Crop,Area.ha,Ad.Rate,Pr.Inc)]
  return(Stats.Marg)
}
#' AvLoss
#' Function to calculate avoided yield loss from a reduction in yield variability (CV
#' @export
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
#' AvLossWrapper
#' Wrapper function for avoided yield loss which recalculates marginal benefits
#' @export
AvLossWrapper<-function(CV_Data,Stats,CV_Change){
  CV_Data[,CVreduced:=CV-CV_Change
  ][,AvoidedLossPerc:=AvLoss(Mean=Mean,SD=SD,Change=CV_Change,Fixed=T),by=list(area,item)]

  setnames(CV_Data,c("area","SpamName","Mean","SD"),c("Country","Crop","Yield_Mean","Yield_SD"))
  setcolorder(CV_Data,c("Country","Crop","N.Years","Yield_Mean","Yield_SD","CV","CVreduced","AvoidedLossPerc"))

  CV_Data[,element:=NULL
  ][,unit:=NULL
  ][,item:=NULL]

  N<-match(
    Stats[,paste(Country,Crop)],
    CV_Data[,paste(Country,Crop)]
  )

  Stats[,AvoidedLossPerc:=abs(CV_Data[N,AvoidedLossPerc])
  ][,VOP_CIS:=Price*Y_adopt*A_adopt*AvoidedLossPerc/100
  ][,VOP_both:=VOP_CIS+VOP_adopt
  ][is.na(VOP_both),VOP_both:=VOP_adopt]


  return(list(Stats=Stats,CV_Data=CV_Data))
}
#' NPVfun
#' @export
NPVfun<-function(ProjectCost,StartPeriod,DiscountRate,Values,ReturnSum){
  if(!all(is.na(Values))){
    if(StartPeriod==0){
      Values[1]<-Values[1]-ProjectCost
    }else{
      Values<-c(-ProjectCost,rep(0,StartPeriod-1),Values)
    }

    Times<-1:length(Values)

    NPV<-Values/(1+DiscountRate)^Times

    if(ReturnSum){
      NPV<-sum(NPV)
    }
  }else{
    NPV<-NA
  }

  return(NPV)

}
#' InvestDataWrapper
#' Create wrapper function to calculate NPV and BCR:
#' Note that irr function throws a wobbly if Startdiff is >2
#' @export
InvestDataWrapper<-function(Data,InvestStatCrops,ProjectCost,DiscountRate,StartPeriod){
  InvestData1<-Data[Crop %in% InvestStatCrops, list(Farming_System,Country,Crop,Ad.Rate, Pr.Inc, Project_benefits, VOP_CIS, Year, Total_adoption, AvoidedLossPerc)
  ][,Project_benefits_CIS:=Project_benefits+VOP_CIS
  ][,VOP_CIS:=NULL]

  InvestData<-melt(InvestData1,
                   id.vars = c("Farming_System","Country","Crop","Ad.Rate", "Pr.Inc", "Year", "Total_adoption", "AvoidedLossPerc"),
                   variable.name = "CISinc",
                   value.name = "Cashflow")[CISinc=="Project_benefits_CIS",CISinc:="Yes"
                   ][CISinc=="Project_benefits",CISinc:="No"]

  Indicators<-  InvestData[,list(Cashflow=sum(Cashflow,na.rm = T),
                                 Implied_Adoption=max(Total_adoption,na.rm=T)*100,
                                 Avoided_loss_mean=mean(AvoidedLossPerc,na.rm=T)),
                           by=list(Year,Ad.Rate,Pr.Inc,CISinc,Total_adoption)
  ][,list(Periods=max(Year)+StartPeriod,
          StartPeriod=StartPeriod,
          Implied_Adoption=max(Implied_Adoption),
          Avoided_loss_mean=mean(Avoided_loss_mean),
          NPV=NPVfun(ProjectCost=ProjectCost,
                     StartPeriod=StartPeriod,
                     DiscountRate=DiscountRate,
                     Values=Cashflow,
                     ReturnSum=T),
          IRR=jrvFinance::irr(
            if(StartPeriod>0){
              c(-ProjectCost,rep(0,StartPeriod-1),Cashflow)
            }else{
              abs(Cashflow+c(-ProjectCost,rep(0,(.N-1))))
            })),
    by=list(Ad.Rate,Pr.Inc,CISinc)][,BCR:=NPV/abs(ProjectCost)][,IRR:=as.numeric(IRR)*100]


  return(list(InvestData1=InvestData1,InvestData=InvestData,Indicators=Indicators))
}
#' AreaCalc
#' @export
AreaCalc<-function(Stats,CountryAreas){

  Data<-Stats[,ToTArea:=CountryAreas[match(Stats$Country,ADMIN),area]
  ][,Area:=round(Area.ha/ToTArea,1)
  ][,list(Country,Farming_System,Area)]


  ToTArea<-CountryAreas[ADMIN %in% Stats$Country,sum(area)]

  Data2<-Stats[,list(Area.ha=sum(Area.ha,na.rm=T)),by=Farming_System
  ][,Area:=round(100*Area.ha/ToTArea,1)
  ][,Country:="Total"
  ][,list(Country,Farming_System,Area)]

  Data<-rbind(Data,Data2)

  Data<-dcast(data=Data,formula=Country~Farming_System, value.var = "Area")

  Data$Total<-rowSums(Data[,!"Country"],na.rm=T)
  N<-Data$Country=="Total"
  Data<-rbind(Data[!N],Data[N])
  return(Data)
}
#' FormatExtraction
#' @export
FormatExtraction<-function(Data,vop_cols,area_cols,prod_cols,USD_Units,Area_Unit,Prod_Unit,Pop_Unit,Area_Name){
  Data[,(vop_cols):=.SD/USD_Units,.SDcols=vop_cols]
  Data[,(area_cols):=.SD/Area_Unit,.SDcols=area_cols]
  Data[,(prod_cols):=.SD/Prod_Unit,.SDcols=prod_cols]
  Data[,Hpop:=Hpop/Pop_Unit]
  colnames(Data)[colnames(Data)=="Area.ha"]<-gsub("ha",Area_Name,"Area.ha")
  return(Data)
}

