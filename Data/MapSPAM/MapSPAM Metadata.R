Files<-list.files("Data/MapSPAM",".tif")
X<-gsub("spam2017V2r1_SSA_V_","",Files)
X<-gsub(".tif","",X)

Files<-data.table(CropCode=unlist(lapply(strsplit(X,"_"),"[[",1)),
            TechCode=unlist(lapply(strsplit(X,"_"),"[[",2)),
            Filename=Files)

Crops<-fread("Data/MapSPAM/MS Crops.csv")[,-1]
Techs<-fread("Data/MapSPAM/MS Techs.csv")

Files<-merge(Files,Crops,"CropCode")
Files<-merge(Files,Techs,"TechCode")

fwrite(Files,"Data/MapSPAM/MS Files.csv")
