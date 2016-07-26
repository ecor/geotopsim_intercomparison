#! .... 

rm(list=ls())


library(soilwater)
library(geotopbricks)
library(horizons)
library(geotopsim)
library(stringr)
library(ggplot2)
library(reshape2)
#source(
######



#				'/home/ecor/Dropbox/R-packages/geotopsim/R/SoilWaterStorage.R' )

loadDataFromPackage <- FALSE

wpath_pkg <- "/home/ecor/Dropbox/R-packages/geotopsim/inst"
wpath_data <- "/home/ecor/Dropbox/R-packages/geotopsim/data"
wpath_inputdata <- paste(wpath_pkg,"processing_geotop_simulation/inputdata",sep="/")
wpath <- '/home/ecor/Dropbox/R-packages/geotopsim_simulations/Borden/Borden05m_nopit_001'

wpath_outputdata <- "/home/ecor/Dropbox/Public/geotop_intercomparison/Borden05m_depit"



if (!file.exists(wpath_outputdata)) dir.create(wpath_outputdata,recursive=TRUE)

#"/home/ecor/Dropbox/R-packages/geotopsim_simulations/geotop_simulation" ######		paste(wpath_pkg,"geotop_simulation",sep="/")
file.output.csv <- paste(wpath_outputdata,"geotop.borden.output.csv",sep="/")
##file.proofilethetaplot <- paste(wpath_pkg,"processing_geotop_simulation/output/theta.png",sep="/")
file.proofilepsiplot <- paste(wpath_outputdata,"psi_profile.png",sep="/")
file.proofilethetaplot <- paste(wpath_outputdata,"theta_profile.png",sep="/")
file.psiprofile <- paste(wpath_outputdata,"psi_profile.csv",sep="/")
file.thetaprofile <- paste(wpath_outputdata,"ptheta_profile.csv",sep="/")
file.volumepng <- paste(wpath_outputdata,"volume.png",sep="/")
##

##
file.nctheta <-  paste(wpath_outputdata,"geotop.theta.nc",sep="/")
file.ncpsi <- paste(wpath_outputdata,"geotop.soilwaterpressure.nc",sep="/")
file.nchsup <- paste(wpath_outputdata,"geotop.surfacewater.nc",sep="/")
##




#"/home/ecor/Dropbox/R-packages/geotopsim_simulations/geotop_simulation" ######		paste(wpath_pkg,"geotop_simulation",sep="/")
#file.output.csv <- paste(wpath_pkg,"processing_geotop_simulation/output/geotop.borden005.output_wb.csv",sep="/")
##file.proofilethetaplot <- paste(wpath_pkg,"processing_geotop_simulation/output/theta.png",sep="/")


paramPrefix <- "Header"
inpts.file <- "geotop.inpts"
tz <- "GMT"
unit_geotop <- 0.001 # millimeters!
DemMap <- get.geotop.inpts.keyword.value("DemFile",wpath=wpath,inpts.file=inpts.file,raster=TRUE)
SlopeMap <- get.geotop.inpts.keyword.value("SlopeMapFile",wpath=wpath,inpts.file=inpts.file,raster=TRUE)
SoilMap <- get.geotop.inpts.keyword.value("SoilMapFile",wpath=wpath,inpts.file=inpts.file,raster=TRUE)
NetMap <- get.geotop.inpts.keyword.value("RiverNetwork",wpath=wpath,inpts.file=inpts.file,raster=TRUE)
BedrockDepthMap <- get.geotop.inpts.keyword.value("BedrockDepthMapFile",wpath=wpath,inpts.file=inpts.file,raster=TRUE)

if (is.null(SoilMap)) {
	
	soillevel=1
	SoilMap <- DemMap*0+1
	
} else {
	
	soillevel=unique(SoilMap)
}
start <-  get.geotop.inpts.keyword.value("InitDateDDMMYYYYhhmm",date=TRUE,wpath=wpath,inpts.file=inpts.file,tz="GMT")
#### Soil Properties


DzName <- get.geotop.inpts.keyword.value(paste(paramPrefix,"SoilDz",sep=""),wpath=wpath,inpts.file=inpts.file)
SoilPar <- tryCatch(get.geotop.inpts.keyword.value("SoilParFile",wpath=wpath,inpts.file=inpts.file,data.frame=TRUE,level=soillevel),error=function(e){NULL})			
if (is.null(SoilPar)) {
	
	layer  <- get.geotop.inpts.keyword.value("SoilDz",numeric=TRUE,wpath=wpath,inpts.file=inpts.file)
	
}  else {	
	
    SoilPar <- list(SoilPar)
	layer <- SoilPar[[1]][,DzName]
}


time_duration <- 2  # Expressed in hours
dtmap <- get.geotop.inpts.keyword.value("OutputSoilMaps",numeric=TRUE,wpath=wpath,inpts.file=inpts.file)

time_duration_when <- start+seq(from=0,to=time_duration*3600,by=dtmap*3600)   
time_duration_when <- time_duration_when[-1]
#(1:time_duration)*3600




if (loadDataFromPackage==TRUE) { 

	psi <- NULL
	theta <- NULL
	data(PsiTheta_Borden005)
	
} else {
	psi <- brickFromOutputSoil3DTensor("SoilLiqWaterPressTensorFile",
		when=time_duration_when, layers = "SoilLayerThicknesses",
		tz=tz,timestep = "OutputSoilMaps",wpath=wpath,inpts.file=inpts.file)
	theta <- brickFromOutputSoil3DTensor("SoilLiqContentTensorFile",

		when=time_duration_when, layers = "SoilLayerThicknesses",
		tz=tz,timestep = "OutputSoilMaps",wpath=wpath,inpts.file=inpts.file)

	hsup <- rasterFromOutput2DMap("LandSurfaceWaterDepthMapFile",
		
		when=time_duration_when,
		tz=tz,timestep = "OutputSoilMaps",wpath=wpath,inpts.file=inpts.file)

	hsup <- lapply(X=hsup,FUN=function(x) {x+0})
	save(list=c("psi","theta","hsup"),file=paste(wpath_data,"PsiTheta_Borden005.rda",sep="/"))


}
#################### PREPARE CSV 

t <- 5
dz <- layer*unit_geotop  
BedrockDepthMapm <- BedrockDepthMap*unit_geotop 


## 50 millimiters layer depth ##transformed from millimiters to meters
dx <- xres(theta[[1]])

qpoints <- 0 ##Discharge	at	outlet	and	downstream	end	of	slab1	and	2	
names(qpoints) <- c("outlet")


outputCsv <- data.frame(time=as.numeric(time_duration_when-start,units="secs"),geotop_surface_water=NA,
		geotop_soilwater=NA,
		geotop_unsat_soilwater=NA, 
		geotop_groundwt_soilwater=NA,
		geotop_topsoilmoisture_m2=NA
		)

outputCsv[,c("geotop_qsub_outlet","geotop_qsup_outlet")] <- NA

###outputCsv <- outputCsv[,names_v]
unit_geotop <- 0.001

for (t in 1:length(time_duration_when)) {
	
##	 dz is expressed in meters!!
	unsatWaterVolume <- SoilWaterStorage(theta[[t]],psi[[t]],layer=dz,comparison="<",psi_thres=0,fun=sum,bedrock.depth=BedrockDepthMapm)
	satWaterVolume <- SoilWaterStorage(theta[[t]],psi[[t]],layer=dz,comparison=">=",psi_thres=0,fun=sum,bedrock.depth=BedrockDepthMapm)
	WaterVolume <- SoilWaterStorage(theta[[t]],NULL,layer=dz,fun=sum,bedrock.depth=BedrockDepthMapm) 
	TopSoilMoisture <- theta[[t]][[1]]
	## qdischarge_sub <- LateralSubsurfaceDischarge(psi[[t]],wpath=wpath,output.discharge=TRUE)$discharge
	
	qdischarge_sup <- LateralSurfaceDischarge(hsup[[t]],wpath=wpath,output.discharge=TRUE)
	
	
#	transect <- data.frame(x=c(1,100)*dx-dx/2,y=y)
#	qpoints_xy <- data.frame(x=qpoints,y=y)
#	qpoints_xy$names <- names(qpoints)
#	qpoints_xy$icell <- cellFromXY(qdischarge_sup,qpoints_xy[,c("x","y")])
	
	volumetot <- function(x) {sum(as.matrix(x),na.rm=TRUE)*xres(x)*yres(x)}

	outputCsv[t,"geotop_soilwater"]<-  volumetot(WaterVolume)  ##sum(xyFrom2PointLine(r=WaterVolume,points=transect)$value001,na.rm=TRUE)*dx
	outputCsv[t,"geotop_groundwt_soilwater"]<-  volumetot(satWaterVolume) ##sum(xyFrom2PointLine(r=satWaterVolume,points=transect)$value001,na.rm=TRUE)*dx
	outputCsv[t,"geotop_unsat_soilwater"]<- volumetot(unsatWaterVolume) ##sum(xyFrom2PointLine(r=unsatWaterVolume,points=transect)$value001,na.rm=TRUE)*dx
	outputCsv[t,"geotop_surface_water"]<- volumetot(hsup[[t]]*unit_geotop) ##sum(xyFrom2PointLine(r=hsup[[t]]/1000,points=transect)$value001,na.rm=TRUE)*dx
	outputCsv[t,"geotop_topsoilmoisture_m2"]<-  volumetot(TopSoilMoisture) ##sum(xyFrom2PointLine(r=TopSoilMoisture,points=transect)$value001,na.rm=TRUE)*dx
    outputCsv[t,paste("geotop_qsup_outlet",sep="_")] <- qdischarge_sup[NetMap==1]
##	outputCsv[t,paste("geotop_qsub_outlet",sep="_")] <- qdischarge_sub[NetMap==1]
	 
	
	
	

}






write.table(outputCsv,file=file.output.csv,quote=FALSE,sep=",",row.names=FALSE)


## VOLUME PLOT 


outputMelt <- melt(outputCsv,id="time")
variables <- c("geotop_soilwater","geotop_groundwt_soilwater","geotop_unsat_soilwater","geotop_surface_water")
outputMeltv <- outputMelt[outputMelt$variable %in% variables,]
ggv <- ggplot()+geom_line(mapping=aes(x=time,y=value,colour=variable),data=outputMeltv)
ggv <- ggv+xlab("time [s]")+ylab("volume [m3]")+ggtitle("Volume")





###
ggsave(file.volumepng,ggv) 






