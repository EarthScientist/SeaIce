library(ncdf)
library(raster)
library(sp)
library(maptools)

# this is a bit of testing code to figure out what is wrong with the Sea Ice Data that Tracy can't get open

# read in an NC file of interest
nc <- open.ncdf("/workspace/Shared/Michael/CMIP5/data/OImon/mon/historical/r1i1p1/sic_OImon_bcc-csm1-1-m_historical_r1i1p1_185001-201212.nc")
sic <- brick("/workspace/Shared/Michael/CMIP5/data/OImon/mon/historical/r1i1p1/sic_OImon_bcc-csm1-1-m_historical_r1i1p1_185001-201212.nc")
# now lets find out what the vars are inside this thing
# explore the file by looking through the slots available in the ncdf R object.  
#   hitting tab twice with the below command will display on the screen the available variables
#  nc$var$

extractVar <- 'sic'

# this is where we set the input begin/end years and the months
years <- 1850:2012
months <- c("01","02","03","04","05","06","07","08","09","10","11","12")

# OK So we know that we have some vars called "lon", "lat", "sic" that will be used to create raster images from the netcdf timeseries
lon <- nc$dim$rlon$vals
lat <- nc$dim$rlat$vals

time <- nc$dim$time$vals

for(i in 1:12){
	stepIndex <- seq(i,length(time),12)

	for(s in stepIndex){
		print(s)

		mon <- months[i]
		as.vector(get.var.ncdf(nc,sic,start=c(1,1,i),count=c(-1,-1,1)))
	}
}
# now we need to get the time dimension from the netcdf
steps <- getSteps(dat$dim$time$vals,dat$dim$time$units,	yrs,mos)


# OR WE CAN DO SOMETHING LIKE THIS
# get all of the data as columns in a matrix
sic.mat <- cbind(coordinates(sic),as.array(as.matrix(sic)))
# bring the longitudes back to something that is usable
sic.mat[,1] <- sic.mat[,1]+280

for(f in 3:ncol(sic.mat)){
	print(f)
	xyz <- cbind(sic.mat[,1:2],sic.mat[,f])
	#xyz.sel <- xyz[xyz[,f] > 0,]
	r <- rasterFromXYZ(xyz, crs=projection(sic), digits=1)
	writeRaster(r, filename=paste("/workspace/Shared/Michael/CMIP5/data/MLindgren_testing/",sic@layernames[f],".tif",sep=""), overwrite=T)
}






# lon<-get.var.ncdf(nc,nc$var$lon)
# lat<-get.var.ncdf(nc,nc$var$lat)
# sic<-get.var.ncdf(nc,nc$var$sic)

SOME OUTPUTS FROM R:
	length(lat)
	[1] 83520

	length(lon)
	[1] 83520

	range(lat)
	[1] -81.50000  89.48785

	range(lon)
	[1] -279.98032   79.98032

# we actually have the same number of lats and lons which is not what we have seen in the past


# some stuff im working on to get to the data from ncdf package
sic <- nc$var[[6]]
sic.varSize <- v3$varsize
sic.varSize <- sic$varsize
sic.ndims <- sic$ndims
sic.nt <- sic.varSize[sic.ndims]
sic.nt
for(i in 1:sic.nt){
	start <- rep(1:sic.ndims)
	start[sic.ndims] <- i
	count <- sic.varSize
	count[sic.ndims] <- 1
	data <- get.var.ncdf(nc,sic,start=start,count=count)
	write.csv(data,filename=paste("test_output_",i,".csv",sep=""),row.names=F)
}
