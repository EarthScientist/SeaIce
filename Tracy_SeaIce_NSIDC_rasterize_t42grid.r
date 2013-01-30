library(raster)
library(maptools)
library(rgdal)


# this is for the old shapefile thing
# list all of the shapefiles
l <- list.files("/workspace/Shared/NSIDC_Monthly/SeaIceIndex/Extracted_Shapefiles", pattern=".shp$", full.names=T)
ind <- grep("polyline",l)
l <- l[ind]

# we need a template t42grid for the rasterizing process
t42.pcll <- raster("/workspace/Shared/NSIDC_Monthly/SeaIceIndex/t42_template/global_t42grid_template.tif")
t42.greenwich <- rotate(raster("/workspace/Shared/NSIDC_Monthly/SeaIceIndex/t42_template/global_t42grid_template.tif"))

for(i in l){
	print(i)
	# read in the shapefile 
	shp <- readShapeSpatial(fn=i, proj4string=CRS("+proj=stere +lat_0=90 +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
	# add a new value that will be used as a boolean for rasterization
	shp$rasterize <- 1

	# reproject the shapefile
	shp.greenwich <- spTransform(shp, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

	# rasterize the data
	shp.rasterize <- rasterize(shp.greenwich, t42.greenwich, field="rasterize")

	# get an xyz matrix of the data layer
	xyz <- cbind(coordinates(shp.rasterize), getValues(shp.rasterize))

	# flip it
	if(any(xyz[,1] < 0))  xyz[xyz[,1]<0,1] <- xyz[xyz[,1]<0,1]+360

	rst <- rasterFromXYZ(xyz, res=res(t42.pcll), crs=projection(t42.pcll), digits=5)

	writeRaster(rst, filename=paste("/workspace/Shared/NSIDC_Monthly/SeaIceIndex/Rasterized_Shapefiles_t42/ver2/",substr(basename(i),1,nchar(basename(i))-4),"_t42grid.tif",sep=""), options="COMPRESS=LZW", overwrite=T)
}


# here I am goign to try to read in these flat binary files

# some notes to read in the binary data

# Table 1: Grid Dimensions
# Region	Nominal Gridded Resolution (km)	Columns	Rows
# North				25						304	448
library(raster)
library(maptools)
library(rgdal)

# we need a template t42grid for the rasterizing process
# t42.pcll <- raster("/workspace/Shared/NSIDC_Monthly/SeaIceIndex/t42_template/global_t42grid_template.tif")
# t42.greenwich <- rotate(raster("/workspace/Shared/NSIDC_Monthly/SeaIceIndex/t42_template/global_t42grid_template.tif"))
out_path = "/workspace/Shared/NSIDC_Monthly/gsfc_output/converted_GTIFF/"
files = list.files("/workspace/Shared/NSIDC_Monthly/gsfc_data", pattern=".bin$", full.names=T)

for(f in files){

# this works:
# f <- file("/workspace/Shared/NSIDC_Monthly/gsfc_data/bt_197901_n07_v02_n.bin","rb")
# bin <- readBin(f, what="integer", n=136192, size=2)
# length(bin)
# 136192
# r <- raster(matrix(bin, nrow=304,ncol=448))
   #  HIJMANS
	# require(sp) 
	# library(raster) 
	# library(rgdal) 
	print(paste("working on: ",basename(f),sep=""))
	pixel <- 25000 #pixel dimension in meters for both x and y 
	xMin <- -3837500 #From NSIDC: ulxmap -3837500 
	xMax <- xMin + (pixel*304) 
	yMax <- 5837500 #From NSIDC: ulymap 5837500 
	yMin <- yMax - (pixel*448) 
	rr <- raster(nrow=448, ncol=304, xmn=xMin, xmx=xMax, ymn=yMin, ymx=yMax) 
	# rr <- raster(matrix(bin,nrow=448,ncol=304,byrow=TRUE),xmn=-3850,xmx=3750,ymn=-5350,ymx=5850)
	projection(rr) <- '+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs' 

	bin <- file(f,"rb")
	read.bin <- readBin(bin, what="integer", n=136192, size=2, signed=FALSE, endian="little")
	rr <- setValues(rr, (read.bin/10))

	writeRaster(rr, filename=paste(out_path,extension(basename(f),".tif"),sep=""), options="COMPRESS=LZW")
}


###############  TRY # 2

library(raster)
library(maptools)
library(rgdal)
library(sp)

# we need a template t42grid for the rasterizing process
# t42.pcll <- raster("/workspace/Shared/NSIDC_Monthly/SeaIceIndex/t42_template/global_t42grid_template.tif")
# t42.greenwich <- rotate(raster("/workspace/Shared/NSIDC_Monthly/SeaIceIndex/t42_template/global_t42grid_template.tif"))
out_path = "/workspace/Shared/NSIDC_Monthly/gsfc_output/converted_GTIFF/"
files = list.files("/workspace/Shared/NSIDC_Monthly/gsfc_data", pattern=".bin$", full.names=T)

for(f in files){

	print(paste("working on: ",basename(f),sep=""))
	pixel <- 25000 #pixel dimension in meters for both x and y 
	xMin <- -3850000 #From NSIDC: ulxmap -3837500 
	xMax <- xMin + (pixel*304) 
	yMax <- 5850000 #From NSIDC: ulymap 5837500 
	yMin <- yMax - (pixel*448) 
	rr <- raster(nrow=448, ncol=304, xmn=xMin, xmx=xMax, ymn=yMin, ymx=yMax) 
	# rr <- raster(matrix(bin,nrow=448,ncol=304,byrow=TRUE),xmn=-3850,xmx=3750,ymn=-5350,ymx=5850)
	projection(rr) <- "+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #'+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs' 

	bin <- file(f,"rb")
	read.bin <- readBin(bin, what="integer", n=136192, size=2, signed=FALSE, endian="little")
	rr <- setValues(rr, (read.bin/10))

	writeRaster(rr, filename=paste(out_path,extension(basename(f),".tif"),sep=""), options="COMPRESS=LZW")
}



# old shit below

##################################
	
	bin <- file(f,"rb")
	# n= ncols()*nrows()+300byte header
	read.bin <- readBin(bin, what="integer", n=136192, size=2, signed=FALSE, endian="little")
	# test <- read.bin[301:length(read.bin)]
	# test.m <- matrix(test,nrow=448,ncol=304)
	r <- raster(matrix(bin,nrow=448,ncol=304,byrow=TRUE),xmn=-3850,xmx=3750,ymn=-5350,ymx=5850, crs="+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
	# r <- raster(test.m,xmn=-3850,xmx=3750,ymn=-5350,ymx=5850, crs="+proj=stere +a=6378273 +b=6356889.44891 +lat 0=90 +lat ts=70 +lon 0=45")
	
	writeRaster(r, filename=paste("/workspace/Shared/NSIDC_Monthly/monthly_tif/orig_converted_to_tiff/",substr(basename(f),1,nchar(basename(f))-4),".tif",sep=""))
	ext <- projectExtent(r, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
	pr <- projectRaster(r, res=0.432294124917081, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", method="ngb", filename=paste("/workspace/Shared/NSIDC_Monthly/monthly_tif/projected_wgs84/",substr(basename(f),1,nchar(basename(f))-4),"_wgs84greenwich.tif",sep=""))
	
	# flip the projected raster to the pcll	
	xyz <- cbind(coordinates(pr), getValues(pr))
	if(any(xyz[,1] < 0))  xyz[xyz[,1]<0,1] <- xyz[xyz[,1]<0,1]+360
	rst <- rasterFromXYZ(xyz, res=res(pr), crs=projection(pr), digits=5)

	writeRaster(rst, filename=paste("/workspace/Shared/NSIDC_Monthly/monthly_tif/flipped_wgs84_pcll/",substr(basename(f),1,nchar(basename(f))-4),"_pcll.tif",sep=""))

	resample(rst,t42.pcll,method="bilinear", filename=paste("/workspace/Shared/NSIDC_Monthly/monthly_tif/flipped_wgs84_pcll/",substr(basename(f),1,nchar(basename(f))-4),"_t42grid.tif",sep=""))
}

# TESTING AREA
pr <- projectRaster(r, t42.pcll, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0", method="bilinear")

writeRaster(r, filename="/workspace/Shared/NSIDC_Monthly/working/test_data_3.tif")


new="+proj=stere +a=6378273 +b=6356889.44891 +lat 0=90 +lat ts=70 +lon 0=45"

old="+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +ellps=clrk66"

homemade="+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k_0=1 +x_0=0 +y_0=0 +a=6378137 +b=6356752.3 +units=km"
# END TESTING AREA


# here we are going to write an IDRISI MACRO FILE TO DO SOME OF THE TRANSFORMATION WE NEED 

library(raster)
library(maptools)
library(rgdal)

files = list.files("/workspace/Shared/NSIDC_Monthly/monthly", pattern=".bin$", full.names=T)

iml <- character()

for(f in files){
iml <- append(iml, paste("GenericRaster x Y:/NSIDC_Monthly/monthly/", basename(f),"*1*Y:/NSIDC_Monthly/monthly_tif/",substr(basename(f),1,nchar(basename(f))-4),".rst*3*0*2*300*B*N*304*448*C:/Program Files (x86)/IDRISI Taiga/Georef/SSMI Polar Stereographic Projection.ref*km*-3850*3750*-5350*5850*1.0
",sep=""), after=length(iml))
iml <- append(iml, paste("project x 1*Y:/NSIDC_Monthly/monthly_tif/",substr(basename(f),1,nchar(basename(f))-4),".rst*C:/Program Files (x86)/IDRISI Taiga/Georef/ssmi polar stereographic projection.ref*Y:/NSIDC_Monthly/monthly_tif/flipped_wgs84_pcll/",substr(basename(f),1,nchar(basename(f))-4),".rst*C:/Program Files (x86)/IDRISI Taiga/Georef/pclatlong.ref*0.617720318736181*359.756578078447*30.9867291795257*87.6471371704791*304*448*0*2",sep=""), after=length(iml))
iml <- append(iml, paste("geotiff x 2*Y:/NSIDC_Monthly/monthly_tif/flipped_wgs84_pcll/",substr(basename(f),1,nchar(basename(f))-4),".rst*Y:/NSIDC_Monthly/monthly_tif/TIFF/",substr(basename(f),1,nchar(basename(f))-4),".tif*C:/Program Files (x86)/IDRISI Taiga/Symbols/greyscale.smp",sep=""), after=length(iml))
}

write.table(iml,file="/workspace/Shared/NSIDC_Monthly/monthly_tif/iml_convert.iml",col.names=F,row.names=F)

#### THIS LITTLE BIT OF CODE WILL RESAMPLE THE RASTERS FROM THE IDRISI THING ABOVE INTO the t42grid needed for Tracy's work

files = list.files("/workspace/Shared/NSIDC_Monthly/monthly_tif/TIFF", pattern=".tif$", full.names=T)
t42.pcll <- raster("/workspace/Shared/NSIDC_Monthly/SeaIceIndex/t42_template/global_t42grid_template.tif")
count=0
for(f in files){
	count=count+1
	print(paste(count,f, sep=" "))
	r <- raster(f)
	
	# make the longitude extent the same as the t42grid so there is no disconnect
	extent(r) <- c(xmin(t42.pcll),xmax(t42.pcll),ymin(r),ymax(r))

	# The data also need to be rescaled.  I am doing this here now
	ind.250 <- which(values(r) > 250) # hold this and pass the values back as 0 after the scaling of the rest of the data

	# now lets change all those values that are SIC data into their true values with the scaling
	ind <- which(values(r) <= 250)
	values(r)[ind] <- values(r)[ind]/2.5
	
	# turn all of the data that are oob into NA
	values(r)[ind.250] <- NA

	# resample the data to the t42grid
	rr <- resample(r,t42.pcll, method='bilinear')

	# turn the NA cells into 0 to mimick the GCM data at the t42grid
	values(rr)[which(values(rr) == NA)] <- 0

	# write out the result
	writeRaster(rr,paste("/workspace/Shared/NSIDC_Monthly/monthly_tif/t42grid/",substr(basename(f),1,nchar(basename(f))-4),"_t42grid.tif",sep=""), overwrite=T, options="COMPRESS=LZW")
}



