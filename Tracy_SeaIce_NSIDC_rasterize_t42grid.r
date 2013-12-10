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


