require(raster)
require(maptools)
require(rgdal)
require(ncdf)
require(sp)
require(parallel)

#### The problem at hand with the Sea Ice project:
# this is where we set the working directory
setwd("/workspace/Shared/Tech_Projects/Sea_ice_atlas/project_data/DMI_Charts/working_folder/")
output_path <- "/workspace/Shared/Tech_Projects/Sea_ice_atlas/project_data/DMI_Charts/working_folder/"

# list the polygon dmi shapefiles
filelist <- list.files("/workspace/Shared/Tech_Projects/Sea_ice_atlas/project_data/DMI_Charts/raw_from_corey/1920s_DMI_shapefile_TEST", pattern="*.shp$", full.names=TRUE)

# set up some of the mask stuff prior to stepping through the polygons
# read in the mask from Bill (Pacific-Centered LatLong)
mask <- raster("/workspace/Shared/Tech_Projects/Sea_ice_atlas/project_data/raster_mask_BillChapman/new.quarter.deg.mask.001.asc")
# rotate to greenwich centered lat long
mask_greenwich <- rotate(mask)
projection(mask_greenwich) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# project it to polar stereographic WGS1984
mask_polar <- projectRaster(mask_greenwich, crs=CRS('+init=epsg:3995'), method='ngb')

for(f in filelist){
	dmi <- readShapePoly(f)

	print(paste("working on: ", f, sep=""))

	# the map appears to be in wgs 1984 stereographic north pole
	projection(dmi) <- CRS('+init=epsg:3995')

	# this will hold the names of the Source Variable in the attribute table 
	dates <- dmi$Date

	# now we ask it which are unique
	dates_unique <- unique(dates)

	# split the shapefile into unique date shapefiles in a list
	dmi_decade_list <- split(dmi, dmi$Date)

	# run the rasterize_dmi function to rasterize the data.
	mcmapply(function(v,w,x,y,z) rasterize_dmi(v,w,x,y,z), v=dmi_decade_list, w=names(dmi_decade_list), 
		MoreArgs=list(x=mask_polar, y=mask_greenwich, z='Concgrp'), mc.cores=5)
}


# rasterize function for the dmi data
rasterize_dmi <- function(dmi_layer, date_id, template_raster_polar, template_raster_wgs84, field_name='Concgrp',
	output_filename=paste0(output_path, 'DMI_Charts_', gsub('-', '_', date_id),'_wgs84pacific','.tif')){
	# rasterize it
	r <- rasterize(dmi_layer, template_raster_polar, field=field_name, method='ngb')
	
	# re-project it and flip back to the 0-360 pacific centering
	r <- projectRaster(r, template_raster_wgs84, method='ngb') # wgs84 greenwich
	
	r.df <- data.frame(coordinates(r), getValues(r)) 
	rm(r)
	# flip those coordinates to 0-360
	if(any(r.df[,1]<0)) r.df[,1][r.df[,1]<0] <- r.df[,1][r.df[,1]<0]+360
	# rasterize the XYZ table with the flipped coordinates
	r.flip <- rasterFromXYZ(r.df, crs="NA", digits=4)
	
	writeRaster(r.flip, filename=output_filename, options='COMPRESS=LZW')
	
	return(paste('completed', date_id))
	
	gc()
}



