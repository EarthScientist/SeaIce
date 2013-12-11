require(raster)
require(maptools)
require(rgdal)
require(ncdf)
require(sp)
require(parallel)

#### The problem at hand with the Sea Ice project:
# this is where we set the working directory
setwd('/workspace/Shared/Tech_Projects/Sea_Ice_Atlas/project_data/DMI/DMI_raster/')
output_path <- '/workspace/Shared/Tech_Projects/Sea_Ice_Atlas/project_data/DMI/DMI_raster/Conc/landmask/'

# list the polygon dmi shapefiles
# filelist <- list.files("/workspace/Shared/Tech_Projects/Sea_ice_atlas/project_data/DMI", pattern="*.shp$", full.names=TRUE)
filelist <- list('/workspace/Shared/Tech_Projects/Sea_Ice_Atlas/project_data/DMI/DMI_shapefile/DMIshapefile.shp')

# set up some of the mask stuff prior to stepping through the polygons
# read in the mask from Bill (Pacific-Centered LatLong)
landmask_pacific <- raster("/workspace/Shared/Tech_Projects/Sea_Ice_Atlas/project_data/raster_mask_template/new.quarter.deg.mask.001.asc")

# create a file to be updated with the new values so we can retain the landmask
# now convert those values to something not in the range of values 0-10
values(landmask_pacific)[which(values(landmask_pacific) == 0)] <- NA
values(landmask_pacific)[which(values(landmask_pacific) == 1)] <- 9999

# rotate to a standard wgs 1984 greenwich-centered ref sys
landmask_greenwich <- rotate(landmask_pacific)
projection(landmask_greenwich) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

# project it to WGS 1984 North Pole Stereographic -- EPSG:3995
landmask_polar <- projectRaster(landmask_greenwich, crs=CRS('+init=epsg:3995'), method='ngb')

# rasterize function for the dmi data
rasterize_dmi <- function(dmi_layer, date_id, template_raster_polar, template_raster_wgs84, field_name='Conc',
	output_filename=paste0(output_path, 'DMI_Charts_', gsub('-', '_', date_id),'_wgs84pacific','.tif')){
	
	# rasterize it
	# without landmask
	# r <- rasterize(dmi_layer, template_raster_polar, field=field_name)

	# with landmask
	r <- rasterize(dmi_layer, template_raster_polar, field=field_name, update=TRUE, updateValue='NA')
	
	# re-project it and flip back to the 0-360 pacific centering
	r <- projectRaster(r, template_raster_wgs84, method='ngb') # wgs84 greenwich
	
	r.df <- data.frame(coordinates(r), getValues(r)) 
	rm(r) # cleanup
	# flip those coordinates to 0-360
	if(any(r.df[,1]<0)) r.df[,1][r.df[,1]<0] <- r.df[,1][r.df[,1]<0]+360
	# rasterize the XYZ table with the flipped coordinates
	r.flip <- rasterFromXYZ(r.df, res=res(template_raster_wgs84), crs="NA", digits=4)
	
	writeRaster(r.flip, filename=output_filename, options='COMPRESS=LZW', overwrite=TRUE)
	
	return(paste('completed', date_id))
	
	gc()
}



# run the above function on the data
for(f in filelist){
	dmi <- readShapePoly(f)

	print(paste("working on: ", f, sep=""))

	# the map appears to be in wgs 1984 stereographic north pole
	projection(dmi) <- CRS('+init=epsg:3995')

	# split the shapefile into unique date shapefiles in a list
	dmi_decade_list <- split(dmi, dmi$Date)

	# run the rasterize_dmi function to rasterize the data.
	tmp <- mcmapply(function(v,w,x,y,z) rasterize_dmi(v,w,x,y,z), v=dmi_decade_list, w=names(dmi_decade_list), 
		MoreArgs=list(x=landmask_polar, y=landmask_greenwich, z='Conc'), mc.cores=10)
}





# some other post-processing special functions that are used for data delivery to specific users

# this is a little bit of code to output the individual shapefiles for Vivian and give them the proper .prj file from the DMI shapefile
# write out the individual shapefiles
mapply(function(x, y) writeShapeSpatial(x, fn=paste0('/workspace/Shared/Tech_Projects/Sea_Ice_Atlas/project_data/DMI/DMI_shapefile/individual_shapefiles/', 'DMI_Charts_digitized_', y)), x=dmi_decade_list, y=names(dmi_decade_list))
# add in a .prj file the hard way
# get all the shapefiles names in the new directory
shape_names <- gsub('.shp', '.prj', list.files('/workspace/Shared/Tech_Projects/Sea_Ice_Atlas/project_data/DMI/DMI_shapefile/individual_shapefiles', pattern='.shp$', full.names=TRUE))

# run the file creator
Map(function(x,y){ 
	file.copy(y, x) 
	}, y='/workspace/Shared/Tech_Projects/Sea_Ice_Atlas/project_data/DMI/DMI_shapefile/DMIshapefile.prj', x=shape_names)


