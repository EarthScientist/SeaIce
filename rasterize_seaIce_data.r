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



##### OLD STUFF ############

# 	count=0
# 	for (k in u.SourceVar){
# 		######################
# 		print(paste("begin rasterize routine: ",basename(f),sep=""))
# 		# this is the tempate map that will be used to create what will be the outputs.
# 		template.map <- mask.greenwich #raster(nrows=nrow(mask.greenwich), ncols=ncol(mask.greenwich), crs=projection(mask.greenwich), ext=extent(mask.greenwich))
# 		# change the values of the mask to havee mask area == 0 and oob == 9999
# 		ind <- which(getValues(template.map)==0); values(template.map)[ind] <- 9999
# 		ind <- which(getValues(template.map)==1); values(template.map)[ind] <- 0

# 		# these few lines just grab some unique information about the polygon date
# 		# of the unique year day combinations in the attribute table
# 		curYear <- substr(k, 1, 4)
# 		curMonth <- substr(k, 5, 6)

# 		# which elements in SourceVar contain k (returns a list of the indexes of those values)
# 		SourceVar.ind <- grep(k, SourceVar)

# 		# get the days from the SourceVar
# 		days<-substring(SourceVar[SourceVar.ind],12,13)
# 		#to get the days closest to the 15th of the month we will subtract the values from 15 and sort their abs value
# 		days.index<-abs(as.integer(days)-15)
# 		# creating a matrix
# 		days.mat <- cbind(substring(SourceVar[SourceVar.ind],6,15), days, days.index)
		
# 		# this sorts the matrix based on the days.reorder variable
# 		if(nrow(days.mat) == 1){
# 			days.reorder<-days.mat[sort.list(days.mat[3],decreasing=TRUE),]
# 			# this will put the new filename list back together
# 			newValueOrder<-paste("dmi_",days.reorder[1],sep="")
# 		}else{
# 			days.reorder<-days.mat[sort.list(days.mat[,3],decreasing=TRUE),]
# 			# this will put the new filename list back together
# 			newValueOrder<-paste("dmi_",days.reorder[,1],sep="")
# 		}


# 		for(nv in newValueOrder){
# 			print(paste("working on :",nv))
# 			# this grabs all of the files that are associated with the unique data of nv.hold in SourceVar
# 			nv.date <- substring(nv,6,nchar(nv))
# 			nv.hold<-grep(nv.date,SourceVar)
			
# 			# select the polygons based on the indexes grabbed by the nv.hold variable 
# 			dmi_month.Select <- dmi.greenwich@polygons[nv.hold]
# 			# Turn that list of polygons into a polygons layer
# 			dmi.greenwich.polySelect <- as.SpatialPolygons.PolygonsList(dmi_month.Select, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
# 			# now turn that polygon layer into a spatial polygons data frame at the rows identifying the nv.hold var
# 			dmi.greenwich.spdfSelect <- SpatialPolygonsDataFrame(dmi.greenwich.polySelect, data=as.data.frame(dmi.greenwich[nv.hold,]))

# 			#get the unique concentration group values
# 			Conc_grp.unique <- unique(dmi.greenwich.spdfSelect$Conc_grp)

# 			# Series of Nested  loops to figure out how to deal with the union of polygons

# 			for(select.ind in 1:length(Conc_grp.unique)){
# 				Conc_grp.ind <- which(dmi.greenwich.spdfSelect$Conc_grp == Conc_grp.unique[select.ind])
				
# 				print(paste(" Current poly value: ",Conc_grp.unique[select.ind]))

# 				# what is the length of the new list of indexes for the current unique value 
# 				npoly <- length(Conc_grp.ind)
# 				U <- dmi.greenwich.spdfSelect@polygons[Conc_grp.ind]
# 				U.poly <- as.SpatialPolygons.PolygonsList(U, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
# 				U.poly.spdfSelect <- SpatialPolygonsDataFrame(U.poly, data=as.data.frame(dmi.greenwich.spdfSelect[Conc_grp.ind,"Conc_grp"]))
				
# 				# new stuff...
# 				cellInd<-cellFromPolygon(template.map,U.poly.spdfSelect,weight=TRUE)

# 				cellInd.cells <- numeric()
# 				#cellInd.weights <- numeric()

# 				for(i in 1:length(cellInd)){
# 					ind <- which(cellInd[[i]][,2] >= in_weight & values(template.map)[cellInd[[i]][,1]] == 9999)
					
# 					cellInd.cells <- append(cellInd.cells, cellInd[[i]][,1][ind], after=length(cellInd.cells))
# 					#cellInd.weights <- append(cellInd.weights, cellInd[[i]][,2],after=length(cellInd.cells))
# 					#print(cellInd[[i]][,1])
#  				}
#  				values(template.map)[cellInd.cells] <- as.integer(Conc_grp.unique[select.ind])
# 			}
# 		}
# 		#-------------------------------------------------------------------------------------------------------------
# 		# CROP WINDOW SENT BY COREY
# 		# Long: 177.5 to -122.5 degrees
# 		# Lat: 52.5 to 76 degrees
# 		# window <- c(-122.5,177.5,52.5,76)
# 		# window <- window+360
# 		# template.map.window <- crop(template.map, window)
# 		#r <- rasterize(dmi.greenwich.spdfSelect, mask.greenwich, field="Conc_grp")
# 		# get the coordinates and values of the map into a data.frame
# 		r.df <- data.frame(coordinates(template.map), getValues(template.map))
# 		# flip those coordinates to 0-360
# 		if(any(r.df[,1]<0)) r.df[,1][r.df[,1]<0] <- r.df[,1][r.df[,1]<0]+360
# 		# rasterize the XYZ table with the flipped coordinates
# 		r.flip <- rasterFromXYZ(r.df, crs="NA", digits=8)

# 		# write out the new sea ice map
# 		writeRaster(r.flip, filename=paste(output_path,"sic_dmiCharts_",curMonth,"_",curYear,".asc", sep=""), overwrite=TRUE)
# 		print("-----------------------------------------------------------------------------------------------------------------")
# 	}
# }


##### OLD STUFF ############


# 		dmi_month.Select <- dmi.greenwich@polygons[ind]
# 		dmi.greenwich.polySelect <- as.SpatialPolygons.PolygonsList(dmi_month.Select, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
# 		dmi.greenwich.spdfSelect <- SpatialPolygonsDataFrame(dmi.greenwich.polySelect, data=as.data.frame(dmi.greenwich[ind,]))
# 		#-------------------------------------------------------------------------------------------------------------
# 		# template map to hold the new values

# 		# Series of Nested  loops to figure out how to deal with the union of polygons
# 		for(i in 1:length(unique(dmi.greenwich.spdfSelect$Conc_grp))){
# 			U.vals <- unique(dmi.greenwich.spdfSelect$Conc_grp)
# 			ind <- which(dmi.greenwich.spdfSelect$Conc_grp == U.vals[i])
			
# 			print(paste("Current poly value: ",U.vals[i]))
# 			print(paste("Current indexes: ", ind))

# 			# what is the length of the new list of indexes for the current unique value 
# 			npoly <- length(ind)
# 			U <- dmi.greenwich.spdfSelect@polygons[ind]
# 			U.poly <- as.SpatialPolygons.PolygonsList(U, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
# 			U.poly.spdfSelect <- SpatialPolygonsDataFrame(U.poly, data=as.data.frame(dmi.greenwich.spdfSelect[ind,"Conc_grp"]))
			
# 			if(npoly == 1){
# 				# go to the next thing since there is no need for union.
# 			}else if(npoly == 2){
# 				U.union <- gUnion(as.SpatialPolygons.PolygonsList(U.poly.spdfSelect@polygons[1],proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")),as.SpatialPolygons.PolygonsList(U.poly.spdfSelect@polygons[2],proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")))
# 				U.union.spdfSelect <- SpatialPolygonsDataFrame(U.union, data=as.data.frame(U.vals[i]))	
# 			} else {
# 				U.union <- gUnion(as.SpatialPolygons.PolygonsList(U.poly.spdfSelect@polygons[1],proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")),as.SpatialPolygons.PolygonsList(U.poly.spdfSelect@polygons[2],proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")))
# 				#U.union.spdfSelect <- SpatialPolygonsDataFrame(U.union, data=as.data.frame(U.vals[i]))
# 				for(j in 3:npoly){
# 					U.union <- gUnion(U.union, as.SpatialPolygons.PolygonsList(U.poly.spdfSelect@polygons[j],proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")))
# 				}
# 					U.union.spdfSelect <- SpatialPolygonsDataFrame(U.union, data=as.data.frame(U.vals[i]))
# 			}
# 			print("rasterize")

# 			ind <- cellFromPolygon(template.map, U.union.spdfSelect, weights=TRUE)
# 			ind <- ind[[1]]
# 			#ind2 <- numeric()
# 			#for(n in ind[[1]]){ ind2 <- append(ind2, n, after=length(ind2))}

# 			values(template.map)[ind] <- U.vals[i]

# 			#template.map

# 			#r <- rasterize(U.union.spdfSelect, template.map, field="U.vals[i]", update=TRUE, updateValue=U.vals[i])

# 			#function(x) if(U.poly.cells[,2])
# 		}

# 		#-------------------------------------------------------------------------------------------------------------
# 		# CROP WINDOW SENT BY COREY
# 		# Long: 177.5 to -122.5 degrees
# 		# Lat: 52.5 to 76 degrees
# 		# window <- c(-122.5,177.5,52.5,76)
# 		# window <- window+360
# 		# template.map.window <- crop(template.map, window)
# 		#r <- rasterize(dmi.greenwich.spdfSelect, mask.greenwich, field="Conc_grp")
# 		# get the coordinates and values of the map into a data.frame
# 		r.df <- data.frame(coordinates(template.map), getValues(template.map))
# 		# flip those coordinates to 0-360
# 		if(any(r.df[,1]<0)) r.df[,1][r.df[,1]<0] <- r.df[,1][r.df[,1]<0]+360
# 		# rasterize the XYZ table with the flipped coordinates
# 		r.flip <- rasterFromXYZ(r.df, crs="NA", digits=8)

# 		# write out the new sea ice map
# 		writeRaster(r.flip, filename=paste("/workspace/Shared/Michael/SeaIceProject_Rastercreation/test_working/output_rasters/SeaIceConcentration_dmiCharts_",curMonth,"_",curYear,".tif", sep=""), overwrite=TRUE)
# 		print("-----------------------------------------------------------------------------------------------------------------")
# 	}

# }





# lp <- character()


# U.poly.cells <- cellFromPolygon(mask.greenwich, U.poly, weights=T)

# rbind(U.poly.cells[[1]],U.poly.cells[[2]])
# rbind(paste("U.poly.cells[[",1:length(U.poly.cells),"]],", sep=""))


# for(n in ind){
# 	p <- paste("as.SpatialPolygons.PolygonsList(U.poly@polygons[",n,"])", sep="")
# 	lp <- append(lp, p, after=length(lp))
# }

# U.union <- gUnion(lp)


# paste("as.SpatialPolygons.PolygonsList(U.poly@polygons[",1:n,"])", sep="")

#curYear <- substr(u.SourceVar, 6, 9)
#curMonth <- substr(u.SourceVar, 10, 11)
#curDay <- substr(u.SourceVar, 12, 13)
#curMapNum <- substr(u.SourceVar, 14, 15)



# # this little loop is going to be the way that we loop through the unique names in the shapefile "Source" column and rasterize it
# for(i in u.source){
# 	temp.dmi <- dmi.greenwich[dmi.greenwich$Source == i,]

# 	new.dmi.raster.name <- paste("/workspace/Shared/Michael/SeaIceProject_Rastercreation/test_working/output_rasters/", i, ".tif", sep="")

# 	#temp.dmi.select <- temp.dmi[,dmi$Conc_grp]

# 	temp.dmi.raster <- rasterize(temp.dmi, mask.greenwich, field="Conc_grp", filename=new.dmi.raster.name)

# }

# AS I SEE IT:

#u <- unionSpatialPolygons(dmi.greenwich, ind, threshold=NULL, avoidGEOS=FALSE, avoidUnaryUnion=FALSE)






 # 1. the data are rubbersheeted in with the NAD 1983 Alaska Albers projection as the original data are not in a 
 # 	spatial format (paper map scans) --> referred to as dmi Charts

 # 2. that warped JPEG is then used as a basemap for digitization in the NAD 1983 Alaska Albers Projection and 
 # 	turned into a shapefile with attributes referring to the concentration and the dates

 # 3. Many different shapefiles are merged into a single shapefile where the defining attribute for each layer is a date in time
 # 	* it should be noted here that the data are Monthlies, where the sampling for a given month is for the 10 days surrounding 
 # 	  the 15th of a given month.

 # 4. the issue with the mask is something that needs revisiting.  I am not too sure what (or for that matter why) a given dataset
 # 	was taken out of the NAD 1983 Alaska Albers Projection system, but I think it is in reference to what Bill wants for this project.
 # 	* this is the issue where:
 # 		a. Bill has created a mask at the standard WGS 1984 Greenwich centered Latlong reference system.  
 # 		b. Lena projected it to the WGS 1984 Pacific centered Latlong reference system
 # 		c. *i think* that the NAD 1983 data needs to be in the WGS 1984 projection system when rasterized and given to Bill
 # 			IF SO:
 # 			i.  digitized shapefile maps need to be reprojected to WGS1984 
 # 			ii. 
############################################################################
# Steven Petersen
# Apr 27 2012

# to me, Lena, Tom 
# Mr. Lindgren,

# The max boundaries for which data could exist are.

# Long: 177.5 to -122.5 degrees
# Lat: 52.5 to 76 degrees

# Let me know if this doesn't relay the information I intend.

# Corey
############################################################################

# FOR TODAYS MEETING:  06/13/2012

# # questions I have:
# 1. where can I get the clean files?
# 2. what is the final output projection system that Bill wants?
# 3. what is the field that is to be rasterized from the digitized shapefiles?
# 4. what was the deal with the need for recoding a field based on another fields values?  
# 	am I needed to assist with that?

# 	########################################################################newX <- in_xyz[!is.na(in_xyz[,3]),]



