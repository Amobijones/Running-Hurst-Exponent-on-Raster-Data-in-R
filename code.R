library(sp)
library(rgdal)
library(raster)
library(lattice)
library(ggplot2)
library(caret)
library(pracma)


file_names <- list.files('./spring', full.names= T, pattern = '^.*\\.tif$')

length(file_names)


# importing rasters
raster_files <- brick(stack(file_names))

plot(raster_files[[1]])

# convert rasters to vectors and store in list

list_vals <- vector(mode = "list", length = 46)

for(i in 1:46){
  list_vals[[i]] <- getValues(raster_files[[i]])
}

View(list_vals)

# new list with slots for each pixel
list_pixels <- vector(mode = "list", 
                      length = length(list_vals[[1]])) #number of pixels
for(i in 1:length(list_vals[[1]])){ # for each pixel
  for(j in 1:length(list_vals)){    # for each raster
    list_pixels[[i]][j] <- list_vals[[j]][i] # get all pixel values
  }
}

list_pixels[[5000]] # test 5kth pixel for all the rasters of different timescale

# empty vector to store results for each pixel
rezults <- c() 

# loop for each pixel
for(i in 1:length(list_pixels)){
  
  # get pixel values at different rasters(time)
  pixel_valz <- list_pixels[[i]]
  # convert to time series object
  pixel_valz.ts <- ts(pixel_valz, start=c(2015,1), end=c(2016,12), frequency=12)
  
  
  # if any NAs in pixel values
  if(any(is.na(pixel_valz.ts))){
    rezults[i] <- NA # make result NA
    
  }else{ # if no NAs in pixel values
    rezults[i] <- hurstexp(pixel_valz.ts)$Hs # run hust exponent and store result
  }
  
}

# take the settings from an existing raster to 
#    make a new raster for results
plot(raster_files[[1]])
raster_files[[1]]@ncols

# make an empty raster
rezults_raster <- raster( nrow = raster_files[[1]]@nrows,
                          ncol = raster_files[[1]]@ncols,
                          crs = raster_files[[1]]@crs,
                          ext = raster_files[[1]]@extent)

# add the resulting pixel values to it
values(rezults_raster) <- rezults
plot(rezults_raster) # plot

#Export results

writeRaster(rezults_raster, "rezults_raster.tif")

#STOPCODE