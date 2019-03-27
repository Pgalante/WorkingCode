source('/home/pgalante/Projects/Git/maskRAid/R/dataDrivenMask.R')
###   Necessary Parameters
#' @param datedOccs A .csv file containing 3 columns of "longitude", "latitude", "date". Dates must either be year only, or YMD in a lubridate accepted format
#' @param RSenv A rasterStack of remotely sensed data (ie. % Forest Cover) with names as dates (eg. Y2000, Y2001) or date-times YMD (eg. Y2000-02-01)
#' @param dateScale a character vector of the scale the RSenv and corresponding datedOccs. Could be "year", "month", "day".
#' @param Model A raster file (generally in continuous output) with extent, resolution, etc. matching RSenv.
#' @param Bounds User selects which bounds observed for the species are limiting: "upper", "lower", "both"
#'         
#' @author Peter Galante <pgalante[at]amnh.org>
#' 
##############################################################################################################################
setwd('/home/pgalante/Projects/Wallace/maskrangeR/OlinguitoData/')
library(raster)
modis2005 <- raster('MODIS_tiles_stitched/2005/2005_stitched1.tif')
modis2006 <- raster('MODIS_tiles_stitched/2006/Stitched_2006.tif')
modis2008 <- raster('MODIS_tiles_stitched/2008/Stitched_2008.tif')
modis2009 <- raster('MODIS_tiles_stitched/2009/Stitched_2009.tif')
modis2010 <- raster('MODIS_tiles_stitched/2010/stitched_nw.tif')
# Make these stackable
modis2010 <- resample(modis2010, modis2005, method = 'bilinear')
#writeRaster(modis2010, 'MODIS_tiles_stitched/2010/stitched_nw2010fixed_res.tif', overwrite=T)
allModis <- stack(c(modis2005, modis2006, modis2008, modis2009, modis2010))

datedOccs <- read.csv('/home/pgalante/Projects/Wallace/maskrangeR/OlinguitoData/Occurrence_data/New_localities_by_year/All_new_records_by_year.csv')
RSenv <- allModis
dateScale = "year"
Model = raster('/home/pgalante/Projects/Wallace/maskrangeR/OlinguitoData/Outputs/raster_outputs_from_R_code/optimal_niche_model_no_threshold/largeprojection_AIC.tif')
Model <- projectRaster(from = Model, to = modis2005, crs = crs(modis2005)) ##Per Beth's notebook, this needs to be resampled to match modis

rsTest <- RSmask(datedOccs = datedOccs, RSenv = RSenv, dateScale = dateScale, Model = Model, Bounds = "lower")
