library(raster)
library(rgdal)
library(tidyverse)
library(rasterVis)
library(gridExtra)
library(grid)

theme_set(theme_void(base_size = 12))

# Load the image files  ---------------------------
temp = list.files(path = "data/EarthEngine_images", pattern="*.tif")
for (i in 1:length(temp)) assign(gsub("\\..*","",temp[i]) %>% str_replace_all("-", "_"), raster::raster(paste("data/EarthEngine_images/", temp[i], sep = "")))

# Load the shapefiles, transform to image projection  ---------------------------
NTRI <- rgdal::readOGR("data/EarthEngine_shapefiles/NTRI.shp") %>% 
      spTransform(proj4string(bare_before)) 
managedNTRI <- rgdal::readOGR("data/EarthEngine_shapefiles/") %>% 
      spTransform(proj4string(bare_before)) 
unmanagedNTRI <- rgdal::readOGR("data/EarthEngine_shapefiles/unmanagedNTRI.shp") %>% 
      spTransform(proj4string(bare_before)) 
CCRO <- rgdal::readOGR("data/EarthEngine_shapefiles/CCRO.shp") %>% 
      spTransform(proj4string(bare_before))
non_CCRO <- rgdal::readOGR("data/EarthEngine_shapefiles/non_CCRO.shp") %>% 
      spTransform(proj4string(bare_before))

# Make function to make maps for the different areas and times  ---------------------------
plotMaskedArea <- function(rasterimg, area){
   if(grepl('^before', deparse(substitute(rasterimg)), ignore.case=TRUE) == TRUE){
      years = "2012-2016"
   } else {
      years = "2015-2018"
   }
   newplot <- rasterVis::gplot(rasterimg %>% mask(area)) + 
      geom_tile(aes(fill = value)) +
      scale_fill_gradient(low = '#D55E00', high = '#009E73', na.value="white", guide = FALSE) +
      coord_equal() +
      ggtitle(years) +
      geom_path(data = NTRI %>% fortify(), 
                aes(x = long, y = lat, group = group),
                color = 'black') +
      geom_path(data = area %>% fortify(), 
                aes(x = long, y = lat, group = group),
                color = 'black', alpha = .6)
   return(newplot)
}

# Loop through all image files, make masked plots for CCRO and non-CCRO areas  ---------------------------
rasterlist <- ls(pattern = "bare")

for(i in 1:length(rasterlist)){
   new_name1 <- paste("CCRO_", rasterlist[[i]], sep = "")
   assign(new_name1, plotMaskedArea(get(rasterlist[[i]]), CCRO))
   new_name2 <- paste("nonCCRO_", rasterlist[[i]], sep = "")
   assign(new_name2, plotMaskedArea(get(rasterlist[[i]]), non_CCRO))
}

# Arrange the plotted maps  ---------------------------
bare_inv_arranged_maps_raw <- gridExtra::grid.arrange(
   arrangeGrob(CCRO_bare_invasive_before, CCRO_bare_invasive_during, top = grid::textGrob("Within CCRO area", gp = gpar(cex = 1.5), just = "top"), ncol = 2), 
   arrangeGrob(nonCCRO_bare_invasive_before, nonCCRO_bare_invasive_during, top = grid::textGrob("Outside CCRO area", gp = gpar(cex = 1.5), just = "top"), ncol = 2), 
   nrow = 1)

bare_inv_arranged_maps_signif <- gridExtra::grid.arrange(
   arrangeGrob(CCRO_bare_invasive_before_q, CCRO_bare_invasive_during_q, top = grid::textGrob("Within CCRO area", gp = gpar(cex = 1.5), just = "top"), ncol = 2), 
   arrangeGrob(nonCCRO_bare_invasive_before_q, nonCCRO_bare_invasive_during_q, top = grid::textGrob("Outside CCRO area", gp = gpar(cex = 1.5), just = "top"), ncol = 2),  
   nrow = 1)

bare_arranged_maps_raw <- gridExtra::grid.arrange(
   arrangeGrob(CCRO_bare_before, CCRO_bare_during, top = grid::textGrob("Within CCRO area", gp = gpar(cex = 1.5), just = "top"), ncol = 2), 
   arrangeGrob(nonCCRO_bare_before, nonCCRO_bare_during, top = grid::textGrob("Outside CCRO area", gp = gpar(cex = 1.5), just = "top"), ncol = 2), 
   nrow = 1)

bare_arranged_maps_signif <- gridExtra::grid.arrange(
   arrangeGrob(CCRO_bare_before_q, CCRO_bare_during_q, top = grid::textGrob("Within CCRO area", gp = gpar(cex = 1.5), just = "top"), ncol = 2), 
   arrangeGrob(nonCCRO_bare_before_q, nonCCRO_bare_during_q, top = grid::textGrob("Outside CCRO area", gp = gpar(cex = 1.5), just = "top"), ncol = 2),  
   nrow = 1)

# Save the finished maps  ---------------------------
ggsave("output/CCRO_raw_bare_inv.tiff", plot = bare_inv_arranged_maps_raw, device = "tiff", width = 18.4, height = 9, units = "cm", dpi = 150)
ggsave("output/CCRO_signif_bare_inv.tiff", plot = bare_inv_arranged_maps_signif, device = "tiff", width = 18.4, height = 9, units = "cm", dpi = 150)
ggsave("output/CCRO_raw_bare.tiff", plot = bare_arranged_maps_raw, device = "tiff", width = 18.4, height = 9, units = "cm", dpi = 150)
ggsave("output/CCRO_signif_bare.tiff", plot = bare_arranged_maps_signif, device = "tiff", width = 18.4, height = 9, units = "cm", dpi = 150)

