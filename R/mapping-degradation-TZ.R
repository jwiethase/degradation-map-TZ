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
   if (grepl('before', rasterimg, ignore.case=TRUE) == TRUE){
      years = "2012-2016"
   } 
   if(grepl('during', rasterimg, ignore.case=TRUE) == TRUE){
      years = "2015-2018"
   }
   newplot <- rasterVis::gplot(get(rasterimg) %>% mask(area)) + 
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
#grep(list.files(path="data"), pattern='new_', inv=T, value=T)

for(i in 1:length(rasterlist)){
   new_name1 <- paste("CCRO_", rasterlist[[i]], sep = "")
   assign(new_name1, plotMaskedArea(rasterlist[[i]], CCRO))
   new_name2 <- paste("nonCCRO_", rasterlist[[i]], sep = "")
   assign(new_name2, plotMaskedArea(rasterlist[[i]], non_CCRO))
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

# Quick summary statistics  ---------------------------

# Fill in numbers from Google Earth Engine
CCRO_area <- 125206 
nonCCRO_area <- 637459  

CCRO_raw_bareinv_area_before <- 85261  
nonCCRO_raw_bareinv_area_before <- 410687   

CCRO_raw_bareinv_area_during <- 22398   
nonCCRO_raw_bareinv_area_during <- 145212    

CCRO_sign_bareinv_area_before <- 57258   
nonCCRO_sign_bareinv_area_before <- 199219    

CCRO_sign_bareinv_area_during <- 985    
nonCCRO_sign_bareinv_area_during <- 12103     

CCRO_raw_bare_area_before <- 65561   
nonCCRO_raw_bare_area_before <- 317134    

CCRO_raw_bare_area_during <- 21631    
nonCCRO_raw_bare_area_during <- 157130     

CCRO_sign_bare_area_before <- 31184    
nonCCRO_sign_bare_area_before <- 94584     

CCRO_sign_bare_area_during <- 1491     
nonCCRO_sign_bare_area_during <- 13964      

# Area ratio
total_CCRO_ratio <- CCRO_area/(CCRO_area+nonCCRO_area)   # 16.4% CCRO
total_nonCCRO_ratio <- nonCCRO_area/(CCRO_area+nonCCRO_area)  # 83.6% non-CCRO

# Function 
ratio_and_chisq <- function(areaCCRO, area_nonCCRO){
   CCRO_ratio <- areaCCRO/(areaCCRO + area_nonCCRO)
   non_CCRO_ratio <- area_nonCCRO/(area_nonCCRO + areaCCRO)
   observed <- c(CCRO_ratio, non_CCRO_ratio)
   expected <- c(total_CCRO_ratio, total_nonCCRO_ratio)
   df <- data.frame(percentage = observed, 
              variable = c(deparse(substitute(areaCCRO)), deparse(substitute(area_nonCCRO))), 
              chisq = chisq.test(observed, p = expected)$p.value) %>% 
      separate(variable, c("management", "type", "deg", NA, "name")) %>% 
      mutate(testtype = paste(type, deg, sep = "_"))
   return(
 df
   )
   
}

df1 <- ratio_and_chisq(CCRO_raw_bareinv_area_before, nonCCRO_raw_bareinv_area_before)
df2 <- ratio_and_chisq(CCRO_raw_bareinv_area_during, nonCCRO_raw_bareinv_area_during)
df3 <- ratio_and_chisq(CCRO_sign_bareinv_area_before, nonCCRO_sign_bareinv_area_before)
df4 <- ratio_and_chisq(CCRO_sign_bareinv_area_during, nonCCRO_sign_bareinv_area_during)

df5 <- ratio_and_chisq(CCRO_raw_bare_area_before, nonCCRO_raw_bare_area_before)
df6 <- ratio_and_chisq(CCRO_raw_bare_area_during, nonCCRO_raw_bare_area_during)
df7 <- ratio_and_chisq(CCRO_sign_bare_area_before, nonCCRO_sign_bare_area_before)
df8 <- ratio_and_chisq(CCRO_sign_bare_area_during, nonCCRO_sign_bare_area_during)
df9 <- data.frame(percentage = c(total_CCRO_ratio, total_nonCCRO_ratio),
                  management = c("CCRO", "nonCCRO"), 
                  type = NA, 
                  deg = NA,
                  name = NA,
                  chisq = NA,
                  testtype = "total_area")

df <- rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9)

# Plot ratios
ratioplot <- ggplot(df, aes(fill=management, y=percentage, x=name)) + 
   geom_bar(position="stack", stat="identity") +
   facet_wrap(facets = "testtype", ncol = 4) +
   theme_bw() +
   theme(axis.title.x=element_blank()) +
   scale_fill_grey()

# Save the finished map
ggsave("output/ratioplot.tiff", plot = ratioplot, device = "tiff", width = 18.4, height = 9, units = "cm", dpi = 150)




