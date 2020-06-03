library(raster)
library(rgdal)
library(tidyverse)
library(rasterVis)
library(egg)
library(ggpubr)


theme_set(theme_void(base_size = 12))

# Load the image files  ---------------------------
temp = list.files(path = "data/EarthEngine_images", pattern="*.tif")
for (i in 1:length(temp)) assign(gsub("\\..*","",temp[i]) %>% str_replace_all("-", "_"), raster::raster(paste("data/EarthEngine_images/", temp[i], sep = "")))

# Combine image files  ---------------------------
bare_before <- bare_before + bare_before_q
bare_during <- bare_during + bare_during_q
bare_invasive_before <- bare_invasive_before + bare_invasive_before_q
bare_invasive_during <- bare_invasive_during + bare_invasive_during_q

remove(bare_before_q, bare_during_q, bare_invasive_before_q, bare_invasive_during_q)

# Load the shapefiles, transform to image projection  ---------------------------
NTRI <- rgdal::readOGR("data/EarthEngine_shapefiles/NTRI.shp") %>% 
      spTransform(proj4string(bare_before)) 
CCRO <- rgdal::readOGR("data/EarthEngine_shapefiles/CCRO.shp") %>% 
      spTransform(proj4string(bare_before))
non_CCRO <- rgdal::readOGR("data/EarthEngine_shapefiles/non_CCRO.shp") %>% 
      spTransform(proj4string(bare_before))

# Make function to make maps for the different areas and times  ---------------------------
plotMaskedArea <- function(rasterimg, area){
   newplot <- rasterVis::gplot(get(rasterimg) %>% mask(area)) + 
      geom_tile(aes(fill = as.factor(value))) +
      scale_fill_manual(values = c("#999999", "#F0E442", "#D55E00"), na.value="white",
                        labels = c("none", "raw", "large", ""), name = "Improvement scores") +
      coord_equal() +
      geom_path(data = NTRI %>% fortify(), 
                aes(x = long, y = lat, group = group),
                color = 'black') +
      geom_path(data = area %>% fortify(), 
                aes(x = long, y = lat, group = group),
                color = 'black', alpha = .6) +
      theme(legend.position="bottom")
   return(newplot)
}

# Loop through all image files, make masked plots for CCRO and non-CCRO areas  ---------------------------
rasterlist <- ls(pattern = "bare")

for(i in 1:length(rasterlist)){
   new_name1 <- paste("CCRO_", rasterlist[[i]], sep = "")
   assign(new_name1, plotMaskedArea(rasterlist[[i]], CCRO))
   new_name2 <- paste("nonCCRO_", rasterlist[[i]], sep = "")
   assign(new_name2, plotMaskedArea(rasterlist[[i]], non_CCRO))
}

# Arrange the plotted maps  ---------------------------
bare_inv_arranged_maps <- ggpubr::ggarrange(
   egg::tag_facet(CCRO_bare_invasive_before,
             tag_pool = "CCRO: 2012-2016",
             open = "", close = "", vjust = 1.1, hjust = 0), 
   egg::tag_facet(CCRO_bare_invasive_during, 
             tag_pool = "CCRO: 2015-2018",
             open = "", close = "", vjust = 1.1, hjust = 0), 
   egg::tag_facet(nonCCRO_bare_invasive_before,
             tag_pool = "non-CCRO: 2012-2016",
             open = "", close = "", vjust = 1.1, hjust = 0),
   egg::tag_facet(nonCCRO_bare_invasive_during,
             tag_pool = "non-CCRO: 2015-2018",
             open = "", close = "", vjust = 1.1, hjust = 0),
   ncol=4, nrow=1, common.legend = TRUE, legend="bottom")


bare_arranged_maps <- ggpubr::ggarrange(
   egg::tag_facet(CCRO_bare_before,
             tag_pool = "CCRO: 2012-2016",
             open = "", close = "", vjust = 1.1, hjust = 0), 
   egg::tag_facet(CCRO_bare_during, 
             tag_pool = "CCRO: 2015-2018",
             open = "", close = "", vjust = 1.1, hjust = 0), 
   egg::tag_facet(nonCCRO_bare_before,
             tag_pool = "non-CCRO: 2012-2016",
             open = "", close = "", vjust = 1.1, hjust = 0),
   egg::tag_facet(nonCCRO_bare_during,
             tag_pool = "non-CCRO: 2015-2018",
             open = "", close = "", vjust = 1.1, hjust = 0),
   ncol=4, nrow=1, common.legend = TRUE, legend="bottom")

all_maps <- ggpubr::ggarrange(
   egg::tag_facet(bare_inv_arranged_maps,
             tag_pool = "Bare ground & invasive plants",
             open = "", close = "", vjust = 1.1, hjust = 0), 
   egg::tag_facet(bare_arranged_maps,
             tag_pool = "Bare ground alone",
             open = "", close = "", vjust = 1.1, hjust = 0),
   ncol = 1, nrow = 2)



# Save the finished maps  ---------------------------
ggsave("output/CCRO_bare_inv.png", plot = bare_inv_arranged_maps, device = "png", width = 21, height = 9, units = "cm", dpi = 300)
ggsave("output/CCRO_bare.png", plot = bare_arranged_maps, device = "png", width = 21, height = 9, units = "cm", dpi = 300)
ggsave("output/CCRO_bare_and_bareinv.png", plot = all_maps, device = "png", width = 21, height = 16, units = "cm", dpi = 300)



