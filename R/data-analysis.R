library(tidyverse)

# Summary statistics ------------------------------------------------------

# Fill in numbers from Google Earth Engine ------------------------------------------------------
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

# Question 1: Were improvements before the project proportional to land area? -----------------------------------------------

# Test H0: Improvement ratio between CCRO and non-CCRO before project is same as total land area ratio
chisq.test(c(CCRO_raw_bareinv_area_before, nonCCRO_raw_bareinv_area_before),
           p = c(CCRO_area, nonCCRO_area), 
           rescale.p = TRUE)
chisq.test(c(CCRO_sign_bareinv_area_before, nonCCRO_sign_bareinv_area_before),
           p = c(CCRO_area, nonCCRO_area), 
           rescale.p = TRUE)

chisq.test(c(CCRO_raw_bare_area_before, nonCCRO_raw_bare_area_before),
           p = c(CCRO_area, nonCCRO_area), 
           rescale.p = TRUE)
chisq.test(c(CCRO_sign_bare_area_before, nonCCRO_sign_bare_area_before),
           p = c(CCRO_area, nonCCRO_area), 
           rescale.p = TRUE)

# H0 is rejected in all cases. Statistically they are different, ecologically the difference
# is very minor

# Function to calculate ratio of improved land between CCRO and non-CCRO
ratios <- function(areaCCRO, area_nonCCRO){
   CCRO_ratio <- areaCCRO/(areaCCRO + area_nonCCRO)
   non_CCRO_ratio <- area_nonCCRO/(area_nonCCRO + areaCCRO)
   observed <- c(CCRO_ratio, non_CCRO_ratio)
   expected <- c(total_CCRO_ratio, total_nonCCRO_ratio)
   df <- data.frame(percentage = observed, 
                    variable = c(deparse(substitute(areaCCRO)), deparse(substitute(area_nonCCRO)))) %>% 
      separate(variable, c("management", "type", "deg", NA, "name")) %>% 
      mutate(testtype = paste(type, deg, sep = "_"))
   return(
      df
   )
}

# Make dataframe for plotting ------------------------------------------------------
df1 <- ratios(CCRO_raw_bareinv_area_before, nonCCRO_raw_bareinv_area_before)
df2 <- ratios(CCRO_sign_bareinv_area_before, nonCCRO_sign_bareinv_area_before)
df3 <- ratios(CCRO_raw_bare_area_before, nonCCRO_raw_bare_area_before)
df4 <- ratios(CCRO_sign_bare_area_before, nonCCRO_sign_bare_area_before)
df5 <- data.frame(percentage = c(total_CCRO_ratio, total_nonCCRO_ratio),
                  management = c("CCRO", "nonCCRO"), 
                  type = NA, 
                  deg = NA,
                  name = NA,
                  # chisq = NA,
                  testtype = "total_area")

before_total <- rbind(df1, df2, df3, df4, df5)

# Plot ratios ------------------------------------------------------
ratioplot_before_total <- ggplot(before_total, aes(fill=management, y=percentage, x=testtype)) + 
      geom_bar(position="stack", stat="identity") +
      theme_bw() +
      theme(axis.title.x=element_blank()) +
      scale_fill_grey()

# Question 2: Are improvements independent of project initialisation? -----------------------------------------------
# Established in question 1 that, statistically, improvements before project are not proportional to land ratios.
# Therefore, we compare the improvement ratio during the project with the before ratio, rather than land ratio.


# Test H0: Improvement ratio between CCRO and non-CCRO during project is same as before project
chisq.test(c(CCRO_raw_bareinv_area_during, nonCCRO_raw_bareinv_area_during),
           p = c(CCRO_raw_bareinv_area_before, nonCCRO_raw_bareinv_area_before), 
           rescale.p = TRUE)
chisq.test(c(CCRO_sign_bareinv_area_during, nonCCRO_sign_bareinv_area_during),
           p = c(CCRO_sign_bareinv_area_before, nonCCRO_sign_bareinv_area_before), 
           rescale.p = TRUE)

chisq.test(c(CCRO_raw_bare_area_before, nonCCRO_raw_bare_area_before),
           p = c(CCRO_raw_bare_area_during, nonCCRO_raw_bare_area_during), 
           rescale.p = TRUE)
chisq.test(c(CCRO_sign_bare_area_during, nonCCRO_sign_bare_area_during),
           p = c(CCRO_sign_bare_area_before, nonCCRO_sign_bare_area_before), 
           rescale.p = TRUE)
# H0 is rejected in all cases. 

# Make dataframe for plotting ------------------------------------------------------
df6 <- ratios(CCRO_raw_bareinv_area_during, nonCCRO_raw_bareinv_area_during)
df7 <- ratios(CCRO_sign_bareinv_area_during, nonCCRO_sign_bareinv_area_during)
df8 <- ratios(CCRO_raw_bare_area_during, nonCCRO_raw_bare_area_during)
df9 <- ratios(CCRO_sign_bare_area_during, nonCCRO_sign_bare_area_during)

before_during <- rbind(df1, df2, df3, df4, df6, df7, df8, df9)

# Plot ratios ------------------------------------------------------
ratioplot_before_during <- ggplot(before_during, aes(fill=management, y=percentage, x=name)) + 
   geom_bar(position="stack", stat="identity") +
   facet_wrap(facets = "testtype", ncol = 5, scales = "free") +
   theme_bw() +
   theme(axis.title.x=element_blank()) +
   scale_fill_grey()


# Save all maps ------------------------------------------------------
ggsave("output/ratioplot_before_total.jpeg", plot = ratioplot_before_total, device = "jpg", width = 21, height = 9, units = "cm", dpi = 150)
ggsave("output/ratioplot_before_during.jpeg", plot = ratioplot_before_during, device = "jpg", width = 21, height = 9, units = "cm", dpi = 150)
