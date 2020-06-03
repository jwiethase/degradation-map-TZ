# Summary statistics  ---------------------------

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
      facet_wrap(facets = "testtype", ncol = 5, scales = "free") +
      theme_bw() +
      theme(axis.title.x=element_blank()) +
      scale_fill_grey()

# Save the finished map
ggsave("output/ratioplot.jpeg", plot = ratioplot, device = "jpg", width = 21, height = 9, units = "cm", dpi = 150)


