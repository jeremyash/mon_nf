## SPATIAL
library(sp)
library(rgeos)
library(raster)
library(rgdal)
library(maptools)
library(sf)

## DATA MANAGEMENT
library(tidyverse)
library(skimr)
library(patchwork)
library(readxl)
# library(zoo)
library(lubridate)

## PLOTTING
library(scales)
library(units)
library(viridis)
library(extrafont)
library(gtable)
library(grid)
#----------------------------------------------------------------------------



#----------------------------------------------------------------------------
# class one sample sites
c1 <- read_excel("raw_data/class_one_sites.xlsx")

# water quality samples
water_dat <- read_excel("raw_data/WaterChem_MasterResults_031320.xlsx",
                        na = c(".", "NS", "ND", "<0.04", "<0.03")) %>% 
  filter(ID %in% c1$sample_site) %>% 
  left_join(., c1, by = c("ID" = "sample_site")) %>% 
  select(ID, Waterbody, Collection, area, ANC_ueq_L) %>% 
  mutate(year = year(Collection),
         month = month(Collection)) %>% 
  mutate(ID = as.character(ID),
         season = if_else(month %in% c(9,10,11), "Fall", "Spring")) %>% 
  rename(anc = ANC_ueq_L)

# %>% 
#   group_by(area, year, season) %>% 
#   summarise(mean_anc = mean(ANC_ueq_L, na.rm = TRUE),
#             sd_anc = sd(ANC_ueq_L, na.rm = TRUE)) %>% 
#   ungroup()



acid_df <- tibble(xmin = c(2000, 2000, 2000, 2000),
                  xmax = c(Inf, Inf, Inf, Inf),
                  ymin = c(200, 50, 0, -Inf),
                  ymax = c(Inf, 200, 50, 0))

#---------------------------------------------------------------------------
anc_comb_plot <- function(C1, TITLE) {
  
  dat <- water_dat %>% 
    filter(area == C1)
  
  
  # analysed together
  ggplot(data = dat) +
    
    # color shading
    # geom_rect(aes(xmin = 2019.02, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "white")) +
    geom_rect(aes_all(vars = c('xmin', 'xmax', 'ymin', 'ymax', 'fill')), alpha = .6,
              data.frame(xmin = 2000, xmax = Inf, ymin = 200, ymax = Inf, fill = "Well buffered")) +  
    geom_rect(aes_all(vars = c('xmin', 'xmax', 'ymin', 'ymax', 'fill')), alpha = .6,
              data.frame(xmin = 2000, xmax = Inf, ymin = 50, ymax = 200, fill = "Buffered")) +    
    geom_rect(aes_all(vars = c('xmin', 'xmax', 'ymin', 'ymax', 'fill')), alpha = .6,
              data.frame(xmin = 2000, xmax = Inf, ymin = 0, ymax = 50, fill = "Acid sensitive")) +
    geom_rect(aes_all(vars = c('xmin', 'xmax', 'ymin', 'ymax', 'fill')), alpha = .6,
              data.frame(xmin = 2000, xmax = Inf, ymin = -Inf, ymax = 0, fill = "Acidic")) +
    
    scale_fill_manual(name = NULL,
                      values = c("darkgreen", "darkolivegreen3", "tomato3","darkred" ),
                      labels = c("Well buffered", "Buffered", "Acid sensitive", "Acidic"),
                      breaks = c("Well buffered", "Buffered", "Acid sensitive", "Acidic")) +
    
    # labels
    # geom_text(aes(x = 2024, y = 260),label = "Well buffered", size = 5) +
    # geom_text(aes(x = 2024, y = 180),label = "Buffered", size = 5) +
    # geom_text(aes(x = 2024, y = 30),label = "Acid sensitive", size = 5) +
    # geom_text(aes(x = 2024, y = -20),label = "Acidic (chronic)", size = 5) +
    
    
    # geom_errorbar(aes( year, ymin = mean_anc + sd_anc, ymax = mean_anc - sd_anc),
    #               data = dat) +
    # geom_point(aes(year, mean_anc), 
    #            size = 4, 
    #            color = "black",
    #            data = dat) +
    # geom_line(aes(year,  mean_anc), 
    #           size = 1.2, 
    #           show.legend = FALSE,
    #           data = dat) +
    
    
    geom_line(aes(year, anc, group = season, color = season), size = 1.2) +
    geom_point(aes(year, anc, group = season, shape = season, color = season), size = 3) +
    
    scale_shape_manual(values = c(15,19), name = NULL) +
    scale_color_manual(values = c("black", "grey65"), name = NULL) +
    
    # # spring
    # geom_line(aes(year, Spring), linetype = "dashed", size = 1.2) +
    # geom_point(aes(year, Spring), size = 2) +
    # 
    theme_minimal() +
    
    labs(y = expression(ANC~(mu*eq~L^-1)),
         x = "Year",
         title = TITLE) +
    
    facet_wrap(vars(ID)) +
    
    
    scale_x_continuous(breaks=seq(2001,2019, by=2), limits=c(2000,2020)) +
    scale_y_continuous(breaks = seq(-200, 260, 40), limits = c(-200, 260)) +
    theme(panel.grid.major=element_blank()) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.line = element_line(colour = "black", size=0.7), axis.ticks = element_line(color="black",size=0.7)) +
    theme(axis.text.x=element_text(size=13,color="black", angle = 45, vjust = 0.95, hjust = 0.95)) +
    theme(axis.text.y=element_text(size=13,color="black")) +
    theme(axis.title.x=element_text(size=14,color="black", vjust=0)) +
    theme(axis.title.y=element_text(size=14,color="black", vjust=1.25),
          title = element_text(size = 16, face = "bold"),
          strip.text = element_text(size = 13)) +
    theme(legend.text = element_text(size = 13),
          legend.key.width = unit(1,"cm")) 
    guides(linetype = guide_legend(override.aes = list(shape = c(15,19), color = c("black", "grey65")),
                                   order = 1),
           fill = guide_legend(order = 2))
  
  
  ggsave(filename = paste("figures/",
                          C1,
                          "_combined.pdf",
                          sep = ""),
         height = 8,
         width = 11,
         units = "in")
}

anc_comb_plot("dolly_sods", "Dolly Sods")
anc_comb_plot("otter_creek", "Otter Creek")










# analysed separately
ggplot(doso, aes(x = year, y = ANC_ueq_L, group = ID)) +
  geom_line(aes(color = ID), size = 1.2) +
  geom_point(aes(color = ID), size = 2) +
  # geom_smooth(aes(color = ID), method = "lm") +
  scale_color_viridis(discrete = TRUE, name = "Sample Site") +
  theme_minimal() +
  labs(y = expression(ANC~(mu*eq~L^-1)),
       x = "Year",
       title = "Dolly Sods") +
  # scale_y_continuous(breaks=seq(0,160, by=20), limits=c(0,160)) +
  scale_x_continuous(breaks=seq(2001,2019, by=1), limits=c(2001,2019)) +
  theme(panel.grid.major=element_blank()) +
  theme(panel.grid.minor=element_blank()) +
  theme(axis.line = element_line(colour = "black", size=0.7), axis.ticks = element_line(color="black",size=0.7)) +
  theme(axis.text.x=element_text(size=13,color="black", angle = 45, vjust = 0.95, hjust = 0.95)) +
  theme(axis.text.y=element_text(size=13,color="black")) +
  theme(axis.title.x=element_text(size=14,color="black", vjust=0)) +
  theme(axis.title.y=element_text(size=14,color="black", vjust=1.25),
        title = element_text(size = 16, face = "bold")) 

ggsave(filename = "figures/dolly_sods_separate.pdf",
       height = 5,
       width = 8,
       units = "in")
# 








ggplot(doso, aes(x = year, y = ANC_ueq_L)) +
  # geom_line(aes(color = ID), size = 1.2) +
  geom_point() +
  geom_smooth(method = "lm") +
  # scale_color_viridis(discrete = TRUE, name = "") +
  theme_minimal() 






# shapefile with liming
lime_shp <- readOGR("../appa_CL_model/GIS/waterchemsites_master_1213_n83")
lime_shp <- spTransform(lime_shp, CRS("+proj=longlat +datum=WGS84"))
lime_points <- data.frame(Latitude = lime_shp@coords[,2], Longitude = lime_shp@coords[,1], lime_shp@data)
lime_points$ID <- as.character(lime_points$ID)



# join the two together to explore liming and water trends
water <- lime_points %>% 
  select(ID, Latitude, Longitude, Water_Unit_Type = Site_Locat, Monitoring_Site_Name = Waterbody, Limed) %>% 
  left_join(water_dat, by = "ID") %>% 
  mutate(FS_ID = rep(NA, n()),
         CSU_ID = rep(NA, n()),
         ST = rep("WV", n()),
         Project_Name = rep(NA, n()),
         Sample_Type = rep(NA, n()),
         NF_Wilderness = rep("Monongahela NF", n()),
         Time_Sampled = rep(NA, n()),
         Lab_Receive_Date = rep(NA, n()),
         Sample_Collector= rep(NA, n()),
         Smpl_Wgt	= rep(NA, n()),
         Field_Filtered	= rep(NA, n()), 
         Field_pH= rep(NA, n()),
         Field_Cond= rep(NA, n()),
         NH4_mg_per_l = rep(NA, n()),
         NH4_N_mg_per_l = rep(NA, n()),
         NO3_mg_per_l = rep(NA, n()),
         F_mg_per_l = rep(NA, n()),
         TP_mg_per_l = rep(NA, n()),
         ICP_mg_per_l = rep(NA, n())) 