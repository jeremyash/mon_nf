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
library(ggnewscale)
library(ggpubr)
library(cowplot)
library(patchwork)
#----------------------------------------------------------------------------



# water chem shapefile
wat_shp <- readOGR("../appa_CL_model/GIS/waterchemsites_master_1213_n83")
wat_shp <- spTransform(wat_shp, CRS("+proj=longlat +datum=WGS84"))
wat_points <- data.frame(Latitude = wat_shp@coords[,2], Longitude = wat_shp@coords[,1], wat_shp@data)
wat_points$ID <- as.character(wat_points$ID)
crs_new <- proj4string(wat_shp)

mon <- readOGR("gis/mon_nf")
mon <- spTransform(mon, crs_new)
mon_sf <- st_as_sf(mon)

mon_c1 <- readOGR("../fs_admin/data/mon_c1")
mon_c1 <- spTransform(mon_c1, crs_new)
mon_c1_sf <- st_as_sf(mon_c1)

lime_wv <- readOGR("gis/WV_LimestoneSites_06162017") 
lime_wv <- spTransform(lime_wv, crs_new)
lime_wv <- lime_wv[lime_wv$Active == "active",]

lime_mon <- over(lime_wv, mon) %>% 
  bind_cols(., lime_wv@data) %>% 
  filter(!(is.na(NAME))) %>% 
  pull(OBJECTID1)

lime_wv <- lime_wv[lime_wv$OBJECTID %in% lime_mon, ]

lime_ctds <- as.data.frame(lime_wv@coords) 
lime_ctds$limed <- rep("active", dim(lime_ctds)[1])
colnames(lime_ctds)[1:2] <- c("lon", "lat")

# relief map
mon_relief <- readRDS("../appa_CL_model/GIS/mon_relief.RDS")

# relief
relief_df <- as.data.frame(mon_relief, xy = TRUE) %>% 
  dplyr::select(x, y, relief = srgr48i0100a_Value) %>% 
  filter(!(is.na(relief)))

rm(mon_relief)

acid <- readOGR("gis/mon_acid_sens_soils")
acid <- spTransform(acid, crs_new)
acid_sf <- st_as_sf(acid) %>% 
  rename(sens = SENSITIVIT) %>% 
  mutate(sens = factor(sens, 
                       levels = c("H", "M", "L", "NA"),
                       labels = c("High", "Moderate", "Low", "NA")))


water_chem <- read_excel("raw_data/WaterChem_MasterResults_031320.xlsx",
                                      na = c(".", "NS", "ND", "<0.04", "<0.03")) %>% 
  # select(ID, Waterbody, Collection, area, ANC_ueq_L) %>% 
  mutate(year = year(Collection),
         month = month(Collection)) %>% 
  mutate(ID = as.character(ID),
         season = if_else(month %in% c(9,10,11), "Fall", "Spring")) %>% 
  rename(anc = ANC_ueq_L) %>% 
  group_by(ID, season) %>% 
  arrange(desc(year)) %>% 
  slice(1:3) %>% 
  summarise(mean_anc = mean(anc)) %>% 
  ungroup() 
  


# join the two together to explore liming and water trends
water_dat <- wat_points %>% 
  select(ID, Latitude, Longitude, Water_Unit_Type = Site_Locat, Monitoring_Site_Name = Waterbody, Limed) %>% 
  left_join(water_chem, by = "ID") %>% 
  mutate(limed = str_sub(Limed, 1, 1)) %>% 
  select(-Limed) 


# create breaks in data for plotting
brks  <- c(-Inf, 0, 50, 200, Inf)
labels <- c("Acidified", "Acid sensitive", "Buffered",  "Well buffered")

water_dat$anc_fac <- cut(water_dat$mean_anc,
                                  breaks = brks,
                                  include.lowest = TRUE,
                                  labels = labels)

# water_dat$anc_fac_ch <- as.character(water_dat$anc_fac)
water_dat$buffered <- if_else(water_dat$limed == "Y" & water_dat$anc_fac %in% c("Buffered", "Well buffered"),
                                "Buffered from liming",
                                as.character(water_dat$anc_fac))

water_dat <- water_dat %>% 
  mutate(anc_ch = coalesce(buffered, as.character(anc_fac))) %>% 
  mutate(anc_buf_fac = factor(anc_ch,
                              levels = c("Acidified", "Acid sensitive", "Buffered",  "Well buffered", "Buffered from liming"))) %>% 
  filter(!(is.na(anc_buf_fac)))

water_dat$limed[water_dat$limed == "?"] <- "N"
water_dat$limed[is.na(water_dat$limed)] <- "N"


fall_dat <- water_dat %>% 
  filter(season == "Fall") 

spring_dat <- water_dat %>% 
  filter(season == "Spring")

acidic_limed <- water_dat %>% 
  filter(limed == "Y") %>% 
  filter(anc_buf_fac != "Buffered from liming") %>% 
  select(1:8)

write_csv(acidic_limed,
          "data/acidified_limed_sites.csv")




#----------------------------------------------------------------------------



# plot ANC points to get legend
anc_plot <- ggplot() +
  geom_point(aes(Longitude, Latitude, fill = anc_fac), 
             shape = 21,
             color = "grey15",
             size = 1.5, 
             data = fall_dat) +
  scale_fill_manual(values = c("darkred", "tomato3", "darkolivegreen3", "darkgreen"),
                    name = expression(bold(Acid~Neutralizing~Capacity~(mu*eq~L^-1))))  +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_line(color = "white"),
        legend.text  = element_text(size = 13),
        legend.title = element_text(size = 13, face = "bold")) +
  guides(fill = guide_legend(override.aes = list(size = 4)))

anc_legend <- get_legend(anc_plot)

# plot ANC points to get legend
sens_plot <- ggplot() +
  geom_sf(aes(fill = NULL), color = "black", mon_sf) +
  geom_sf(aes(fill = sens), color = NA, acid_sf) +
  scale_fill_manual(values = alpha(c("red", "yellow", "green", "grey85"), 0.3),
                    na.translate = FALSE,
                    name = "Acid Sensitive Geology") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_line(color = "white"),
        legend.text  = element_text(size = 13),
        legend.title = element_text(size = 13, face = "bold")) 

sens_legend <- get_legend(sens_plot)

# limed_plot <- ggplot() +
#   geom_point(aes(Longitude, Latitude, shape = limed), 
#              # shape = 21,
#              color = "grey15",
#              size = 1.5, 
#              data = spring_dat) +
#   scale_shape_manual(values = c(21, 24),
#                      name = "Lime Addition?",
#                      labels = c("No", "Yes")) +
#   theme_minimal() +
#   theme(axis.title = element_blank(),
#         axis.text = element_blank(),
#         panel.grid.major = element_line(color = "white"),
#         legend.text  = element_text(size = 13),
#         legend.title = element_text(size = 13, face = "bold")) +
#   guides(shape = guide_legend(override.aes = list(size = 4)))

limed_plot <- ggplot() +
  geom_point(aes(lon, lat, shape = "limed"),
             color = "midnightblue",
             size = 1.5,
             data = lime_ctds) +
  scale_shape_manual(values = c(17),
                     name = "Liming Site",
                     labels = "") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_line(color = "white"),
        legend.text  = element_text(size = 13),
        legend.title = element_text(size = 13, face = "bold")) +
  guides(shape = guide_legend(override.aes = list(size = 4)))


limed_legend <- get_legend(limed_plot)

# plot_map <- ggplot() +
#   geom_sf(aes(fill = NULL), alpha = 0, color = "black", mon_sf) +
#   # geom_raster(data = relief_df, aes(x = x,
#   #                                   y = y,
#   #                                   alpha = relief)) +
#   # use the "alpha hack"
#   scale_alpha(name = "", range = c(0.6, 0), guide = F)  +
#   geom_sf(aes(fill = sens), color = NA, acid_sf) +
#   geom_sf(aes(fill = NULL), alpha = 0, color = "black", size = 1.5, mon_c1_sf) +
#   scale_fill_manual(values = alpha(c("red", "yellow", "green", "grey85"), 0.3),
#                     na.translate = FALSE,
#                     name = "Acid Sensitive Geology",
#                     guide = FALSE) +
#   new_scale_fill() +
#   geom_point(aes(Longitude, Latitude, fill = anc_buf_fac), 
#              shape = 21,
#              color = "grey15",
#              size = 2.3, 
#              data = spring_dat) +
#   scale_fill_manual(values = c("darkred", "tomato3", "darkolivegreen3", "darkgreen", "midnightblue"),
#                      name = expression(bold(Acid~Neutralizing~Capacity~(mu*eq~L^-1))),
#                     guide = FALSE)  +
#   theme_minimal() +
#   scale_x_continuous(limits = c(-80, -79.1)) +
#   scale_y_continuous(limits = c(38.8, 39.3)) +
#   theme(axis.title = element_blank(),
#         axis.text = element_blank(),
#         panel.grid.major = element_line(color = "white"),
#         legend.text  = element_text(size = 13),
#         legend.title = element_text(size = 13, face = "bold"),
#         plot.margin = margin(0,0,0,0)) 

plot_map <- ggplot() +
  geom_sf(aes(fill = NULL), alpha = 0, color = "black", mon_sf) +
  # geom_raster(data = relief_df, aes(x = x,
  #                                   y = y,
  #                                   alpha = relief)) +
  # use the "alpha hack"
  scale_alpha(name = "", range = c(0.6, 0), guide = F)  +
  geom_sf(aes(fill = sens), color = NA, acid_sf) +
  geom_sf(aes(fill = NULL), alpha = 0, color = "black", size = 1.5, mon_c1_sf) +
  scale_fill_manual(values = alpha(c("red", "yellow", "green", "grey85"), 0.3),
                    na.translate = FALSE,
                    name = "Acid Sensitive Geology",
                    guide = FALSE) +
  new_scale_fill() +
  geom_point(aes(Longitude, Latitude, fill = anc_fac),
   
             shape = 21,
             color = "grey15",
             size = 3.5, 
             data = fall_dat) +
  geom_point(aes(lon, lat, shape = "limed"),
             color = "midnightblue",
             size = 3.5,
             data = lime_ctds) +
  scale_shape_manual(values = c(17),
                     name = "Liming Site",
                     guide = FALSE) +
  scale_fill_manual(values = c("darkred", "tomato3", "darkolivegreen3", "darkgreen"),
                    name = expression(bold(Acid~Neutralizing~Capacity~(mu*eq~L^-1))),
                    guide = FALSE)  +
  theme_minimal() +
  scale_x_continuous(limits = c(-80, -79.1)) +
  scale_y_continuous(limits = c(38.8, 39.3)) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_line(color = "white"),
        legend.text  = element_text(size = 13),
        legend.title = element_text(size = 13, face = "bold"),
        plot.margin = margin(0,0,0,0)) 


bbox_df <- tibble(x = c(-80, -80, -79.1, -79.1, -80),
                  y =  c(39.3, 38.8, 38.8, 39.3, 39.3))

inset_map <- ggplot() +
  geom_sf(fill = NA,  color = "black", data = mon_sf) +
  geom_path(data=bbox_df, aes(x,y), color="red", lwd=1) + 
  theme_void()


plot_inset_map <- ggdraw() +
  draw_plot(plot_map) +
  draw_plot(inset_map, x = 0.65, y = 0.65, width = 0.3, height = 0.3)


# create a blank plot for legend alignment 
blank_p <- plot_spacer() + theme_void()

# combine legend 1 & 2
leg12 <- plot_grid(blank_p, 
                   anc_legend,
                   limed_legend,
                   sens_legend, 
                   blank_p,
                   ncol = 5)

# # combine legend 3 & blank plot
# leg30 <- plot_grid(leg3, blank_p,
#                    blank_p, 
#                    nrow = 3
# )
# 
# # combine all legends
# leg123 <- plot_grid(leg12, leg30,
#                     ncol = 2
# )


final_p <- plot_grid(plot_inset_map,
                     leg12,
                     nrow = 2,
                     align = "hv",
                     axis = "l",
                     # hjust = 0.5,
                     rel_heights = c(4, 1)
)

save_plot(filename = "figures/anc_3yr_avg_map_fall.pdf",
          plot = final_p,
          nrow = 1,
          base_height = 8,
          base_width = 11)


































