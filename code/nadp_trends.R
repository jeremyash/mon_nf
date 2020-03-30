library(tidyverse)
library(skimr)
library(patchwork)
library(readxl)
library(sp)
library(rgeos)
library(raster)
library(rgdal)
library(scales)
library(units)
library(viridis)
library(extrafont)
library(grid)
library(zoo)
#----------------------------------------------------------------------------


#############################################################################
## load data: http://nadp.slh.wisc.edu/data/ntn/ntnAllsites.aspx
#############################################################################

# airshed
mon <- readOGR("gis/mon_nf") 
mon <- spTransform(mon, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

# nadp site ctds
site_ctds_df <-read_csv("C:/Users/jash/Documents/projects/tdepraw_data/nadp_site_ctds.csv") %>% 
  dplyr::select(siteID = "Site ID", Latitude, Longitude)


# Annual Precipitation-Weighted Mean Concentrations:
nadp_conc <- read_csv("raw_data/NTN-All-cy.csv") %>% 
  dplyr::select(siteID, yr, pH)

# Annual Depositions ()
nadp_dep <- read_csv("raw_data/NTN-All-cydep.csv") %>% 
  dplyr::select(siteID, yr, SO4, totalN)

# NADP dat
nadp_dat <- left_join(nadp_conc, nadp_dep, by = c("siteID", "yr"))

# create NA values
nadp_dat[nadp_dat == -9] <- NA




#############################################################################
## overlay sites to get NADP sites in airshed
#############################################################################


site_ctds <- with(site_ctds_df, data.frame(Longitude, Latitude, row.names = siteID)) 
site_ctds <- na.omit(site_ctds)

site_sp_points <- SpatialPoints(site_ctds, proj4string = CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")) 
site_sp_points <- spTransform(site_sp_points, proj4string(planning))


wayne_nadp <- over(site_sp_points, wayne) %>% 
  mutate(siteID = row.names(.)) %>%
  mutate_if(is.factor, as.character) %>% 
  filter(!(is.na(NAME))) %>% 
  dplyr::select(siteID) %>% 
  left_join(., nadp_dat, by = "siteID")

planning_nadp <- over(site_sp_points, planning) %>% 
  mutate(siteID = row.names(.)) %>%
  mutate_if(is.factor, as.character) %>% 
  filter(!(is.na(OBJECTID))) %>% 
  dplyr::select(siteID) %>% 
  left_join(., nadp_dat, by = "siteID")

#############################################################################
## summarise data
#############################################################################

wayne_sum <- wayne_nadp %>% 
  gather(variable, value, pH:totalN) %>% 
  group_by(yr, variable) %>% 
  summarise(mean_value = mean(value, na.rm = TRUE),
            min_value = min(value, na.rm = TRUE),
            max_value = max(value, na.rm = TRUE),
            sd_value = sd(value, na.rm = TRUE)
            ) %>% 
  ungroup()
  



#############################################################################
## plotting
#############################################################################




##-------------
## ph
##-------------

ph_lims <- range(wayne_nadp$pH, na.rm = TRUE )

ph_dat <- wayne_sum %>% 
  filter(variable == "pH") %>% 
  filter(yr >= 1993)

ggplot(aes(yr, mean_value, group = variable), data = ph_dat) +
  geom_errorbar(aes(ymin=mean_value-sd_value, ymax=mean_value+sd_value), colour="black", width=.6) +
  geom_line(color = "darkred", alpha = 0.8, size = 1.2) +
  geom_point(size=4, shape=21, fill="darkred", color = "darkred", alpha = 0.8) +
    theme_minimal() +
    labs(x = "Year", 
         y = "Lab pH", 
         title = "Acidity of Precipitation",
         subtitle = "in Wayne NF Airshed") +
    # scale_color_viridis(discrete = TRUE, name = NULL,
    #                     guide = guide_legend(
    #                       # keywidth = 3,
    #                       # keyheight = 5,
    #                       # units = "cm",
    #                       nrow = 2,
    #                       ncol = 2)) +
    scale_color_manual(values = cols,
                       name = NULL,
                       ) +
    # scale_x_continuous(limits = c(min(plot_dat$yr) - 1, max(plot_dat$yr) + 1),
    #                    breaks = seq(min(plot_dat$yr), max(plot_dat$yr), x_breaks)) +
    scale_x_continuous(limits = c(1992, 2018),
                       breaks = seq(1993, 2017, 3),
                       minor_breaks = seq(1993, 2017, 1),
                       labels = seq(1993, 2017, 3)) +
    scale_y_continuous(lim = c(4, 6), breaks = seq(4,6, 0.25), labels = c("More acidic", 4.25, 4.50, 4.75, 5.00, 5.25, 5.50, 5.75, "Less acidic")) +
  theme(axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 0.95, vjust = 0.95),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 20),
        legend.text = element_text(size = 18))

  
 
  ggsave(filename = "figures/ph_trend.jpg",
         height = 4,
         width = 6,
         units = "in")
  

##-------------
## so4
##-------------

s_lims <- range(wayne_nadp$SO4, na.rm = TRUE)

s_dat <- wayne_sum %>% 
    filter(variable == "SO4") %>% 
  filter(yr >= 1993)


ggplot(aes(yr, mean_value, group = variable), data = s_dat) +
  geom_errorbar(aes(ymin=mean_value-sd_value, ymax=mean_value+sd_value), colour="black", width=.6) +
  geom_line(color = "darkred", alpha = 0.8, size = 1.2) +
  geom_point(size=4, shape=21, fill="darkred", color = "darkred", alpha = 0.8) +
    theme_minimal() +
    labs(x = "Year", 
         y = expression(paste(SO[4]^paste("  2", "-"), " kg/ha")), 
         title = "Sulfur Deposition",
         subtitle = "in Wayne NF Airshed") +
  scale_x_continuous(limits = c(1992, 2018),
                     breaks = seq(1993, 2017, 3),
                     minor_breaks = seq(1993, 2017, 1),
                     labels = seq(1993, 2017, 3)) +
    scale_y_continuous(lim = c(0, 36), breaks = seq(0,36, 4)) +
  theme(axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 0.95, vjust = 0.95),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 20),
        legend.text = element_text(size = 18))

  

ggsave(filename = "figures/so4_trend.jpg",
       height = 4,
       width = 6,
       units = "in")

##-------------
## n
##-------------

n_lims <- range(nadp_sites$totalN, na.rm = TRUE)


n_dat <- wayne_sum %>% 
  filter(variable == "totalN") %>% 
  filter(yr >= 1993)


ggplot(aes(yr, mean_value, group = variable), data = n_dat) +
  geom_errorbar(aes(ymin=mean_value-sd_value, ymax=mean_value+sd_value), colour="black", width=.6) +
  geom_line(color = "darkred", alpha = 0.8, size = 1.2) +
  geom_point(size=4, shape=21, fill="darkred", color = "darkred", alpha = 0.8) +
    theme_minimal() +
    labs(x = "Year", 
         y = expression(paste(NO[3], " & ", NH[4], " kg/ha")), 
         title = "Nitrogen Deposition", 
         subtitle = "in Wayne NF Airshed") +
   scale_x_continuous(limits = c(1992, 2018),
                     breaks = seq(1993, 2017, 3),
                     minor_breaks = seq(1993, 2017, 1),
                     labels = seq(1993, 2017, 3)) +
    scale_y_continuous(lim = c(0, 9), breaks = seq(0,9, 1)) +
  theme(axis.title = element_text(size = 16),
        axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14, angle = 45, hjust = 0.95, vjust = 0.95),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 20),
        legend.text = element_text(size = 18))
  




ggsave(filename = "figures/n_trend.jpg",
       height = 4,
       width = 6,
       units = "in") 




