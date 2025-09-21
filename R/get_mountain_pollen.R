



install.packages("neotoma2")

library(neotoma2)
library(sf)
library(dplyr)
library(purrr)
library(ggplot2)


## Search for sites
#all_pollen <- get_sites(datasettype = "pollen", all_data = TRUE) # heavy on the server!

# global count
#all_meta <- as.data.frame(all_pollen)
#head(all_meta)
# download for later use
#write.csv(all_meta, "all_pollen_sites_global.csv", row.names = FALSE)
#saveRDS(all_meta, "all_pollen_sites_global.rds")

# reload later 
all_meta <- readRDS("all_pollen_sites_global.rds")


# Filter to mountain sites (> 100 m)
mountain_sites_100elev <- subset(all_meta, elev >= 100)
nrow(mountain_sites_100elev) # number of sites above 100 m is 3207

summary(all_meta$elev)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -4087.0    35.0   264.0   585.8   784.0  4866.0

## Option 2: overlay all pollen records with GMBA mountain inventory

# Convert to sf for GMBA overlay
sites_sf <- st_as_sf(all_meta,
                     coords = c("long", "lat"),
                     crs = 4326)

gmba <- st_read("shapefiles/GMBA_Inventory_v2.0_standard_300.shp") %>%
  st_transform(4326)


# some polygons in the GMBA shapefile are self-intersecting or otherwise invalid
sf_use_s2(FALSE) # disable s2


## intersect pollen records coordinates with GMBA polygons
idx <- st_intersects(sites_sf, gmba)
sites_in_gmba <- sites_sf[lengths(idx) > 0, ]
nrow(sites_in_gmba) # 1275 records


# EXPORT FOR LATER USE

# Drop the geometry column to get a plain data frame
sites_export <- st_drop_geometry(sites_in_gmba)

# export
write.csv(sites_export, "pollen_sites_in_mountains.csv", row.names = FALSE)


# -----------------------------
# FIGURES

## base map
plot(st_geometry(gmba), col = "lightgrey", border = "white")
plot(st_geometry(sites_in_gmba), col = "red", pch = 20, cex = 0.6, add = TRUE)

ggplot() +
  geom_sf(data = gmba, fill = "grey95", color = "grey80") +
  geom_sf(data = sites_in_gmba, color = "black", size = 0.8, alpha = 0.7) +
  theme_minimal()


# colored by GMBA

# Re-run the join so each site gets the GMBA attributes
sites_in_gmba <- st_join(sites_sf, gmba, join = st_intersects, left = FALSE)
names(sites_in_gmba)


# level 03 of GMBA with legend
ggplot() +
  geom_sf(data = gmba, fill = "grey95", color = "grey80") +
  geom_sf(data = sites_in_gmba, aes(color = Level_03), size = 1, alpha = 0.8) +
  theme_minimal() +
  theme(legend.position = "right") +
  guides(color = guide_legend(ncol = 2, override.aes = list(size = 3))) +
  labs(color = "GMBA Level 03",
       title = "Fossil Pollen Records by GMBA Level 03")


# level 03 of GMBA without legend
ggplot() +
  geom_sf(data = gmba, fill = "grey95", color = "grey80") +
  geom_sf(data = sites_in_gmba, aes(color = Level_03), size = 1, alpha = 0.8, show.legend = FALSE) +
  theme_minimal() +
  labs(title = "Fossil Pollen Records by GMBA Level 03")

# level 02 of GMBA with legend
ggplot() +
  geom_sf(data = gmba, fill = "grey95", color = "grey80") +
  geom_sf(data = sites_in_gmba, aes(color = Level_02), size = 1, alpha = 0.8) +
  theme_minimal() +
  theme(legend.position = "right") +
  guides(color = guide_legend(ncol = 2, override.aes = list(size = 3))) +
  labs(color = "GMBA Level 02",
       title = "Fossil Pollen Records by GMBA Level 03")


# level 02 of GMBA without legend
ggplot() +
  geom_sf(data = gmba, fill = "grey95", color = "grey80") +
  geom_sf(data = sites_in_gmba, aes(color = Level_02), size = 1, alpha = 0.8, show.legend = FALSE) +
  theme_minimal() +
  labs(title = "Fossil Pollen Records by GMBA Level 03")





# facet by continent level 01
ggplot(sites_in_gmba) +
  # Plot GMBA polygons in grey for context
  geom_sf(data = gmba, fill = "grey95", color = "grey80", inherit.aes = FALSE) +
  # Plot pollen sites, colored by Level_03
  geom_sf(aes(color = Level_03), size = 1, alpha = 0.8, show.legend = FALSE) +
  # One map per continent
  facet_wrap(~ Level_01) +
  theme_minimal() +
  labs(title = "Fossil Pollen Records by GMBA Level 03, Faceted by Continent (Level_01)")


## legend inside each facet  - TOO BIG THE LEGEND - not very useful
# ggplot(sites_in_gmba) +
#   geom_sf(data = gmba, fill = "grey95", color = "grey80", inherit.aes = FALSE) +
#   geom_sf(aes(color = Level_03), size = 1, alpha = 0.8) +
#   facet_wrap(~ Level_01) +
#   theme_minimal() +
#   guides(color = guide_legend(ncol = 1, override.aes = list(size = 2))) +
#   labs(color = "GMBA Level 03",
#        title = "Fossil Pollen Records by GMBA Level 03, Faceted by Continent")


# -------------------------------------
# SUMMARIZE

### Summarize counts per Level_03
sites_summary_lvl3 <- sites_in_gmba %>%
  st_drop_geometry() %>%
  count(Level_03, sort = TRUE)

head(sites_summary_lvl3, 20)

write.csv(sites_summary_lvl3, "sites_summary_lvl3.csv", row.names = FALSE)



