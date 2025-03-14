##### Required Packages #####
setwd("C:/Users/gmsan/Dropbox/Object Detection")
#install.packages("sf")
#install.packages("tmap")
library(sf)
library(SpatialKDE)
library(sp)
library(sf)
library(dplyr)
library(tmap)
library(raster)
library(adehabitatHR)
library(terra)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(spatstat)
library(gdalraster)
library(readxl)
library(dplyr)
library(viridis)
library(png)
library(leaflet)
epsg_code = 4326 # fix the epsg_code
crs_wkt = st_crs(epsg_code)$wkt
cat("WKT Representation:\n", crs_wkt)
crs_proj = st_crs(epsg_code)$proj4string
cat("\n\nPROJ String:\n", crs_proj)
##### Count #####
shape_sf_obs_4 = read_sf(dsn = paste(getwd(), sep ="", "/Dati/Manual Count/13-4-22_manual_count.shp"))
shape_sf_est_4 = read_sf(dsn = paste(getwd(), sep = "", "/Dati/Modello 200 Null no tiles/13-04-22_Conf_0.47-IoU_0.5.shp"))

shape_sf_obs_2 = read_sf(dsn = paste(getwd(), sep ="", "/Dati/Manual Count/20-2-22_manual_count.shp"))
shape_sf_est_2 = read_sf(dsn = paste(getwd(), sep = "", "/Dati/Modello 200 Null no tiles/20-02-22_Conf_0.47-IoU_0.5.shp"))

shape_sf_obs_6 = read_sf(dsn = paste(getwd(), sep ="", "/Dati/Manual Count/20-6-22_manual_count.shp"))
shape_sf_est_6 = read_sf(dsn = paste(getwd(), sep = "", "/Dati/Modello 200 Null no tiles/20-06-22_Conf_0.47-IoU_0.5.shp"))

shape_sf_obs_8 = read_sf(dsn = paste(getwd(), sep ="", "/Dati/Manual Count/24-8-22_manual_count.shp"))
shape_sf_est_8 = read_sf(dsn = paste(getwd(), sep = "", "/Dati/Modello 200 Null no tiles/24-08-22_Conf_0.47-IoU_0.5.shp"))

shape_sf_obs_10 = read_sf(dsn = paste(getwd(), sep ="", "/Dati/Manual Count/20-10-22_manual_count.shp"))
shape_sf_est_10 = read_sf(dsn = paste(getwd(), sep = "", "/Dati/Modello 200 Null no tiles/20-10-22_Conf_0.47-IoU_0.5.shp"))

#shape_sf_obs = read_sf(dsn = paste(getwd(), sep ="", "/Dati/Manual Count/13-4-22_manual_count.shp"))
#shape_sf_est = read_sf(dsn = paste(getwd(), sep = "", "/Dati/Modello 200 Null no tiles/13-04-22_Conf_0.47-IoU_0.5.shp"))

st_crs(shape_sf_est)
shape_sf_obs$geometry

my_crs = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +type=crs"
#st_set_crs(shape_sf_obs, value = 4326)   
#st_set_crs(shape_sf_est, value = 4326)  
#shape_sf_est$geometry

shape_utm_obs = st_transform(shape_sf_obs, crs = 4326)
shape_utm_est = st_transform(shape_sf_est, crs = 4326)
#st_crs(shape_utm_obs)


plot(st_geometry(shape_utm_est));plot(st_geometry(shape_utm_obs))
coord_obs = st_coordinates(st_centroid(shape_sf_obs))
coord_est = st_coordinates(st_centroid(shape_sf_est))
coord_est_2 = as.data.frame(coord_est)
coord_obs_2 = as.data.frame(coord_obs)

colnames(coord_obs) = c("X", "Y")
colnames(coord_est) = c("X", "Y")
coord_obs_2$category = "observed"
coord_est_2$category = "estimated"
combined = rbind(coord_obs_2, coord_est_2)
colnames(combined) = c("X", "Y", "Type")

p_combined = ggplot(combined, aes(X, Y, color = Type)) +
  geom_point(alpha = 1) +
  scale_color_manual(values = c("observed" = "red", "estimated" = "blue", color = "boh")) +
  coord_fixed() +
  labs(title = "Observed vs Estimated Points",
       x = "long", 
       y = "lat") +
  theme_minimal()


p_combined


p1 = ggplot(data = coord_obs_2, aes(X, Y)) +
  geom_density2d() +
  geom_point(alpha = 0.4, color = "darkgreen") +
  coord_fixed() +
  labs(title = "Observed 13-04-2022",
       x = "long", y = "lat") +
  theme_minimal()
p1
p2 = ggplot(data = as.data.frame(coord_est), aes(X, Y)) +
  geom_density2d() +
  geom_point(alpha = 0.4, color = "darkblue") +
  coord_fixed() +
  labs(title = "Estimated 13-04-2022",
       x = "long", y = "lat") +
  theme_minimal()
grid.arrange(p1, p2, p3,  nrow = 1, ncol = 3)


p3 = ggplot(data = r, aes(X, Y)) +
  #geom_density2d() +
  geom_point(alpha = 0.4, color = "firebrick") +
  coord_fixed() +
  labs(title = "Miss Detection 13-04-2022",
       x = "long", y = "lat") +
  theme_minimal()

p1 + p2 + p3


p_combined

###### Miss detection #####
FP = read_excel(path = paste(getwd(), sep = "", "/Dati/Habitat Match/FP.xlsx"))
Miss = read_excel(path = paste(getwd(), sep = "", "/Dati/Habitat Match/Miss.xlsx"))
coord_fp = FP[, 7:8]
habitat = colnames(FP)[9:13]
habitat = unlist(strsplit(habitat, fixed = TRUE, split = "..."))[c(1, 3, 5, 7, 9)]

unique(FP$Layer)
r1 = subset(Miss[, 9:13], Miss$Layer <=  as.POSIXct("2022-04-13 UTC"))
r2 = subset(Miss[,9:13], Miss$Layer <= as.POSIXct("2022-06-20 UTC") & Miss$Layer >= as.POSIXct("2022-02-21 UTC"))
r3 = subset(Miss[, 9:13], Miss$Layer >= as.POSIXct("2022-04-14 UTC") & Miss$Layer <= as.POSIXct("2022-08-24 UTC"))
r4 = subset(Miss[, 9:13], Miss$Layer >= as.POSIXct("2022-06-21 UTC") & Miss$Layer <= as.POSIXct("2022-10-20 UTC"))
r5 = subset(Miss[, 9:13], Miss$Layer >= as.POSIXct("2022-08-25 UTC"))

r1 = subset(FP[, 9:13], FP$Layer <=  as.POSIXct("2022-04-13 UTC"))
r2 = subset(FP[,9:13], FP$Layer <= as.POSIXct("2022-06-20 UTC") & FP$Layer >= as.POSIXct("2022-02-21 UTC"))
r3 = subset(FP[, 9:13], FP$Layer >= as.POSIXct("2022-04-14 UTC") & FP$Layer <= as.POSIXct("2022-08-24 UTC"))
r4 = subset(FP[, 9:13], FP$Layer >= as.POSIXct("2022-06-21 UTC") & FP$Layer <= as.POSIXct("2022-10-20 UTC"))
r5 = subset(FP[, 9:13], FP$Layer >= as.POSIXct("2022-08-25 UTC"))


df = data.frame(Campagna = rep(c("February", "April", "June", "August", "October"), each = 5), habitat = rep(habitat, 5), 
                Proporzioni = numeric(25))


hab = as.numeric(r5$POSIDONIA_TRANSPLANTED...9 > 0)
hab
u = r5[hab ==  0, ]
somma = apply(u[,1:4], MARGIN = 1, sum)
hab2 = sapply(1:nrow(u), function(z){ifelse(somma[z] < 20, 5, which.max(u[z, 1:4])) })
hab[hab == 0] = hab2
hab
xx = prop.table(table(hab))
df$Proporzioni[21:25] = xx
names(xx) = habitat
save(df, file = "FP.RData")
load("FP.RData")

df
ggplot(df, aes(x = habitat, y = Proporzioni, fill = habitat)) +
  geom_bar(stat = "identity", color = "black") + 
  geom_text(aes(label = round(Proporzioni, 2)), vjust = -0.5, size = 5) + 
  facet_wrap(~ Campagna) +
  scale_fill_brewer(palette = "Set3") + 
  theme_minimal(base_size = 14) +
  ylim(c(0, 0.5))+
  theme(axis.text.x = element_blank(),  
        axis.ticks.x = element_blank(), 
        legend.position = "bottom",  
        legend.background = element_rect(color = "black", fill = "white"),  
        panel.border = element_rect(color = "black", fill = NA),  
        strip.text = element_text(face = "bold")) +  
  labs(title = "Mispredicted images per habitat for the different sampling",
       x = "",
       y = "",
       fill = "Habitat") + 
  guides(fill = guide_legend(nrow = 1))  


df$Campagna <- factor(df$Campagna, levels = c("20-02-2022", "13-04-2022", 
                                              "20-06-2022", "24-08-2022", "20-10-2022"))


unique(df$habitat)

df[df$habitat == "POSIDONIA_TRANSPLANTED", 2] = "POSIDONIA TRANSPLANTED"
df[df$habitat == "DEAD__MATTE__WITH_SAND", 2] = "DEAD MATTE WITH SAND"
df[df$habitat == "POSIDONIA_OCEANICA", 2] = "POSIDONIA OCEANICA"
df[df$habitat == "HARD_BOTTOMS_WITH_PHOTOPHILIC_ALGAE", 2] = "HARD BOTTOMS WITH PHOTOPHILIC ALGAE"
df[df$habitat == "SANDY_BOTTOMS", 2] = "SANDY BOTTOMS"
ggplot(df, aes(x = habitat, y = Proporzioni, fill = habitat)) +
  geom_bar(stat = "identity", color = "black") + 
  geom_text(aes(label = round(Proporzioni, 2)), vjust = -0.5, size = 5) + 
  facet_wrap(~ Campagna, 
             labeller = as_labeller(c("20-02-2022" = "February", 
                                      "13-04-2022" = "April",
                                      "20-06-2022" = "June", 
                                      "24-08-2022" = "August", 
                                      "20-10-2022" = "October"))) + 
  scale_fill_manual(values = c(
    "POSIDONIA TRANSPLANTED" = "green",
    "POSIDONIA OCEANICA" = "#257401",
    "HARD BOTTOMS WITH PHOTOPHILIC ALGAE" = "#73b2ff",
    "SANDY BOTTOMS" = "#feff01",
    "DEAD MATTE WITH SAND" = "brown"
  )) + 
  theme_minimal(base_size = 14) +
  ylim(c(0, 0.5)) +
  theme(axis.text.x = element_blank(),  
        axis.ticks.x = element_blank(), 
        legend.position = "bottom",  
        legend.background = element_rect(color = "black", fill = "white"),  
        panel.border = element_rect(color = "black", fill = NA),  
        strip.text = element_text(face = "bold")) +  
  labs(title = "",
       fill = "Habitat",
       y = "Frequency", 
       x = "") + 
  guides(fill = guide_legend(nrow = 1))




###### Intermediate Step ######

coord_est = st_coordinates(st_centroid(shape_sf_est_6))
coord_obs = st_coordinates(st_centroid(shape_sf_obs_6))

bbox_est = st_bbox(shape_sf_est_6)
bbox_obs = st_bbox(shape_sf_obs_6)

window = owin(
  xrange = c(bbox_est["xmin"], bbox_est["xmax"]),
  yrange = c(bbox_est["ymin"], bbox_est["ymax"])
)

window_obs = owin(
  xrange = c(bbox_obs["xmin"], bbox_obs["xmax"]),
  yrange = c(bbox_obs["ymin"], bbox_obs["ymax"])
)


ppp_est = ppp(x = coord_est[,1], y = coord_est[,2], window = window)
ppp_obs = ppp(x = coord_obs[,1], y = coord_obs[,2], window = window_obs)

bandwidth = bw.ppl(ppp_obs)


kde = density.ppp(ppp_est, sigma = bandwidth, kernel = "gaussian", at = "pixels", diggle = TRUE)
kde_obs = density.ppp(ppp_obs, sigma = bandwidth, kernel = "gaussian", at = "pixels", diggle = TRUE)

kde_raster = raster(kde)
kde_raster_obs = raster(kde_obs)

kde_df = as.data.frame(kde_raster, xy = TRUE)
colnames(kde_df)[3] = "density"
kde_df = kde_df[kde_df$density > 0.00012, ]

kde_df_obs = as.data.frame(kde_raster_obs, xy = TRUE)
colnames(kde_df_obs)[3] = "density"
kde_df_obs = kde_df_obs[kde_df_obs$density > 0.00012, ]


density_range = range(c(kde_df$density, kde_df_obs$density), na.rm = TRUE)

x_range = range(kde_df$x, kde_df_obs$x, na.rm = TRUE)
y_range = range(kde_df$y, kde_df_obs$y, na.rm = TRUE)

margin_x = 0.08 * diff(x_range)
margin_y = 0.08 * diff(y_range)

expanded_x = c(x_range[1] - margin_x, x_range[2] + margin_x)
expanded_y = c(y_range[1] - margin_y, y_range[2] + margin_y)
##### Kernel Density Estimation  ####


generate_density_map = function(shape_sf_obs, shape_sf_est, title_date) {

  
  coord_est = st_coordinates(st_centroid(shape_sf_est))
  coord_obs = st_coordinates(st_centroid(shape_sf_obs))
  
  bbox_est = st_bbox(shape_sf_est)
  bbox_obs = st_bbox(shape_sf_obs)
  
  window = owin(
    xrange = c(bbox_est["xmin"], bbox_est["xmax"]),
    yrange = c(bbox_est["ymin"], bbox_est["ymax"])
  )
  
  window_obs = owin(
    xrange = c(bbox_obs["xmin"], bbox_obs["xmax"]),
    yrange = c(bbox_obs["ymin"], bbox_obs["ymax"])
  )
  
  
  ppp_est = ppp(x = coord_est[,1], y = coord_est[,2], window = window)
  ppp_obs = ppp(x = coord_obs[,1], y = coord_obs[,2], window = window_obs)
  
  
  bandwidth = bw.ppl(ppp_obs)
  
  kde = density.ppp(ppp_est, sigma = bandwidth, kernel = "gaussian", at = "pixels", diggle = TRUE)
  kde_obs = density.ppp(ppp_obs, sigma = bandwidth, kernel = "gaussian", at = "pixels", diggle = TRUE)
  
  kde_raster = raster(kde)
  kde_raster_obs = raster(kde_obs)
  
  kde_df = as.data.frame(kde_raster, xy = TRUE)
  colnames(kde_df)[3] = "density"
  kde_df = kde_df[kde_df$density > 0.00012, ]
  
  kde_df_obs = as.data.frame(kde_raster_obs, xy = TRUE)
  colnames(kde_df_obs)[3] = "density"
  kde_df_obs = kde_df_obs[kde_df_obs$density > 0.00012, ]
  
  
  density_range = range(c(kde_df$density, kde_df_obs$density), na.rm = TRUE)
  
  x_range = range(kde_df$x, kde_df_obs$x, na.rm = TRUE)
  y_range = range(kde_df$y, kde_df_obs$y, na.rm = TRUE)
  

  
  map_est = ggplot() +
    geom_raster(data = kde_df, aes(x = x, y = y, fill = density)) +
    geom_sf(data = shape_sf_est, fill = NA, color = "black", alpha = 0.5) +
    scale_fill_viridis_c(name = "Density", limits = density_range) +
    theme_minimal() +
    coord_sf(xlim = expanded_x, ylim = expanded_y) +
    labs(title = paste("Detection Density Estimation", title_date),
         subtitle = paste("Gaussian Kernel", "Bandwidth:", round(bandwidth, 2)),
         x = "Longitude", y = "Latitude") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 9.5), 
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  map_obs = ggplot() +
    geom_raster(data = kde_df_obs, aes(x = x, y = y, fill = density)) +
    geom_sf(data = shape_sf_obs, fill = NA, color = "black", alpha = 0.5) +
    scale_fill_viridis_c(name = "Density", limits = density_range, guide = "none") +  
    theme_minimal() +
    coord_sf(xlim = expanded_x, ylim = expanded_y) +
    labs(title = paste("Manual Count Density Estimation", title_date),
         subtitle = paste("Gaussian Kernel", "Bandwidth:", round(bandwidth, 2)),
         x = "Longitude", y = "Latitude") +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), 
          axis.title = element_text(size = 14),
          panel.spacing = unit(20,"mm"),
          axis.text = element_text(size = 9.5), 
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  combined_plot = map_est + map_obs + plot_layout(ncol = 2, guides = "collect")
  
  return(combined_plot)
}

plot_4 = generate_density_map(shape_sf_obs_4, shape_sf_est_4, "April")
plot_4
plot_2 = generate_density_map(shape_sf_obs_2, shape_sf_est_2, "February")
plot_6 = generate_density_map(shape_sf_obs_6, shape_sf_est_6, "June")
plot_8 = generate_density_map(shape_sf_obs_8, shape_sf_est_8, "August")
plot_10 = generate_density_map(shape_sf_obs_10, shape_sf_est_10, "October")

plot_4 = plot_4 + theme(plot.margin = margin(0, 0, 0, 0))
plot_2 = plot_2 + theme(plot.margin = margin(0, 0, 0, 0))
plot_6 = plot_6 + theme(plot.margin = margin(0, 0, 0, 0))
plot_8 = plot_8 + theme(plot.margin = margin(0, 0, 0, 0))
plot_10 = plot_10 + theme(plot.margin = margin(0, 0, 0, 0))
plot_4

combined_all <- wrap_plots(
  plot_2, plot_4, 
  plot_6, plot_8, 
  plot_10, 
  ncol = 2, 
  nrow = 3, 
  widths = c(1.3, 1.3),  
  heights = c(1.1, 1.1, 1.1)
) & 
  theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))  


ggsave("combined_density_plots.png", 
       combined_all, 
       width = 25,  
       height = 16) 
getwd()


?grid.arrange

grid.arrange(plot_2, plot_4, plot_6, plot_8, plot_10, layout_matrix = rbind(c(1,2), c(3, 4), 5))
?grid.arrange
dev.off()
rbind(c(1,2, 3, 4), 5)
summary(kde_df$density)
summary(kde_df_obs$density)
rbind(c(1, 2), 3)
##### leaflet (BONUS) #####

my_crs


kde_raster[kde_raster < 0.001] = NA
crs(kde_raster) = sp::CRS(my_crs)
kde_raster_obs[kde_raster_obs < 0.001] = NA
crs(kde_raster_obs) = sp::CRS(my_crs)

combined_values = c(values(kde_raster), values(kde_raster_obs))
range_combined = range(combined_values, na.rm = TRUE)

pal  = colorNumeric(
  palette = c("lightblue", "#F4D166", "#B71D3E"), 
  domain = range_combined, 
  na.color = "transparent"
)

m1 = leaflet() %>% 
  addTiles() %>%
  addRasterImage(kde_raster, colors = pal, opacity = 1) %>%
  addLegend(pal = pal, values = combined_values,
            title = "Density Map Est")



m2 = leaflet() %>% 
  addTiles() %>%
  addRasterImage(kde_raster_obs, colors = pal, opacity = 1) %>%
  addLegend(pal = pal, values = combined_values,
            title = "Density Map Obs")

m1
m2



##### Confidence #####

shape_sf_est_4 = read_sf(dsn = paste(getwd(), sep = "", "/Dati/Confidence/13-04-22_Conf_0.47.shp"))
shape_sf_est_2 = read_sf(dsn = paste(getwd(), sep = "", "//Dati/Confidence/20-02-22_Conf_0.47.shp"))
shape_sf_est_6 = read_sf(dsn = paste(getwd(), sep = "", "/Dati/Confidence/20-06-22_Conf_0.47.shp"))
shape_sf_est_8 = read_sf(dsn = paste(getwd(), sep = "", "/Dati/Confidence/24-08-22_Conf_0.47.shp"))
shape_sf_est_10 = read_sf(dsn = paste(getwd(), sep = "", "/Dati/Confidence/20-10-22_Conf_0.47.shp"))
griglia =  read_sf(dsn = paste(getwd(), sep = "", "/Dati/Griglia/oloturie_grid_1_x_1_m.shp"))
griglia_2 = read_sf(dsn = paste(getwd(), sep = "", "/Boundary area/boundary.shp"))
griglia_3 = st_union(griglia, griglia_2)

shape_sf = shape_sf_est_2
coord_est
st_centroid(shape_utm_est)
?st_buffer
conf = function(shape_sf, griglia, month){
  shape_utm_est = st_transform(shape_sf, crs = 4326)
  shape_utm_est = st_buffer(shape_utm_est, dist = 0.25)
  griglia_utm = st_transform(griglia, crs = 4326)
  griglia_utm = griglia

  shape_utm_est$conf = cut(shape_utm_est$confidence, 
                           breaks = c(0.45, 0.65, 0.8, 1), 
                           include.lowest = TRUE)
  shape_utm_est$conf = factor(shape_utm_est$conf, 
                              levels = c("[0.45,0.65]", "(0.65,0.8]", "(0.8,1]"))
  
  bbox_griglia = st_bbox(griglia_utm)

  ggplot() +
    geom_sf(data = griglia_utm, fill = NA, color = "gray70", lwd = 0.5) +  
    geom_sf(data = shape_utm_est, aes(fill = conf), color = NA, alpha = 0.7) + 
    scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb")) +  
    theme_minimal() +
    labs(title = paste("Confidence Class Distribution", sep = " ", month),
         x = "Longitude", y = "Latitude", fill = "Confidence Class") +
    theme(legend.position = "bottom") +
    coord_sf(xlim = c(bbox_griglia["xmin"], bbox_griglia["xmax"]),
             ylim = c(bbox_griglia["ymin"], bbox_griglia["ymax"]))
}
conf(shape_sf_est_6, griglia, "February")
