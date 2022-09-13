
# Installing and loading packages

pacotes <- c("rgdal","plotly","tidyverse","knitr","kableExtra","gridExtra",
             "png","grid","magick","rgl","devtools","GISTools","rayshader",
             "tmap","broom")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

library(dplyr) 

# Loading the shapefile -------------------------------------------------
shp_mundo <- readOGR(dsn = "~/Desktop/dsa/Analise Espacial/Espacial 2/shapefile_mundo", layer = "mundo",
                     encoding = "ISO-8859-1")

shp_mundo@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)


# Loading the dataset about potential and generation of electricity with solar power


setwd("/Users/robertorodrigues/Desktop/dsa/Solar_panel/")
dados_mundo <- read.csv2("solar_dados_mapas.csv", sep = ";", dec = ".", header = T)

dados_mundo %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Preprocessing


dados_mundo <- dados_mundo %>% dplyr::select(Country.or.region, Avg.Theoretical.potential..kWh.m2.day., Generation)
dados_mundo$Generation=as.numeric(dados_mundo$Generation)

# insert rows 

z <- data.frame(Country.or.region = "Greenland", Avg.Theoretical.potential..kWh.m2.day. = 2, Generation = 0)
dados_mundo <- rbind(dados_mundo, z)
z <- data.frame(name = "Antarctica", Avg.Theoretical.potential..kWh.m2.day. = 2, Generation = 0)
dados_mundo <- rbind(dados_mundo, z)
z <- data.frame(name = "Iceland", Avg.Theoretical.potential..kWh.m2.day. = 2, Generation = 0)
dados_mundo <- rbind(dados_mundo, z)
z <- data.frame(name = "Syria", Avg.Theoretical.potential..kWh.m2.day. = 5.4, Generation = 0)
dados_mundo <- rbind(dados_mundo, z)
z <- data.frame(name = "Cote d'Ivoire", Avg.Theoretical.potential..kWh.m2.day. = 5.1, Generation = 0)
dados_mundo <- rbind(dados_mundo, z)
z <- data.frame(name = "Somaliland", Avg.Theoretical.potential..kWh.m2.day. = 5.8, Generation = 0)
dados_mundo <- rbind(dados_mundo, z)
z <- data.frame(name = "Swaziland", Avg.Theoretical.potential..kWh.m2.day. = 5.6, Generation = 0)
dados_mundo <- rbind(dados_mundo, z)
z <- data.frame(name = "Korea", Avg.Theoretical.potential..kWh.m2.day. = 4.5, Generation = 0)
dados_mundo <- rbind(dados_mundo, z)
z <- data.frame(name = "Solomon Is.", Avg.Theoretical.potential..kWh.m2.day. = 4.5, Generation = 0)
dados_mundo <- rbind(dados_mundo, z)
z <- data.frame(name = "Falkland Is.", Avg.Theoretical.potential..kWh.m2.day. = 5.1, Generation = 0)
dados_mundo <- rbind(dados_mundo, z)
z <- data.frame(name = "Fr. S. Antarctic Lands", Avg.Theoretical.potential..kWh.m2.day. = 5.1, Generation = 0)
dados_mundo <- rbind(dados_mundo, z)
z <- data.frame(name = "Macedonia", Avg.Theoretical.potential..kWh.m2.day. = 4.4, Generation = 0)
dados_mundo <- rbind(dados_mundo, z)
z <- data.frame(name = "W. Sahara", Avg.Theoretical.potential..kWh.m2.day. = 6, Generation = 0)
dados_mundo <- rbind(dados_mundo, z)
z <- data.frame(name = "Taiwan", Avg.Theoretical.potential..kWh.m2.day. = 4.1 , Generation = 0)
dados_mundo <- rbind(dados_mundo, z)
z <- data.frame(name = "N. Cyprus", Avg.Theoretical.potential..kWh.m2.day. = 5.2, Generation = 0)
dados_mundo <- rbind(dados_mundo, z)
z <- data.frame(name = "Brunei", Avg.Theoretical.potential..kWh.m2.day. = 4.7, Generation = 0)
dados_mundo <- rbind(dados_mundo, z)
z <- data.frame(name = "Palestine", Avg.Theoretical.potential..kWh.m2.day. = 5.7 , Generation = 0)
dados_mundo <- rbind(dados_mundo, z)
z <- data.frame(name = "Puerto Rico", Avg.Theoretical.potential..kWh.m2.day. = 5.4, Generation = 0)
dados_mundo <- rbind(dados_mundo, z)
z <- data.frame(name = "Bahamas", Avg.Theoretical.potential..kWh.m2.day. = 5.4, Generation = 0)
dados_mundo <- rbind(dados_mundo, z)
z <- data.frame(name = "Gambia", Avg.Theoretical.potential..kWh.m2.day. = 6, Generation = 0)
dados_mundo <- rbind(dados_mundo, z)




# NA equal to 0

dados_mundo[dados_mundo=="#N/D"] <- 0
dados_mundo["Generation"] <- dados_mundo["Generation"] %>% 
  mutate_all(replace_na, 0)


dados_mundo[dados_mundo=="United States of America"] <- "United States"
dados_mundo[dados_mundo=="Central African Republic"] <- "Central African Rep."
dados_mundo[dados_mundo=="Russian Federation"] <- "Russia"
dados_mundo[dados_mundo=="South Sudan"] <- "S. Sudan"
dados_mundo[dados_mundo=="Syrian"] <- "Syria"
dados_mundo[dados_mundo=="Democratic Republic of Congo"] <- "Dem. Rep. Congo"
dados_mundo[dados_mundo=="C<f4>te d'Ivoire"] <- "Cote d'Ivoire"
dados_mundo[dados_mundo=="Bosnia and Herzegovina"] <- "Bosnia and Herz."
dados_mundo[dados_mundo=="Czech Republic"] <- "Czech Rep."
dados_mundo[dados_mundo=="North Korea"] <- "Dem. Rep. Korea"
dados_mundo[dados_mundo=="Dominican Republic"] <- "Dominican Rep."
dados_mundo[dados_mundo=="Equatorial Guinea"] <- "Eq. Guinea"
dados_mundo[dados_mundo=="Kyrgyz Republic"] <- "Kyrgyzstan"
dados_mundo[dados_mundo=="Korea"] <- "South Korea"
dados_mundo[dados_mundo=="Lao People's Democratic Republic"] <- "Lao PDR"
dados_mundo[dados_mundo=="New Caledonia (Fr.)"] <- "New Caledonia"
dados_mundo[dados_mundo=="Republic of Yemen"] <- "Yemen"
dados_mundo[dados_mundo=="Republic of Yemen"] <- "Yemen"


library(dplyr)
colnames(dados_mundo)[1] <- "name"
colnames(dados_mundo)
summary(dados_mundo)

# Merging dataset and shapefile

shp_dados_mundo <- merge(x = shp_mundo,
                         y = dados_mundo,
                         by.x = "name",
                         by.y = "name")



shp_dados_mundo@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)



shp_mundo_df  %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)


#  VISUALIZING


# Convert the shapefile into a dataframe and import data to the new dataframe

shp_mundo_df <- tidy(shp_dados_mundo, region = "name") %>% 
  dplyr::rename(name = id) %>% 
  left_join(shp_dados_mundo@data,
            by = "name")

# The plot.

plotly::ggplotly(
  
  shp_mundo_df %>% 
    ggplot() +
    geom_polygon(aes(x = long, y = lat, group = group, fill = Avg.Theoretical.potential..kWh.m2.day., label = name),
                 color = "black") +
    labs(x = "Longitude",
         y = "Latitude",
         fill = "theoretical.potential") +
    scale_fill_viridis_c() +
    theme_bw()
)


# Generating a map in ggplot2
shp_mundo_df %>%
  ggplot(aes(x = long,
             y = lat, 
             group = group, 
             fill = Avg.Theoretical.potential..kWh.m2.day.)) +
  geom_polygon() +
  ggplot2::scale_fill_gradient(limits = range(2:7),
                               low = "#E09F3E", 
                               high="#FFFFE0") +
  layer(geom = "path", 
        stat = "identity", 
        position = "identity",
        mapping = aes(x = long, 
                      y = lat, 
                      group = group, 
                      color = I('#FFFFFF'))) +
  theme(legend.position = "none", 
        axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank())



mapa_mundo <- shp_mundo_df %>%
  ggplot(aes(x = long,
             y = lat, 
             group = group, 
             fill = Avg.Theoretical.potential..kWh.m2.day.)) +
  geom_polygon() +
  ggplot2::scale_fill_gradient(limits = range(2:7),
                               low = "#E09F3E", 
                               high="#FFFFE0")+
  layer(geom = "path", 
        stat = "identity", 
        position = "identity",
        mapping = aes(x = long, 
                      y = lat, 
                      group = group, 
                      color = I('#FFFFFF'))) +
  theme(legend.position = "none", 
        axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank())


# Saving the object mapa_mundo as a *.png file

:
xlim <- ggplot_build(mapa_mundo)$layout$panel_scales_x[[1]]$range$range
ylim <- ggplot_build(mapa_mundo)$layout$panel_scales_y[[1]]$range$range

ggsave(filename = "mapa_mundo.png",
       width = diff(xlim) * 0.1, 
       height = diff(ylim) * 0.1, 
       units = "cm")

# Loading the file
background_mapa <- readPNG("mapa_mundo.png")


# Capturing geographic coordanates 

coordinates(shp_mundo) %>% 
  data.frame() %>% 
  rename(longitude = 1,
         latitude = 2) %>% 
  mutate(name = shp_mundo@data$name) %>% 
  dplyr::select(latitude, everything()) -> coords_mundo

# Adding geographic coordanates into the object map

shp_mundo_df <- shp_mundo_df %>% 
  left_join(coords_mundo, by = "name")


# Georrefering the PNG image PNG and ploting the poligon

shp_mundo_df %>%
  ggplot() + 
  annotation_custom(
    rasterGrob(background_mapa, 
               width=unit(1,"npc"),
               height=unit(1,"npc")),-Inf, Inf, -Inf, Inf) + 
  xlim(xlim[1],xlim[2]) + # x-axis Mapping
  ylim(ylim[1],ylim[2]) + # y-axis Mapping
  geom_point(aes(x = longitude, y = latitude, color = Generation), size = 0.25) + 
  scale_colour_gradient(name = "Generation..Terawatts.hour.", 
                        limits = range(0:328), 
                        low = "#B23A48", 
                        high = "#008D8C") + 
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank())

# Saving the result
mapa_solar <- shp_mundo_df %>%
  ggplot() + 
  annotation_custom(
    rasterGrob(background_mapa, 
               width=unit(1,"npc"),
               height=unit(1,"npc")),-Inf, Inf, -Inf, Inf) + 
  xlim(xlim[1],xlim[2]) + # x-axis Mapping
  ylim(ylim[1],ylim[2]) + # y-axis Mapping
  geom_point(aes(x = longitude, y = latitude, color = Generation), size = 0.25) + 
  scale_colour_gradient(name = "Generation..Terawatts.hour.", 
                        limits = range(0:328), 
                        low = "#B23A48", 
                        high = "#008D8C") + 
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank())


#  Generating a 3D map of solar energy by country (don't close pop-up window until the script is completed) 
plot_gg(ggobj = mapa_solar, 
        width = 11, 
        height = 6, 
        scale = 400, 
        multicore = TRUE, 
        windowsize = c(1000, 800))

# Getting a better visual from the result 

render_camera(fov = 70, 
              zoom = 0.4, 
              theta = 130, 
              phi = 35)

# Video generation 

azimute_metade <- 30 + 60 * 1/(1 + exp(seq(-7, 20, length.out = 180)/2))
azimute_completo <- c(azimute_metade, rev(azimute_metade))

rotacao <- 0 + 45 * sin(seq(0, 359, length.out = 360) * pi/180)

zoom_metade <- 0.4 + 0.2 * 1/(1 + exp(seq(-5, 20, length.out = 180)))
zoom_completo <- c(zoom_metade, rev(zoom_metade))

render_movie(filename = "resutado1_mundo", 
             type = "custom", 
             frames = 360, 
             phi = azimute_completo, 
             zoom = zoom_completo, 
             theta = rotacao)

