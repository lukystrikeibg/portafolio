#creacion de mapa  comparativo de precipitacón

#leer librerias

require(pacman)
pacman::p_load(raster,foreign,rgdal,osmdata,RColorBrewer, ggspatial,rgeos,stringr,sf,tidyverse,gtools)

g<-gc(reset=TRUE)

rm(list=ls())

# load data
fles<- list.files('../sig en  R con udemy/sig/WORLDCLIM_COLOMBIA/WORLDCLIM_COLOMBIA/', full.names=TRUE, pattern ='.tif$' )
fles <- grep('prec', fles, value=TRUE)
dpto <- shapefile('../sig en  R con udemy/sig/MGN2018_DPTO_POLITICO/MGN_DPTO_POLITICO.shp')
prec <- stack(fles)
plot(prec[[1]])

unique(dpto$DPTO_CNMBR)

ptm<-dpto[dpto@data$DPTO_CNMBR== 'PUTUMAYO',]
gjr<-dpto[dpto@data$DPTO_CNMBR== 'LA GUAJIRA',]

plot(ptm)
plot(gjr)

plot(ptm, add=TRUE, col='red')
plot(gjr, add=TRUE, col='blue')

#extrac by mask
prec <- sum(prec)
prec_ptm<- raster::crop(prec, ptm)
prec_ptm<- raster::mask(prec_ptm, ptm)

prec_gjr<- raster::crop(prec, gjr)
prec_gjr<- raster::mask(prec_gjr, gjr)

prec_ptm
prec_gjr

# raster to table

prec_ptm <- rasterToPoints(prec_ptm, spatial = FALSE)
prec_ptm <- as_tibble(prec_ptm)
prec_ptm<- mutate(prec_ptm, dpto= 'PUTUMAYO')

prec_gjr <- rasterToPoints(prec_gjr, spatial = FALSE)
prec_gjr <- as_tibble(prec_gjr)
prec_gjr<- mutate(prec_gjr, dpto= 'La Guajira')

prec_tble<- rbind(prec_gjr,prec_ptm)
prec_tble

summary(prec_tble$layer)

# to make the map ---------------------------------
g_ptm <-ggplot(prec_tble %>% filter (dpto =='PUTUMAYO'))+
  geom_tile(aes(x=x, y=y, fill=layer))+
  scale_fill_gradientn(colours=RColorBrewer::brewer.pal(n=8, name = 'GnBu'),
                      na.value='white', limits=c(0,6000),breaks=seq(0,6000,2000))+
  geom_sf(data=st_as_sf(dpto),fill=NA) +
  ggtitle('Precipitación acumulada - Putumayo') +
  theme_bw() +
  coord_sf(xlim = extent(ptm)[1:2], ylim = extent(ptm)[3:4]) +
  labs(x = 'Longitud', y = 'Latitud', fill = 'Prec (mm)') +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "white"),
        legend.background = element_blank(),
        legend.position = c(.005, .005),
        legend.direction = 'horizontal',
        legend.key = element_blank(), 
        legend.title=element_text(size = 9, face = "bold"), 
        legend.text =element_text(size = 8), 
        legend.justification = c("left", "bottom"),
        legend.box.just = "left",
        legend.key.width = unit(1.5, 'line'),
        plot.title=element_text(color = "#666666", size = 14, vjust = 1.25,  family = "Raleway",  hjust = 0.5))
  

g_ptm

g_gjr <- ggplot(prec_tble %>% filter(dpto == 'La Guajira'))  +
  geom_tile(aes(x = x, y =  y, fill = layer)) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 8, name = "GnBu"), 
                       na.value = 'white', limits = c(0, 6000), breaks = seq(0, 6000, 2000)) +
  geom_sf(data = st_as_sf(dpto), fill = NA) +
  ggtitle('Precipitación acumulada - La Guajira') +
  theme_bw() +
  coord_sf(xlim = extent(gjr)[1:2], ylim = extent(gjr)[3:4]) +
  labs(x = 'Longitud', y = 'Latitud', fill = 'Prec (mm)') +
  theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "white"),
        plot.title=element_text(color = "#666666", size = 14, vjust = 1.25,  family = "Raleway",  hjust = 0.5))


g_gjr

library(ggpubr)
g_all <- ggarrange(g_ptm,g_gjr, common.legend = TRUE, legend = 'bottom')

g_all

ggsave(plot = g_all, filename = '../sig en  R con udemy/outputs/comparativa_putumayo_guajira.png', units = 'in', width = 12, height = 7, dpi = 300)


