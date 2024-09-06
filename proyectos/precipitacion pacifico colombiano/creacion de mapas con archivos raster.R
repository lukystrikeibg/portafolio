# creacion de mapas con archivos raster
#load libraries
require(pacman)
pacman::p_load(raster,foreign,rgdal,osmdata,RColorBrewer, ggspatial,rgeos,stringr,sf,tidyverse,gtools)
g<-gc(reset=TRUE)

rm(list=ls())

clmt <- list.files(path='../sig en  R con udemy/sig/WORLDCLIM_COLOMBIA/WORLDCLIM_COLOMBIA',full.names = TRUE, pattern ='.tif$') 
clmt <- stack(clmt)
clmt


# calcular la precipitacion acumulada
names(clmt)
post <- grep('prec', names(clmt), value=F)
prec<-clmt[[post]]
prec<-sum(prec)
prec_tble<-rasterToPoints(prec, spatial = F) %>% as.tibble()

coll <-raster::getData(name = 'GADM', country='COL', level=1)
coll <- st_as_sf(coll)

same <- rnaturalearth::ne_countries(continent= 'south america', returnclass='sf')
name <- rnaturalearth::ne_countries(continent= 'north america', returnclass='sf')
amer<- rbind(same,name)
View(amer)

cntr <-st_read('../sig en  R con udemy/sig/all_countries/all_countries.shp')
cntr <- cntr %>% filter(CONTINENT %in% c('South America','North America'))

gmap <- ggplot()+
  geom_sf(data=cntr, fill='grey')+
  geom_sf_text(data=cntr %>% filter(!NAME =='Colombia'), aes(label=NAME), size=1.4) +
  geom_tile(data=prec_tble, aes(x=x,y=y,fill=layer)) +
  scale_fill_gradientn(colours=brewer.pal(name='GnBu',n=9),
                      labels=scales::comma,
                      breaks= seq(2000,8000,2000))+
  geom_sf(data=coll, fill = NA) + 
  geom_sf_text(data=coll,aes(label=NAME_1),size=2)+
  coord_sf(xlim = extent(coll)[1:2], ylim = c(-4,13))+
  ggtitle(label= 'precipitacion acumulada en colombia(1970-2000)')+
  theme_bw()+
  theme(legend.position = 'bottom', legend.text=element_text(size = 12,face='bold'),
        plot.title=element_text(size=14,face='bold', hjust=0.5),
        legend.key.width = unit(3,'line'),
        panel.grid.major = element_line(color=gray(.5),linetype = 'dashed',size = 0,5),
        panel.background = element_rect(fill='#63799B'))+
  labs(x='Lon', y='Lat',
       caption = ' Fuente: Worldclim', fill = 'Prec(mm)')+
  annotation_scale(location= 'bl', width_hint = 0.2)+
  annotation_north_arrow(location="tr", which_north = "true",
                         height = unit(1.2, 'cm'), width = unit(0.9,'cm'),
                         style = north_arrow_fancy_orienteering)
  

gmap

ggsave(plot=gmap, filename = '../sig en  R con udemy/outputs/precipitacion.png',
       units= 'in', width=8,height=10, dpi=300)
  
  
  
  