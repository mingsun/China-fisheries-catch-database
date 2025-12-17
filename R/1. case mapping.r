library(geosphere)

case <- read.csv("data/csv/02. spatio-temporal info.csv")[,c(1:3, 5:9)]
case$x.mid <- (case$x1 + case$x2)/2
case$y.mid <- (case$y1 + case$y2)/2
case$size <- ((case$x2 - case$x1) * (case$y2 - case$y1))^2 *3

Calc.Size <- function(x1, x2, y1, y2) {
  p <- rbind(c(x1, y1), c(x1, y2), c(x2, y2), c(x2, y1))
  return(areaPolygon(p))
}

case$abs.size <- mapply(Calc.Size, case$x1, case$x2, case$y1, case$y2)/10^6

### ----------------------------------------------------------- ###
### ------------------------ mapping -------------------------- ###
### ----------------------------------------------------------- ###

# load the plot packages
library(ggplot2)
theme_set(theme_bw())
library(sf)

# load the country map packages
library(rnaturalearth)
library(rnaturalearthdata)

# extract the regional data
world <- ne_countries(scale = "medium", returnclass = "sf")
world_points<- st_centroid(world)

# plot
sea.name.size <- 4

case.map <- ggplot(data = world) +
  geom_sf(fill= "cornsilk") +
  # geom_sf(fill= "grey99") +
  # geom_sf(data = CHN, fill= "antiquewhite") +
  # annotate(geom = "text", x = 121, y = 38.5, label = "Bohai Sea", fontface = "italic", color = "grey22", size = sea.name.size) +
  # annotate(geom = "text", x = 122.5, y = 35, label = "Yellow Sea", fontface = "italic", color = "grey22", size = sea.name.size) +
  # annotate(geom = "text", x = 123.5, y = 27.5, label = "East China Sea", fontface = "italic", color = "grey22", size = sea.name.size) +
  # annotate(geom = "text", x = 114, y = 17, label = "South China Sea", fontface = "italic", color = "grey22", size = sea.name.size) +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(106, 128), ylim = c(10, 41)) +
  geom_point(data = case, aes(x = x.mid, y = y.mid, size = abs.size), color = "darkblue", alpha = 0.3) +
  # geom_point(data = case, aes(x = x.mid, y = y.mid), color = "red", alpha = 0.4, size = 2) +
  theme(panel.grid.major = element_line(color = gray(.85), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"), legend.position = "none")
  # geom_rect(data = case, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "red", alpha = 0.2)

png("plot/Fig. 1/case.map.png",  width = 6, height = 7.5, units = 'in', res = 800)
print(case.map)
dev.off()


### ----------------------------------------------------------- ###
### ------------------ coastline mapping  --------------------- ###
### ----------------------------------------------------------- ###

# # CHN <- ne_countries(scale = "medium", returnclass = "sf", country = c("china","taiwan"))
# CHN <- ne_countries(scale = "medium", returnclass = "sf", country = c("china"))
# CHN_points<- st_centroid(CHN)

coast.map <- ggplot(data = world) +
  geom_sf(fill= "antiquewhite") +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(108, 123), ylim = c(10, 40)) +
  theme(panel.grid.minor.x = element_blank(),
        panel.border = element_blank())

png("plot/coast.map.partial.no.province.png",  width = 3.5, height = 7.5, units = 'in', res = 800)
print(coast.map)
dev.off()


# ---- province specific
china <- read_sf("data/provincial map sf/bou2_4p.shp", as_tibble = FALSE)

# 将 NAME 列转码
china$NAME <- iconv(china$NAME, from = "CP936", to = "UTF-8")

# 设置坐标参考系
st_crs(china) <- 4326

# 绘图
coast.map <- ggplot() +
  geom_sf(data = world, fill= "antiquewhite") +
  geom_sf(data = china, fill= "antiquewhite") +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(108, 123), ylim = c(19, 40)) 

png("plot/coast.map.partial.png",  width = 3.5, height = 7.5, units = 'in', res = 800)
print(coast.map)
dev.off()





