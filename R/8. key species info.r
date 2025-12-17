library(ggplot2)
library(RColorBrewer)


# load the key species  file
key.species.df <- read.csv("data/csv/06.1 key species detail.csv")[,c(1,4,7,8)]

# load the spatio-temporal case info
spa.tem.df <- read.csv(file = "Data/csv/02. spatio-temporal info.csv")[,c(1,3,5:9)]
spa.tem.df$x.mid <- (spa.tem.df$x1 + spa.tem.df$x2)/2
spa.tem.df$y.mid <- (spa.tem.df$y1 + spa.tem.df$y2)/2
spa.tem.df$size <- (spa.tem.df$x2 - spa.tem.df$x1) * (spa.tem.df$y2 - spa.tem.df$y1)

# load the gear info
gear.df <- read.csv(file = "Data/csv/03. gears.csv")[,c(1,8,13)]

# merge the two df
key.species.df <- merge(merge(key.species.df, spa.tem.df[,c(1,2,7:10)]), gear.df)
remove(spa.tem.df, gear.df)

### ---------------------------------------------------- ### 
### --------------- mapping distribution --------------- ###
### ---------------------------------------------------- ###

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# extract the regional data
world <- ne_countries(scale = "medium", returnclass = "sf")
world_points<- st_centroid(world)

# plot
sea.name.size <- 4
unique(key.species.df$Species.adjusted)
key.species.df$Species.adjusted <- factor(key.species.df$Species.adjusted, 
                                          levels = c("chub.mackerel", "Japanese.anchovy", "Japanese.scad", "Japanese.Spanish.mackerel",
                                                     "largehead.hairtail", "silver.pomfret", "small.yellow.croaker",
                                                     "mantis.shrimp", "planktonic.shrimp", "swimming.crab", "white-hair.rough.shrimp",
                                                     "octopus", "squid", "shellfish", "jellyfish"))

ks.map <- ggplot(data = world) +
  geom_sf(fill= "cornsilk") +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(106, 128), ylim = c(10, 41)) +
  # geom_point(data = key.species.df, aes(x = x.mid, y = y.mid, size = size^2 * 3, color = Group), alpha = 0.8) +
  geom_point(data = key.species.df, aes(x = x.mid, y = y.mid, size = 0.1, color = Group), alpha = 0.8) +
  # scale_fill_manual(values = c("#C77CFF", "#F8766D", "#7CAE00", "#00BFC4")) +
  scale_color_brewer(palette = "Set2") +
  facet_wrap(.~Species.adjusted, ncol = 5) +  
  theme(panel.grid.major = element_line(color = gray(.85), linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "aliceblue"), legend.position = "none")

png("plot/Fig. 7/key.species.map.png",  width = 9, height = 13.5, units = 'in', res = 800)
print(ks.map)
dev.off()


### ---------------------------------------------------- ### 
### ---------- key species weight ratio ------------- ###
### ---------------------------------------------------- ###

key.species.df$Species.adjusted <- factor(key.species.df$Species.adjusted, 
                                          levels = rev(c("chub.mackerel", "Japanese.anchovy", "Japanese.scad", "Japanese.Spanish.mackerel",
                                                     "largehead.hairtail", "silver.pomfret", "small.yellow.croaker",
                                                     "mantis.shrimp", "planktonic.shrimp", "swimming.crab", "white-hair.rough.shrimp",
                                                     "octopus", "squid", "shellfish", "jellyfish")))

library(plyr)
ddply(key.species.df, .(Species.adjusted), summarize, median = median(weight.ratio))


ks.ratio.p <- ggplot(key.species.df) +
  geom_boxplot(aes(x = Species.adjusted, y = weight.ratio, fill = Group), width = 0.6) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "", y = "") +
  theme_classic() +
  theme(legend.position = "none", panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = gray(.85), linetype = "dashed", size = 0.5)) +
  coord_flip()

png("plot/Fig. 7/key.species.ratio.png",  width = 6, height = 7, units = 'in', res = 800)
print(ks.ratio.p)
dev.off()

# overall violin plot
library(plyr)
mv <- ddply(key.species.df, .(Group), summarize, median = median(weight.ratio))


key.species.df$v <- mv$v <- "all"
median(key.species.df$weight.ratio)

overall.p <- ggplot(key.species.df) + 
  # geom_vline(xintercept = 0.08, linetype = 2) +
  geom_violin(aes(y = v, x = weight.ratio), fill = "skyblue2", color = "skyblue2", alpha = 0.8) +
  # geom_jitter(data = mv, aes(x = median, y = v, color = Group), height = 0.54) +
  # scale_color_brewer(palette = "Set2") +
  labs(y = "", x = "") +
  theme_classic() +
  theme(axis.text.y = element_blank())

png("plot/Fig. 7/ts.ratio.overall.p.png",  width = 6, height = 1, units = 'in', res = 800)
print(overall.p)
dev.off()

zoom.in.p <- ggplot(key.species.df) + 
  geom_violin(aes(y = v, x = weight.ratio), fill = "skyblue2", color = "skyblue2", alpha = 0.8) +
  geom_vline(xintercept = 0.08, linetype = 2) +
  # geom_jitter(data = mv, aes(x = median, y = v, color = Group), height = 0.54) +
  # scale_color_brewer(palette = "Set2") +
  labs(y = "", x = "") +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.line.y = element_blank(), axis.ticks.y =element_blank()) +
  coord_cartesian(c(0,0.2))

png("plot/Fig. 7/overall.zoomin.png",  width = 3, height = 1, units = 'in', res = 800)
print(zoom.in.p)
dev.off()


### ---------------------------------------------------- ### 
### ---------- key species by gear type ------------- ###
### ---------------------------------------------------- ###
library(plyr)

key.species.rec.df <- ddply(key.species.df, .(Species.adjusted, Group, FAO.class, gear.use), summarize,
                            records = length(case))

key.species.rec.df$Species.adjusted <- factor(key.species.rec.df$Species.adjusted, 
                                          levels = c("chub.mackerel", "Japanese.anchovy", "Japanese.scad", "Japanese.Spanish.mackerel",
                                                         "largehead.hairtail", "silver.pomfret", "small.yellow.croaker",
                                                         "mantis.shrimp", "planktonic.shrimp", "swimming.crab", "white-hair.rough.shrimp",
                                                         "octopus", "squid", "shellfish", "jellyfish"))

ks.gear.p <- ggplot(key.species.rec.df) +
  geom_bar(aes(x = Species.adjusted, y = records, fill = FAO.class), position = "fill", stat = "identity") +
  labs(x = "", y = "") +
  scale_fill_brewer(palette = "Set3")  +
  theme_classic() +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0.04,0.96))


# ks.gear.p <- ggplot(key.species.df) + 
#   geom_bar(aes(x = Species.adjusted, y = weight.ratio, fill = FAO.class), position = "fill", stat = "identity") +
#   labs(x = "", y = "") +
#   scale_fill_brewer(palette = "Set3")  +
#   theme_classic() +
#   theme(legend.position = "none") +
#   coord_cartesian(ylim = c(0.04,0.96))

png("plot/key.species.gear.png",  width = 9, height = 3, units = 'in', res = 800)
print(ks.gear.p)
dev.off()

a <- ddply(key.species.rec.df, .(Group, Species.adjusted), summarize, n.gear = length(unique(FAO.class)))
b <- ddply(a, .(Group), summarize, median.n.gear = mean(n.gear))

### ---------------------------------------------------- ### 
### ------- key species by gear regulation ---------- ###
### ---------------------------------------------------- ###

key.species.rec.df$gear.use <- factor(key.species.rec.df$gear.use, levels = c("permissible", "transitional", "prohibited"))

ks.regulate.p <- ggplot(key.species.rec.df) + 
  geom_bar(aes(x = Species.adjusted, y = records, fill = gear.use), position = "fill", stat = "identity", alpha = 0.75) +
  labs(x = "", y = "") +
  scale_fill_manual(values = c("springgreen4", "darkgoldenrod1", "firebrick3")) +
  theme_classic() +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0.04,0.96))


# ks.regulate.p <- ggplot(key.species.df) + 
#   geom_bar(aes(x = Species.adjusted, y = weight.ratio, fill = gear.use), position = "fill", stat = "identity", alpha = 0.75) +
#   labs(x = "", y = "") +
#   scale_fill_manual(values = c("springgreen4", "darkgoldenrod1", "firebrick3")) +
#   theme_classic() +
#   theme(legend.position = "none") +
#   coord_cartesian(ylim = c(0.04,0.96))

png("plot/key.species.regulate.png",  width = 9, height = 3, units = 'in', res = 800)
print(ks.regulate.p)
dev.off()

