library(plyr)
library(ggplot2)

# load the species composition file
comp.df <- read.csv(file = "Data/csv/05. species composition.csv")[,c(1,5,6,7,9,10)] 

# load the spatio-temporal case info
spa.tem.df <- read.csv(file = "Data/csv/02. spatio-temporal info.csv")[,c(1,3,9)]

# load the gear info
gear.df <- read.csv(file = "Data/csv/03. gears.csv")[,c(1,8,13)]

# merge the two df
comp.df <- merge(merge(comp.df, spa.tem.df), gear.df)
remove(spa.tem.df, gear.df)

### ---------------------------------------------------- ### 
# ----------- observed gear pattern by latitude ------------
### ---------------------------------------------------- ###

# merge with the coordinates
co.df <-  read.csv("data/csv/02. spatio-temporal info.csv")[,-c(2,3,4,9,10,11)]
gear.pattern.df <- merge(comp.df, co.df); remove(co.df)
gear.pattern.df$y.mid <- round(0.5 * (gear.pattern.df$y1 + gear.pattern.df$y2), 2)

test.gear <- aov(y.mid ~ FAO.class, data = gear.pattern.df)
summary(test.gear) # p = 0

# combine Bohai and Yellow Sea
gear.pattern.df$sea.basin[gear.pattern.df$sea.basin %in% c("Bohai Sea", "Yellow Sea")] <- "Bohai and Yellow Sea"


library(ggridges)

gear.map <- ggplot(gear.pattern.df, aes(x = y.mid, fill = sea.basin)) + 
  geom_histogram(bins = 30, alpha=0.6, position = 'identity', color = "black") +
  facet_wrap(.~FAO.class, nrow = 1) +
  geom_hline(yintercept = 0, color = "black") +
  # geom_density_ridges(stat="binline", bins = 30, alpha = 0.6) +
  scale_x_continuous(breaks = seq(10, 40, by = 5)) +
  scale_y_continuous(breaks = seq(0, 60, by = 30)) +
  labs(y = "", x = "") +
  theme_bw() +
  theme(axis.line.x = element_line(colour = "black"),
        # panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank(), 
        panel.border = element_blank(),
        legend.position = "none") + 
  coord_flip()

png("plot/Fig. 2/gear.geo.pattern.png",  width = 11*0.7, height = 6*0.7, units = 'in', res = 800)
print(gear.map)
dev.off()
  
# gear.map <- ggplot(gear.pattern.df, aes(x = y.mid, y = FAO.class, fill = sea.basin)) + 
#   geom_density_ridges(stat="binline", bins = 30, alpha = 0.6) +
#   # geom_jitter(aes(x = y.mid, y = FAO.class, color = sea.basin)) +
#   scale_x_continuous(breaks = seq(10, 40, by = 5)) +
#   labs(y = "", x = "") +
#   theme_bw() +
#   theme(axis.line.x = element_line(colour = "black"),
#         panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         axis.text.y = element_blank(), axis.ticks.y = element_blank(),
#         panel.border = element_blank(),
#         legend.position = "none") + 
#   coord_flip()


### ---------------------------------------------------- ### 
# ----------- yearbook gear pattern by latitude ------------
### ---------------------------------------------------- ###

gear.yb <- read.csv("data/csv/03.1 yearbook vessel by province.csv")[c(-12),]
province.list <- c(arrange(gear.yb, -ymid)$province)

# wide to long
library(reshape2)
gear.yb <- melt(gear.yb, id.vars = c("province", "ymid"))

library(scales)
show_col(hue_pal()(3))

gear.map <- ggplot(gear.yb) +
  geom_blank(data = gear.yb, aes(x = province, y = value)) +
  annotate("rect", xmin = 6.5, xmax = 11.7, ymin = -Inf, ymax = Inf, alpha = 0.6, fill = "#F8766D", alpha = 0.6) +
  annotate("rect", xmin = 3.5, xmax = 6.5, ymin = -Inf, ymax = Inf, alpha = 0.6, fill = "#00BA38", alpha = 0.6) + 
  annotate("rect", xmin = 0.3, xmax = 3.5, ymin = -Inf, ymax = Inf, alpha = 0.6, fill = "#619CFF", alpha = 0.6)  +
  geom_bar(aes(x = province, y = value), stat = "identity", fill = "black") +
  scale_x_discrete(limits = rev(province.list)) +
  facet_wrap(~variable, nrow = 1) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  coord_flip() 

png("plot/Fig. x/gear.yearbook.png",  width = 8, height = 6, units = 'in', res = 800)
print(gear.map)
dev.off()




