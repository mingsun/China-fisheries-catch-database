library(plyr)
library(ggplot2)

div.df <- read.csv("results/diversity.index.editted.csv")[,-10]
trash.df <- read.csv("results/trash.fish.csv")

# merge with the coordinates
co.df <-  read.csv("data/csv/02. spatio-temporal info.csv")[,-c(2,3,4,9,10,11)]
div.df <- merge(div.df, co.df); remove(co.df)
div.df$y.mid <- round(0.5 * (div.df$y1 + div.df$y2), 2)
trash.df$y.mid <- round(0.5 * (trash.df$y1 + trash.df$y2), 2)

### ------------- ###
# calculate correlation ----------- 
### ------------- ###


# manually change the gear type

df <- subset(div.df, FAO.class == "g")
df.t <- subset(trash.df, FAO.class == "trawls")

shapiro.test(df$y.mid)
shapiro.test(df$No.species)

## No. species ----
lm <- lm(df$No.species ~ df$y.mid)
summary(lm) 

cor.test(df$y.mid, df$No.species, method = c("pearson"))


## H ----
lm <- lm(df$H ~ df$y.mid)
summary(lm) 

cor.test(df$y.mid, df$H, method = c("pearson"))

## J ----
df <- subset(df, J <= 1)

lm <- lm(df$J ~ df$y.mid)
summary(lm) 

cor.test(df$y.mid, df$J, method = c("pearson"))

## trash fish ----
lm <- lm(df.t$weight.ratio ~ df.t$y.mid)
summary(lm) 

cor.test(df.t$y.mid, df.t$weight.ratio, method = c("pearson"))


### ------------- ###
# plot ----------- 
### ------------- ###

div.gear.df <- subset(div.df, FAO.class %in% c("trawls", "g", "traps"))

# wide to long
library(reshape2)
div.gear.df <- melt(div.gear.df[,-c(1, 3, 5, 10:13)], id.vars = c("sea.basin", "FAO.class", "y.mid"))

# combine Bohai and Yellow Sea
div.gear.df$sea.basin[div.gear.df$sea.basin %in% c("Bohai Sea", "Yellow Sea")] <- "Bohai and Yellow Sea"

## No. species----

N.S.p <- ggplot(subset(div.gear.df, variable == "No.species")) +
  geom_point(aes(x = y.mid, y = value, fill = sea.basin), shape =  21, size = 3, alpha = 0.6) +
  geom_smooth(aes(x = y.mid, y = value), method = 'lm', formula = y ~ x) +
  labs(x = "", y = "") +
  scale_x_continuous(breaks = seq(10, 40, by = 5)) +
  facet_wrap(.~FAO.class, ncol = 3) +
  theme_bw() +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "none") +
  coord_flip(ylim = c(5, 133))

png("plot/Fig. 3/gear.specific.No.species.png",  width = 5, height = 3, units = 'in', res = 800)
print(N.S.p)
dev.off()

## Shannon Wiener index ----

H.p <- ggplot(subset(div.gear.df, variable == "H")) +
  geom_point(aes(y = value, x = y.mid, fill = sea.basin), shape =  21, size = 3, alpha = 0.6) +
  geom_smooth(aes(y = value, x = y.mid), method = 'lm', formula = y ~ x) +
  labs(x = "", y = "") +
  scale_x_continuous(breaks = seq(10, 40, by = 5)) +
  facet_wrap(.~FAO.class, ncol = 3) +
  theme_bw() +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "none") +
  coord_flip(ylim = c(0, 4.2))

png("plot/Fig. 3/gear.specific.H index.png",  width = 5, height = 3, units = 'in', res = 800)
print(H.p)
dev.off()

## Pielou index ----

J.p <- ggplot(subset(div.gear.df, variable == "J" & value <=1)) +
  geom_point(aes(y = value, x = y.mid, fill = sea.basin), shape =  21, size = 3, alpha = 0.6) +
  geom_smooth(aes(y = value, x = y.mid), method = 'lm', formula = y ~ x) +
  labs(x = "", y = "") +
  scale_x_continuous(breaks = seq(10, 40, by = 5)) +
  facet_wrap(.~FAO.class, ncol = 3) +
  theme_bw() +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "none") +
  coord_flip(ylim = c(0, 1))

png("plot/Fig. 3/gear.specific.J index.png",  width = 5, height = 3, units = 'in', res = 800)
print(J.p)
dev.off()

## trash fish ratio ----

trash.df <- read.csv("results/trash.fish.csv")
trash.df$y.mid <- round(0.5 * (trash.df$y1 + trash.df$y2), 2)

# combine Bohai and Yellow Sea
trash.df$sea.basin[trash.df$sea.basin %in% c("Bohai Sea", "Yellow Sea")] <- "Bohai and Yellow Sea"

trash.scatter.p <- ggplot(subset(trash.df, FAO.class %in% c("g", "traps", "trawls"))) +
  geom_point(aes(y = weight.ratio, x = y.mid, fill = sea.basin), shape =  21, size = 3, alpha = 0.6) +
  geom_smooth(aes(y = weight.ratio, x = y.mid), method = 'lm', formula = y ~ x) +
  labs(x = "", y = "") +
  scale_x_continuous(breaks = seq(10, 40, by = 5)) +
  facet_wrap(.~FAO.class, ncol = 3) +
  theme_bw() +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "none") +
  coord_flip(ylim = c(0, 1))

png("plot/Fig. 3/gear.specific.trash.png",  width = 5, height = 3, units = 'in', res = 800)
print(trash.scatter.p)
dev.off()