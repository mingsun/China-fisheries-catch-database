library(ggplot2)

# load the species composition file
trash.df <- read.csv(file = "Data/csv/05. species composition.csv")[,c(1,7,9)] 
trash.df <- subset(trash.df, Species == "trash.fish")

# load the spatio-temporal case info
spa.tem.df <- read.csv(file = "Data/csv/02. spatio-temporal info.csv")[,c(1,3,5,6,7,8,9)]

# load the gear info
gear.df <- read.csv(file = "Data/csv/03. gears.csv")[,c(1,8,13)]

# merge the two df
trash.df <- merge(merge(trash.df, spa.tem.df), gear.df)
remove(spa.tem.df, gear.df)

write.csv(trash.df, "results/trash.fish.csv")


### ---------------------------------------------------- ### 
### --------------- scatter plot by latitude ----------- ###
### ---------------------------------------------------- ###

trash.df <- read.csv("results/trash.fish.csv")

# merge with the coordinates
trash.df$y.mid <- round(0.5 * (trash.df$y1 + trash.df$y2), 2)

# combine Bohai and Yellow Sea
trash.df$sea.basin[trash.df$sea.basin %in% c("Bohai Sea", "Yellow Sea")] <- "Bohai and Yellow Sea"

trash.scatter.p <- ggplot(trash.df) +
  geom_point(aes(y = weight.ratio, x = y.mid, fill = sea.basin), shape =  21, size = 3, alpha = 0.6) +
  geom_smooth(aes(y = weight.ratio, x = y.mid), method = 'lm', formula = y ~ x) +
  labs(y = "by-product catch ratio \n(percent by weight in decimal)", x = "") +
  scale_x_continuous(breaks = seq(10, 40, by = 5)) +
  theme_bw() +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        legend.position = "none") +
  coord_flip(ylim = c(0, 1))

png("plot/Fig. 4/trash.scatter.png",  width = 3.4, height = 7.6, units = 'in', res = 800)
print(trash.scatter.p)
dev.off()

lm.trash <- lm(trash.df$weight.ratio ~ trash.df$y.mid)
summary(lm.trash) # significant 0.03

shapiro.test(div.df$y.mid)
shapiro.test(div.df$H)
cor.test(div.df$y.mid, div.df$H, method = c("pearson"))


### ---------------------------------------------------- ### 
### ------------- boxplot by gear and season ------------ ###
### ---------------------------------------------------- ###

### -------------- by season ---------- ###

trash.df <- read.csv("results/trash.fish.csv")

## 1 season and year-round only ----

trash.full.df <- subset(trash.df, Season %in% c("spring", "summer", "autumn", "winter", "all year"))

trash.full.df$Season <- factor(trash.full.df$Season, 
                             levels = c("spring", "summer", "autumn", "winter", "all year"))

TS.season.p <- ggplot(trash.full.df) +
  geom_boxplot(aes(x = Season, y = weight.ratio), fill = "red", alpha = 0.55, width = 0.45) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_blank())

png("plot/Fig. 4/trash.fish.season.png",  width = 5, height = 2, units = 'in', res = 800)
print(TS.season.p)
dev.off()

## 1, 2, and 3 seasons ----

# create empty data for x-ticks to seperate 1,2,3,and 4 seasons
z.tier1 <- z.tier2 <- z.tier3 <- trash.df[1,]
z.tier1[,1:12] <- z.tier2[,1:12] <- z.tier3[,1:12] <- NA
z.tier1$period <- "1"; z.tier1$Season <- "a"
z.tier2$period <- "2"; z.tier2$Season <- "b"
z.tier3$period <- "3"; z.tier3$Season <- "c"

trash.full.df <- rbind(trash.df,z.tier1,z.tier2,z.tier3)
rm(list=setdiff(ls(), c("trash.df","trash.full.df")))

# order the x axis level according to season and period
trash.full.df$Season <- factor(trash.full.df$Season, 
                             levels = c("spring", "summer", "autumn", "winter", "a",
                                        "spring+summer", "spring+autumn", "summer+autumn", "summer+winter", "b",
                                        "summer+autumn+winter", "c",
                                        "all year"))
list.season <-  c("spring", "summer", "autumn", "winter", "",
                  "spring\n+\nsummer", "spring\n+\nautumn", "summer\n+\nautumn", "summer\n+\nwinter", "",
                  "summer\n+\nautumn\n+\nwinter", "",
                  "all year")

TS.season.p <- ggplot(trash.full.df) +
  geom_boxplot(aes(x = Season, y = weight.ratio, fill = period)) +
  scale_x_discrete(labels= list.season) +
  labs(x = "", y = "") +
  geom_vline(xintercept = c(5,10,12), linetype = 2, color = "grey20") +
  theme_bw() +
  theme(legend.position = "none")

png("plot/trash.fish.season.png",  width = 7, height = 3, units = 'in', res = 800)
print(TS.season.p)
dev.off()

### -------------- by gear ---------- ###

median(trash.df$weight.ratio)

TS.gear.p <- ggplot(trash.df) +
  geom_hline(yintercept = 0.11, linetype = 2) +
  geom_boxplot(aes(x = FAO.class, y = weight.ratio), fill = "skyblue2", width = 0.45) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none")

png("plot/Fig. 4/trash.fish.gear.png",  width = 9, height = 3, units = 'in', res = 800)
print(TS.gear.p)
dev.off()

ddply(trash.df, .(FAO.class), summarize, median = median(weight.ratio))
