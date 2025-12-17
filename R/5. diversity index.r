library(plyr)

# load the species composition file
comp.df <- read.csv(file = "Data/csv/05. species composition.csv")[,c(1,5,6,7,9,10)] 

# load the spatio-temporal case info
spa.tem.df <- read.csv(file = "Data/csv/02. spatio-temporal info.csv")[,c(1,3,9)]

# load the gear info
gear.df <- read.csv(file = "Data/csv/03. gears.csv")[,c(1,8,13)]

# merge the two df
comp.df <- merge(merge(comp.df, spa.tem.df), gear.df)
remove(spa.tem.df, gear.df)

# comp.df <- subset(comp.df, !is.na(catch))

### ---------------------------------------------------- ### 
# Calculate Diversity ----------------
### ---------------------------------------------------- ###

# Margalef index un viable (D = (S-1)/ln(W), where S is the number of species, W is the total weight)
# simply use species number instead

comp.df.D <- ddply(comp.df, .(case, sea.basin, Season, FAO.class, gear.use), summarize,
                   No.species = max(No.species))

# Shannon-Wiener index
# H = -sum(Pi * ln(Pi)), where Pi is the weight proportion of ith species to total weight

comp.df.H <- ddply(comp.df, .(case, sea.basin, Season, FAO.class, gear.use), mutate,
                   Pi = weight.ratio/1)

comp.df.H <- ddply(comp.df.H, .(case, sea.basin, Season, FAO.class, gear.use), summarize,
                   H = -sum(Pi*log(Pi)))

# Pielou index
# J = H/ln(S), where H is the SW index and S is the number of species

comp.df.J <- ddply(comp.df, .(case, sea.basin, Season, FAO.class, gear.use), summarize,
                   S = length(case))

comp.df.J <- merge(comp.df.H, comp.df.J)
comp.df.J$J <- comp.df.J$H/log(comp.df.J$S)

# combine
div.df <- merge(comp.df.D, comp.df.J) 
div.df$H <- round(div.df$H, 2); div.df$J <- round(div.df$J, 2)
remove(comp.df.D, comp.df.H, comp.df.J, comp.df)

write.csv(div.df, "results/diversity.index.draft.csv")
# this file needs to be manually editted becaues some studies already include diversity index


### ---------------------------------------------------- ### 
# scatter plot by latitude ----------- 
### ---------------------------------------------------- ###

library(ggplot2)

# reload the editting-finished file
div.df <- read.csv("results/diversity.index.editted.csv")[,-10]

# merge with the coordinates
co.df <-  read.csv("data/csv/02. spatio-temporal info.csv")[,-c(2,3,4,9,10,11)]
div.df <- merge(div.df, co.df); remove(co.df)
div.df$y.mid <- round(0.5 * (div.df$y1 + div.df$y2), 2)

# combine Bohai and Yellow Sea
div.df$sea.basin[div.df$sea.basin %in% c("Bohai Sea", "Yellow Sea")] <- "Bohai and Yellow Sea"

## No.species ----
N.S.p <- ggplot(div.df) +
  # geom_point(aes(y = No.species, x = y.mid), fill = "skyblue2", shape =  21, size = 5, alpha = 0.6) +
  geom_point(aes(y = No.species, x = y.mid, fill = sea.basin), shape =  21, size = 3, alpha = 0.6) +
  geom_smooth(aes(y = No.species, x = y.mid), method = 'lm', formula = y ~ x) +
  labs(y = "Number of species\nspecies richness in catch composition", x = "") +
  scale_x_continuous(breaks = seq(10, 40, by = 5)) +
  theme_bw() +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        legend.position = "none") +
  coord_flip(ylim = c(5, 133))

png("plot/Fig. 3/No.species.png",  width = 3.4, height = 7.6, units = 'in', res = 800)
print(N.S.p)
dev.off()

lm.NS <- lm(div.df$No.species ~ div.df$y.mid)
summary(lm.NS) # significant 0.12

shapiro.test(div.df$y.mid)
shapiro.test(div.df$No.species)
cor.test(div.df$y.mid, div.df$No.species, method = c("pearson"))

## Shannon-Wienner Index (H') ----
H.p <- ggplot(div.df) +
  geom_point(aes(y = H, x = y.mid, fill = sea.basin), shape =  21, size = 3, alpha = 0.6) +
  geom_smooth(aes(y = H, x = y.mid), method = 'lm', formula = y ~ x) +
  labs(y = "Shannon-Wiener Index (H')\ndiversity in catch composition", x = "") +
  scale_x_continuous(breaks = seq(10, 40, by = 5)) +
  theme_bw() +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        legend.position = "none") +
  coord_flip(ylim = c(0, 4.2))

png("plot/Fig. 3/H index.png",  width = 3.4, height = 7.6, units = 'in', res = 800)
print(H.p)
dev.off()

lm.H <- lm(div.df$H ~ div.df$y.mid)
summary(lm.H) # not significant

shapiro.test(div.df$y.mid)
shapiro.test(div.df$H)
cor.test(div.df$y.mid, div.df$H, method = c("pearson"))

## Pielou (J') ----
J.p <- ggplot(div.df[-c(103,105,110,112,114),]) +
  geom_point(aes(y = J, x = y.mid, fill = sea.basin), shape =  21, size = 3, alpha = 0.6) +
  geom_smooth(aes(y = J, x = y.mid), method = 'lm', formula = y ~ x) +
  labs(y = "Pielou Index (J')\nevenness in catch composition", x = "") +
  scale_x_continuous(breaks = seq(10, 40, by = 5)) +
  theme_bw() +
  theme(axis.line.x = element_line(colour = "black"),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.border = element_blank(),
        legend.position = "none") +
  coord_flip(ylim = c(0, 1))

png("plot/Fig. 4/J index.png",  width = 3.4, height = 7.6, units = 'in', res = 800)
print(J.p)
dev.off()

lm.J <- lm(div.df[-c(103,105,110,112,114),]$J ~ div.df[-c(103,105,110,112,114),]$y.mid)
summary(lm.J) # not significant

shapiro.test(div.df[-c(103,105,110,112,114),]$y.mid)
shapiro.test(div.df[-c(103,105,110,112,114),]$J)
cor.test(div.df[-c(103,105,110,112,114),]$y.mid, div.df[-c(103,105,110,112,114),]$J, method = c("pearson"))

### ---------------------------------------------------- ### 
# boxplot by gear and season ----
### ---------------------------------------------------- ###

### -------------- by season ---------- ###

div.df <- read.csv("results/diversity.index.seasons.csv")[,-10]

## 1 season and year-round only ----

div.full.df <- subset(div.df, Season %in% c("spring", "summer", "autumn", "winter", "all year"))

div.full.df$Season <- factor(div.full.df$Season, 
                             levels = c("spring", "summer", "autumn", "winter", "all year"))

### no of species ----
NS.p <- ggplot(div.full.df) +
  geom_boxplot(aes(x = Season, y = No.species), fill = "red", alpha = 0.55, width = 0.45) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_blank())

png("plot/Fig. 3/No.species.png",  width = 5, height = 2, units = 'in', res = 800)
print(NS.p)
dev.off()

### H ----
H.p <- ggplot(div.full.df) +
  geom_boxplot(aes(x = Season, y = H), fill = "red", alpha = 0.55, width = 0.45) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_blank())

png("plot/Fig. 3/H.index.png",  width = 5, height = 2, units = 'in', res = 800)
print(H.p)
dev.off()

### J ----
J.p <- ggplot(subset(div.full.df, J <=1)) +
  geom_boxplot(aes(x = Season, y = J), fill = "red", alpha = 0.55, width = 0.45) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(legend.position = "none", axis.text.x = element_blank())

png("plot/Fig. 3/J.index.png",  width = 5, height = 2, units = 'in', res = 800)
print(J.p)
dev.off()


## 1, 2, and 3 seasons ----

# create empty data for x-ticks to seperate 1,2,3,and 4 seasons

z.tier1 <- z.tier2 <- z.tier3 <- div.df[1,]
z.tier1[,1:10] <- z.tier2[,1:10] <- z.tier3[,1:10] <- NA
z.tier1$period <- "1"; z.tier1$Season <- "a"
z.tier2$period <- "2"; z.tier2$Season <- "b"
z.tier3$period <- "3"; z.tier3$Season <- "c"

div.full.df <- rbind(div.df,z.tier1,z.tier2,z.tier3)
rm(list=setdiff(ls(), c("div.df","div.full.df")))

# order the x axis level according to season and period
div.full.df$Season <- factor(div.full.df$Season, 
                             levels = c("spring", "summer", "autumn", "winter", "a",
                                        "spring+summer", "spring+autumn", "summer+autumn", "summer+winter", "b",
                                        "summer+autumn+winter", "c",
                                        "all year"))
list.season <-  c("spring", "summer", "autumn", "winter", "",
                  "spring\n+\nsummer", "spring\n+\nautumn", "summer\n+\nautumn", "summer\n+\nwinter", "",
                  "summer\n+\nautumn\n+\nwinter", "",
                  "all year")
# No.species
NS.p <- ggplot(div.full.df) +
  geom_boxplot(aes(x = Season, y = No.species, fill = period)) +
  scale_x_discrete(labels= list.season) +
  labs(x = "", y = "") +
  geom_vline(xintercept = c(5,10,12), linetype = 2, color = "grey20") +
  theme_bw() +
  theme(legend.position = "none")
  
png("plot/No.species.png",  width = 7, height = 3, units = 'in', res = 800)
print(NS.p)
dev.off()
  
# H
H.p <- ggplot(div.full.df) +
  geom_boxplot(aes(x = Season, y = H, fill = period)) +
  scale_x_discrete(labels= list.season) +
  labs(x = "", y = "") +
  geom_vline(xintercept = c(5,10,12), linetype = 2, color = "grey20") +
  theme_bw() +
  theme(legend.position = "none")

png("plot/H.index.png",  width = 7, height = 3, units = 'in', res = 800)
print(H.p)
dev.off()

# J
J.p <- ggplot(div.full.df[-c(103,105,110,112,114),]) +
  geom_boxplot(aes(x = Season, y = J, fill = period)) +
  scale_x_discrete(labels= list.season) +
  labs(x = "", y = "") +
  geom_vline(xintercept = c(5,10,12), linetype = 2, color = "grey20") +
  theme_bw() +
  theme(legend.position = "none")

png("plot/J.index.png",  width = 7, height = 3, units = 'in', res = 800)
print(J.p)
dev.off()

