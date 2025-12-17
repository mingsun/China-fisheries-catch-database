library(ggplot2)
library(RColorBrewer)


### ---------------------------------------------------- ### 
### ----------------- target species ratio ------------- ###
### ---------------------------------------------------- ###

### ---------- boxplot

ts.ratio.df <- read.csv("data/csv/04.1. target species ratio by gear.csv")[,c(1,6,10)]

# add a row of with 0 target species catch
ts.ratio.df <- rbind(ts.ratio.df, 
                     data.frame(case = NA, gear.use = "prohibited", target.species.ratio = 0))

ts.ratio.df$gear.use <- factor(ts.ratio.df$gear.use, levels = c("permissible", "transitional", "prohibited"))

TS.p <- ggplot(ts.ratio.df) +
  geom_boxplot(aes(x = gear.use, y = target.species.ratio, fill = gear.use), width = 0.5) +
  labs(x = "", y = "") +
  scale_fill_manual(values = c("springgreen4", "darkgoldenrod1", "firebrick3")) +
  theme_bw() +
  theme(legend.position = "none")

png("plot/target.species.ratio.png",  width = 4, height = 4, units = 'in', res = 800)
print(TS.p)
dev.off()

### ---------- a violin plot for overview

ts.ratio.df$group <- "a"

TS.violin.p <- ggplot(ts.ratio.df[1:58,]) +
  geom_violin(aes(x = group, y = target.species.ratio), fill = "skyblue2", alpha = 0.6, color = "skyblue2") +
  # ylim(c(90000, 2000000)) +
  # ylab("catch[mt]") +
  theme_bw() +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), 
        axis.title.x = element_blank(), axis.line.x = element_blank() )

png("plot/TS.violin.png",  width = 2, height = 3.6, units = 'in', res = 800)
print(TS.violin.p)
dev.off()


### ---------------------------------------------------- ### 
### ----------------- diversity index ------------------ ###
### ---------------------------------------------------- ###

div.df <- read.csv("results/diversity.index.editted.csv")[,c(1,5,6,7,9)]

div.df$J[c(103,105,110,112,114)] <- NA
div.df$group <- "a"

# no. of species
NS.p <- ggplot(div.df) +
  geom_boxplot(aes(x = gear.use, y = No.species, fill = gear.use), width = 0.5) +
  labs(x = "", y = "") +
  scale_fill_manual(values = c("springgreen4", "darkgoldenrod1", "firebrick3")) +
  theme_bw() +
  theme(legend.position = "none")

png("plot/Fig. 7/No.species.png",  width = 4, height = 4, units = 'in', res = 800)
print(NS.p)
dev.off()

NS.violin.p <- ggplot(div.df) +
  geom_violin(aes(x = group, y = No.species), fill = "skyblue2", alpha = 0.6, color = "skyblue2") +
  theme_bw() +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), 
        axis.title.x = element_blank(), axis.line.x = element_blank() )

png("plot/Fig. 7/No.species.violin.png",  width = 2, height = 3.6, units = 'in', res = 800)
print(NS.violin.p)
dev.off()


# H
H.p <- ggplot(div.df) +
  geom_boxplot(aes(x = gear.use, y = H, fill = gear.use), width = 0.5) +
  labs(x = "", y = "") +
  scale_fill_manual(values = c("springgreen4", "darkgoldenrod1", "firebrick3")) +
  theme_bw() +
  theme(legend.position = "none")

png("plot/Fig. 7/H.png",  width = 4, height = 4, units = 'in', res = 800)
print(H.p)
dev.off()

H.violin.p <- ggplot(div.df) +
  geom_violin(aes(x = group, y = H), fill = "skyblue2", alpha = 0.6, color = "skyblue2") +
  theme_bw() +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), 
        axis.title.x = element_blank(), axis.line.x = element_blank() )

png("plot/Fig. 7/H.violin.png",  width = 2, height = 3.6, units = 'in', res = 800)
print(H.violin.p)
dev.off()

# J
J.p <- ggplot(div.df) +
  geom_boxplot(aes(x = gear.use, y = J, fill = gear.use), width = 0.5) +
  labs(x = "", y = "") +
  scale_fill_manual(values = c("springgreen4", "darkgoldenrod1", "firebrick3")) +
  theme_bw() +
  theme(legend.position = "none")

png("plot/Fig. 7/J.png",  width = 4, height = 4, units = 'in', res = 800)
print(J.p)
dev.off()

J.violin.p <- ggplot(div.df) +
  geom_violin(aes(x = group, y = J), fill = "skyblue2", alpha = 0.6, color = "skyblue2") +
  theme_bw() +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), 
        axis.title.x = element_blank(), axis.line.x = element_blank() )

png("plot/Fig. 7/J.violin.png",  width = 2, height = 3.6, units = 'in', res = 800)
print(J.violin.p)
dev.off()

### ----------------- below are the complex version of indices ------------------ ###

library(reshape2)
library(patchwork)

div.df <- melt(div.df, id.vars = c("case", "gear.use"),
               measure.vars = c("No.species", "H", "J"),
               variable.name = "Index",
               value.name = "value")

# prepare for a second axis
coef <- 0.03

div.df$value[div.df$Index != "No.species"] <- div.df$value[div.df$Index != "No.species"]/coef

div.df$Index

# use index as x axis

DI.p <- ggplot(div.df) +
    geom_boxplot(aes(x = Index, y = value, fill = gear.use, alpha = Index), width = 0.6) +
    labs(x = "", y = "") +
    scale_y_continuous(name = "first axis", breaks = seq(from = 0, to = 140, by = 35),
                       sec.axis = sec_axis(trans = ~.*coef, name = "second axis")) +
    scale_fill_manual(values = c("springgreen4", "darkgoldenrod1", "firebrick3")) +
    scale_alpha_manual(values = c(0.3, 0.6, 1)) +
    theme_bw() +
    coord_cartesian(ylim = c(5, 135)) +
    theme(legend.position = "none")

ddply(div.df, .(gear.use, Index), summarize, median = median(value, na.rm = TRUE))


# # use geat category as x axis
# DI.p <- ggplot(div.df) +
#   geom_boxplot(aes(x = gear.use, y = value, fill = gear.use, alpha = Index), width = 0.6) +
#   labs(x = "", y = "") +
#   scale_y_continuous(name = "first axis", breaks = seq(from = 0, to = 140, by = 35),
#                      sec.axis = sec_axis(trans = ~.*coef, name = "second axis")) +
#   scale_fill_manual(values = c("springgreen4", "darkgoldenrod1", "firebrick3")) +
#   scale_alpha_manual(values = c(0.3, 0.6, 1)) +
#   theme_bw() +
#   coord_cartesian(ylim = c(5, 135)) +
#   theme(legend.position = "none")


# DI.p <- ggplot(div.df) +
#   geom_boxplot(aes(x = gear.use, y = value, fill = Index), width = 0.6) +
#   labs(x = "", y = "") +
#   scale_y_continuous(name = "first axis", breaks = seq(from = 0, to = 140, by = 35),
#                      sec.axis = sec_axis(trans = ~.*coef, name = "second axis")) +
#   scale_fill_brewer(palette = "Set2") +
#   theme_bw() +
#   coord_cartesian(ylim = c(5, 135)) +
#   theme(legend.position = "none")

png("plot/Fig. 7/diversity.index.png",  width = 4, height = 4, units = 'in', res = 800)
print(DI.p)
dev.off()


### ---------- a violin plot for overview

DI.violin.p <- ggplot(div.df) +
  geom_violin(aes(x = Index, y = value, alpha = Index), color = "skyblue2", fill = "skyblue2") +
  coord_cartesian(ylim = c(5, 135)) +
  scale_alpha_manual(values = c(0.3, 0.6, 1)) +
  scale_y_continuous(name = "", breaks = seq(from = 0, to = 140, by = 35)) +
  theme_bw() +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), 
        axis.title.x = element_blank(), axis.line.x = element_blank(),
        legend.position = "none")

png("plot/Fig. 6/DI.violin.png",  width = 2, height = 3.6, units = 'in', res = 800)
print(DI.violin.p)
dev.off()

### ---------------------------------------------------- ### 
### -------------------- trash fish -------------------- ###
### ---------------------------------------------------- ###

trash.df <- read.csv("results/trash.fish.csv")[c(2,4,13)]

trash.fish.p <- ggplot(trash.df) +
  geom_boxplot(aes(x = gear.use, y = weight.ratio, fill = gear.use), width = 0.5) +
  labs(x = "", y = "") +
  scale_y_continuous(name = "", breaks = seq(from = 0, to = 1, by = 0.25)) +
  scale_fill_manual(values = c("springgreen4", "darkgoldenrod1", "firebrick3")) +
  theme_bw() +
  coord_cartesian(ylim = c(0.03, 0.97)) +
  theme(legend.position = "none")

png("plot/trash.fish.png",  width = 4, height = 4, units = 'in', res = 800)
print(trash.fish.p)
dev.off()

### ---------- a violin plot for overview

trash.df$group <- "a"

trash.violin.p <- ggplot(trash.df) +
  geom_violin(aes(x = group, y = weight.ratio), fill = "skyblue2", alpha = 0.6, color = "skyblue2") +
  coord_cartesian(ylim = c(0.03, 0.97)) +
  theme_bw() +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), 
        axis.title.x = element_blank(), axis.line.x = element_blank() )

png("plot/trash.violin.png",  width = 2, height = 3.6, units = 'in', res = 800)
print(trash.violin.p)
dev.off()

