library(ggplot2)
library(reshape2)


### -------------------------------------------------------------- ###
### ---------- proportion of fishery with target species --------- ###
### -------------------------------------------------------------- ###

# all toagther c("58 with target species", "82 without target species"),

# by gear
ts.gear.df <- data.frame(category = c("traps", "g", "trawls", "f", "multi", "lift", "hl", "sur"),
                         # total.ca = c(42,     41,   29,       10,  7,       6,      4,    1),
                         without.ts = c(31,     25,   19,       0,   7,       0,      0,    0),
                         with.ts    = c(11,     16,   10,       10,  0,       6,      4,    1))


# wide to long
ts.gear.df <- melt(ts.gear.df,
     id.vars = c("category"),
     measure.vars = c("with.ts", "without.ts"),
     variable.name = "target.species",
     value.name = "amount")

# plot
ts.gear.p <- ggplot(ts.gear.df) + 
  geom_bar(aes(x = category, y = amount, fill = target.species), position = "stack", stat = "identity", color = "black") +
  labs(y = "", x = "") +
  scale_fill_manual(values = c("skyblue2", "white")) +
  theme_classic() +
  theme(legend.position = "none", panel.border = element_blank())+
  coord_cartesian(ylim = c(40.5,1))+
  coord_flip()

png("plot/ts.gear.p.png",  width = 4, height = 5, units = 'in', res = 800)
print(ts.gear.p)
dev.off()

### ---------------------------------------------------------- ###
### ---------- proportion of target species in catch --------- ###
### ---------------------------------------------------------- ###

library(ggridges)

# by gear
ts.ratio.df <- read.csv("data/csv/04.1. target species ratio by gear.csv")[,c(4,5,6,8,9,10,11)]

library(plyr)
ddply(ts.ratio.df, .(FAO.class), summarize, median = median(target.species.ratio))

# ridgeline plot
ts.ratio.p <- ggplot(ts.ratio.df, aes(y = FAO.class, x = target.species.ratio)) + 
  # geom_violin(aes(x = FAO.class, y = target.species.ratio), fill = "skyblue2", color = "skyblue2", adjust = 1, alpha = 0.8) +
  geom_density_ridges(stat="binline", bins = 25, fill = "skyblue2") +
  labs(y = "", x = "") +
  theme_ridges() +
  theme(legend.position = "none", panel.border = element_blank())

png("plot/ts.ratio.p.png",  width = 4, height = 5, units = 'in', res = 800)
print(ts.ratio.p)
dev.off()

# overall violin plot
ts.ratio.df$group <- "all"

median(ts.ratio.df$target.species.ratio, na.rm = TRUE)

ts.ratio.overall.p <- ggplot(ts.ratio.df, aes(y = group, x = target.species.ratio)) + 
  geom_violin( fill = "skyblue2", color = "skyblue2", alpha = 0.8) +
  labs(y = "", x = "") +
  geom_vline(xintercept = 0.735, linetype = 2) +
  theme_classic() +
  theme(legend.position = "none", panel.border = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank(),
        panel.grid.major.x = element_line(size = 0.8))

png("plot/Fig. 2/ts.ratio.overall.p.png",  width = 4, height = 2, units = 'in', res = 800)
print(ts.ratio.overall.p)
dev.off()


### -------------------------------------------------------------- ###
### ---------- proportion of target species groups by gear --------- ###
### -------------------------------------------------------------- ###

# all toagther c("58 with target species", "82 without target species"),

# by gear
ts.group.df <- read.csv("data/csv/04.2. target species group by gear.csv")
# finfish 35; crustacean 24; cephalopod 16; total=
# 46.7%  32%   21.3%

# plot
ts.group.p <- ggplot(ts.group.df) + 
  geom_bar(aes(y = appearance, x = FAO.class, fill = target.species.group), position = "stack", stat = "identity", color = "black") +
  labs(y = "", x = "") +
  scale_fill_brewer(palette = "Set2") +
  theme_classic() +
  # theme(legend.position = "none", panel.border = element_blank())+
  coord_flip()

png("plot/ts.group.p.png",  width = 4, height = 5, units = 'in', res = 800)
print(ts.group.p)
dev.off()