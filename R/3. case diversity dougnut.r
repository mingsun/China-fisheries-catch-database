library(ggplot2)
library(plyr)
library(RColorBrewer)
library(ggrepel)

par(mar=c(3,4,2,2))
display.brewer.all()
pallete.4 <- c("firebrick2", "royalblue", "springgreen4", "goldenrod1")

### ------------------------------------------------------ ###
### ---------------------- sea.basin --------------------- ###
### ------------------------------------------------------ ###

sea.basin.df <- data.frame(category = c("Bohai Sea", "Yellow Sea", "East China Sea", "South China Sea"),
                           count    = c(17, 51, 33, 39),
                           fraction = c(17, 51, 33, 39)/140)

# compute the cumulative percentages (top and bottom of each rectangle)
sea.basin.df$ymax <- cumsum(sea.basin.df$fraction)
sea.basin.df$ymin <- c(0, head(sea.basin.df$ymax, n = -1))

# state the labels and compute their positions
sea.basin.df$label.position <- (sea.basin.df$ymax + sea.basin.df$ymin) / 2
sea.basin.df$label <- paste0(sea.basin.df$category, "\n", paste0(round(sea.basin.df$fraction, 2) * 100, "%"))

# # pie plot
# sea.basin.p <- ggplot(sea.basin.df) +
#   geom_rect(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category), alpha = 0.85) +
#   geom_text(x = 1.9, aes(y = label.position, label = label), size = 5, color = "grey15") + # x here controls label position (inner / outer)
#   scale_fill_brewer(palette = "Set2") +
#   coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
#   xlim(c(-1, 4)) + # Try to remove that to see how to make a pie chart
#   theme_void() +
#   theme(legend.position = "none")

# bar plot
sea.basin.p <- ggplot(sea.basin.df) +
  geom_bar(aes(x = category, y = count), fill = "red", alpha = 0.5, stat = "identity", width = 0.65) +
  scale_fill_brewer(palette = "Set2") +
  xlab("") +
  ylab("No. of case") +
  scale_x_discrete(limits=c("Bohai Sea", "Yellow Sea", "East China Sea", "South China Sea")) +
  theme_classic()

png("plot/Fig. 1/sea.basin.dougnut.png",  width = 7, height = 3, units = 'in', res = 800)
print(sea.basin.p)
dev.off()

### ------------------------------------------------------ ###
### --------------------- temporal ----------------------- ###
### ------------------------------------------------------ ###

temporal.df <- data.frame(category = c("spring", "summer", "autumn", "winter", "two seasons", "three seasons", "year round"),
                          count    = c(25, 15, 42, 10, 8, 1, 39),
                          fraction = c(25, 15, 42, 10, 8, 1, 39)/140)

# compute the cumulative percentages (top and bottom of each rectangle)
temporal.df$ymax <- cumsum(temporal.df$fraction)
temporal.df$ymin <- c(0, head(temporal.df$ymax, n = -1))

# state the labels and compute their positions
temporal.df$label.position <- (temporal.df$ymax + temporal.df$ymin) / 2
temporal.df$label <- paste0(temporal.df$category, " ", paste0(round(temporal.df$fraction, 2) * 100, "%"))

# # pie plot
# temporal.p <- ggplot(temporal.df) +
#   geom_rect(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category), alpha = 0.85) +
#   geom_text(x = 2.5, aes(y = label.position, label = label), size = 5, color = "grey15") + # x here controls label position (inner / outer)
#   # geom_text_repel(x = 3, aes(y = label.position, label = label), size = 5, color = "grey15") +  # avoid overlap in text
#   scale_fill_brewer(palette = "Set2") +
#   coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
#   xlim(c(-1, 4)) + # Try to remove that to see how to make a pie chart
#   theme_void() +
#   theme(legend.position = "none")

# bar plot
temporal.p <- ggplot(temporal.df) +
  geom_bar(aes(x = category, y = count), fill = "grey20", alpha = 0.55, stat = "identity", width = 0.65) +
  xlab("") +
  ylab("No. of case") +
  scale_x_discrete(limits = c("spring", "summer", "autumn", "winter", "two seasons", "three seasons", "year round")) +
  scale_y_continuous(limits = c(0, 43), expand = c(0, 0)) +
  theme_classic()

png("plot/Fig. 1/temporal.png",  width = 6, height = 2.5, units = 'in', res = 800)
print(temporal.p)
dev.off()

### ------------------------------------------------------ ###
### ---------- gear: combine gear type and regulation ----------------------- ###
### ------------------------------------------------------ ###

gear <- read.csv("data/csv/03. gears.csv")[,c(1, 7, 8, 13)]

gear <- ddply(gear, .(FAO.class), summarize, 
              n.total = length(FAO.class), 
              n.perm = sum(gear.use == "permissible"),
              n.tran = sum(gear.use == "transitional"),
              n.proh = sum(gear.use == "prohibited"))

library(reshape2)

gear <- melt(gear, id.vars = c("FAO.class", "n.total"))
gear <- arrange(gear, FAO.class, variable) 

gear.p <- ggplot(data = gear) +
  geom_bar(aes(x = FAO.class, y = value, fill = variable), width= 0.75, stat = "identity", position = "stack", alpha = 0.85)+
  scale_fill_manual(values = c("springgreen4", "darkgoldenrod1", "firebrick3")) +
  scale_y_continuous(limits = c(0, 43), expand = c(0, 0)) +
  xlab("") +
  ylab("No. of cases") +
  theme_classic() + 
  theme(legend.position = "none")

png("plot/Fig. 1/gear.png",  width = 6, height = 2.5, units = 'in', res = 800)
print(gear.p)
dev.off()


### ------------------------------------------------------ ###
### ------------------------- gear ----------------------- ###
### ------------------------------------------------------ ###

gear.type.df <- data.frame(category = c("falling gear", "gillnets and entangling nets", "hooks and lines", "lift nets", 
                                        "multiple gears", "seine", "traps", "trawls"),
                           count    = c(10, 41, 4, 6, 7, 1, 42, 29),
                           fraction = c(10, 41, 4, 6, 7, 1, 42, 29)/140)

# compute the cumulative percentages (top and bottom of each rectangle)
gear.type.df$ymax <- cumsum(gear.type.df$fraction)
gear.type.df$ymin <- c(0, head(gear.type.df$ymax, n = -1))

# state the labels and compute their positions
gear.type.df$label.position <- (gear.type.df$ymax + gear.type.df$ymin) / 2
gear.type.df$label <- paste0(gear.type.df$category, " ", paste0(round(gear.type.df$fraction, 2) * 100, "%"))

# # pie plot
# gear.type.p <- ggplot(gear.type.df) +
#   geom_rect(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category), alpha = 0.85) +
#   # geom_text(x = 1.9, aes(y = label.position, label = label), size = 5, color = "grey15") + # x here controls label position (inner / outer)
#   # geom_text_repel(x = 3.2, aes(y = label.position, label = label), size = 5, color = "grey15") +  # avoid overlap in text
#   scale_fill_brewer(palette = "Set2") +
#   coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
#   xlim(c(-1, 4)) + # Try to remove that to see how to make a pie chart
#   theme_void() +
#   theme(legend.position = "none")

# bar plot
sea.basin.p <- ggplot(gear.type.df) +
  geom_bar(aes(x = category, y = count), fill = "red", alpha = 0.5, stat = "identity", width = 0.65) +
  scale_fill_brewer(palette = "Set2") +
  xlab("") +
  ylab("No. of case") +
  theme_classic()

png("plot/gear.type.dougnut.png",  width = 5, height = 5, units = 'in', res = 800)
print(gear.type.p)
dev.off()

### ------------------------------------------------------ ###
### ---------------------- gear.reg --------------------- ###
### ------------------------------------------------------ ###

gear.reg.df <- data.frame(category = c("permissible", "transitional", "prohibited"),
                          count    = c(33, 105, 2),
                          fraction = c(33, 105, 2)/140)

# compute the cumulative percentages (top and bottom of each rectangle)
gear.reg.df$ymax <- cumsum(gear.reg.df$fraction)
gear.reg.df$ymin <- c(0, head(gear.reg.df$ymax, n = -1))

# state the labels and compute their positions
gear.reg.df$label.position <- (gear.reg.df$ymax + gear.reg.df$ymin) / 2
gear.reg.df$label <- paste0(gear.reg.df$category, "\n", paste0(round(gear.reg.df$fraction, 2) * 100, "%"))


# plot
gear.reg.p <- ggplot(gear.reg.df) +
  geom_rect(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category), alpha = 0.85) +
  geom_text(x = 1.9, aes(y = label.position, label = label), size = 5, color = "grey15") + # x here controls label position (inner / outer)
  scale_fill_manual(values = c("springgreen4", "firebrick3", "darkgoldenrod1")) +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(-1, 4)) + # Try to remove that to see how to make a pie chart
  theme_void() +
  theme(legend.position = "none")

png("plot/gear.reg.dougnut.png",  width = 5, height = 5, units = 'in', res = 800)
print(gear.reg.p)
dev.off()


