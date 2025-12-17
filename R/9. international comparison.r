library(ggplot2)

# international data
int.df <- read.csv("data/csv/international comparison.csv")
int.df <- int.df[c(-25),]

# china data

# total fisheries with ts = 0.41
# trawl with ts = 10/29 = 0.345

c.df <- data.frame(country = c("database trawl China", "database overall China"), rate = c(NA, NA), fisheries = c("trawl", "overall"))
final.df <- rbind(int.df, c.df)

final.df$country <- factor(final.df$country, levels = rev(c(final.df$country)))

# target species weight ratio
ts.ratio.df <- read.csv("data/csv/04.1. target species ratio by gear.csv")[,c(4,5,6,8,9,10,11)]
ts.ratio.df <- ts.ratio.df[,c(1,2,3,6)]

ts.ratio.overall.df <- ts.ratio.df
ts.ratio.overall.df$country <- "database overall China"
ts.ratio.overall.df$rate <- 1 - ts.ratio.overall.df$target.species.ratio * 0.41
median(ts.ratio.overall.df$rate, na.rm = TRUE)

ts.ratio.trawl.df <- subset(ts.ratio.df, FAO.class == "trawls")
ts.ratio.trawl.df$country <- "database trawl China"
ts.ratio.trawl.df$rate <- 1 - ts.ratio.trawl.df$target.species.ratio * 0.41
median(ts.ratio.trawl.df$rate, na.rm = TRUE)


# plot
comp.p <- ggplot(final.df) + 
  geom_bar(aes(x = rate, y = country), position = "stack", stat = "identity", fill = "skyblue2") +
  geom_vline(xintercept = 0.69865, linetype = 2, color = "khaki3") +
  geom_vline(xintercept = 0.86675, linetype = 2, color = "firebrick1") +
  geom_boxplot(data = ts.ratio.trawl.df, aes(x = rate, y = country), fill = "firebrick1") +
  geom_boxplot(data = ts.ratio.overall.df, aes(x = rate, y = country), fill = "khaki3") +
  labs(y = "", x = "") +
  theme_classic() +
  theme(legend.position = "none", panel.border = element_blank())+
  coord_cartesian(xlim = c(0.04,1))

png("plot/compare.png",  width = 6, height = 6, units = 'in', res = 800)
print(comp.p)
dev.off()
