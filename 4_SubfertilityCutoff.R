## Subfertilty cutoff
## Ali Galezo

library(dplyr)

female_dates <- readRDS(file = "Data/Raw/female_first_pregnancies.Rda")
range <- 1:max(female_dates$cycle_number)
cumulative <- sapply(range, function(x){
  nrow(female_dates[female_dates$cycle_number <= x, ]) / nrow(female_dates)
})

subfertplot <- ggplot(data = data.frame(range = range, cumulative = cumulative), aes(x = range, y = cumulative)) +
  geom_point() +
  theme_classic() +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  scale_y_continuous(expand = c(0,0.01), breaks = seq(0, 1, 0.25), limits = c(0, 1)) +
  xlab("cycle number") +
  ylab("cumulative proportion conceptive females")

ggsave("subfertility_plot.pdf", plot = subfertplot, device = "pdf", path = "Output", width = 5.5, height = 4, units = "in", dpi = 300)
