## Plot coresidency lengths
## Ali Galezo

library(dplyr)
library(ggplot2)

########################################
## Load data
########################################

relatives <- readRDS(file = "Data/Processed/relatives_coresidency.Rda")

########################################
## Only include related pairs whose coresidency length is uncensored and whose lifespans overlap
########################################

relatives <-
  relatives %>%
  filter(censor == "uncensored") %>%
  filter((birth_male >= birth_female & birth_male <= statdate_female) | (birth_female >= birth_male & birth_female <= statdate_male))

########################################
## Summary stats
########################################

names <- data.frame(short = c("MotherSon","FatherDaughter","MatSibs","PatSibs","HalfAuntNephew","HalfUncleNiece","HalfFirstCousins"),
                    long = factor(c("mother-\nson\npairs", "father-\ndaughter\npairs", "maternal\nsiblings", "paternal\nsiblings", "half-\naunt-\nnephew\npairs", "half-\nuncle-\nniece\npairs", "half\nfirst\ncousins"),
                                  levels = c("mother-\nson\npairs", "father-\ndaughter\npairs", "maternal\nsiblings", "paternal\nsiblings", "half-\naunt-\nnephew\npairs", "half-\nuncle-\nniece\npairs", "half\nfirst\ncousins")),
                    wide = factor(c("mother-son pairs", "father-daughter pairs", "maternal siblings", "paternal siblings", "half-aunt-nephew pairs", "half-uncle-niece pairs", "half first cousins"),
                                  levels = c("mother-son pairs", "father-daughter pairs", "maternal siblings", "paternal siblings", "half-aunt-nephew pairs", "half-uncle-niece pairs", "half first cousins")))

## Percent coresident for less than 1 year (overall)
relatives %>%
  filter(is.na(relatives$cores_lt_lodge)) %>%
  filter(cores_rat > 0) %>%
  mutate(less_than_one_year = ifelse(cores_rat < 365.25, TRUE, FALSE)) %>%
  count(less_than_one_year) %>%
  mutate(total = sum(n),
         prop = n/total)

## Percent coresident for less than 1 year (by relative)
relatives %>%
  filter(is.na(relatives$cores_lt_lodge)) %>%
  filter(cores_rat > 0) %>%
  mutate(less_than_one_year = ifelse(cores_rat < 365.25, TRUE, FALSE)) %>%
  count(relation, less_than_one_year) %>%
  group_by(relation) %>%
  mutate(total= sum(n),
         prop = n/total) %>%
  filter(less_than_one_year == TRUE)

########################################
## Plot wild-feeding coresidency lengths: dotplot with violins
########################################

## Pie chart
coresidency_piechart <-
  relatives %>%
  filter(is.na(relatives$cores_lt_lodge)) %>%
  mutate(relation = factor(relation, levels = c("MotherSon","FatherDaughter","MatSibs","PatSibs","HalfAuntNephew","HalfUncleNiece","HalfFirstCousins"))) %>%
  group_by(relation) %>%
  mutate(coresided_as_adults = ifelse(cores_rat == 0, "do not coreside as adults", "coreside as adults")) %>%
  count(coresided_as_adults) %>%
  arrange(desc(coresided_as_adults)) %>%
  mutate(prop = n/sum(n)*100) %>%
  mutate(ypos = cumsum(prop)-0.5*prop) %>%
  ggplot(aes(x="", y = prop, fill = coresided_as_adults, label = n)) +
  geom_bar(stat = "identity", width = 1, color = "gray") +
  geom_text(aes(y = ypos, color = coresided_as_adults), size = 3, fontface = "bold") +
  coord_polar("y", start = 0) +
  theme_void() +
  scale_fill_manual(values = c("white","gray")) +
  scale_color_manual(values = c("black","white")) +
  facet_wrap(~ relation, nrow = 1) +
  guides(color = FALSE) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.title = element_text(angle = 90),
        plot.margin = unit(c(1,4,1,3),"mm")) +
  labs(x = "n pairs\ncoresident\nas adults\n\n", y = NULL)

## Dotplot with violins
coresidency_densityplot <-
  relatives %>%
  filter(is.na(relatives$cores_lt_lodge)) %>%
  filter(cores_rat > 0) %>%
  merge(x = ., y = names, by.x = "relation", by.y = "short", all.x = TRUE) %>%
  arrange(long) %>%
  group_by(relation) %>%
  mutate(n = n(),
         long = paste0(long, "\n(n = ", n, ")")) %>%
  ggplot(aes(x = factor(long, levels = unique(long)), y = cores_rat)) +
  geom_violin(fill = "gray", alpha = 0.5) +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.1, dotsize = 0.7) +
  ylab("time coresident as adults") +
  scale_y_log10(limits = c(1,4200),
                breaks = c(1, 30, 183, 365, 1825, 3650),
                labels = c("1 day", "1 month", "6 months", "1 year", "5 years", "10 years")) +
  ylab("time coresident as adults") +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", size = 0.3),
        axis.title.x = element_blank())
ggsave("coresidency_densityplot.pdf", plot = coresidency_densityplot, device = "pdf", path = "Output", width = 5.5, height = 3.5, units = "in", dpi = 300)

coresidency_density_withpie <- gridExtra::grid.arrange(coresidency_piechart, coresidency_densityplot,
                                                       nrow = 2,
                                                       heights = c(1.05, 3.5))
ggsave("coresidency_densityplot_withpie.pdf", plot = coresidency_density_withpie, device = "pdf", path = "Output", width = 5.5, height = 4.55, units = "in", dpi = 300)

########################################
## Plot wild-feeding vs. semi-provisioned coresidency lengths
########################################

# Barplot
wild_lodge_barplot <-
  relatives %>%
  mutate(category = ifelse(!is.na(cores_lt) & !is.na(cores_lt_lodge), "both",
                           ifelse(!is.na(cores_lt), "wild",
                                  ifelse(!is.na(cores_lt_lodge), "semi-provisioned", "")))) %>%
  filter(category != "both") %>%
  mutate(ndays = ifelse(category == "wild", cores_rat,
                        ifelse(category == "semi-provisioned", cores_rat_lodge, "ERROR"))) %>%
  mutate(zero = ifelse(ndays == 0, "never coreside as adults", "coreside as adults")) %>%
  count(category, zero) %>%
  group_by(category) %>%
  mutate(total = sum(n),
         prop = n/total,
         name = paste0(category, "\n(n = ", total, ")")) %>%
  ggplot(aes(x =  reorder(name, desc(name)),
             y = prop, fill = zero)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_classic() +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = c("black","gray")) +
  ylab("proportion of individuals") +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "top",
        legend.direction = "vertical")

# Violin and dotplot
wild_lodge_violin <-
  relatives %>%
  mutate(category = ifelse(!is.na(cores_lt) & !is.na(cores_lt_lodge), "both",
                           ifelse(!is.na(cores_lt), "wild",
                                  ifelse(!is.na(cores_lt_lodge), "semi-provisioned", "")))) %>%
  filter(category != "both") %>%
  mutate(ndays = ifelse(category == "wild", cores_rat,
                        ifelse(category == "semi-provisioned", cores_rat_lodge, "ERROR"))) %>%
  mutate(ndays = as.numeric(ndays)) %>%
  filter(ndays > 0) %>%
  mutate(category = factor(category, levels = c("wild","semi-provisioned"))) %>%
  arrange(category) %>%
  group_by(category) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  mutate(category = paste0(category, "\n(n = ", n, ")")) %>%
  ggplot(aes(x = factor(category, levels = unique(category)), y = ndays)) +
  geom_violin(fill = "gray", alpha = 0.5) +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.1, dotsize = 0.6) +
  ylab("time coresident as adults") +
  scale_y_log10(limits = c(1,4200),
                breaks = c(1, 30, 183, 365, 1825, 3650),
                labels = c("1 day", "1 month", "6 months", "1 year", "5 years", "10 years")) +
  theme_classic() +
  theme(panel.grid.major.y = element_line(color = "gray", size = 0.3),
        axis.title.x = element_blank())

## Combine bar and violin plot
blank <- ggplot() + theme_void()
wild_lodge <- ggpubr::ggarrange(wild_lodge_barplot, blank, wild_lodge_violin,
                                ncol = 3, nrow = 1, widths = c(0.5, 0.15, 1),
                                labels = c("A","","B"))
ggsave("coresidency_wildvlodge.pdf", plot = wild_lodge, device = "pdf", path = "Output", width = 7, height = 3, units = "in", dpi = 300)

## In-text lodge vs. wild-feeding statistics
relatives %>%
  mutate(category = ifelse(!is.na(cores_lt) & !is.na(cores_lt_lodge), "both",
                           ifelse(!is.na(cores_lt), "wild",
                                  ifelse(!is.na(cores_lt_lodge), "semi-provisioned", "")))) %>%
  filter(category != "both") %>%
  mutate(ndays = ifelse(category == "wild", cores_rat,
                        ifelse(category == "semi-provisioned", cores_rat_lodge, "ERROR"))) %>%
  mutate(ndays = as.numeric(ndays)) %>%
  filter(ndays > 0) %>%
  group_by(category) %>%
  summarize(mean = mean(ndays),
            median = median(ndays),
            n = n())
