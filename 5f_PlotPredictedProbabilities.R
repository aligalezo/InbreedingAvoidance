## Plot all mate choice model results
## Ali Galezo

library(rstanarm)
library(dplyr)
library(ggplot2)

main <- readRDS("Output/Bayesian Output/mainmodel.RData")
subfert <- readRDS("Output/Bayesian Output/subfertmodel.RData")
lodge <- readRDS("Output/Bayesian Output/lodgemodel.RData")

## Main model
main_combos <- expand.grid(relation = unique(main$data$relation),
                      female_rank = 1,
                      male_rank = c(1:10),
                      num_males = 10,
                      female_age = 10,
                      in_natal_grp = FALSE,
                      ndays = 5)
pp_main <- posterior_linpred(main, newdata = main_combos, transform = TRUE, re.form = NA)
colnames(pp_main) <- paste(main_combos$relation, main_combos$male_rank, sep = "_")
pp_main <- as.data.frame(pp_main) %>% tidyr::gather(class, probability)
pp_main$class_split <- strsplit(pp_main$class, "[_]")
pp_main$relation <- sapply(pp_main$class_split, "[[", 1)
pp_main$male_rank <- sapply(pp_main$class_split, "[[", 2)
pp_main$male_rank <- as.numeric(pp_main$male_rank)
plotPredictedBars <- function(relations){
  pp_main %>%
    filter(relation %in% relations) %>%
    mutate(relation = factor(relation, levels = c("Unrelated",
                                                  "FatherDaughter",
                                                  "MotherSon",
                                                  "PatSibs",
                                                  "MatSibs",
                                                  "HalfAuntNephew",
                                                  "HalfUncleNiece",
                                                  "HalfFirstCousins"))) %>%
    arrange(relation) %>%
    mutate(relation = recode(relation,
                             Unrelated = "unrelated",
                             FatherDaughter = "father-daughter",
                             HalfAuntNephew = "half aunt-nephew",
                             HalfFirstCousins = "half first cousins",
                             HalfUncleNiece = "half uncle-niece",
                             MatSibs = "maternal siblings",
                             MotherSon = "mother-son",
                             PatSibs = "paternal siblings")) %>%
    group_by(relation, male_rank) %>%
    summarize(median = median(probability),
              lower = quantile(probability, 0.05),
              upper = quantile(probability, 0.95)) %>%
    ggplot(aes(x = male_rank, y = median, ymin = lower, ymax = upper, color = relation)) +
    geom_point(stat = "identity", position = position_dodge(width = 0.8)) +
    geom_errorbar(stat = "identity", position = position_dodge(width = 0.8), width = 0) +
    theme_classic() +
    scale_x_continuous(breaks = seq(1,10,1)) +
    scale_y_continuous(breaks = seq(0,0.3,0.05)) +
    scale_color_manual(values = c("gray80","gray50","black")) +
    xlab("male dominance rank") +
    ylab("predicted probability\nof being consort partner") +
    theme(legend.title = element_blank(),
          legend.position = c(0.65, 0.83),
          legend.text = element_text(size = 10),
          legend.background = element_rect(fill="transparent"),
          axis.title.x = element_blank(),
          plot.background = element_rect(fill = "grey96", color = NA))
}
parent_offspring_pp <- plotPredictedBars(c("Unrelated","MotherSon","FatherDaughter"))
sibling_pp <- plotPredictedBars(c("Unrelated","MatSibs","PatSibs")) + ylab(" \n ")
combined_pp <- ggpubr::ggarrange(parent_offspring_pp, sibling_pp, nrow = 1, labels = c("A","B"), label.x = 0.15)
combined_pp <- ggpubr::annotate_figure(combined_pp,
                                       bottom = ggpubr::text_grob("male dominance rank", size = 12)) +
  theme(plot.background = element_rect(fill = "grey96", color = NA))
combined_pp <- ggpubr::annotate_figure(combined_pp,
                                       left = ggpubr::text_grob("Main Model", face = "bold", rot = 90, size = 14))

## Adolescent model
subf_combos <- expand.grid(relation = unique(subfert$data$relation),
                           fert_status = c("fertility","subfertility"),
                           female_rank = 1,
                           male_rank = c(1:10),
                           num_males = 10,
                           female_age = 10,
                           in_natal_grp = FALSE,
                           ndays = 5)
subf_main <- posterior_linpred(subfert, newdata = subf_combos, transform = TRUE, re.form = NA)
colnames(subf_main) <- paste(subf_combos$relation, subf_combos$fert_status, subf_combos$male_rank, sep = "_")
subf_main <- as.data.frame(subf_main) %>% tidyr::gather(class, probability)
subf_main$class_split <- strsplit(subf_main$class, "[_]")
subf_main$relation <- sapply(subf_main$class_split, "[[", 1)
subf_main$fert_status <- sapply(subf_main$class_split, "[[", 2)
subf_main$male_rank <- sapply(subf_main$class_split, "[[", 3)
subf_main$male_rank <- as.numeric(subf_main$male_rank)
plotPredictedBars_subf <- function(relations){
  subf_main %>%
    filter(relation %in% relations) %>%
    mutate(relation = recode(relation,
                             "FatherDaughter" = "father-daughter",
                             "PatSibs" = "paternal siblings")) %>%
    mutate(fert_status = recode(fert_status,
                                "fertility" = "adult fertility",
                                "subfertility" = "adolescent\nsubfertility")) %>%
    group_by(relation, fert_status, male_rank) %>%
    summarize(median = median(probability),
              lower = quantile(probability, 0.95),
              upper = quantile(probability, 0.05)) %>%
    ggplot(aes(x = male_rank, y = median, ymin = lower, ymax = upper, color = fert_status)) +
    geom_point(stat = "identity", position = position_dodge(width = 0.6)) +
    geom_errorbar(stat = "identity", position = position_dodge(width = 0.6), width = 0) +
    theme_classic() +
    scale_x_continuous(breaks = c(1:10)) +
    scale_y_continuous(limits = c(0, 0.35), breaks = seq(0,0.35,0.05)) +
    scale_color_manual(values = c("gray70","gray10")) +
    xlab("male dominance rank") +
    ylab("predicted probability\nof being consort partner") +
    theme(legend.title = element_blank(),
          legend.position = c(0.65, 0.83),
          legend.text = element_text(size = 10),
          legend.background = element_rect(fill="transparent"),
          plot.title = element_text(hjust = 0.5, vjust = 0, size = 12, face = "bold"),
          axis.title.x = element_blank(),
          plot.background = element_rect(fill = "grey96", color = NA))
}

subf_patsibs_pp <- plotPredictedBars_subf("PatSibs") + ggtitle("paternal siblings")
subf_fatherdaughter_pp <- plotPredictedBars_subf("FatherDaughter") + ggtitle("father-daughter") + ylab(" \n ")
subf_combined_pp <- ggpubr::ggarrange(subf_patsibs_pp, subf_fatherdaughter_pp, nrow = 1, labels = c("C","D"), label.x = 0.15)
subf_combined_pp <- ggpubr::annotate_figure(subf_combined_pp,
                                       bottom = ggpubr::text_grob("male dominance rank", size = 12)) +
  theme(plot.background = element_rect(fill = "grey96", color = NA))
subf_combined_pp <- ggpubr::annotate_figure(subf_combined_pp,
                                       left = ggpubr::text_grob("Adolescent Model", face = "bold", rot = 90, size = 14))

## Lodge model
lodge_combos <- expand.grid(relation_new = unique(lodge$data$relation_new),
                            lodge_grp = c("wild-feeding","semi-provisioned"),
                           female_rank = 1,
                           male_rank = c(1:10),
                           num_males = 10,
                           female_age = 10,
                           ndays = 5)
pp_lodge <- posterior_linpred(lodge, newdata = lodge_combos, transform = TRUE, re.form = NA)
colnames(pp_lodge) <- paste(lodge_combos$relation_new, lodge_combos$lodge_grp, lodge_combos$male_rank, sep = "_")
pp_lodge <- as.data.frame(pp_lodge) %>% tidyr::gather(class, probability)
pp_lodge$class_split <- strsplit(pp_lodge$class, "[_]")
pp_lodge$relation_new <- sapply(pp_lodge$class_split, "[[", 1)
pp_lodge$lodge_grp <- sapply(pp_lodge$class_split, "[[", 2)
pp_lodge$male_rank <- sapply(pp_lodge$class_split, "[[", 3)
pp_lodge$male_rank <- as.numeric(pp_lodge$male_rank)
lodgeModelFunc <- function(lodge_val){
  pp_lodge %>%
    filter(lodge_grp %in% lodge_val) %>%
    mutate(relation_new = recode(relation_new,
                                 "Related" = "related",
                                 "Unrelated" = "unrelated")) %>%
    mutate(relation_new = factor(relation_new, levels = c("unrelated","related"))) %>%
    group_by(relation_new, lodge_grp, male_rank) %>%
    summarize(median = median(probability),
              lower = quantile(probability, 0.95),
              upper = quantile(probability, 0.05)) %>%
    ggplot(aes(x = male_rank, y = median, ymin = lower, ymax = upper, group = relation_new, color = relation_new)) +
    geom_point(stat = "identity", position = position_dodge(width = 0.6)) +
    geom_errorbar(stat = "identity", position = position_dodge(width = 0.6), width = 0) +
    theme_classic() +
    scale_x_continuous(breaks = c(1:10)) +
    scale_y_continuous(limits = c(0,0.4), breaks = seq(0,0.4,0.05)) +
    scale_color_manual(values = c("gray70","gray10")) +
    xlab("male dominance rank") +
    ylab("predicted probability\nof being consort partner") +
    theme(legend.title = element_blank(),
          legend.position = c(0.65, 0.83),
          legend.text = element_text(size = 10),
          legend.background = element_rect(fill="transparent"),
          plot.title = element_text(hjust = 0.5, vjust = 0, size = 12, face = "bold"),
          axis.title.x = element_blank(),
          plot.background = element_rect(fill = "grey96", color = NA))
}
lodge_wild <- lodgeModelFunc("wild-feeding") + ggtitle("wild-feeding")
lodge_provisioned <- lodgeModelFunc("semi-provisioned") + ggtitle("semi-provisioned")  + ylab(" \n ")
lodge_combined <- ggpubr::ggarrange(lodge_wild, lodge_provisioned, nrow = 1, labels = c("E","F"), label.x = 0.15)
lodge_combined <- ggpubr::annotate_figure(lodge_combined,
                                          bottom = ggpubr::text_grob("male dominance rank", size = 12)) +
  theme(plot.background = element_rect(fill = "grey96", color = NA))
lodge_combined <- ggpubr::annotate_figure(lodge_combined,
                                          left = ggpubr::text_grob("Lodge Model", face = "bold", rot = 90, size = 14))

## Combine all
ggpubr::ggarrange(combined_pp,
                  NULL,
                  subf_combined_pp,
                  NULL,
                  lodge_combined,
                  nrow = 5,
                  heights = c(1, 0.05, 1, 0.05, 1))
ggsave(filename = "matechoicemodels_points.tif", plot = last_plot(), device = "tiff", path = "Output", width = 5.5, height = 7, units = "in", dpi = 300)



