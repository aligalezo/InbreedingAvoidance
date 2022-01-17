## Age at first dispersal: lodge vs. nonlodge
## Ali Galezo

library(tidyverse)
library(survival)
library(survminer)

## Load natal dispersal dates.
dispersals <- readRDS(file = "Data/Raw/dispersals.csv")

## Survival analysis.
surv <- Surv(time = dispersals$dispersal_age,
             event = dispersals$disp_status)
surv_fit <- survfit(surv ~ grp_type, data = dispersals)

## Results.
surv_pvalue(surv_fit)

## Plot.
plot <- ggsurvplot(surv_fit,
           data = dispersals,
           pval = FALSE,
           conf.int = TRUE,
           xlab = "age",
           ylab = "proportion of males not yet dispersed",
           legend.title = "",
           legend.labs = c(paste("provisioned (n = ", summary(surv_fit)$n[1], ")", sep = ""),
                           paste("wild-feeding (n = ", summary(surv_fit)$n[2], ")", sep = "")),
           legend = c(0.26, 0.12),
           palette = "Dark2",
           censor.size = 3,
           font.x = 11,
           font.y = 11,
           font.tickslab = 11,
           font.legend = 9,
           censor.shape = 124)
plot$plot <- plot$plot + theme(legend.background = element_rect(fill="transparent"))
print(plot)

ggsave("dispersal_figure.pdf", plot = last_plot(), device = "pdf", path = "Output", width = 4, height = 4, units = "in", dpi = 300)
