#################################################################################
# This code is written by Dazhi Yang
# Singapore Institute of Manufacturing Technology (SIMTech)
# Agency for Science, Technology and Research (A*STAR)
# emails: yangdazhi.nus@gmail.com
#################################################################################

#Clear all workspace
rm(list = ls(all = TRUE))
libs <- c("dplyr", "ggplot2", "ggthemes", "scoringRules", "xtable")
invisible(lapply(libs, library, character.only = TRUE))

#################################################################################
# Inputs
#################################################################################
# working directory
dir0 <- "/Users/DYang/Dropbox/Working papers/lasso"
# source the functions
source(file.path(dir0, "Code/functions.R"))
# day
day <- "20100731"
# 1-min data
agg <- 60 
# plotting parameters 
plot.size = 7; line.size = 0.1; point.size = 0.25; legend.size = 0.4; text.size = 1.5;
# set of 21 quantiles
tau <- c(0.025, seq(0.05, 0.95, by = 0.05), 0.975)
Q <- length(tau) # 21

# other settings
ns <- 17 # 17 stations
nt <- ifelse(agg==60, 5, ifelse(agg==30, 10, ifelse(agg==10, 30, 75))) # number of lags
n <-  150 # query length
m <- 21 # how many analogs to select
#################################################################################

# read forecasts
setwd(file.path(dir0, "Data/Fcst"))
load(paste0(day, "-", agg, ".RData"))

########################################################
# plot
########################################################
data.plot <- NULL
period <- 200:350
for(i in 3:19)
{
  tmp <- tibble(x = data$Time[period], y = (pull(data,i)/pull(data, McClear))[period], group = names(data)[i])
  data.plot <- bind_rows(data.plot, tmp)
}

p <- ggplot(data.plot) + 
  geom_line(aes(x = x, y = y, group = group), size = line.size, color = "gray80") +
  geom_line(aes(x = x, y = y, group = group), size = line.size*2, data = subset(data.plot, group=="DH3"), color = colorblind_pal()(4)[3]) +
  scale_x_datetime(name = "Time [HH:MM]") +
  scale_y_continuous(name = "Clear-sky index\n [dimensionless]") +
  theme_bw() +
  theme(plot.margin = unit(c(0.1,0.1,0,0), "lines"), text = element_text(family = "Times", size = plot.size), axis.text = element_text(size = plot.size), axis.title.x = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "transparent"), plot.background = element_rect(fill = "transparent", color = NA), strip.text = element_text(size = plot.size, margin = margin(0.1,0,0.1,0, "lines")), panel.spacing = unit(0 , "lines"))
p
#setwd(file.path(dir0, "tex"))
#ggsave(filename = "DH3.pdf", plot = p, path = getwd(), scale = 1, width = 85, height = 30, unit = "mm", dpi = 300)
########################################################

########################################################
# PICP, PIAW, CRPS, pinball loss
########################################################
obs <- pull(data, "DH3")[verification]
models <- c("CLIM", "TMPPEEN", "SPTPEEN", "ANEN", "LAG1LPQR", "ANENLPQR")
error.table <- NULL
for(mm in models)
{
  tmp <- error(obs = obs, pred = get(mm)[[1]][verification,], tau = tau)
  error.table <- bind_rows(error.table, tmp)
}

error.table <- error.table %>%
  mutate_if(is.numeric, round, digits=1) %>%
  mutate(model = c("\\underline{\\textsc{Clim}}", "\\underline{\\textsc{Tmp-PeEn}}", "\\underline{\\textsc{Spt-PeEn}}", "\\underline{\\textsc{AnEn}}", "\\underline{\\textsc{Lag1+lpQR}}", "\\underline{\\textsc{AnEn+lpQR}}")) %>%
  dplyr::select("model", everything())

print(xtable(error.table, digits = 1), include.rownames=FALSE, sanitize.text.function = function(x){x})

