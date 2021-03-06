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
# staiton names according to along-wind direction
along.wind.order <- c("AP7", "AP4", "AP3", "AP6", "DH5", "AP1", "DH2", "AP5", "DH3", "DH4", "DH1", "DH7", "DH10", "DH11", "DH9", "DH6", "DH8")
# 17 stations
ns <- 17 
#################################################################################

# read forecasts
setwd(file.path(dir0, "Data/Fcst"))
load(paste0(day, "-", agg, ".RData"))

########################################################
# error data.frame
########################################################
models <- c("CLIM", "TMPPEEN", "SPTPEEN", "ANEN", "LAG1LPQR", "ANENLPQR")
methods <- c("Clim", "Tmp-PeEn", "Spt-PeEn", "AnEn", "Lag1+lpQR", "AnEn+lpQR")
data.plot <- NULL
for(k in 1:ns)
{
  obs <- pull(data, k+2)[verification] # the first two columns are time and zenith angle
  error.table <- NULL
  for(mm in models)
  {
    tmp <- error(obs = obs, pred = get(mm)[[k]][verification,], tau = tau)
    error.table <- bind_rows(error.table, tmp)
  }
  
  for(metric in 1:ncol(error.table))
  {
    tmp <- tibble(y = pull(error.table[,metric], 1), 
                  metric = names(error.table)[metric], 
                  station = names(data)[k+2], 
                  method = methods)
    data.plot <- bind_rows(data.plot, tmp)
  }
  
}

########################################################
# plot
########################################################
data.plot$method <- factor(data.plot$method, levels = methods)
data.plot$station <- factor(data.plot$station, levels = along.wind.order)
data.plot$metric <- factor(data.plot$metric, levels = c("PICP", "PIAW", "CRPS", "pinball"))

p1 <- ggplot(data.plot) +
  #geom_jitter(aes(x = station, y = y, color = method), size = point.size, width = 0.2, alpha = 0.7) +
  geom_point(aes(x = station, y = y, color = method), size = point.size*5, alpha = 0.7, stroke = 0) +
  facet_wrap(~metric, scales = "free_y", ncol = 1) +
  scale_color_manual(values = colorblind_pal()(7)[2:7]) +
  scale_x_discrete(name = "Station") +
  scale_y_continuous(name = expression(paste("Metric [%] or [W", m^2, "]"))) +
  theme_bw() +
  theme(plot.margin = unit(c(0.1,0.1,0,0), "lines"), text = element_text(family = "Times", size = plot.size), axis.text.y = element_text(size = plot.size), axis.text.x = element_text(size = plot.size, angle = 45), axis.title.x = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "transparent"), plot.background = element_rect(fill = "transparent", color = NA), strip.text = element_text(size = plot.size, margin = margin(0.05,0,0.05,0, "lines")), panel.spacing = unit(0.05, "lines"), legend.position = "bottom", legend.text = element_text(family = "Times", size = plot.size), legend.title = element_text(family = "Times", size = plot.size), legend.key.height = unit(0.2, "lines"), legend.key.width = unit(1, "lines"), legend.key = element_rect(fill = "transparent"), legend.box.margin = unit(c(-1.5,0,-0.1,0), "lines"), legend.background = element_rect(fill = "transparent"))

p1  

#setwd(file.path(dir0, "tex"))
#ggsave(filename = "July31.pdf", plot = p1, path = getwd(), scale = 1, width = 85, height = 70, unit = "mm", dpi = 300)  
  