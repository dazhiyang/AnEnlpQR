#################################################################################
# This code is written by Dazhi Yang
# Singapore Institute of Manufacturing Technology (SIMTech)
# Agency for Science, Technology and Research (A*STAR)
# emails: yangdazhi.nus@gmail.com
#################################################################################

#Clear all workspace
rm(list = ls(all = TRUE))
libs <- c("dplyr", "lubridate", "SolarData", "camsRad")
invisible(lapply(libs, library, character.only = TRUE))

#################################################################################
# Inputs
#################################################################################
dir0 <- "/Users/DYang/Dropbox/Working papers/lasso"
agg <- 4 # change this to 10, 30, and 60 for other aggregation
cams_set_user("yangdazhi.nus@gmail.com") # this is needed to download McClear
#################################################################################

# read data
setwd(file.path(dir0, "Data/raw"))
files <- dir()

for(i in 1:length(files))
{
  # read data into tibble
  data <- OSMG.read(files = files[i], directory.LI200 = file.path(dir0, "Data/raw"), clear.sky = T, agg = agg)
  # remove unused columns, see 10.1016/j.solener.2018.06.107
  data <- data %>%
    select(-one_of("Ics", "Ioh", "DH1T", "AP6T"))
  
  # get McClear data from SoDa web services
  day <- substr(files[i], 1, 8)
  day <- as.Date(paste(substr(day, 1, 4), "-", substr(day, 5, 6), "-", substr(day, 7, 8), sep = ""))
  McClear <- cams_get_mcclear(lat = 21.31234, lng = -158.0841, date_begin = as.character(day-1), date_end = as.character(day+1), time_step = "PT01M", alt = 3.9, verbose = FALSE)
  # select the useful columns
  McClear <- as_tibble(McClear)
  McClear <- McClear %>% 
    mutate(Time = ymd_hms(timestamp)) %>% 
    mutate(McClear = `Clear sky GHI`) %>% dplyr::select(-c(1:6)) %>%
    mutate(McClear = McClear*60) 
  # McClear is UTC, need to adjust back to local time
  attributes(McClear$Time)$tzone <- "HST"  
  
  # combine data
  data <- data %>% 
    left_join(., McClear, by = "Time") %>%
    mutate(McClear = zoo::na.approx(McClear, rule = 2))
  
  # save data
  current.dir <- getwd()
  dir.create(file.path(dir0, "Data", paste0("Avg-", agg, "s")), showWarnings = FALSE)
  setwd(file.path(dir0, "Data", paste0("Avg-", agg, "s")))
  save(data, file = paste0(substr(files[i], 1, 8), ".RData"))
  setwd(current.dir)
}




