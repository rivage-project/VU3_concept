# extract data for Climate change
# create a database combining the three CC metrics
# for all selected islands

rm(list=ls())

library(tidyverse)

# Load island list and shapes
isl <- readRDS("data/derived-data/01_shp_45_major_isl.rds")


##### Download data from CHELSA ####

# set the period to dwld
past_period <- c("1979", "1980", "1981", "1982", "1983")
present_period <- c("2015","2016","2017","2018", "2019")

# take all month for each year
month <- sprintf("%02d", 1:12)
year <- c(past_period, present_period)

# select the var to dwld
var <- "pr" # tas, tasmax, pr 
# ! for tas & tasmax, files start at 02_1989 (no january 89) => add 01 1984
# ! for pr, no files after 07 2019 => add file from 07 2014 to 01 2015
for(i in month){
  print(i)
  for(j in year){
    print(j)
    # download monthly timeseries 
    downloader::download(
      paste0("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/", 
            var, "/CHELSA_", var, "_", i, "_", j, "_V.2.1.tif"), 
      paste0(here::here(), "/data/raw-data/climate/CHELSA_monthly_",
             var, "_", i, "_", j, "_V.2.1.tif"),
      mode = "wb")
  }
}

# must be 120 files per variable
# 360 files in total
length(list.files(paste0(here::here(), "/data/raw-data/climate/")))



####### Extract the data for the focal islands ########

# Function for extracting data,
# for each variable and present or past period
# input: 
# - the variable to extract (in tas, tasmax or pr)
# - the time period (past or present)
# - the island shapefile to extract data for
# output:
# a data-frame containing all the values for the 55 islands
# for each time period and climate variable

extract_val <- function(var, time, shp){
  # values for testing
  # var = "pr"
  # time = "present"
  # shp=shp_55
  
  if (!time %in% c("present","past")) stop("the time must be 'present' or 'past'")
  if (!var %in% c("tas","tasmax", "pr")) stop("var must be 'tas', 'tasmax', or 'pr'")
  
  # set the ouputs
  output <- data.frame()
  # make sure the variable is between underscores 
  varok <- paste0("_", var, "_")
  # list all files in directory
  files <- list.files(paste0(here::here(), "/data/raw-data/climate/"))
  # select only files related to the variable
  files_var <- files[grep(varok, files)]
  # and from the good time period
  if(time == "past"){
    period = paste(as.character(1979:1984), collapse='|')
    } else {
    period = paste(as.character(2014:2019), collapse='|')
      }
  files_var_time <- files_var[grep(period, files_var)]
  
  for (file in files_var_time){
    # test value
    # file = files_var_time[8]
    r <- terra::rast(paste0(here::here(), "/data/raw-data/climate/", file))
    sf::st_crs(shp) <- terra::crs(r)
    ext <- terra::extract(r, shp)
    colnames(ext) <- c('ID', var)
    ext$period <- time
    ext$file <- file
    ext$pixel <- 1:nrow(ext)
    output <- bind_rows(output, ext)
  }
  return(output)
}

tas_pres <- extract_val("tas", "present", isl)
tas_past <- extract_val("tas", "past", isl)
pr_pres <- extract_val("pr", "present", isl)
pr_past <- extract_val("pr", "past", isl)

# save outputs for continuing analyses
saveRDS(tas_pres, paste0(here::here(), "/data/derived-data/21_tas_pres_45_isl.rds"))
saveRDS(tas_past, paste0(here::here(), "/data/derived-data/21_tas_past_45_isl.rds"))
saveRDS(pr_pres, paste0(here::here(), "/data/derived-data/21_pr_pres_45_isl.rds"))
saveRDS(pr_past, paste0(here::here(), "/data/derived-data/21_pr_past_45_isl.rds"))


####### Calculate SED following Williams et al 2007 #######

# several options
# annual mean T + annual mean P
# mean T/P JJA and DJF

# variable 1: tas

tas_pres <- readRDS(paste0(here::here(), "/data/derived-data/21_tas_pres_45_isl.rds"))
tas_past <- readRDS(paste0(here::here(), "/data/derived-data/21_tas_past_45_isl.rds"))

# group by year first to have the total annual precipitation amount
# and then calculate sd and mean on the grouped values

tas_pres_pix_y <- tas_pres %>% 
  separate(col = file, 
           into = c("name1", "name2", "name3", "month", "year", "name4"),
           sep="_") %>%
  select(-contains("name")) %>%
  mutate(month_year = paste(year, month, sep="_"))

month_year_pres_tas <- data.frame(
  month_year = sort(unique(tas_pres_pix_y$month_year)),
  num_y = rep(1:5, each = 12)
)

tas_pres_pix_y_ok <- left_join(tas_pres_pix_y, month_year_pres_tas) %>%
  group_by(ID, pixel, num_y) %>%
  summarize(annual_tas = mean(tas))

tas_past_pix_y <- tas_past %>% 
  separate(col = file, 
           into = c("name1", "name2", "name3", "month", "year", "name4"),
           sep="_") %>%
  select(-contains("name")) %>%
  mutate(month_year = paste(year, month, sep="_"))

month_year_past_tas <- data.frame(
  month_year = sort(unique(tas_past_pix_y$month_year)),
  num_y = rep(1:5, each = 12)
)

tas_past_pix_y_ok <- left_join(tas_past_pix_y, month_year_past_tas) %>%
  group_by(ID, pixel, num_y) %>%
  summarize(annual_tas = mean(tas))

tas_t1_y <- tas_past_pix_y_ok %>%
  group_by(ID, pixel)%>%
  summarize(mean_tas_t1 = mean(annual_tas),
            sd_tas_t1 = sd(annual_tas))
tas_t2_y <- tas_pres_pix_y_ok %>%
  group_by(ID, pixel)%>%
  summarize(mean_tas_t2 = mean(annual_tas))

SED_tas_y <- left_join(tas_t1_y, tas_t2_y) %>%
  mutate(sed_tas = ((mean_tas_t2-mean_tas_t1)^2)/(sd_tas_t1^2))

SED_tas_isl_y <- SED_tas_y %>%
  group_by(ID) %>%
  summarize(sed_tas_isl_med = median(sed_tas),
            sed_tas_isl_mean = mean(sed_tas),
            sed_tas_isl_sd = sd(sed_tas)) %>%
  rename(X = ID) %>%
  mutate(ID = isl$ID)

hist(SED_tas_isl_y$sed_tas_isl_mean)
hist(SED_tas_isl_y$sed_tas_isl_med)
hist(log(SED_tas_isl_y$sed_tas_isl_mean+1))
hist(log(SED_tas_isl_y$sed_tas_isl_med+1))
summary(SED_tas_isl_y)
ggplot(SED_tas_isl_y)+
  geom_point(aes(x=sed_tas_isl_mean, y = sed_tas_isl_med ))

# variable 2: precipitation amount

pr_pres <- readRDS(paste0(here::here(), "/data/derived-data/21_pr_pres_45_isl.rds"))
pr_past <- readRDS(paste0(here::here(), "/data/derived-data/21_pr_past_45_isl.rds"))

# group by year first to have the mean annual precipitation 
# and then calculate sd and mean on the grouped values
# to avoid having too high sd due to monthly variations

pr_pres_pix_y <- pr_pres %>% 
  separate(col = file, 
           into = c("name1", "name2", "name3", "month", "year", "name4"),
           sep="_") %>%
  select(-contains("name")) %>%
  mutate(month_year = paste(year, month, sep="_"))

month_year_pres <- data.frame(
  month_year = sort(unique(pr_pres_pix_y$month_year)),
  num_y = rep(1:5, each = 12)
)

pr_pres_pix_y_ok <- left_join(pr_pres_pix_y, month_year_pres) %>%
  group_by(ID, pixel, num_y) %>%
  summarize(annual_pr = mean(pr))

pr_past_pix_y <- pr_past %>% 
  separate(col = file, 
           into = c("name1", "name2", "name3", "month", "year", "name4"),
           sep="_") %>%
  select(-contains("name")) %>%
  mutate(month_year = paste(year, month, sep="_"))

month_year_past <- data.frame(
  month_year = sort(unique(pr_past_pix_y$month_year)),
  num_y = rep(1:5, each = 12)
)

pr_past_pix_y_ok <- left_join(pr_past_pix_y, month_year_past) %>%
  group_by(ID, pixel, num_y) %>%
  summarize(annual_pr = mean(pr))

pr_t1_y <- pr_past_pix_y_ok %>%
  group_by(ID, pixel)%>%
  summarize(mean_pr_t1 = mean(annual_pr),
            sd_pr_t1 = sd(annual_pr))
pr_t2_y <- pr_pres_pix_y_ok %>%
  group_by(ID, pixel)%>%
  summarize(mean_pr_t2 = mean(annual_pr))

SED_pr_y <- left_join(pr_t1_y, pr_t2_y) %>%
  mutate(sed_pr = ((mean_pr_t2-mean_pr_t1)^2)/(sd_pr_t1^2))

SED_pr_isl_y <- SED_pr_y %>%
  group_by(ID) %>%
  summarize(sed_pr_isl_med = median(sed_pr),
            sed_pr_isl_mean = mean(sed_pr),
            sed_pr_isl_sd = sd(sed_pr)) %>%
  rename(X = ID) %>%
  mutate(ID = isl$ID)


summary(SED_pr_isl_y)
ggplot(SED_pr_isl_y)+
  geom_point(aes(x=sed_pr_isl_mean, y = sed_pr_isl_med ))

hist(SED_pr_isl_y$sed_pr_isl_mean)
hist(SED_pr_isl_y$sed_pr_isl_med)
hist(log(SED_pr_isl_y$sed_pr_isl_mean))
hist(log(SED_pr_isl_y$sed_pr_isl_med))

# what's the matter with Hawaii island #1?
ggplot(pr_past_pix_y_ok %>% filter(ID==1))+
  geom_point(aes(x=num_y, y = annual_pr), position = "jitter", alpha=.5)
ggplot(pr_pres_pix_y_ok %>% filter(ID==1))+
  geom_point(aes(x=num_y, y = annual_pr), position = "jitter", alpha=.5)


ggplot(pr_past_pix_y_ok%>% filter(ID==1))+
  geom_boxplot(aes(x=as.factor(num_y), y = annual_pr))
ggplot(pr_pres_pix_y_ok%>% filter(ID==1))+
  geom_boxplot(aes(x=as.factor(num_y), y = annual_pr))




#### Combine tas and pr per pixel to obtain final SED 

# according to Williams: sum the variables and then sqrt
# mean or med by isl must be done after the transformation for each pixel

SED <- left_join(SED_tas_y, SED_pr_y) %>%
  mutate(SED_tot = sed_tas + sed_pr,
         SED_sqrt = sqrt(sed_tas + sed_pr)) %>%
  group_by(ID) %>%
  summarize(sed_pr_isl_med = median(sed_pr),
            sed_pr_isl_mean = mean(sed_pr),
            sed_pr_isl_sd = sd(sed_pr),
            sed_tas_isl_med = median(sed_tas),
            sed_tas_isl_mean = mean(sed_tas),
            sed_tas_isl_sd = sd(sed_tas),
            sed_tot_med = median(SED_tot),
            sed_tot_mean = mean(SED_tot),
            sed_tot_sd = sd(SED_tot),
            sed_sqrt_med = median(SED_sqrt),
            sed_sqrt_mean = mean(SED_sqrt),
            sed_sqrt_sd = sd(SED_sqrt)) %>%
  rename(X = ID) %>%
  mutate(ID = isl$ID)



summary(SED)
colnames(SED)

ggplot(SED)+
  geom_point(aes(x=sed_sqrt_med, y = sed_sqrt_mean))
hist(SED$sed_sqrt_med)
hist(SED$sed_tot_med)
hist(SED$sed_sqrt_mean)
ggplot(SED)+
  geom_point(aes(x=sed_tot_med, y = sed_sqrt_med))


# save file
saveRDS(SED %>% select(-X), "data/derived-data/21_CC_SED_exposure_45_isl.rds")


####### Explore variation SED #######

cc <- readRDS("data/derived-data/21_CC_SED_exposure_45_isl.rds") %>% 
  select(-contains("_sd"))

# 1. check correlation between variables

corr = cor(cc %>% select(-ID))
p.mat <- ggcorrplot::cor_pmat(cc %>% select(-ID))

ggcorrplot::ggcorrplot(corr, p.mat = p.mat, hc.order = TRUE,
                       type = "lower", insig = "blank", lab = T)

# 2. check change within archipelagos

cc_ar <- left_join(cc, isl %>% select(ID, Archip, Long, Lat))


# PR and TAS
ggplot(cc_ar)+
  geom_point(aes(x=sed_tas_isl_med, y = sed_pr_isl_med, color = Archip), size = 3)+
  geom_abline(slope=1, intercept = 0, lty=2)
cor.test(cc_ar$sed_tas_isl_med, cc_ar$sed_pr_isl_med)
# not correlated at all

ggplot(cc_ar)+
  geom_boxplot(aes(x=Archip, y = sed_tot_med), color = "firebrick")+
  geom_point(aes(x=Archip, y = sed_tot_med), color = "orange2", position = "jitter")
ggplot(cc_ar)+
  geom_boxplot(aes(x=Archip, y = sed_sqrt_med), color = "blue4")+
  geom_point(aes(x=Archip, y = sed_sqrt_med), color = "cyan3", position = "jitter")

# effect of latitude
ggplot(cc_ar)+
  geom_point(aes(x=sed_tot_med, y = abs(Lat), color = Archip))


