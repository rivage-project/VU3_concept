
rm(list = ls())

library(tidyverse)

# load sensitivity files
sensit_mam <- readRDS("data/derived-data/13_Sensitivity_mam_major_isl.rds")
sensit_bird <- readRDS("data/derived-data/13_Sensitivity_bird_major_isl.rds")
sensit_plant


# visualise contrib to sensitivity

# correlations between components

# mammals with max-min
corr = cor(sensit_mam[["sm_max_min"]] %>% select(-ID))
p.mat <- ggcorrplot::cor_pmat(sensit_mam[["sm_max_min"]] %>% select(-ID))

ggcorrplot::ggcorrplot(corr, p.mat = p.mat, hc.order = TRUE,
                       type = "lower", insig = "blank", lab = T)

# birds with max-min
corr = cor(sensit_bird[["sb_max_min"]] %>% select(-ID))
p.mat <- ggcorrplot::cor_pmat(sensit_bird[["sb_max_min"]] %>% select(-ID))

ggcorrplot::ggcorrplot(corr, p.mat = p.mat, hc.order = TRUE,
                       type = "lower", insig = "blank", lab = T)



# bind all groups in one df
sb <- lapply(sensit_bird, function(x){
  x %>% rename(sens_bird = sens) %>%
    select(ID, sens_bird, SR_bird)
}) 
sm <- lapply(sensit_mam, function(x){
  x %>% rename(sens_mam = sens) %>%
    select(ID, sens_mam, SR_mam)
})

s_all <- sb
for (i in 1:3){
  s_all[[i]] <- left_join(sm[[i]], sb[[i]])
}

names(s_all) <- c("s_max_min","s_log","s_rank")

saveRDS(s_all, "data/derived-data/15_Sensitivity_BMP_major_isl.rds")

# sensib of mammals and birds: what differences?

ggplot(s_all[["s_max_min"]])+
  geom_point(aes(x=sens_mam, y = sens_bird))
