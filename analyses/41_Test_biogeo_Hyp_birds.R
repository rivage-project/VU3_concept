# testing hypotheses Exposure, sensitivity ~ biogeo
# for birds on major islands
# max_min normalization

rm(list = ls())

library(tidyverse)


# select a group and normalization method
taxon = "bird"
norm.method = "max_min"
vu <- readRDS(paste0("outputs/40_Vulnerability_", taxon, "_", norm.method, ".rds"))

colnames(vu)

# distribution of exposure, sensitivity & AC values
ggplot(vu)+
  geom_histogram(aes(x=Exposure01, fill = Archip), color = "white")
# two groups: all Galapagos with low exposure values
# other archip with intermediaate and high values
ggplot(vu)+
  geom_histogram(aes(x=Sensitivity01, fill = Archip), color = "white")
# two groups of sensitivity:
# - Azores & Canarias: low sensitivity
# - Mascarenes, Galapagos and Hawaii: high sensitivity

ggplot(vu)+
  geom_histogram(aes(x=AdaptCapacity01), color ="white", fill = "green3", alpha=.5) 
# most values are below 0.5, but rather continuously distributed


# distribution of VU values, depending on calculation method
ggplot(vu)+
  geom_histogram(aes(x=Vu_TOPSIS), color = "white", fill = "red3",  alpha=.5)+
  geom_histogram(aes(x=Vu_sum), color = "white", fill = "blue4", alpha=.5)+
  geom_histogram(aes(x=Vu_prod), color ="white", fill = "green3", alpha=.5) 
  
# product of components: most Exposure values are low
# sum and TOPSIS have different ranges but similar homogeneous distribution


##### Archipelago effect ####

# VU by archip
ggplot(vu)+
  geom_boxplot(aes(x=Archip, y = Vu_sum))+
  geom_point(aes(x=Archip, y = Vu_sum, size = Area, color = Archip), 
             position = 'jitter', alpha = .5)+
  theme_classic()

av <- aov(Vu_sum~Archip, vu)
#plot(av)
anova(av)
# significant effect of Archip on final VU value
TukeyHSD(av) 
# Hawaii >> Az, Can, Gal
# Masc > Can
# other 2-by-2 groups are non signif


# Exposure by archip
ggplot(vu)+
  geom_boxplot(aes(x=Archip, y = Exposure01))+
  geom_point(aes(x=Archip, y = Exposure01, size = Area, color = Archip), 
             position = 'jitter', alpha = .5)+
  theme_classic()

ae <- aov(Exposure01~Archip, vu)
#plot(ae)
anova(ae)
# significant effect of Archip on exposure
TukeyHSD(ae) 
# Gal << Hawaii, Masc, Can, Az
# all other archip have the same mean 

# sensitivity by archip
ggplot(vu)+
  geom_boxplot(aes(x=Archip, y = Sensitivity01))+
  geom_point(aes(x=Archip, y = Sensitivity01, size = Area, color = Archip), 
             position = 'jitter', alpha = .5)+
  theme_classic()

as <- aov(Sensitivity01~Archip, vu)
anova(as) # signif
TukeyHSD(as) 
# Az, Can << Masc, Hawaii, Gal

# AC by archip
ggplot(vu)+
  geom_boxplot(aes(x=Archip, y = AdaptCapacity01))+
  geom_point(aes(x=Archip, y = AdaptCapacity01, size = Area, color = Archip), 
             position = 'jitter', alpha = .5)+
  theme_classic()

aa  <- aov(AdaptCapacity01~Archip, vu)
anova(aa) # no archipelago effect


##### Relationship E, S, AC, VU ~ area ####

#---------- Exposure 

ggplot(vu)+
  geom_point(aes(x = log(Area), y = Exposure01, color = Archip))+
  geom_smooth(aes(x = log(Area), y = Exposure01), method = "lm")+
  theme_classic()
# differences between archipelagos, one lm per Archip
ggplot(vu)+
  geom_point(aes(x = log(Area), y = Exposure01, color = Archip))+
  geom_smooth(aes(x = log(Area), y = Exposure01, color = Archip),
              method = "lm")+
  theme_classic()
# Total E is higher for bigger islands, but E is Archip dependant

#---------- Sensitivity 

ggplot(vu)+
  geom_point(aes(x = log(Area), y = Sensitivity01, color = Archip))
# no obvious relationship with Area when all archip are pooled
ggplot(vu)+
  geom_point(aes(x = log(Area), y = Sensitivity01, color = Archip))+
  geom_smooth(aes(x = log(Area), y = Sensitivity01, color = Archip),
              method = "lm")
# But a slight decrease per archipelago

#---------- Adaptive Capacity 

ggplot(vu)+
  geom_point(aes(x = log(Area), y = AdaptCapacity01, color = Archip))
# no obvious pattern, even by archipelago

#---------- Final VU

ggplot(vu)+
  geom_point(aes(x = log(Area), y = Vu_sum, color = Archip))+
  theme_classic()
mod <- lm(Vu_sum~log(Area), vu)
plot(mod)
summary(mod)
# slightly significant, but poor R² (0.13), p = 0.02


##### relationship E, S, AC, VU ~ Dist ####

#---------- Exposure 

ggplot(vu)+
  geom_point(aes(x = Dist, y = Exposure01, color = Archip))+
  geom_smooth(aes(x = Dist, y = Exposure01), method = "lm")+
  theme_classic()
mod <- lm(Exposure01~Dist, vu)
summary(mod)
# Total E is higher for more distant islands
# but only small R² , pval = 0.1
# => mostly explained by the positive IAS - Dist relationship?

#---------- Sensitivity 

ggplot(vu)+
  geom_point(aes(x = Dist, y = Sensitivity01, color = Archip))+
  geom_smooth(aes(x = Dist, y = Sensitivity01), method = "lm")+
  theme_classic()
mod <- lm(Sensitivity01~Dist, vu)
plot(mod)
summary(mod)
# And S is higher for more distant islands
# but only small R² , pval = 0.002

#---------- Adaptive Capacity 

ggplot(vu)+
  geom_point(aes(x = Dist, y = AdaptCapacity01, color = Archip))+
  theme_classic()
mod <- lm(AdaptCapacity01~Dist, vu)
plot(mod)
summary(mod)
# no significant relationship

#---------- Final Vu

ggplot(vu)+
  geom_point(aes(x = Dist, y = Vu_sum, color = Archip))+
  theme_classic()
mod <- lm(Vu_sum~Dist, vu)
plot(mod)
summary(mod)
# relationship with the best R² = 0.56
# positive relationship between final VU and Distance to mainland
# VU is higher for islands far from the mainland, p < 0.0001


##### relationship E, S, AC, VU ~ SLMP ####

#---------- Exposure 

ggplot(vu)+
  geom_point(aes(x = SLMP, y = Exposure01, color = Archip))+
  theme_classic()
mod <- lm(Exposure01~SLMP, vu)
summary(mod) # not signif

#---------- Sensitivity 

ggplot(vu)+
  geom_point(aes(x = SLMP, y = Sensitivity01, color = Archip))+
  geom_smooth(aes(x = SLMP, y = Sensitivity01), method = "lm")+
  theme_classic()
mod <- lm(Sensitivity01~SLMP, vu)
plot(mod)
summary(mod)
# S is lower for islands with higher surrounding landmass
# high R² = 0.65, p < 0.0001

#---------- Adaptive Capacity 

ggplot(vu)+
  geom_point(aes(x = SLMP, y = AdaptCapacity01, color = Archip))+
  theme_classic()
mod <- lm(AdaptCapacity01~SLMP, vu)
summary(mod) # no significant relationship

#---------- Final Vu

ggplot(vu)+
  geom_point(aes(x = SLMP, y = Vu_sum, color = Archip))+
  geom_smooth(aes(x = SLMP, y = Vu_sum), method = "lm")+
  theme_classic()
# all pooled
mod <- lm(Vu_sum~SLMP, vu)
summary(mod)
# slightly signif, R² = 0.15, p = 0.01
# VU is lower for islands with more surrounding landmass
# but relationship for Canaries is different
# VU higher for high SLMP
# try other relationship than linear?


##### relationship E, S, AC, VU ~ Elevation ####

#---------- Exposure 

ggplot(vu)+
  geom_point(aes(x = Elev, y = Exposure01, color = Archip))+
  geom_smooth(aes(x = Elev, y = Exposure01), method = "lm")+
  theme_classic()
mod <- lm(Exposure01~Elev, vu)
summary(mod) 
# E is higher for elevated islands
# opposed to what we expected in the proposal
# R² = 0.23, p= 0.002


#---------- Sensitivity 

ggplot(vu)+
  geom_point(aes(x = Elev, y = Sensitivity01, color = Archip))+
  theme_classic() # no relationship
mod <- lm(Sensitivity01~Elev, vu)
summary(mod) # no significant linear relationship

#---------- Adaptive Capacity 

ggplot(vu)+
  geom_point(aes(x = Elev, y = AdaptCapacity01, color = Archip))+
  theme_classic() # positive linear relationship
mod <- lm(AdaptCapacity01~Elev, vu)
summary(mod) 
# as expected, positive relationship, bc Elev is an AC components

#---------- Final Vu

ggplot(vu)+
  geom_point(aes(x = Elev, y = Vu_sum, color = Archip))+
  theme_classic() # apparently nothing
mod <- lm(Vu_sum~Elev, vu)
summary(mod)# no signif linear relationship


##### E, S, AC, VU ~ Species richness ####

#--------- SR, Area and Exposure
ggplot(vu)+
  geom_point(aes(x = log(Area), y = SR, color = Archip))
# classic positive relationship

# thus positive correlation between exposure and SR
ggplot(vu)+
  geom_point(aes(x = Exposure01, y = SR, color = Archip))

#---------- Sensitivity

ggplot(vu)+
  geom_point(aes(x = SR, y = Sensitivity01, color = Archip))
# no obvious relationship when all archip are pooled
ggplot(vu)+
  geom_point(aes(x = SR, y = Sensitivity01, color = Archip))+
  geom_smooth(aes(x = SR, y = Sensitivity01, color = Archip),
              method = "lm")
# Sensitivity decreases with SR for Azores

#---------- Adaptive Capacity 

ggplot(vu)+
  geom_point(aes(x = SR, y = AdaptCapacity01, color = Archip))+
  geom_smooth(aes(x = SR, y = Sensitivity01, color = Archip),
              method = "lm")
# AC decreases with SR for Azores, but overall no obvious pattern

#----------- Final VU 

ggplot(vu)+
  geom_point(aes(x = SR, y = Vu_sum))
mod = lm(Vu_sum~SR, data = vu)
plot(mod)
summary(mod)
# no signif relationship between SR and VU, all islands pooled

# But a significant relationship when considering archipelagos 
ggplot(vu)+
  geom_point(aes(x = SR, y = Vu_sum, color = Archip))+
  geom_smooth(aes(x = SR, y = Vu_sum, color = Archip),
              method = "lm")

ggplot(vu)+
  geom_smooth(aes(x = SR, y = Vu_sum, color = Archip),
              method = "lm")







