# Calculate Probability of extinction associated with IAS threat

rm(list=ls())
library(tidyverse)


# load threat information

threat <- openxlsx::read.xlsx("Data/Species_threat_IAS_terr_vert.xlsx") 

#### GE2 = propability of extinction due to IAS threat ####

# following the method from Gumbs et al. 2023
# https://doi.org/10.1371/journal.pbio.3001991
# for calculating 1000 extinction probability per species
# according to thair iUCN category and the severity of IAS threat

table(threat$cate_p_ext, threat$ias_8.1, threat$AS)

# select only species associated with IAS (threat 8.1 in IUCN)
threat_ias <- threat %>% filter(ias_8.1 ==1) %>%
  select(-c(orderName, cate_threat, ias_8.1)) %>%
  mutate_if(is.factor, as.character) %>%
  filter(cate_p_ext != "EX") # remove extinct species
table(threat_ias$cate_p_ext, threat_ias$AS, threat_ias$className)


# create a dataframe containing the median of each IUCN category
pCR <- 0.97
df <- data.frame(rank = c(1:20000),
                 p = rep(c(pCR/16, pCR/8, pCR/4, pCR/2, pCR), each = 4000))

#plot(df$rank, df$p)

fit4 <- lm(p ~ poly(rank,4,raw=TRUE), data=df)
#summary(fit4)
#plot(df$rank, fit4$fitted.values)
range(fit4$fitted.values)

df$fit_val <- fit4$fitted.values

# rescale between 0 and 1
rescale_fit <- function(x){
  (x-min(fit4$fitted.values))/
    (max(fit4$fitted.values) - min(fit4$fitted.values))}

df <- df %>% mutate(fit_val_scale = rescale_fit(fit_val))

plot(df$rank, df$fit_val_scale)
range(df$fit_val_scale)

df$cat <- rep(c("LC","NT","VU","EN","CR"), each = 4000)
df$sev <- rep(c("0","1"), times = 5, each =2000)

df %>% group_by(cat, sev) %>% summarise(
  med_raw = median(p),
  # med_fit = median(fit_val),
  med_fit_scale = median(fit_val_scale))

# remove p=0 and p=1
df_ok <- df[c(2:(nrow(df)-1)), ]
range(df_ok$fit_val_scale)


# extract randomly 1000 probabilities for each species
# within the range of values of its group (combination category-severity)

set.seed(456) # for having the same randomness

plist <- list()

for (i in 1:nrow(threat_ias)){
  
  class = threat_ias$className[i]
  sp = threat_ias$binomial[i]
  cate = threat_ias$cate_p_ext[i]
  AS = threat_ias$AS[i]
  
  if (cate == "DD"){ # takes p within all distribution [0-1]
    p_to_samp <- df_ok %>% 
      pull(fit_val_scale)
  } else { # filter for the good IUCN category
    df_temp <- df_ok %>%
      filter(cat == cate)
    if (AS == "DD"){ # takes p within the whole IUCN category
      p_to_samp <- df_temp %>% 
        pull(fit_val_scale)
    } else { # separate according to severity (upper or lower part of IUN category)
      p_to_samp <- df_temp %>%
        filter(sev==AS) %>%
        pull(fit_val_scale)
    }
  }
  p1000 <- sample(p_to_samp, 1000)
  
  plist[[i]] <- data.frame(
    className = class,
    binomial = sp,
    cate_p_ext = cate,
    severity = AS,
    p = p1000
  )
}

# run for all sp ?
length(plist)==length(threat_ias$binomial)
# bind all df together (big df)
p_all <- bind_rows(plist)

# separate for each class (for fusias final)
# define a list with a table for each taxon
GE2_list = list(
  amph = p_all %>% filter(className=="AMPHIBIA"),
  bird = p_all %>% filter(className=="AVES"),
  mam = p_all %>% filter(className=="MAMMALIA"),
  rept = p_all %>% filter(className=="REPTILIA")
)


saveRDS(GE2_list, "Output/01_Proba_extinction_IAS_ABMR.rds")


#### Plot for Fig. 1 in Methods ####


df_meth <- left_join(
  df_ok,
  left_join(df_ok %>% group_by(cat, sev) %>% summarise(
    med_raw = median(p),
    med_fit = median(fit_val),
    med_fit_scale = median(fit_val_scale)),
    df_ok %>% group_by(cat) %>% summarise(
      cat_group_med = median(fit_val_scale)))
)

seg_dat <- round(unique(df_meth$cat_group_med), 2)

png("Fig/01_proba_extinction_IAS_method.png")

ggplot(df_meth) + 
  geom_bar(aes(x=rank, y=med_fit_scale, fill = cat, alpha = sev), 
           width = 1,
           stat = "identity")+
  scale_alpha_manual( values = c("0" = 0.5, "1"= 1))+
  # colors according to IUCN categories
  scale_fill_manual(
    values = c("LC"="mediumseagreen", "NT"="olivedrab2", 
               "VU"="gold1", "EN"="darkorange2", "CR"="firebrick2"))+
  theme_classic()+
  xlab("IAS-threat groups")+ ylab("Probability of extinction (p)")+
  # clean axis values for showing extinction rates per cate
  scale_x_continuous(breaks=NULL)+
  scale_y_continuous(breaks = seg_dat)+
  # add dashed lines for median values of extinction probability
  geom_segment(aes(x = 0, y = seg_dat[1], xend = 2000, yend = seg_dat[1]),
               color = "mediumseagreen", linetype = 2)+
  geom_segment(aes(x = 0, y = seg_dat[2], xend = 6000, yend = seg_dat[2]),
               color = "olivedrab2", linetype = 2)+
  geom_segment(aes(x = 0, y = seg_dat[3], xend = 10000, yend = seg_dat[3]),
               color = "gold1", linetype = 2)+
  geom_segment(aes(x = 0, y = seg_dat[4], xend = 14000, yend = seg_dat[4]),
               color = "darkorange2", linetype = 2)+
  geom_segment(aes(x = 0, y = seg_dat[5], xend = 18000, yend = seg_dat[5]),
               color = "firebrick2", linetype = 2)+
  # add curve for quartic distribution
  geom_line(aes(x=rank, y=fit_val_scale), linewidth = 1, color = "midnightblue")+
  theme(text=element_text(size=15))


dev.off()
