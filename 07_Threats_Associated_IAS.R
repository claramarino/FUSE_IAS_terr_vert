################################################
################### FUSE IAS ###################
################################################

library(dplyr)
library(ggplot2)
library(tidyverse)
library(Cairo)
library(gridExtra)

## 1. Extract all datasets
## 2. Prepare databases
## 3. No. and identity of associated IAS species 
## 4. No. and identity of other threats (besides IAS)
## 5. Conservation actions in place for CORE species
## 6. Analyse TOP50 birds



### 1. Extract all datasets 
core_list <- readRDS("C:/Users/fmacedocoutinhodeoli/OneDrive - MNHN/Outros/FUSE-IAS/threat_cons_summary/05_FUSIAS_core_list_all_groups")
core_list_birds <- data.frame(core_list$bird)
core_list_amphibians <- data.frame(core_list$amph)
core_list_mammals <- data.frame(core_list$mam)
core_list_reptiles <- data.frame(core_list$rept)

watch_list <- readRDS("C:/Users/fmacedocoutinhodeoli/OneDrive - MNHN/Outros/FUSE-IAS/threat_cons_summary/05_FUSIAS_watch_list_all_groups")
watch_list_birds <- data.frame(watch_list$bird)
watch_list_amphibians <- data.frame(watch_list$amph)
watch_list_mammals <- data.frame(watch_list$mam)
watch_list_reptiles <- data.frame(watch_list$rept)

border_list <- readRDS("C:/Users/fmacedocoutinhodeoli/OneDrive - MNHN/Outros/FUSE-IAS/threat_cons_summary/05_FUSIAS_borderline_list_all_groups")
border_list_birds <- data.frame(border_list$bird)
border_list_amphibians <- data.frame(border_list$amph)
border_list_mammals <- data.frame(border_list$mam)
border_list_reptiles <- data.frame(border_list$rept)


### 2. Prepare databases
birds_nonpass_threats <- read.csv("C:/Users/fmacedocoutinhodeoli/OneDrive - MNHN/Outros/FUSE-IAS/threat_cons_summary/birds_non_passerif/threats.csv")
birds_pass_threats <- read.csv("C:/Users/fmacedocoutinhodeoli/OneDrive - MNHN/Outros/FUSE-IAS/threat_cons_summary/birds_passerif/threats.csv")

amph_mam_rept_threats <- read.csv("C:/Users/fmacedocoutinhodeoli/OneDrive - MNHN/Outros/FUSE-IAS/threat_cons_summary/amph_mam_rept/threats.csv")

birds_nonpass_conservation <- read.csv("C:/Users/fmacedocoutinhodeoli/OneDrive - MNHN/Outros/FUSE-IAS/threat_cons_summary/birds_non_passerif/conservation_needed.csv")
birds_pass_conservation <- read.csv("C:/Users/fmacedocoutinhodeoli/OneDrive - MNHN/Outros/FUSE-IAS/threat_cons_summary/birds_passerif/conservation_needed.csv")

amph_mam_rept_conservation <- read.csv("C:/Users/fmacedocoutinhodeoli/OneDrive - MNHN/Outros/FUSE-IAS/threat_cons_summary/amph_mam_rept/conservation_needed.csv")


## Check if column names are the same and join databases if yes
colnames(birds_nonpass_threats)
colnames(birds_pass_threats)
birds_threats <- rbind(birds_nonpass_threats, birds_pass_threats)

colnames(birds_nonpass_conservation)
colnames(birds_pass_conservation)
birds_conservation <- rbind(birds_nonpass_conservation, birds_pass_conservation)


### 3. No. and identity of IAS species associated with CORE species
core_birds_threats <- merge(core_list_birds, birds_threats, by.x = "binomial", by.y = "scientificName", all.x = TRUE)
table(core_birds_threats$code, useNA = "always") # we have 9 species that don't have any threat <NA>
core_mammals_threats <- merge(core_list_mammals, amph_mam_rept_threats, by.x = "binomial", by.y = "scientificName", all.x = TRUE)
table(core_mammals_threats$code, useNA = "always") # we don't species that don't have any threat <NA>
core_amphibians_threats <- merge(core_list_amphibians, amph_mam_rept_threats, by.x = "binomial", by.y = "scientificName", all.x = TRUE)
table(core_amphibians_threats$code, useNA = "always") # we have 1 species that don't have any threat <NA>
core_reptiles_threats <- merge(core_list_reptiles, amph_mam_rept_threats, by.x = "binomial", by.y = "scientificName", all.x = TRUE)
table(core_reptiles_threats$code, useNA = "always") # we don't species that don't have any threat <NA>

watch_birds_threats <- merge(watch_list_birds, birds_threats, by.x = "binomial", by.y = "scientificName", all.x = TRUE)
table(watch_birds_threats$code, useNA = "always") # we don't have species that don't have any threat <NA>
watch_mammals_threats <- merge(watch_list_mammals, amph_mam_rept_threats, by.x = "binomial", by.y = "scientificName", all.x = TRUE)
table(watch_mammals_threats$code, useNA = "always") # we don't species that don't have any threat <NA>
watch_amphibians_threats <- merge(watch_list_amphibians, amph_mam_rept_threats, by.x = "binomial", by.y = "scientificName", all.x = TRUE)
table(watch_amphibians_threats$code, useNA = "always") # we don't species that don't have any threat <NA>
watch_reptiles_threats <- merge(watch_list_reptiles, amph_mam_rept_threats, by.x = "binomial", by.y = "scientificName", all.x = TRUE)
table(watch_reptiles_threats$code, useNA = "always") # we don't species that don't have any threat <NA>

border_birds_threats <- merge(border_list_birds, birds_threats, by.x = "binomial", by.y = "scientificName", all.x = TRUE)
table(border_birds_threats$code, useNA = "always") # we don't species that don't have any threat <NA>
border_mammals_threats <- merge(border_list_mammals, amph_mam_rept_threats, by.x = "binomial", by.y = "scientificName", all.x = TRUE)
table(border_mammals_threats$code, useNA = "always") # we don't species that don't have any threat <NA>
border_amphibians_threats <- merge(border_list_amphibians, amph_mam_rept_threats, by.x = "binomial", by.y = "scientificName", all.x = TRUE)
table(border_amphibians_threats$code, useNA = "always") # we have 1 species that don't have any threat <NA>
border_reptiles_threats <- merge(border_list_reptiles, amph_mam_rept_threats, by.x = "binomial", by.y = "scientificName", all.x = TRUE)
table(border_reptiles_threats$code, useNA = "always") # we don't species that don't have any threat <NA>



## Extract only the higher level of the threat classification scheme
core_birds_threats$threat <- sub("\\..*", "", core_birds_threats$code)
core_birds_threats <- core_birds_threats[!is.na(core_birds_threats$threat),] # remove 9 birds without threats
n_distinct(core_birds_threats$binomial) # 334

core_mammals_threats$threat <- sub("\\..*", "", core_mammals_threats$code)
core_mammals_threats <- core_mammals_threats[!is.na(core_mammals_threats$threat),] 
n_distinct(core_mammals_threats$binomial) # 152

core_amphibians_threats$threat <- sub("\\..*", "", core_amphibians_threats$code)
core_amphibians_threats <- core_amphibians_threats[!is.na(core_amphibians_threats$threat),] # remove 1 spp without threats
n_distinct(core_amphibians_threats$binomial) # 683 species

core_reptiles_threats$threat <- sub("\\..*", "", core_reptiles_threats$code)
core_reptiles_threats <- core_reptiles_threats[!is.na(core_reptiles_threats$threat),] 
n_distinct(core_reptiles_threats$binomial) # 199

watch_birds_threats$threat <- sub("\\..*", "", watch_birds_threats$code)
n_distinct(watch_birds_threats$binomial) # 19

watch_mammals_threats$threat <- sub("\\..*", "", watch_mammals_threats$code)
n_distinct(watch_mammals_threats$binomial) # 22

watch_amphibians_threats$threat <- sub("\\..*", "", watch_amphibians_threats$code)
n_distinct(watch_amphibians_threats$binomial) # 32

watch_reptiles_threats$threat <- sub("\\..*", "", watch_reptiles_threats$code)
n_distinct(watch_reptiles_threats$binomial) # 6

border_birds_threats$threat <- sub("\\..*", "", border_birds_threats$code)
n_distinct(border_birds_threats$binomial) # 9

border_mammals_threats$threat <- sub("\\..*", "", border_mammals_threats$code)
n_distinct(border_mammals_threats$binomial) # 6

border_amphibians_threats$threat <- sub("\\..*", "", border_amphibians_threats$code)
n_distinct(border_amphibians_threats$binomial) # 12

border_reptiles_threats$threat <- sub("\\..*", "", border_reptiles_threats$code)
n_distinct(border_reptiles_threats$binomial) # 16


## Identify how many species are only threatened by IAS

core_birds_threats2 <- core_birds_threats %>% group_by(binomial, threat) %>% summarise(count = n())
core_birds_threats3 <- core_birds_threats2 %>% group_by(binomial) %>% filter(n() == 1)
table(core_birds_threats3$threat) # 19 species are only threatened by IAS
View(core_birds_threats %>% filter(binomial == "Apteryx australis"))

core_mammals_threats2 <- core_mammals_threats %>% group_by(binomial, threat) %>% summarise(count = n())
core_mammals_threats3 <- core_mammals_threats2 %>% group_by(binomial) %>% filter(n() == 1)
table(core_mammals_threats3$threat) # 8 species are only threatened by IAS

core_reptiles_threats2 <- core_reptiles_threats %>% group_by(binomial, threat) %>% summarise(count = n())
core_reptiles_threats3 <- core_reptiles_threats2 %>% group_by(binomial) %>% filter(n() == 1)
table(core_reptiles_threats3$threat) # 26 species are only threatened by IAS

core_amphibians_threats2 <- core_amphibians_threats %>% group_by(binomial, threat) %>% summarise(count = n())
core_amphibians_threats3 <- core_amphibians_threats2 %>% group_by(binomial) %>% filter(n() == 1)
table(core_amphibians_threats3$threat) # 11 species are only threatened by IAS

core_birds_threats3$list <- "core"
core_birds_threats3$taxon <- "birds"
core_mammals_threats3$list <- "core"
core_mammals_threats3$taxon <- "mammals"
core_reptiles_threats3$list <- "core"
core_reptiles_threats3$taxon <- "reptiles"
core_amphibians_threats3$list <- "core"
core_amphibians_threats3$taxon <- "amphibians"
core_threats_onlyIAS <- rbind(core_birds_threats3,core_mammals_threats3,core_reptiles_threats3,core_amphibians_threats3)

watch_birds_threats2 <- watch_birds_threats %>% group_by(binomial, threat) %>% summarise(count = n())
watch_birds_threats3 <- watch_birds_threats2 %>% group_by(binomial) %>% filter(n() == 1)
table(watch_birds_threats3$threat) # 3 species are only threatened by IAS

watch_mammals_threats2 <- watch_mammals_threats %>% group_by(binomial, threat) %>% summarise(count = n())
watch_mammals_threats3 <- watch_mammals_threats2 %>% group_by(binomial) %>% filter(n() == 1)
table(watch_mammals_threats3$threat) # 1 species are only threatened by IAS

watch_reptiles_threats2 <- watch_reptiles_threats %>% group_by(binomial, threat) %>% summarise(count = n())
watch_reptiles_threats3 <- watch_reptiles_threats2 %>% group_by(binomial) %>% filter(n() == 1)
table(watch_reptiles_threats3$threat) # 4 species are only threatened by IAS

watch_amphibians_threats2 <- watch_amphibians_threats %>% group_by(binomial, threat) %>% summarise(count = n())
watch_amphibians_threats3 <- watch_amphibians_threats2 %>% group_by(binomial) %>% filter(n() == 1)
table(watch_amphibians_threats3$threat) # 1 species are only threatened by IAS

watch_birds_threats3$list <- "watch"
watch_birds_threats3$taxon <- "birds"
watch_mammals_threats3$list <- "watch"
watch_mammals_threats3$taxon <- "mammals"
watch_reptiles_threats3$list <- "watch"
watch_reptiles_threats3$taxon <- "reptiles"
watch_amphibians_threats3$list <- "watch"
watch_amphibians_threats3$taxon <- "amphibians"
watch_threats_onlyIAS <- rbind(watch_birds_threats3,watch_mammals_threats3,watch_reptiles_threats3,watch_amphibians_threats3)

border_birds_threats2 <- border_birds_threats %>% group_by(binomial, threat) %>% summarise(count = n())
border_birds_threats3 <- border_birds_threats2 %>% group_by(binomial) %>% filter(n() == 1)
table(border_birds_threats3$threat) # 3 species are only threatened by IAS

border_mammals_threats2 <- border_mammals_threats %>% group_by(binomial, threat) %>% summarise(count = n())
border_mammals_threats3 <- border_mammals_threats2 %>% group_by(binomial) %>% filter(n() == 1)
table(border_mammals_threats3$threat) # no species are only threatened by IAS

border_reptiles_threats2 <- border_reptiles_threats %>% group_by(binomial, threat) %>% summarise(count = n())
border_reptiles_threats3 <- border_reptiles_threats2 %>% group_by(binomial) %>% filter(n() == 1)
table(border_reptiles_threats3$threat) # 2 species are only threatened by IAS

border_amphibians_threats2 <- border_amphibians_threats %>% group_by(binomial, threat) %>% summarise(count = n())
border_amphibians_threats3 <- border_amphibians_threats2 %>% group_by(binomial) %>% filter(n() == 1)
table(border_amphibians_threats3$threat) # no species are only threatened by IAS

border_birds_threats3$list <- "border"
border_birds_threats3$taxon <- "birds"
border_reptiles_threats3$list <- "border"
border_reptiles_threats3$taxon <- "reptiles"
border_threats_onlyIAS <- rbind(border_birds_threats3,border_reptiles_threats3)


threats_onlyIAS <- rbind(core_threats_onlyIAS,watch_threats_onlyIAS,border_threats_onlyIAS)
write.csv(threats_onlyIAS, "C:/Users/fmacedocoutinhodeoli/OneDrive - MNHN/Outros/FUSE-IAS/threats_onlyIAS.csv", row.names = FALSE)
view(threats_onlyIAS)


## Filter to only have IAS threat - category 8
core_birds_IASthreat <- core_birds_threats[core_birds_threats$threat %in% c("8"), ]  
# the no. rows should be equal to the no. obs in threat = 8
table(core_birds_threats$threat, useNA = "always") 
nrow(core_birds_IASthreat) # 598
n_distinct(core_birds_IASthreat$binomial) # 334 species

core_mammals_IASthreat <- core_mammals_threats[core_mammals_threats$threat %in% c("8"), ]  
# the no. rows should be equal to the no. obs in threat = 8
table(core_mammals_threats$threat, useNA = "always") 
nrow(core_mammals_IASthreat) # 224
n_distinct(core_mammals_IASthreat$binomial) # 152

core_reptiles_IASthreat <- core_reptiles_threats[core_reptiles_threats$threat %in% c("8"), ]  
# the no. rows should be equal to the no. obs in threat = 8
table(core_reptiles_threats$threat, useNA = "always") 
nrow(core_reptiles_IASthreat) # 235
n_distinct(core_reptiles_IASthreat$binomial) # 199

core_amphibians_IASthreat <- core_amphibians_threats[core_amphibians_threats$threat %in% c("8"), ]  
# the no. rows should be equal to the no. obs in threat = 8
table(core_amphibians_threats$threat, useNA = "always") 
nrow(core_amphibians_IASthreat) # 808
n_distinct(core_amphibians_IASthreat$binomial) # 682 species; 1 species was removed because it is not threatened by IAS

watch_birds_IASthreat <- watch_birds_threats[watch_birds_threats$threat %in% c("8"), ]  
# the no. rows should be equal to the no. obs in threat = 8
table(watch_birds_threats$threat, useNA = "always") 
nrow(watch_birds_IASthreat) # 32
n_distinct(watch_birds_IASthreat$binomial) # 19 species

watch_mammals_IASthreat <- watch_mammals_threats[watch_mammals_threats$threat %in% c("8"), ]  
# the no. rows should be equal to the no. obs in threat = 8
table(watch_mammals_threats$threat, useNA = "always") 
nrow(watch_mammals_IASthreat) # 43
n_distinct(watch_mammals_IASthreat$binomial) # 22

watch_reptiles_IASthreat <- watch_reptiles_threats[watch_reptiles_threats$threat %in% c("8"), ]  
# the no. rows should be equal to the no. obs in threat = 8
table(watch_reptiles_threats$threat, useNA = "always") 
nrow(watch_reptiles_IASthreat) # 7
n_distinct(watch_reptiles_IASthreat$binomial) # 6

watch_amphibians_IASthreat <- watch_amphibians_threats[watch_amphibians_threats$threat %in% c("8"), ]  
# the no. rows should be equal to the no. obs in threat = 8
table(watch_amphibians_threats$threat, useNA = "always") 
nrow(watch_amphibians_IASthreat) # 40
n_distinct(watch_amphibians_IASthreat$binomial) # 31 species

border_birds_IASthreat <- border_birds_threats[border_birds_threats$threat %in% c("8"), ]  
# the no. rows should be equal to the no. obs in threat = 8
table(border_birds_threats$threat, useNA = "always") 
nrow(border_birds_IASthreat) # 13
n_distinct(border_birds_IASthreat$binomial) # 9 species

border_mammals_IASthreat <- border_mammals_threats[border_mammals_threats$threat %in% c("8"), ]  
# the no. rows should be equal to the no. obs in threat = 8
table(border_mammals_threats$threat, useNA = "always") 
nrow(border_mammals_IASthreat) # 10
n_distinct(border_mammals_IASthreat$binomial) # 6

border_reptiles_IASthreat <- border_reptiles_threats[border_reptiles_threats$threat %in% c("8"), ]  
# the no. rows should be equal to the no. obs in threat = 8
table(border_reptiles_threats$threat, useNA = "always") 
nrow(border_reptiles_IASthreat) # 22
n_distinct(border_reptiles_IASthreat$binomial) # 16

border_amphibians_IASthreat <- border_amphibians_threats[border_amphibians_threats$threat %in% c("8"), ]  
# the no. rows should be equal to the no. obs in threat = 8
table(border_amphibians_threats$threat, useNA = "always") 
nrow(border_amphibians_IASthreat) # 14
n_distinct(border_amphibians_IASthreat$binomial) # 12 species


## Count no. IAS of each species


table(core_birds_IASthreat$name, useNA = "always") # 491 with identified IAS 
core_birds_IASthreat_named <- core_birds_IASthreat %>% filter(name == "Named species")
core_birds_IASthreat_unspecified <- core_birds_IASthreat %>% filter(name == "Unspecified species")
# 35 that don't have species name, so we need to move them to unspecified
table(core_birds_IASthreat_named$ias, useNA = "always") 
core_birds_IASthreat_blank <- core_birds_IASthreat_named %>% filter(!grepl("[A-Za-z]", ias))
core_birds_IASthreat_unspecified <- rbind(core_birds_IASthreat_unspecified, core_birds_IASthreat_blank)
# but we still need to remove them from named
core_birds_IASthreat_named <- core_birds_IASthreat_named %>% filter(grepl("[A-Za-z]", ias)) 

# there is one that is wrongly categorized as unspecified because it has species name, so we need to move it to named
table(core_birds_IASthreat_unspecified$ias) 
core_birds_IASthreat_unspecified <- core_birds_IASthreat_unspecified %>% filter(ias != "Felis catus") # remove it from unspecified
core_birds_IASthreat_named2 <- core_birds_IASthreat_unspecified %>% filter(ias == "Felis catus")
core_birds_IASthreat_named <- rbind(core_birds_IASthreat_named2, core_birds_IASthreat_named)

# count the no. of times each IAS occurs for each species
core_birds_sumIASthreats_named <- core_birds_IASthreat_named %>% count(binomial,ias)
# now count the no. of IAS for each species
core_birds_sumIASthreat_named <- core_birds_sumIASthreats_named %>% count(binomial)
names(core_birds_sumIASthreat_named)[names(core_birds_sumIASthreat_named) == "n"] <- "NoIAS"
# Histogram no. species x no. IAS 
n_distinct(core_birds_sumIASthreat_named$binomial) # 316 species because we are only looking at those that are named
core_birds_IASthreat_hist <- ggplot(data = core_birds_sumIASthreat_named, aes(x = NoIAS)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) + 
  labs(title = "Core birds - IAS identified", x = "No. IAS", y = "No. bird species") + 
  coord_flip()

core_birds_sumIASthreat_unspecified <- core_birds_IASthreat_unspecified %>% count(binomial)
names(core_birds_sumIASthreat_unspecified)[names(core_birds_sumIASthreat_unspecified) == "n"] <- "NoIAS"
core_birds_IASthreat_hist2 <- ggplot(data = core_birds_sumIASthreat_unspecified, aes(x = NoIAS)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) + 
  labs(title = "Core birds - IAS unspecified", x = "No. IAS", y = "No. bird species") + 
  coord_flip() # 800*600
layout_matrix <- matrix(c(1, 2), ncol = 2, byrow = TRUE)
grid.arrange(core_birds_IASthreat_hist,core_birds_IASthreat_hist2,layout_matrix = layout_matrix) # 1000*700


table(core_mammals_IASthreat$name, useNA = "always") # 136 with identified IAS 
core_mammals_IASthreat_named <- core_mammals_IASthreat %>% filter(name == "Named species")
core_mammals_IASthreat_unspecified <- core_mammals_IASthreat %>% filter(name == "Unspecified species")
# 8 that don't have species name, so we need to move them to unspecified
table(core_mammals_IASthreat_named$ias, useNA = "always") 
core_mammals_IASthreat_blank <- core_mammals_IASthreat_named %>% filter(!grepl("[A-Za-z]", ias))
core_mammals_IASthreat_unspecified <- rbind(core_mammals_IASthreat_unspecified, core_mammals_IASthreat_blank)
# but we still need to remove them from named
core_mammals_IASthreat_named <- core_mammals_IASthreat_named %>% filter(grepl("[A-Za-z]", ias)) 

# there is no wrong categorized as unspecified 
table(core_mammals_IASthreat_unspecified$ias) 

# count the no. of times each IAS occurs for each species
core_mammals_sumIASthreats_named <- core_mammals_IASthreat_named %>% count(binomial,ias)
# now count the no. of IAS for each species
core_mammals_sumIASthreat_named <- core_mammals_sumIASthreats_named %>% count(binomial)
names(core_mammals_sumIASthreat_named)[names(core_mammals_sumIASthreat_named) == "n"] <- "NoIAS"
# Histogram no. species x no. IAS 
n_distinct(core_mammals_sumIASthreat_named$binomial) # 116 species because we are only looking at those that are named
core_mammals_IASthreat_hist <- ggplot(data = core_mammals_sumIASthreat_named, aes(x = NoIAS)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) + 
  labs(title = "Core mammals - IAS identified", x = "No. IAS", y = "No. bird species") + 
  coord_flip()

core_mammals_sumIASthreat_unspecified <- core_mammals_IASthreat_unspecified %>% count(binomial)
names(core_mammals_sumIASthreat_unspecified)[names(core_mammals_sumIASthreat_unspecified) == "n"] <- "NoIAS"
core_mammals_IASthreat_hist2 <- ggplot(data = core_mammals_sumIASthreat_unspecified, aes(x = NoIAS)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) + 
  labs(title = "Core mammals - IAS unspecified", x = "No. IAS", y = "No. bird species") + 
  coord_flip() # 800*600
grid.arrange(core_mammals_IASthreat_hist,core_mammals_IASthreat_hist2,layout_matrix = layout_matrix) # 1000*700


table(core_reptiles_IASthreat$name, useNA = "always") # 188 with identified IAS 
core_reptiles_IASthreat_named <- core_reptiles_IASthreat %>% filter(name == "Named species")
core_reptiles_IASthreat_unspecified <- core_reptiles_IASthreat %>% filter(name == "Unspecified species")
# no species without name
table(core_reptiles_IASthreat_named$ias, useNA = "always") 
# there is no wrong categorized as unspecified 
table(core_reptiles_IASthreat_unspecified$ias) 
# count the no. of times each IAS occurs for each species
core_reptiles_sumIASthreats_named <- core_reptiles_IASthreat_named %>% count(binomial,ias)
# now count the no. of IAS for each species
core_reptiles_sumIASthreat_named <- core_reptiles_sumIASthreats_named %>% count(binomial)
names(core_reptiles_sumIASthreat_named)[names(core_reptiles_sumIASthreat_named) == "n"] <- "NoIAS"
# Histogram no. species x no. IAS 
n_distinct(core_reptiles_sumIASthreat_named$binomial) # 181 species because we are only looking at those that are named
core_reptiles_IASthreat_hist <- ggplot(data = core_reptiles_sumIASthreat_named, aes(x = NoIAS)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) + 
  labs(title = "Core reptiles - IAS identified", x = "No. IAS", y = "No. bird species") + 
  coord_flip()

core_reptiles_sumIASthreat_unspecified <- core_reptiles_IASthreat_unspecified %>% count(binomial)
names(core_reptiles_sumIASthreat_unspecified)[names(core_reptiles_sumIASthreat_unspecified) == "n"] <- "NoIAS"
core_reptiles_IASthreat_hist2 <- ggplot(data = core_reptiles_sumIASthreat_unspecified, aes(x = NoIAS)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) + 
  labs(title = "Core reptiles - IAS unspecified", x = "No. IAS", y = "No. bird species") + 
  coord_flip() # 800*600
grid.arrange(core_reptiles_IASthreat_hist,core_reptiles_IASthreat_hist2,layout_matrix = layout_matrix) # 1000*700


table(core_amphibians_IASthreat$name, useNA = "always") # 691 with identified IAS 
core_amphibians_IASthreat_named <- core_amphibians_IASthreat %>% filter(name == "Named species")
core_amphibians_IASthreat_unspecified <- core_amphibians_IASthreat %>% filter(name == "Unspecified species")
# 2 that don't have species name, so we need to move them to unspecified
table(core_amphibians_IASthreat_named$ias, useNA = "always") 
core_amphibians_IASthreat_blank <- core_amphibians_IASthreat_named %>% filter(!grepl("[A-Za-z]", ias))
core_amphibians_IASthreat_unspecified <- rbind(core_amphibians_IASthreat_unspecified, core_amphibians_IASthreat_blank)
# but we still need to remove them from named
core_amphibians_IASthreat_named <- core_amphibians_IASthreat_named %>% filter(grepl("[A-Za-z]", ias)) 

# there is one that is wrongly categorized as unspecified because it has species name, so we need to move it to named
table(core_amphibians_IASthreat_unspecified$ias) 
core_amphibians_IASthreat_unspecified <- core_amphibians_IASthreat_unspecified %>% filter(ias != "Batrachochytrium dendrobatidis") # remove it from unspecified
core_amphibians_IASthreat_named2 <- core_amphibians_IASthreat_unspecified %>% filter(ias == "Batrachochytrium dendrobatidis")
core_amphibians_IASthreat_named <- rbind(core_amphibians_IASthreat_named2, core_amphibians_IASthreat_named)

# count the no. of times each IAS occurs for each species
core_amphibians_sumIASthreats_named <- core_amphibians_IASthreat_named %>% count(binomial,ias)
# now count the no. of IAS for each species
core_amphibians_sumIASthreat_named <- core_amphibians_sumIASthreats_named %>% count(binomial)
names(core_amphibians_sumIASthreat_named)[names(core_amphibians_sumIASthreat_named) == "n"] <- "NoIAS"
# Histogram no. species x no. IAS 
n_distinct(core_amphibians_sumIASthreat_named$binomial) # 622 species because we are only looking at those that are named
core_amphibians_IASthreat_hist <- ggplot(data = core_amphibians_sumIASthreat_named, aes(x = NoIAS)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) + 
  labs(title = "Core amphibians _ IAS identified", x = "No. IAS", y = "No. bird species") + 
  coord_flip()

core_amphibians_sumIASthreat_unspecified <- core_amphibians_IASthreat_unspecified %>% count(binomial)
names(core_amphibians_sumIASthreat_unspecified)[names(core_amphibians_sumIASthreat_unspecified) == "n"] <- "NoIAS"
core_amphibians_IASthreat_hist2 <- ggplot(data = core_amphibians_sumIASthreat_unspecified, aes(x = NoIAS)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) + 
  labs(title = "Core amphibians - IAS unspecified", x = "No. IAS", y = "No. bird species") + 
  coord_flip() # 800*600
grid.arrange(core_amphibians_IASthreat_hist,core_amphibians_IASthreat_hist2,layout_matrix = layout_matrix) # 1000*700



table(watch_birds_IASthreat$name, useNA = "always") # 24 with identified IAS 
watch_birds_IASthreat_named <- watch_birds_IASthreat %>% filter(name == "Named species")
watch_birds_IASthreat_unspecified <- watch_birds_IASthreat %>% filter(name == "Unspecified species")
# 2 that don't have species name, so we need to move them to unspecified
table(watch_birds_IASthreat_named$ias, useNA = "always") 
watch_birds_IASthreat_blank <- watch_birds_IASthreat_named %>% filter(!grepl("[A-Za-z]", ias))
watch_birds_IASthreat_unspecified <- rbind(watch_birds_IASthreat_unspecified, watch_birds_IASthreat_blank)
# but we still need to remove them from named
watch_birds_IASthreat_named <- watch_birds_IASthreat_named %>% filter(grepl("[A-Za-z]", ias)) 

# there no wrong categorized as unspecified
table(watch_birds_IASthreat_unspecified$ias) 

# count the no. of times each IAS occurs for each species
watch_birds_sumIASthreats_named <- watch_birds_IASthreat_named %>% count(binomial,ias)
# now count the no. of IAS for each species
watch_birds_sumIASthreat_named <- watch_birds_sumIASthreats_named %>% count(binomial)
names(watch_birds_sumIASthreat_named)[names(watch_birds_sumIASthreat_named) == "n"] <- "NoIAS"
# Histogram no. species x no. IAS 
n_distinct(watch_birds_sumIASthreat_named$binomial) # 16 species because we are only looking at those that are named
watch_birds_IASthreat_hist <- ggplot(data = watch_birds_sumIASthreat_named, aes(x = NoIAS)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) + 
  labs(title = "watch birds - IAS identified", x = "No. IAS", y = "No. bird species") + 
  coord_flip()

watch_birds_sumIASthreat_unspecified <- watch_birds_IASthreat_unspecified %>% count(binomial)
names(watch_birds_sumIASthreat_unspecified)[names(watch_birds_sumIASthreat_unspecified) == "n"] <- "NoIAS"
watch_birds_IASthreat_hist2 <- ggplot(data = watch_birds_sumIASthreat_unspecified, aes(x = NoIAS)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) + 
  labs(title = "watch birds - IAS unspecified", x = "No. IAS", y = "No. bird species") + 
  coord_flip() # 800*600
grid.arrange(watch_birds_IASthreat_hist,watch_birds_IASthreat_hist2,layout_matrix = layout_matrix) # 1000*700


table(watch_mammals_IASthreat$name, useNA = "always") # 21 with identified IAS 
watch_mammals_IASthreat_named <- watch_mammals_IASthreat %>% filter(name == "Named species")
watch_mammals_IASthreat_unspecified <- watch_mammals_IASthreat %>% filter(name == "Unspecified species")
# 3 that don't have species name, so we need to move them to unspecified
table(watch_mammals_IASthreat_named$ias, useNA = "always") 
watch_mammals_IASthreat_blank <- watch_mammals_IASthreat_named %>% filter(!grepl("[A-Za-z]", ias))
watch_mammals_IASthreat_unspecified <- rbind(watch_mammals_IASthreat_unspecified, watch_mammals_IASthreat_blank)
# but we still need to remove them from named
watch_mammals_IASthreat_named <- watch_mammals_IASthreat_named %>% filter(grepl("[A-Za-z]", ias)) 

# there is no wrong categorized as unspecified 
table(watch_mammals_IASthreat_unspecified$ias) 

# count the no. of times each IAS occurs for each species
watch_mammals_sumIASthreats_named <- watch_mammals_IASthreat_named %>% count(binomial,ias)
# now count the no. of IAS for each species
watch_mammals_sumIASthreat_named <- watch_mammals_sumIASthreats_named %>% count(binomial)
names(watch_mammals_sumIASthreat_named)[names(watch_mammals_sumIASthreat_named) == "n"] <- "NoIAS"
# Histogram no. species x no. IAS 
n_distinct(watch_mammals_sumIASthreat_named$binomial) # 17 species because we are only looking at those that are named
watch_mammals_IASthreat_hist <- ggplot(data = watch_mammals_sumIASthreat_named, aes(x = NoIAS)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) + 
  labs(title = "watch mammals - IAS identified", x = "No. IAS", y = "No. bird species") + 
  coord_flip()

watch_mammals_sumIASthreat_unspecified <- watch_mammals_IASthreat_unspecified %>% count(binomial)
names(watch_mammals_sumIASthreat_unspecified)[names(watch_mammals_sumIASthreat_unspecified) == "n"] <- "NoIAS"
watch_mammals_IASthreat_hist2 <- ggplot(data = watch_mammals_sumIASthreat_unspecified, aes(x = NoIAS)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) + 
  labs(title = "watch mammals - IAS unspecified", x = "No. IAS", y = "No. bird species") + 
  coord_flip() # 800*600
grid.arrange(watch_mammals_IASthreat_hist,watch_mammals_IASthreat_hist2,layout_matrix = layout_matrix) # 1000*700


table(watch_reptiles_IASthreat$name, useNA = "always") # 4 with identified IAS 
watch_reptiles_IASthreat_named <- watch_reptiles_IASthreat %>% filter(name == "Named species")
watch_reptiles_IASthreat_unspecified <- watch_reptiles_IASthreat %>% filter(name == "Unspecified species")
# no species without name
table(watch_reptiles_IASthreat_named$ias, useNA = "always") 
# there is no wrong categorized as unspecified 
table(watch_reptiles_IASthreat_unspecified$ias) 
# count the no. of times each IAS occurs for each species
watch_reptiles_sumIASthreats_named <- watch_reptiles_IASthreat_named %>% count(binomial,ias)
# now count the no. of IAS for each species
watch_reptiles_sumIASthreat_named <- watch_reptiles_sumIASthreats_named %>% count(binomial)
names(watch_reptiles_sumIASthreat_named)[names(watch_reptiles_sumIASthreat_named) == "n"] <- "NoIAS"
# Histogram no. species x no. IAS 
n_distinct(watch_reptiles_sumIASthreat_named$binomial) # 4 species because we are only looking at those that are named
watch_reptiles_IASthreat_hist <- ggplot(data = watch_reptiles_sumIASthreat_named, aes(x = NoIAS)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) + 
  labs(title = "watch reptiles - IAS identified", x = "No. IAS", y = "No. bird species") + 
  coord_flip()

watch_reptiles_sumIASthreat_unspecified <- watch_reptiles_IASthreat_unspecified %>% count(binomial)
names(watch_reptiles_sumIASthreat_unspecified)[names(watch_reptiles_sumIASthreat_unspecified) == "n"] <- "NoIAS"
watch_reptiles_IASthreat_hist2 <- ggplot(data = watch_reptiles_sumIASthreat_unspecified, aes(x = NoIAS)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) + 
  labs(title = "watch reptiles - IAS unspecified", x = "No. IAS", y = "No. bird species") + 
  coord_flip() # 800*600
grid.arrange(watch_reptiles_IASthreat_hist,watch_reptiles_IASthreat_hist2,layout_matrix = layout_matrix) # 1000*700


table(watch_amphibians_IASthreat$name, useNA = "always") # 27 with identified IAS 
watch_amphibians_IASthreat_named <- watch_amphibians_IASthreat %>% filter(name == "Named species")
watch_amphibians_IASthreat_unspecified <- watch_amphibians_IASthreat %>% filter(name == "Unspecified species")
# all have species name

# nothing wrongly categorized as unspecified 
table(watch_amphibians_IASthreat_unspecified$ias) 

# count the no. of times each IAS occurs for each species
watch_amphibians_sumIASthreats_named <- watch_amphibians_IASthreat_named %>% count(binomial,ias)
# now count the no. of IAS for each species
watch_amphibians_sumIASthreat_named <- watch_amphibians_sumIASthreats_named %>% count(binomial)
names(watch_amphibians_sumIASthreat_named)[names(watch_amphibians_sumIASthreat_named) == "n"] <- "NoIAS"
# Histogram no. species x no. IAS 
n_distinct(watch_amphibians_sumIASthreat_named$binomial) # 622 species because we are only looking at those that are named
watch_amphibians_IASthreat_hist <- ggplot(data = watch_amphibians_sumIASthreat_named, aes(x = NoIAS)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) + 
  labs(title = "watch amphibians _ IAS identified", x = "No. IAS", y = "No. bird species") + 
  coord_flip()

watch_amphibians_sumIASthreat_unspecified <- watch_amphibians_IASthreat_unspecified %>% count(binomial)
names(watch_amphibians_sumIASthreat_unspecified)[names(watch_amphibians_sumIASthreat_unspecified) == "n"] <- "NoIAS"
watch_amphibians_IASthreat_hist2 <- ggplot(data = watch_amphibians_sumIASthreat_unspecified, aes(x = NoIAS)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) + 
  labs(title = "watch amphibians - IAS unspecified", x = "No. IAS", y = "No. bird species") + 
  coord_flip() # 800*600
grid.arrange(watch_amphibians_IASthreat_hist,watch_amphibians_IASthreat_hist2,layout_matrix = layout_matrix) # 1000*700


table(border_birds_IASthreat$name, useNA = "always") # 10 with identified IAS 
border_birds_IASthreat_named <- border_birds_IASthreat %>% filter(name == "Named species")
border_birds_IASthreat_unspecified <- border_birds_IASthreat %>% filter(name == "Unspecified species")
# 2 that don't have species name, so we need to move them to unspecified
table(border_birds_IASthreat_named$ias, useNA = "always") 
border_birds_IASthreat_blank <- border_birds_IASthreat_named %>% filter(!grepl("[A-Za-z]", ias))
border_birds_IASthreat_unspecified <- rbind(border_birds_IASthreat_unspecified, border_birds_IASthreat_blank)
# but we still need to remove them from named
border_birds_IASthreat_named <- border_birds_IASthreat_named %>% filter(grepl("[A-Za-z]", ias)) 

# nothing wrongly categorized as unspecified 
table(border_birds_IASthreat_unspecified$ias) 

# count the no. of times each IAS occurs for each species
border_birds_sumIASthreats_named <- border_birds_IASthreat_named %>% count(binomial,ias)
# now count the no. of IAS for each species
border_birds_sumIASthreat_named <- border_birds_sumIASthreats_named %>% count(binomial)
names(border_birds_sumIASthreat_named)[names(border_birds_sumIASthreat_named) == "n"] <- "NoIAS"
# Histogram no. species x no. IAS 
n_distinct(border_birds_sumIASthreat_named$binomial) # 7 species because we are only looking at those that are named
border_birds_IASthreat_hist <- ggplot(data = border_birds_sumIASthreat_named, aes(x = NoIAS)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) + 
  labs(title = "border birds - IAS identified", x = "No. IAS", y = "No. bird species") + 
  coord_flip()

border_birds_sumIASthreat_unspecified <- border_birds_IASthreat_unspecified %>% count(binomial)
names(border_birds_sumIASthreat_unspecified)[names(border_birds_sumIASthreat_unspecified) == "n"] <- "NoIAS"
border_birds_IASthreat_hist2 <- ggplot(data = border_birds_sumIASthreat_unspecified, aes(x = NoIAS)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) + 
  labs(title = "border birds - IAS unspecified", x = "No. IAS", y = "No. bird species") + 
  coord_flip() # 800*600
grid.arrange(border_birds_IASthreat_hist,border_birds_IASthreat_hist2,layout_matrix = layout_matrix) # 1000*700


table(border_mammals_IASthreat$name, useNA = "always") # 5 with identified IAS 
border_mammals_IASthreat_named <- border_mammals_IASthreat %>% filter(name == "Named species")
border_mammals_IASthreat_unspecified <- border_mammals_IASthreat %>% filter(name == "Unspecified species")
# all have species name
table(border_mammals_IASthreat_named$ias, useNA = "always") 
# nothing wrongly categorized as unspecified 
table(border_mammals_IASthreat_unspecified$ias) 

# count the no. of times each IAS occurs for each species
border_mammals_sumIASthreats_named <- border_mammals_IASthreat_named %>% count(binomial,ias)
# now count the no. of IAS for each species
border_mammals_sumIASthreat_named <- border_mammals_sumIASthreats_named %>% count(binomial)
names(border_mammals_sumIASthreat_named)[names(border_mammals_sumIASthreat_named) == "n"] <- "NoIAS"
# Histogram no. species x no. IAS 
n_distinct(border_mammals_sumIASthreat_named$binomial) # 4 species because we are only looking at those that are named
border_mammals_IASthreat_hist <- ggplot(data = border_mammals_sumIASthreat_named, aes(x = NoIAS)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) + 
  labs(title = "border mammals - IAS identified", x = "No. IAS", y = "No. bird species") + 
  coord_flip()

border_mammals_sumIASthreat_unspecified <- border_mammals_IASthreat_unspecified %>% count(binomial)
names(border_mammals_sumIASthreat_unspecified)[names(border_mammals_sumIASthreat_unspecified) == "n"] <- "NoIAS"
border_mammals_IASthreat_hist2 <- ggplot(data = border_mammals_sumIASthreat_unspecified, aes(x = NoIAS)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) + 
  labs(title = "border mammals - IAS unspecified", x = "No. IAS", y = "No. bird species") + 
  coord_flip() # 800*600
grid.arrange(border_mammals_IASthreat_hist,border_mammals_IASthreat_hist2,layout_matrix = layout_matrix) # 1000*700


table(border_reptiles_IASthreat$name, useNA = "always") # 15 with identified IAS 
border_reptiles_IASthreat_named <- border_reptiles_IASthreat %>% filter(name == "Named species")
border_reptiles_IASthreat_unspecified <- border_reptiles_IASthreat %>% filter(name == "Unspecified species")
# no species without name
table(border_reptiles_IASthreat_named$ias, useNA = "always") 
# there is no wrong categorized as unspecified 
table(border_reptiles_IASthreat_unspecified$ias) 
# count the no. of times each IAS occurs for each species
border_reptiles_sumIASthreats_named <- border_reptiles_IASthreat_named %>% count(binomial,ias)
# now count the no. of IAS for each species
border_reptiles_sumIASthreat_named <- border_reptiles_sumIASthreats_named %>% count(binomial)
names(border_reptiles_sumIASthreat_named)[names(border_reptiles_sumIASthreat_named) == "n"] <- "NoIAS"
# Histogram no. species x no. IAS 
n_distinct(border_reptiles_sumIASthreat_named$binomial) # 15 species because we are only looking at those that are named
border_reptiles_IASthreat_hist <- ggplot(data = border_reptiles_sumIASthreat_named, aes(x = NoIAS)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) + 
  labs(title = "border reptiles - IAS identified", x = "No. IAS", y = "No. bird species") + 
  coord_flip()

border_reptiles_sumIASthreat_unspecified <- border_reptiles_IASthreat_unspecified %>% count(binomial)
names(border_reptiles_sumIASthreat_unspecified)[names(border_reptiles_sumIASthreat_unspecified) == "n"] <- "NoIAS"
border_reptiles_IASthreat_hist2 <- ggplot(data = border_reptiles_sumIASthreat_unspecified, aes(x = NoIAS)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) + 
  labs(title = "border reptiles - IAS unspecified", x = "No. IAS", y = "No. bird species") + 
  coord_flip() # 800*600
grid.arrange(border_reptiles_IASthreat_hist,border_reptiles_IASthreat_hist2,layout_matrix = layout_matrix) # 1000*700


table(border_amphibians_IASthreat$name, useNA = "always") # 14 with identified IAS 
border_amphibians_IASthreat_named <- border_amphibians_IASthreat %>% filter(name == "Named species")
# no species with unspecified IAS 

# count the no. of times each IAS occurs for each species
border_amphibians_sumIASthreats_named <- border_amphibians_IASthreat_named %>% count(binomial,ias)
# now count the no. of IAS for each species
border_amphibians_sumIASthreat_named <- border_amphibians_sumIASthreats_named %>% count(binomial)
names(border_amphibians_sumIASthreat_named)[names(border_amphibians_sumIASthreat_named) == "n"] <- "NoIAS"
# Histogram no. species x no. IAS 
n_distinct(border_amphibians_sumIASthreat_named$binomial) # 12 species because we are only looking at those that are named
border_amphibians_IASthreat_hist <- ggplot(data = border_amphibians_sumIASthreat_named, aes(x = NoIAS)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) + 
  labs(title = "border amphibians _ IAS identified", x = "No. IAS", y = "No. bird species") + 
  coord_flip()


# Hypothesis: species with a higher FUSE-IAS score tend to be threatened by a higher number of IAS
# core_birds_sumIASthreat_score <- merge(core_birds_sumIASthreat, core_list_birds, by = "binomial", all.x = TRUE)
# ggplot(core_birds_sumIASthreat_score, aes(x=NoIAS, y=FUSE_IAS_med)) + 
#   geom_point() + geom_smooth(method=lm , color="red", se=FALSE)
# ggplot(core_birds_sumIASthreat_score, aes(x=as.factor(NoIAS), y=FUSE_IAS_med)) + 
#   geom_boxplot(fill="gray", alpha=0.2) + 
#   xlab("No IAS") + geom_jitter(color="black", size=1.5, alpha=1) + coord_flip()



## Classify IAS in broader categories: mammals, birds, etc
birds <- c("Acridotheres tristis", "Alexandrinus krameri", "Ammospiza nelsoni", "Anas chlorotis", "Anas platyrhynchos", "Aplonis opaca", "Ardeola ralloides", "Asio flammeus", "Asio otus", "Bubo scandiacus", "Bubulcus ibis", "Buteo jamaicensis", "Calonectris diomedea", "Catharacta antarctica", "Catharacta skua", "Chenonetta jubata", "Circus approximans", "Clanga pomarina", "Coragyps atratus", "Corvus macrorhynchos", "Corvus macrorhynchos_old", "Corvus splendens", "Cracticus nigrogularis", "Crotophaga ani", "Dicrurus macrocercus", "Eolophus roseicapilla", "Falco novaeseelandiae", "Falco peregrinus", "Gallirallus australis", "Gallus gallus ssp. domesticus", "Garrulax canorus", "Geospiza psittacula", "Gymnorhina tibicen", "Haematopus finschi", "Himantopus himantopus", "Hypotaenidia philippensis", "Hypsipetes olivaceus", "Larus dominicanus", "Larus michahellis", "Larus occidentalis", "Manorina melanocephala", "Melanerpes superciliaris", "Molothrus bonariensis", "Neochmia temporalis", "Nesoenas picturatus", "Oxyura jamaicensis", "Pachyptila vittata", "Pardalotus striatus", "Passer domesticus", "Pycnonotus cafer", "Sephanoides sephaniodes", "Strepera graculina", "Sturnus vulgaris", "Tachybaptus ruficollis", "Telespiza ultima", "Thalasseus bergii", "Todiramphus veneratus", "Tyto alba", "Tyto novaehollandiae", "Unspecified Falco", "Unspecified FALCONIDAE", "Unspecified Macronectes", "Unspecified MELIPHAGIDAE","Aquila audax","Unspecified CORVIDAE","Pavo cristatus","Corvus corax","Unspecified ACTINOPTERYGII","Ardenna pacifica","Aquila chrysaetos","Trichoglossus haematodus","Accipiter fasciatus")

mammals <- c("Arctocephalus forsteri", "Arctocephalus pusillus", "Bos taurus", "Canis familiaris", "Capra hircus", "Cercopithecus mona", "Cervus nippon", "Equus caballus", "Erinaceus europaeus", "Felis catus", "Herpestes auropunctatus", "Herpestes fuscus", "Herpestes javanicus", "Leopardus colocolo", "Lycalopex culpaeus", "Mus musculus", "Mustela erminea", "Mustela furo", "Mustela itatsi", "Mustela nivalis", "Mustela sibirica", "Nasua nasua", "Neovison vison", "Oryctolagus cuniculus", "Ovis aries", "Peromyscus maniculatus", "Petaurus breviceps", "Phocarctos hookeri", "Rangifer tarandus", "Rattus exulans", "Rattus norvegicus", "Rattus rattus", "Rusa timorensis", "Sus domesticus", "Sus scrofa", "Trichosurus vulpecula", "Unspecified Lycalopex", "Unspecified Macropus", "Unspecified OTARIIDAE", "Unspecified Rattus", "Unspecified Trichosurus", "Vulpes lagopus", "Vulpes vulpes", "Camelus dromedarius","Cuon alpinus","Callithrix jacchus","Callithrix penicillata","Crocidura suaveolens","Panthera tigris","Myocastor coypus","Orcinus orca","Molossus molossus","Equus asinus","Lepus europaeus","Unspecified Nycticebus","Unspecified Mus","Peromyscus fraterculus","Macaca fascicularis","Canis latrans","Sciurus carolinensis","Unspecified Canis","Canis lupus ssp. dingo","Unspecified RODENTIA","Capra aegagrus","Herpestes javanicus_old","Unspecified HERPESTIDAE","Tenrec ecaudatus","Unspecified Sus","Suncus murinus","Rusa unicolor","Dama dama","Bison bison","Procyon lotor","Sylvilagus floridanus")
  
plants <- c("Ammophila arenaria", "Brachiaria decumbens", "Cytisus scoparius", "Dolichandra unguis-cati", "Eichhornia crassipes", "Hedychium gardnerianum", "Lantana camara", "Ligustrum robustum", "Maesopsis eminii", "Mikania micrantha", "Mimosa pigra", "Nassella trichotoma", "Pistia stratiotes", "Psidium cattleianum", "Spartina alterniflora", "Ugni molinae", "Unspecified Eucalyptus", "Unspecified Lupinus", "Unspecified POACEAE", "Unspecified Rubus", "Unspecified Salix", "Urochloa arrecta", "Vachellia drepanolobium","Chromolaena odorata","Acacia catechu","Unspecified Mikania","Cocos nucifera","Cenchrus ciliaris","Merremia peltata","Mikania cordata","Unspecified Sesbania","Acacia nilotica","Brachiaria mutica","Cinnamomum verum","Unspecified Prosopis","Bothriochloa pertusa","Unspecified Brachiaria","Unspecified Pinus","Araucaria cunninghamii","Themeda quadrivalvis","Arundo donax","Dichrostachys cinerea","Adelges tsugae","Clusia rosea","Mimulus moschatus","Rubus fruticosus","Phalaris arundinacea","Unspecified ROSALES","Pinus halepensis")
           
fishes <- c("Anguilla dieffenbachii", "Clarias gariepinus", "Cyprinus carpio", "Micropterus salmoides", "Neogobius melanostomus", "Oncorhynchus mykiss", "Salmo trutta", "Unspecified Channa", "Unspecified Tilapia","Lagocephalus sceleratus","Garra ceylonensis","Gambusia holbrooki","Unspecified SALMONIDAE","Oreochromis aureus","Unspecified CYPRINIDAE","Oreochromis mossambicus","Lepomis cyanellus","Unspecified SILURIFORMES","Unspecified PERCIDAE","Unspecified Oncorhynchus","Unspecified CENTRARCHIDAE","Unspecified PERCIFORMES","Salvelinus fontinalis")

ctenophora <- c("Mnemiopsis leidyi")

invertebrates <- c("Anoplolepis gracilipes", "Apis mellifera", "Apis mellifera ssp. scutellata", "Bombus terrestris", "Carcinus maenas", "Philornis downsi", "Schistocerca nitens", "Schistocerca piceifrons", "Unspecified APIDAE", "Unspecified Coenobita", "Unspecified CULICIDAE", "Unspecified FORMICIDAE", "Unspecified HYMENOPTERA", "Unspecified Philornis", "Unspecified Vespula", "Vespula germanica", "Vespula vulgaris", "Wasmannia auropunctata","Unspecified Agrotis","Scolopendra subspinipes","Solenopsis invicta","Procambarus clarkii","Lymantria dispar","Unspecified SARCOPTIDAE","Pheidole megacephala")

reptiles <- c("Boa constrictor", "Boiga irregularis", "Chilabothrus subflavus", "Pantherophis guttatus", "Python bivittatus", "Tarentola gigas", "Trachylepis wrightii", "Unspecified Varanus", "Varanus indicus","Morelia spilota ssp. imbricata","Crotalus mitchellii","Agama agama","Conolophus subcristatus","Ctenosaura similis","Iguana iguana","Borikenophis portoricensis","Lepidodactylus lugubris","Lampropholis delicata","Hemidactylus mabouia","Trachemys scripta ssp. elegans","Sphenodon punctatus","Hemidactylus frenatus_new","Hemidactylus frenatus")

amphibians <- c("Rhinella marina","Duttaphrynus melanostictus","Lithobates catesbeianus","Ambystoma mavortium","Eleutherodactylus martinicensis","Osteopilus septentrionalis","Eleutherodactylus johnstonei","Eleutherodactylus coqui","Litoria genimaculata","Plethodon cinereus","Pleurodeles nebulosus","Pseudophilautus rus","Pseudophilautus hallidayi","Sclerophrys gutturalis","Andrias davidianus","Andrias sligoi")

fungus <- c("Ceratocystis fimbriata", "Phytophthora cinnamomi","Batrachochytrium dendrobatidis","Batrachochytrium salamandrivorans")

parasites <- c("Corynebacterium amycolatum", "Echinuria uncinata", "Erysipelothrix rhusiopathiae", "Pasteurella multocida", "Plasmodium relictum", "Toxoplasma gondii","Bacillus anthracis","Yersinia pestis","Sarcocystis neurona","Saprolegnia diclina","Aeromonas caviae","Aeromonas hydrophila","Unspecified Mycobacterium") # including bacteria, nematodes

core_birds_IASthreat$ias_groups <- with(core_birds_IASthreat, ifelse(ias %in% birds, 'bird', ifelse(ias %in% mammals, 'mammal', ifelse(ias %in% plants, 'plant', ifelse(ias %in% fishes, 'fish', ifelse(ias %in% invertebrates, 'invertebrate', ifelse(ias %in% reptiles, 'reptile', ifelse(ias %in% fungus, 'fungus', ifelse(ias %in% parasites, 'parasite', ifelse(ias %in% amphibians, 'amphibians', ifelse(ias %in% ctenophora, 'ctenophora', 'NA')))))))))))
# check if it is OK
View(core_birds_IASthreat[,c("ias", "ias_groups")])

core_mammals_IASthreat$ias_groups <- with(core_mammals_IASthreat, ifelse(ias %in% birds, 'bird', ifelse(ias %in% mammals, 'mammal', ifelse(ias %in% plants, 'plant', ifelse(ias %in% fishes, 'fish', ifelse(ias %in% invertebrates, 'invertebrate', ifelse(ias %in% reptiles, 'reptile', ifelse(ias %in% fungus, 'fungus', ifelse(ias %in% parasites, 'parasite', ifelse(ias %in% amphibians, 'amphibian', ifelse(ias %in% ctenophora, 'ctenophora', 'NA')))))))))))
# check if it is OK
View(core_mammals_IASthreat[,c("ias", "ias_groups")])

core_reptiles_IASthreat$ias_groups <- with(core_reptiles_IASthreat, ifelse(ias %in% birds, 'bird', ifelse(ias %in% mammals, 'mammal', ifelse(ias %in% plants, 'plant', ifelse(ias %in% fishes, 'fish', ifelse(ias %in% invertebrates, 'invertebrate', ifelse(ias %in% reptiles, 'reptile', ifelse(ias %in% fungus, 'fungus', ifelse(ias %in% parasites, 'parasite', ifelse(ias %in% amphibians, 'amphibian', ifelse(ias %in% ctenophora, 'ctenophora', 'NA')))))))))))
# check if it is OK
View(core_reptiles_IASthreat[,c("ias", "ias_groups")])

core_amphibians_IASthreat$ias_groups <- with(core_amphibians_IASthreat, ifelse(ias %in% birds, 'bird', ifelse(ias %in% mammals, 'mammal', ifelse(ias %in% plants, 'plant', ifelse(ias %in% fishes, 'fish', ifelse(ias %in% invertebrates, 'invertebrate', ifelse(ias %in% reptiles, 'reptile', ifelse(ias %in% fungus, 'fungus', ifelse(ias %in% parasites, 'parasite', ifelse(ias %in% amphibians, 'amphibian', ifelse(ias %in% ctenophora, 'ctenophora', 'NA')))))))))))
# check if it is OK
View(core_amphibians_IASthreat[,c("ias", "ias_groups")])


watch_birds_IASthreat$ias_groups <- with(watch_birds_IASthreat, ifelse(ias %in% birds, 'bird', ifelse(ias %in% mammals, 'mammal', ifelse(ias %in% plants, 'plant', ifelse(ias %in% fishes, 'fish', ifelse(ias %in% invertebrates, 'invertebrate', ifelse(ias %in% reptiles, 'reptile', ifelse(ias %in% fungus, 'fungus', ifelse(ias %in% parasites, 'parasite', ifelse(ias %in% amphibians, 'amphibian', ifelse(ias %in% ctenophora, 'ctenophora', 'NA')))))))))))
# check if it is OK
View(watch_birds_IASthreat[,c("ias", "ias_groups")])

watch_mammals_IASthreat$ias_groups <- with(watch_mammals_IASthreat, ifelse(ias %in% birds, 'bird', ifelse(ias %in% mammals, 'mammal', ifelse(ias %in% plants, 'plant', ifelse(ias %in% fishes, 'fish', ifelse(ias %in% invertebrates, 'invertebrate', ifelse(ias %in% reptiles, 'reptile', ifelse(ias %in% fungus, 'fungus', ifelse(ias %in% parasites, 'parasite', ifelse(ias %in% amphibians, 'amphibian', ifelse(ias %in% ctenophora, 'ctenophora', 'NA')))))))))))
# check if it is OK
View(watch_mammals_IASthreat[,c("ias", "ias_groups")])

watch_reptiles_IASthreat$ias_groups <- with(watch_reptiles_IASthreat, ifelse(ias %in% birds, 'bird', ifelse(ias %in% mammals, 'mammal', ifelse(ias %in% plants, 'plant', ifelse(ias %in% fishes, 'fish', ifelse(ias %in% invertebrates, 'invertebrate', ifelse(ias %in% reptiles, 'reptile', ifelse(ias %in% fungus, 'fungus', ifelse(ias %in% parasites, 'parasite', ifelse(ias %in% amphibians, 'amphibian', ifelse(ias %in% ctenophora, 'ctenophora', 'NA')))))))))))
# check if it is OK
View(watch_reptiles_IASthreat[,c("ias", "ias_groups")])

watch_amphibians_IASthreat$ias_groups <- with(watch_amphibians_IASthreat, ifelse(ias %in% birds, 'bird', ifelse(ias %in% mammals, 'mammal', ifelse(ias %in% plants, 'plant', ifelse(ias %in% fishes, 'fish', ifelse(ias %in% invertebrates, 'invertebrate', ifelse(ias %in% reptiles, 'reptile', ifelse(ias %in% fungus, 'fungus', ifelse(ias %in% parasites, 'parasite', ifelse(ias %in% amphibians, 'amphibian', ifelse(ias %in% ctenophora, 'ctenophora', 'NA')))))))))))
# check if it is OK
View(watch_amphibians_IASthreat[,c("ias", "ias_groups")])


border_birds_IASthreat$ias_groups <- with(border_birds_IASthreat, ifelse(ias %in% birds, 'bird', ifelse(ias %in% mammals, 'mammal', ifelse(ias %in% plants, 'plant', ifelse(ias %in% fishes, 'fish', ifelse(ias %in% invertebrates, 'invertebrate', ifelse(ias %in% reptiles, 'reptile', ifelse(ias %in% fungus, 'fungus', ifelse(ias %in% parasites, 'parasite', ifelse(ias %in% amphibians, 'amphibian', ifelse(ias %in% ctenophora, 'ctenophora', 'NA')))))))))))
# check if it is OK
View(border_birds_IASthreat[,c("ias", "ias_groups")])

border_mammals_IASthreat$ias_groups <- with(border_mammals_IASthreat, ifelse(ias %in% birds, 'bird', ifelse(ias %in% mammals, 'mammal', ifelse(ias %in% plants, 'plant', ifelse(ias %in% fishes, 'fish', ifelse(ias %in% invertebrates, 'invertebrate', ifelse(ias %in% reptiles, 'reptile', ifelse(ias %in% fungus, 'fungus', ifelse(ias %in% parasites, 'parasite', ifelse(ias %in% amphibians, 'amphibian', ifelse(ias %in% ctenophora, 'ctenophora', 'NA')))))))))))
# check if it is OK
View(border_mammals_IASthreat[,c("ias", "ias_groups")])

border_reptiles_IASthreat$ias_groups <- with(border_reptiles_IASthreat, ifelse(ias %in% birds, 'bird', ifelse(ias %in% mammals, 'mammal', ifelse(ias %in% plants, 'plant', ifelse(ias %in% fishes, 'fish', ifelse(ias %in% invertebrates, 'invertebrate', ifelse(ias %in% reptiles, 'reptile', ifelse(ias %in% fungus, 'fungus', ifelse(ias %in% parasites, 'parasite', ifelse(ias %in% amphibians, 'amphibian', ifelse(ias %in% ctenophora, 'ctenophora', 'NA')))))))))))
# check if it is OK
View(border_reptiles_IASthreat[,c("ias", "ias_groups")])

border_amphibians_IASthreat$ias_groups <- with(border_amphibians_IASthreat, ifelse(ias %in% birds, 'bird', ifelse(ias %in% mammals, 'mammal', ifelse(ias %in% plants, 'plant', ifelse(ias %in% fishes, 'fish', ifelse(ias %in% invertebrates, 'invertebrate', ifelse(ias %in% reptiles, 'reptile', ifelse(ias %in% fungus, 'fungus', ifelse(ias %in% parasites, 'parasite', ifelse(ias %in% amphibians, 'amphibian', ifelse(ias %in% ctenophora, 'ctenophora', 'NA')))))))))))
# check if it is OK
View(border_amphibians_IASthreat[,c("ias", "ias_groups")])



# Correct those that say "Named species" but then don't have any
sum(core_birds_IASthreat$name == "Named species" & core_birds_IASthreat$ias_groups == "NA", na.rm=TRUE) # 35
core_birds_IASthreat$name[core_birds_IASthreat$name == "Named species" & core_birds_IASthreat$ias_groups == "NA"] <- "Unspecified species"
sum(core_mammals_IASthreat$name == "Named species" & core_mammals_IASthreat$ias_groups == "NA", na.rm=TRUE) # 8
core_mammals_IASthreat$name[core_mammals_IASthreat$name == "Named species" & core_mammals_IASthreat$ias_groups == "NA"] <- "Unspecified species"
sum(core_reptiles_IASthreat$name == "Named species" & core_reptiles_IASthreat$ias_groups == "NA", na.rm=TRUE) # 0
sum(core_amphibians_IASthreat$name == "Named species" & core_amphibians_IASthreat$ias_groups == "NA", na.rm=TRUE) # 2
core_amphibians_IASthreat$name[core_amphibians_IASthreat$name == "Named species" & core_amphibians_IASthreat$ias_groups == "NA"] <- "Unspecified species"

sum(watch_birds_IASthreat$name == "Named species" & watch_birds_IASthreat$ias_groups == "NA", na.rm=TRUE) # 2
watch_birds_IASthreat$name[watch_birds_IASthreat$name == "Named species" & watch_birds_IASthreat$ias_groups == "NA"] <- "Unspecified species"
sum(watch_mammals_IASthreat$name == "Named species" & watch_mammals_IASthreat$ias_groups == "NA", na.rm=TRUE) # 3
watch_mammals_IASthreat$name[watch_mammals_IASthreat$name == "Named species" & watch_mammals_IASthreat$ias_groups == "NA"] <- "Unspecified species"
sum(watch_reptiles_IASthreat$name == "Named species" & watch_reptiles_IASthreat$ias_groups == "NA", na.rm=TRUE) # 0
sum(watch_amphibians_IASthreat$name == "Named species" & watch_amphibians_IASthreat$ias_groups == "NA", na.rm=TRUE) # 0

sum(border_birds_IASthreat$name == "Named species" & border_birds_IASthreat$ias_groups == "NA", na.rm=TRUE) # 2
border_birds_IASthreat$name[border_birds_IASthreat$name == "Named species" & border_birds_IASthreat$ias_groups == "NA"] <- "Unspecified species"
sum(border_mammals_IASthreat$name == "Named species" & border_mammals_IASthreat$ias_groups == "NA", na.rm=TRUE) # 0
sum(border_reptiles_IASthreat$name == "Named species" & border_reptiles_IASthreat$ias_groups == "NA", na.rm=TRUE) # 0
sum(border_amphibians_IASthreat$name == "Named species" & border_amphibians_IASthreat$ias_groups == "NA", na.rm=TRUE) # 0



# Identify the threats "Diseases of unknown cause" and "Unspecified species" and "Problematic native species/diseases"
table(core_birds_IASthreat$ias_groups)
core_birds_IASthreat$ias_groups[core_birds_IASthreat$name == "Diseases of unknown cause" & core_birds_IASthreat$ias_groups == "NA"] <- "diseases_unknown_cause"
core_birds_IASthreat$ias_groups[core_birds_IASthreat$name == "Problematic native species/diseases" & core_birds_IASthreat$ias_groups == "NA"] <- "problematic_native_diseases"
core_birds_IASthreat$ias_groups[core_birds_IASthreat$name == "Unspecified species" & core_birds_IASthreat$ias_groups == "NA"] <- "unspecified_spp"
View(core_birds_IASthreat[,c("name", "ias", "ias_groups")])

table(core_mammals_IASthreat$ias_groups)
core_mammals_IASthreat$ias_groups[core_mammals_IASthreat$name == "Diseases of unknown cause" & core_mammals_IASthreat$ias_groups == "NA"] <- "diseases_unknown_cause"
core_mammals_IASthreat$ias_groups[core_mammals_IASthreat$name == "Problematic native species/diseases" & core_mammals_IASthreat$ias_groups == "NA"] <- "problematic_native_diseases"
core_mammals_IASthreat$ias_groups[core_mammals_IASthreat$name == "Unspecified species" & core_mammals_IASthreat$ias_groups == "NA"] <- "unspecified_spp"

table(core_reptiles_IASthreat$ias_groups)
table(core_reptiles_IASthreat$name) # one category called "Introduced genetic material"
core_reptiles_IASthreat$ias_groups[core_reptiles_IASthreat$name == "Diseases of unknown cause" & core_reptiles_IASthreat$ias_groups == "NA"] <- "diseases_unknown_cause"
core_reptiles_IASthreat$ias_groups[core_reptiles_IASthreat$name == "Problematic native species/diseases" & core_reptiles_IASthreat$ias_groups == "NA"] <- "problematic_native_diseases"
core_reptiles_IASthreat$ias_groups[core_reptiles_IASthreat$name == "Unspecified species" & core_reptiles_IASthreat$ias_groups == "NA"] <- "unspecified_spp"
core_reptiles_IASthreat$ias_groups[core_reptiles_IASthreat$name == "Introduced genetic material" & core_reptiles_IASthreat$ias_groups == "NA"] <- "introduced_genetic_material"

table(core_amphibians_IASthreat$ias_groups)
core_amphibians_IASthreat$ias_groups[core_amphibians_IASthreat$name == "Diseases of unknown cause" & core_amphibians_IASthreat$ias_groups == "NA"] <- "diseases_unknown_cause"
core_amphibians_IASthreat$ias_groups[core_amphibians_IASthreat$name == "Problematic native species/diseases" & core_amphibians_IASthreat$ias_groups == "NA"] <- "problematic_native_diseases"
core_amphibians_IASthreat$ias_groups[core_amphibians_IASthreat$name == "Unspecified species" & core_amphibians_IASthreat$ias_groups == "NA"] <- "unspecified_spp"



table(watch_birds_IASthreat$ias_groups)
watch_birds_IASthreat$ias_groups[watch_birds_IASthreat$name == "Diseases of unknown cause" & watch_birds_IASthreat$ias_groups == "NA"] <- "diseases_unknown_cause"
watch_birds_IASthreat$ias_groups[watch_birds_IASthreat$name == "Problematic native species/diseases" & watch_birds_IASthreat$ias_groups == "NA"] <- "problematic_native_diseases"
watch_birds_IASthreat$ias_groups[watch_birds_IASthreat$name == "Unspecified species" & watch_birds_IASthreat$ias_groups == "NA"] <- "unspecified_spp"

table(watch_mammals_IASthreat$ias_groups)
table(watch_mammals_IASthreat$name)
watch_mammals_IASthreat$ias_groups[watch_mammals_IASthreat$name == "Diseases of unknown cause" & watch_mammals_IASthreat$ias_groups == "NA"] <- "diseases_unknown_cause"
watch_mammals_IASthreat$ias_groups[watch_mammals_IASthreat$name == "Problematic native species/diseases" & watch_mammals_IASthreat$ias_groups == "NA"] <- "problematic_native_diseases"
watch_mammals_IASthreat$ias_groups[watch_mammals_IASthreat$name == "Unspecified species" & watch_mammals_IASthreat$ias_groups == "NA"] <- "unspecified_spp"
watch_mammals_IASthreat$ias_groups[watch_mammals_IASthreat$name == "Introduced genetic material" & watch_mammals_IASthreat$ias_groups == "NA"] <- "introduced_genetic_material"

table(watch_reptiles_IASthreat$ias_groups)
watch_reptiles_IASthreat$ias_groups[watch_reptiles_IASthreat$name == "Diseases of unknown cause" & watch_reptiles_IASthreat$ias_groups == "NA"] <- "diseases_unknown_cause"
watch_reptiles_IASthreat$ias_groups[watch_reptiles_IASthreat$name == "Problematic native species/diseases" & watch_reptiles_IASthreat$ias_groups == "NA"] <- "problematic_native_diseases"
watch_reptiles_IASthreat$ias_groups[watch_reptiles_IASthreat$name == "Unspecified species" & watch_reptiles_IASthreat$ias_groups == "NA"] <- "unspecified_spp"

table(watch_amphibians_IASthreat$ias_groups)
watch_amphibians_IASthreat$ias_groups[watch_amphibians_IASthreat$name == "Diseases of unknown cause" & watch_amphibians_IASthreat$ias_groups == "NA"] <- "diseases_unknown_cause"
watch_amphibians_IASthreat$ias_groups[watch_amphibians_IASthreat$name == "Problematic native species/diseases" & watch_amphibians_IASthreat$ias_groups == "NA"] <- "problematic_native_diseases"
watch_amphibians_IASthreat$ias_groups[watch_amphibians_IASthreat$name == "Unspecified species" & watch_amphibians_IASthreat$ias_groups == "NA"] <- "unspecified_spp"


table(border_birds_IASthreat$ias_groups)
border_birds_IASthreat$ias_groups[border_birds_IASthreat$name == "Diseases of unknown cause" & border_birds_IASthreat$ias_groups == "NA"] <- "diseases_unknown_cause"
border_birds_IASthreat$ias_groups[border_birds_IASthreat$name == "Problematic native species/diseases" & border_birds_IASthreat$ias_groups == "NA"] <- "problematic_native_diseases"
border_birds_IASthreat$ias_groups[border_birds_IASthreat$name == "Unspecified species" & border_birds_IASthreat$ias_groups == "NA"] <- "unspecified_spp"

table(border_mammals_IASthreat$ias_groups)
border_mammals_IASthreat$ias_groups[border_mammals_IASthreat$name == "Diseases of unknown cause" & border_mammals_IASthreat$ias_groups == "NA"] <- "diseases_unknown_cause"
border_mammals_IASthreat$ias_groups[border_mammals_IASthreat$name == "Problematic native species/diseases" & border_mammals_IASthreat$ias_groups == "NA"] <- "problematic_native_diseases"
border_mammals_IASthreat$ias_groups[border_mammals_IASthreat$name == "Unspecified species" & border_mammals_IASthreat$ias_groups == "NA"] <- "unspecified_spp"

table(border_reptiles_IASthreat$ias_groups)
border_reptiles_IASthreat$ias_groups[border_reptiles_IASthreat$name == "Diseases of unknown cause" & border_reptiles_IASthreat$ias_groups == "NA"] <- "diseases_unknown_cause"
border_reptiles_IASthreat$ias_groups[border_reptiles_IASthreat$name == "Problematic native species/diseases" & border_reptiles_IASthreat$ias_groups == "NA"] <- "problematic_native_diseases"
border_reptiles_IASthreat$ias_groups[border_reptiles_IASthreat$name == "Unspecified species" & border_reptiles_IASthreat$ias_groups == "NA"] <- "unspecified_spp"

table(border_amphibians_IASthreat$ias_groups)
border_amphibians_IASthreat$ias_groups[border_amphibians_IASthreat$name == "Diseases of unknown cause" & border_amphibians_IASthreat$ias_groups == "NA"] <- "diseases_unknown_cause"
border_amphibians_IASthreat$ias_groups[border_amphibians_IASthreat$name == "Problematic native species/diseases" & border_amphibians_IASthreat$ias_groups == "NA"] <- "problematic_native_diseases"
border_amphibians_IASthreat$ias_groups[border_amphibians_IASthreat$name == "Unspecified species" & border_amphibians_IASthreat$ias_groups == "NA"] <- "unspecified_spp"


## Count no. type of IAS 

core_birds_sumIASthreat_type <- core_birds_IASthreat %>% count(binomial, ias_groups)
names(core_birds_sumIASthreat_type)[names(core_birds_sumIASthreat_type) == "n"] <- "NoIAS"
core_mammals_sumIASthreat_type <- core_mammals_IASthreat %>% count(binomial, ias_groups)
names(core_mammals_sumIASthreat_type)[names(core_mammals_sumIASthreat_type) == "n"] <- "NoIAS"
core_reptiles_sumIASthreat_type <- core_reptiles_IASthreat %>% count(binomial, ias_groups)
names(core_reptiles_sumIASthreat_type)[names(core_reptiles_sumIASthreat_type) == "n"] <- "NoIAS"
core_amphibians_sumIASthreat_type <- core_amphibians_IASthreat %>% count(binomial, ias_groups)
names(core_amphibians_sumIASthreat_type)[names(core_amphibians_sumIASthreat_type) == "n"] <- "NoIAS"

watch_birds_sumIASthreat_type <- watch_birds_IASthreat %>% count(binomial, ias_groups)
names(watch_birds_sumIASthreat_type)[names(watch_birds_sumIASthreat_type) == "n"] <- "NoIAS"
watch_mammals_sumIASthreat_type <- watch_mammals_IASthreat %>% count(binomial, ias_groups)
names(watch_mammals_sumIASthreat_type)[names(watch_mammals_sumIASthreat_type) == "n"] <- "NoIAS"
watch_reptiles_sumIASthreat_type <- watch_reptiles_IASthreat %>% count(binomial, ias_groups)
names(watch_reptiles_sumIASthreat_type)[names(watch_reptiles_sumIASthreat_type) == "n"] <- "NoIAS"
watch_amphibians_sumIASthreat_type <- watch_amphibians_IASthreat %>% count(binomial, ias_groups)
names(watch_amphibians_sumIASthreat_type)[names(watch_amphibians_sumIASthreat_type) == "n"] <- "NoIAS"

border_birds_sumIASthreat_type <- border_birds_IASthreat %>% count(binomial, ias_groups)
names(border_birds_sumIASthreat_type)[names(border_birds_sumIASthreat_type) == "n"] <- "NoIAS"
border_mammals_sumIASthreat_type <- border_mammals_IASthreat %>% count(binomial, ias_groups)
names(border_mammals_sumIASthreat_type)[names(border_mammals_sumIASthreat_type) == "n"] <- "NoIAS"
border_reptiles_sumIASthreat_type <- border_reptiles_IASthreat %>% count(binomial, ias_groups)
names(border_reptiles_sumIASthreat_type)[names(border_reptiles_sumIASthreat_type) == "n"] <- "NoIAS"
border_amphibians_sumIASthreat_type <- border_amphibians_IASthreat %>% count(binomial, ias_groups)
names(border_amphibians_sumIASthreat_type)[names(border_amphibians_sumIASthreat_type) == "n"] <- "NoIAS"


# No. of IAS per group

check_amphibian <- "amphibian"
check_bird <- "bird"
check_invertebrate <- "invertebrate"
check_mammal <- "mammal"
check_plant <- "plant"
check_problematic_native_diseases <- "problematic_native_diseases"
check_ctenophora <- "ctenophora"
check_diseases_unknown_cause <- "diseases_unknown_cause"
check_fish <- "fish"
check_parasite <- "parasite"
check_reptile <- "reptile"
check_unspecified_spp <- "unspecified_spp"
check_fungus <- "fungus"
check_introduced <- "introduced_genetic_material"

# check if all ias_groups are present and if not add them
core_birds_sumIASthreat_type2 <- aggregate(NoIAS ~ ias_groups, core_birds_sumIASthreat_type, sum)

if (!(check_amphibian %in% core_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_amphibian <- data.frame(ias_groups = "amphibian",NoIAS = 0)
  # Add the new row to the existing data frame
  core_birds_sumIASthreat_type2 <- rbind(core_birds_sumIASthreat_type2, row_amphibian)
}

if (!(check_bird %in% core_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_bird <- data.frame(ias_groups = "bird",NoIAS = 0)
  # Add the new row to the existing data frame
  core_birds_sumIASthreat_type2 <- rbind(core_birds_sumIASthreat_type2, row_bird)
}

if (!(check_invertebrate %in% core_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_invertebrate <- data.frame(ias_groups = "invertebrate",NoIAS = 0)
  # Add the new row to the existing data frame
  core_birds_sumIASthreat_type2 <- rbind(core_birds_sumIASthreat_type2, row_invertebrate)
}

if (!(check_mammal %in% core_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_mammal <- data.frame(ias_groups = "mammal",NoIAS = 0)
  # Add the new row to the existing data frame
  core_birds_sumIASthreat_type2 <- rbind(core_birds_sumIASthreat_type2, row_mammal)
}

if (!(check_plant %in% core_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_plant <- data.frame(ias_groups = "plant",NoIAS = 0)
  # Add the new row to the existing data frame
  core_birds_sumIASthreat_type2 <- rbind(core_birds_sumIASthreat_type2, row_plant)
}

if (!(check_problematic_native_diseases %in% core_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_problematic_native_diseases <- data.frame(ias_groups = "problematic_native_diseases",NoIAS = 0)
  # Add the new row to the existing data frame
  core_birds_sumIASthreat_type2 <- rbind(core_birds_sumIASthreat_type2, row_problematic_native_diseases)
}

if (!(check_ctenophora %in% core_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_ctenophora <- data.frame(ias_groups = "ctenophora",NoIAS = 0)
  # Add the new row to the existing data frame
  core_birds_sumIASthreat_type2 <- rbind(core_birds_sumIASthreat_type2, row_ctenophora)
}

if (!(check_diseases_unknown_cause %in% core_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_diseases_unknown_cause <- data.frame(ias_groups = "diseases_unknown_cause",NoIAS = 0)
  # Add the new row to the existing data frame
  core_birds_sumIASthreat_type2 <- rbind(core_birds_sumIASthreat_type2, row_diseases_unknown_cause)
}

if (!(check_fish %in% core_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_fish <- data.frame(ias_groups = "fish",NoIAS = 0)
  # Add the new row to the existing data frame
  core_birds_sumIASthreat_type2 <- rbind(core_birds_sumIASthreat_type2, row_fish)
}

if (!(check_parasite %in% core_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_parasite <- data.frame(ias_groups = "parasite",NoIAS = 0)
  # Add the new row to the existing data frame
  core_birds_sumIASthreat_type2 <- rbind(core_birds_sumIASthreat_type2, row_parasite)
}

if (!(check_reptile %in% core_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_reptile <- data.frame(ias_groups = "reptile",NoIAS = 0)
  # Add the new row to the existing data frame
  core_birds_sumIASthreat_type2 <- rbind(core_birds_sumIASthreat_type2, row_reptile)
}

if (!(check_unspecified_spp %in% core_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_unspecified_spp <- data.frame(ias_groups = "unspecified_spp",NoIAS = 0)
  # Add the new row to the existing data frame
  core_birds_sumIASthreat_type2 <- rbind(core_birds_sumIASthreat_type2, row_unspecified_spp)
}

if (!(check_fungus %in% core_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_fungus <- data.frame(ias_groups = "fungus",NoIAS = 0)
  # Add the new row to the existing data frame
  core_birds_sumIASthreat_type2 <- rbind(core_birds_sumIASthreat_type2, row_fungus)
}

if (!(check_introduced %in% core_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_introduced <- data.frame(ias_groups = "introduced_genetic_material",NoIAS = 0)
  # Add the new row to the existing data frame
  core_birds_sumIASthreat_type2 <- rbind(core_birds_sumIASthreat_type2, row_introduced)
}



core_mammals_sumIASthreat_type2 <- aggregate(NoIAS ~ ias_groups, core_mammals_sumIASthreat_type, sum)

if (!(check_amphibian %in% core_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_amphibian <- data.frame(ias_groups = "amphibian",NoIAS = 0)
  # Add the new row to the existing data frame
  core_mammals_sumIASthreat_type2 <- rbind(core_mammals_sumIASthreat_type2, row_amphibian)
}

if (!(check_bird %in% core_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_bird <- data.frame(ias_groups = "bird",NoIAS = 0)
  # Add the new row to the existing data frame
  core_mammals_sumIASthreat_type2 <- rbind(core_mammals_sumIASthreat_type2, row_bird)
}

if (!(check_invertebrate %in% core_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_invertebrate <- data.frame(ias_groups = "invertebrate",NoIAS = 0)
  # Add the new row to the existing data frame
  core_mammals_sumIASthreat_type2 <- rbind(core_mammals_sumIASthreat_type2, row_invertebrate)
}

if (!(check_mammal %in% core_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_mammal <- data.frame(ias_groups = "mammal",NoIAS = 0)
  # Add the new row to the existing data frame
  core_mammals_sumIASthreat_type2 <- rbind(core_mammals_sumIASthreat_type2, row_mammal)
}

if (!(check_plant %in% core_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_plant <- data.frame(ias_groups = "plant",NoIAS = 0)
  # Add the new row to the existing data frame
  core_mammals_sumIASthreat_type2 <- rbind(core_mammals_sumIASthreat_type2, row_plant)
}

if (!(check_problematic_native_diseases %in% core_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_problematic_native_diseases <- data.frame(ias_groups = "problematic_native_diseases",NoIAS = 0)
  # Add the new row to the existing data frame
  core_mammals_sumIASthreat_type2 <- rbind(core_mammals_sumIASthreat_type2, row_problematic_native_diseases)
}

if (!(check_ctenophora %in% core_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_ctenophora <- data.frame(ias_groups = "ctenophora",NoIAS = 0)
  # Add the new row to the existing data frame
  core_mammals_sumIASthreat_type2 <- rbind(core_mammals_sumIASthreat_type2, row_ctenophora)
}

if (!(check_diseases_unknown_cause %in% core_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_diseases_unknown_cause <- data.frame(ias_groups = "diseases_unknown_cause",NoIAS = 0)
  # Add the new row to the existing data frame
  core_mammals_sumIASthreat_type2 <- rbind(core_mammals_sumIASthreat_type2, row_diseases_unknown_cause)
}

if (!(check_fish %in% core_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_fish <- data.frame(ias_groups = "fish",NoIAS = 0)
  # Add the new row to the existing data frame
  core_mammals_sumIASthreat_type2 <- rbind(core_mammals_sumIASthreat_type2, row_fish)
}

if (!(check_parasite %in% core_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_parasite <- data.frame(ias_groups = "parasite",NoIAS = 0)
  # Add the new row to the existing data frame
  core_mammals_sumIASthreat_type2 <- rbind(core_mammals_sumIASthreat_type2, row_parasite)
}

if (!(check_reptile %in% core_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_reptile <- data.frame(ias_groups = "reptile",NoIAS = 0)
  # Add the new row to the existing data frame
  core_mammals_sumIASthreat_type2 <- rbind(core_mammals_sumIASthreat_type2, row_reptile)
}

if (!(check_unspecified_spp %in% core_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_unspecified_spp <- data.frame(ias_groups = "unspecified_spp",NoIAS = 0)
  # Add the new row to the existing data frame
  core_mammals_sumIASthreat_type2 <- rbind(core_mammals_sumIASthreat_type2, row_unspecified_spp)
}

if (!(check_fungus %in% core_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_fungus <- data.frame(ias_groups = "fungus",NoIAS = 0)
  # Add the new row to the existing data frame
  core_mammals_sumIASthreat_type2 <- rbind(core_mammals_sumIASthreat_type2, row_fungus)
}

if (!(check_introduced %in% core_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_introduced <- data.frame(ias_groups = "introduced_genetic_material",NoIAS = 0)
  # Add the new row to the existing data frame
  core_mammals_sumIASthreat_type2 <- rbind(core_mammals_sumIASthreat_type2, row_introduced)
}




core_reptiles_sumIASthreat_type2 <- aggregate(NoIAS ~ ias_groups, core_reptiles_sumIASthreat_type, sum)

if (!(check_amphibian %in% core_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_amphibian <- data.frame(ias_groups = "amphibian",NoIAS = 0)
  # Add the new row to the existing data frame
  core_reptiles_sumIASthreat_type2 <- rbind(core_reptiles_sumIASthreat_type2, row_amphibian)
}

if (!(check_bird %in% core_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_bird <- data.frame(ias_groups = "bird",NoIAS = 0)
  # Add the new row to the existing data frame
  core_reptiles_sumIASthreat_type2 <- rbind(core_reptiles_sumIASthreat_type2, row_bird)
}

if (!(check_invertebrate %in% core_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_invertebrate <- data.frame(ias_groups = "invertebrate",NoIAS = 0)
  # Add the new row to the existing data frame
  core_reptiles_sumIASthreat_type2 <- rbind(core_reptiles_sumIASthreat_type2, row_invertebrate)
}

if (!(check_mammal %in% core_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_mammal <- data.frame(ias_groups = "mammal",NoIAS = 0)
  # Add the new row to the existing data frame
  core_reptiles_sumIASthreat_type2 <- rbind(core_reptiles_sumIASthreat_type2, row_mammal)
}

if (!(check_plant %in% core_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_plant <- data.frame(ias_groups = "plant",NoIAS = 0)
  # Add the new row to the existing data frame
  core_reptiles_sumIASthreat_type2 <- rbind(core_reptiles_sumIASthreat_type2, row_plant)
}

if (!(check_problematic_native_diseases %in% core_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_problematic_native_diseases <- data.frame(ias_groups = "problematic_native_diseases",NoIAS = 0)
  # Add the new row to the existing data frame
  core_reptiles_sumIASthreat_type2 <- rbind(core_reptiles_sumIASthreat_type2, row_problematic_native_diseases)
}

if (!(check_ctenophora %in% core_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_ctenophora <- data.frame(ias_groups = "ctenophora",NoIAS = 0)
  # Add the new row to the existing data frame
  core_reptiles_sumIASthreat_type2 <- rbind(core_reptiles_sumIASthreat_type2, row_ctenophora)
}

if (!(check_diseases_unknown_cause %in% core_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_diseases_unknown_cause <- data.frame(ias_groups = "diseases_unknown_cause",NoIAS = 0)
  # Add the new row to the existing data frame
  core_reptiles_sumIASthreat_type2 <- rbind(core_reptiles_sumIASthreat_type2, row_diseases_unknown_cause)
}

if (!(check_fish %in% core_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_fish <- data.frame(ias_groups = "fish",NoIAS = 0)
  # Add the new row to the existing data frame
  core_reptiles_sumIASthreat_type2 <- rbind(core_reptiles_sumIASthreat_type2, row_fish)
}

if (!(check_parasite %in% core_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_parasite <- data.frame(ias_groups = "parasite",NoIAS = 0)
  # Add the new row to the existing data frame
  core_reptiles_sumIASthreat_type2 <- rbind(core_reptiles_sumIASthreat_type2, row_parasite)
}

if (!(check_reptile %in% core_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_reptile <- data.frame(ias_groups = "reptile",NoIAS = 0)
  # Add the new row to the existing data frame
  core_reptiles_sumIASthreat_type2 <- rbind(core_reptiles_sumIASthreat_type2, row_reptile)
}

if (!(check_unspecified_spp %in% core_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_unspecified_spp <- data.frame(ias_groups = "unspecified_spp",NoIAS = 0)
  # Add the new row to the existing data frame
  core_reptiles_sumIASthreat_type2 <- rbind(core_reptiles_sumIASthreat_type2, row_unspecified_spp)
}

if (!(check_fungus %in% core_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_fungus <- data.frame(ias_groups = "fungus",NoIAS = 0)
  # Add the new row to the existing data frame
  core_reptiles_sumIASthreat_type2 <- rbind(core_reptiles_sumIASthreat_type2, row_fungus)
}

if (!(check_introduced %in% core_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_introduced <- data.frame(ias_groups = "introduced_genetic_material",NoIAS = 0)
  # Add the new row to the existing data frame
  core_reptiles_sumIASthreat_type2 <- rbind(core_reptiles_sumIASthreat_type2, row_introduced)
}




core_amphibians_sumIASthreat_type2 <- aggregate(NoIAS ~ ias_groups, core_amphibians_sumIASthreat_type, sum)

if (!(check_amphibian %in% core_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_amphibian <- data.frame(ias_groups = "amphibian",NoIAS = 0)
  # Add the new row to the existing data frame
  core_amphibians_sumIASthreat_type2 <- rbind(core_amphibians_sumIASthreat_type2, row_amphibian)
}

if (!(check_bird %in% core_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_bird <- data.frame(ias_groups = "bird",NoIAS = 0)
  # Add the new row to the existing data frame
  core_amphibians_sumIASthreat_type2 <- rbind(core_amphibians_sumIASthreat_type2, row_bird)
}

if (!(check_invertebrate %in% core_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_invertebrate <- data.frame(ias_groups = "invertebrate",NoIAS = 0)
  # Add the new row to the existing data frame
  core_amphibians_sumIASthreat_type2 <- rbind(core_amphibians_sumIASthreat_type2, row_invertebrate)
}

if (!(check_mammal %in% core_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_mammal <- data.frame(ias_groups = "mammal",NoIAS = 0)
  # Add the new row to the existing data frame
  core_amphibians_sumIASthreat_type2 <- rbind(core_amphibians_sumIASthreat_type2, row_mammal)
}

if (!(check_plant %in% core_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_plant <- data.frame(ias_groups = "plant",NoIAS = 0)
  # Add the new row to the existing data frame
  core_amphibians_sumIASthreat_type2 <- rbind(core_amphibians_sumIASthreat_type2, row_plant)
}

if (!(check_problematic_native_diseases %in% core_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_problematic_native_diseases <- data.frame(ias_groups = "problematic_native_diseases",NoIAS = 0)
  # Add the new row to the existing data frame
  core_amphibians_sumIASthreat_type2 <- rbind(core_amphibians_sumIASthreat_type2, row_problematic_native_diseases)
}

if (!(check_ctenophora %in% core_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_ctenophora <- data.frame(ias_groups = "ctenophora",NoIAS = 0)
  # Add the new row to the existing data frame
  core_amphibians_sumIASthreat_type2 <- rbind(core_amphibians_sumIASthreat_type2, row_ctenophora)
}

if (!(check_diseases_unknown_cause %in% core_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_diseases_unknown_cause <- data.frame(ias_groups = "diseases_unknown_cause",NoIAS = 0)
  # Add the new row to the existing data frame
  core_amphibians_sumIASthreat_type2 <- rbind(core_amphibians_sumIASthreat_type2, row_diseases_unknown_cause)
}

if (!(check_fish %in% core_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_fish <- data.frame(ias_groups = "fish",NoIAS = 0)
  # Add the new row to the existing data frame
  core_amphibians_sumIASthreat_type2 <- rbind(core_amphibians_sumIASthreat_type2, row_fish)
}

if (!(check_parasite %in% core_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_parasite <- data.frame(ias_groups = "parasite",NoIAS = 0)
  # Add the new row to the existing data frame
  core_amphibians_sumIASthreat_type2 <- rbind(core_amphibians_sumIASthreat_type2, row_parasite)
}

if (!(check_reptile %in% core_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_reptile <- data.frame(ias_groups = "reptile",NoIAS = 0)
  # Add the new row to the existing data frame
  core_amphibians_sumIASthreat_type2 <- rbind(core_amphibians_sumIASthreat_type2, row_reptile)
}

if (!(check_unspecified_spp %in% core_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_unspecified_spp <- data.frame(ias_groups = "unspecified_spp",NoIAS = 0)
  # Add the new row to the existing data frame
  core_amphibians_sumIASthreat_type2 <- rbind(core_amphibians_sumIASthreat_type2, row_unspecified_spp)
}

if (!(check_fungus %in% core_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_fungus <- data.frame(ias_groups = "fungus",NoIAS = 0)
  # Add the new row to the existing data frame
  core_amphibians_sumIASthreat_type2 <- rbind(core_amphibians_sumIASthreat_type2, row_fungus)
}

if (!(check_introduced %in% core_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_introduced <- data.frame(ias_groups = "introduced_genetic_material",NoIAS = 0)
  # Add the new row to the existing data frame
  core_amphibians_sumIASthreat_type2 <- rbind(core_amphibians_sumIASthreat_type2, row_introduced)
}


core_sumIASthreat_type2 <- merge(merge(merge(core_birds_sumIASthreat_type2,core_mammals_sumIASthreat_type2, by = "ias_groups",suffixes = c("_birds", "_mammals"),all = TRUE),core_reptiles_sumIASthreat_type2,by = "ias_groups",suffixes = c("_", "_reptiles"),all = TRUE),core_amphibians_sumIASthreat_type2,by = "ias_groups",suffixes = c("_", "_amphibians"),all = TRUE)
colnames(core_sumIASthreat_type2)[colnames(core_sumIASthreat_type2) == "NoIAS_"] <- "NoIAS_reptiles"

core_sumIASthreat_type2$ias_groups <- factor(core_sumIASthreat_type2$ias_groups, levels = c("introduced_genetic_material","diseases_unknown_cause","problematic_native_diseases","unspecified_spp","plant","mammal","bird","reptile","amphibian","fish","invertebrate","ctenophora","fungus","parasite"))

core_sumIASthreat_type2_birds <- ggplot(core_sumIASthreat_type2, aes(x=ias_groups, y=NoIAS_birds)) + geom_bar(stat = "identity") +
  geom_text(aes(label = NoIAS_birds), hjust = -0.5, size = 3) + coord_flip() + ggtitle("CORE birds")
core_sumIASthreat_type2_mammals <- ggplot(core_sumIASthreat_type2, aes(x=ias_groups, y=NoIAS_mammals)) + geom_bar(stat = "identity") + geom_text(aes(label = NoIAS_mammals), hjust = -0.5, size = 3) + coord_flip() + ggtitle("CORE mammals")
core_sumIASthreat_type2_reptiles <- ggplot(core_sumIASthreat_type2, aes(x=ias_groups, y=NoIAS_reptiles)) + geom_bar(stat = "identity") + geom_text(aes(label = NoIAS_reptiles), hjust = -0.5, size = 3) +  coord_flip() + ggtitle("CORE reptiles")
core_sumIASthreat_type2_amphibians <- ggplot(core_sumIASthreat_type2, aes(x=ias_groups, y=NoIAS_amphibians)) + geom_bar(stat = "identity") + geom_text(aes(label = NoIAS_amphibians), hjust = -0.5, size = 3) +  coord_flip() + ggtitle("CORE amphibians")

layout_matrix2 <- matrix(c(1,2,3,4), ncol = 2, byrow = TRUE)
grid.arrange(core_sumIASthreat_type2_birds,core_sumIASthreat_type2_mammals,core_sumIASthreat_type2_reptiles,core_sumIASthreat_type2_amphibians,layout_matrix = layout_matrix2) # 1000*800




watch_birds_sumIASthreat_type2 <- aggregate(NoIAS ~ ias_groups, watch_birds_sumIASthreat_type, sum)

if (!(check_amphibian %in% watch_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_amphibian <- data.frame(ias_groups = "amphibian",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_birds_sumIASthreat_type2 <- rbind(watch_birds_sumIASthreat_type2, row_amphibian)
}

if (!(check_bird %in% watch_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_bird <- data.frame(ias_groups = "bird",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_birds_sumIASthreat_type2 <- rbind(watch_birds_sumIASthreat_type2, row_bird)
}

if (!(check_invertebrate %in% watch_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_invertebrate <- data.frame(ias_groups = "invertebrate",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_birds_sumIASthreat_type2 <- rbind(watch_birds_sumIASthreat_type2, row_invertebrate)
}

if (!(check_mammal %in% watch_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_mammal <- data.frame(ias_groups = "mammal",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_birds_sumIASthreat_type2 <- rbind(watch_birds_sumIASthreat_type2, row_mammal)
}

if (!(check_plant %in% watch_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_plant <- data.frame(ias_groups = "plant",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_birds_sumIASthreat_type2 <- rbind(watch_birds_sumIASthreat_type2, row_plant)
}

if (!(check_problematic_native_diseases %in% watch_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_problematic_native_diseases <- data.frame(ias_groups = "problematic_native_diseases",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_birds_sumIASthreat_type2 <- rbind(watch_birds_sumIASthreat_type2, row_problematic_native_diseases)
}

if (!(check_ctenophora %in% watch_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_ctenophora <- data.frame(ias_groups = "ctenophora",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_birds_sumIASthreat_type2 <- rbind(watch_birds_sumIASthreat_type2, row_ctenophora)
}

if (!(check_diseases_unknown_cause %in% watch_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_diseases_unknown_cause <- data.frame(ias_groups = "diseases_unknown_cause",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_birds_sumIASthreat_type2 <- rbind(watch_birds_sumIASthreat_type2, row_diseases_unknown_cause)
}

if (!(check_fish %in% watch_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_fish <- data.frame(ias_groups = "fish",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_birds_sumIASthreat_type2 <- rbind(watch_birds_sumIASthreat_type2, row_fish)
}

if (!(check_parasite %in% watch_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_parasite <- data.frame(ias_groups = "parasite",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_birds_sumIASthreat_type2 <- rbind(watch_birds_sumIASthreat_type2, row_parasite)
}

if (!(check_reptile %in% watch_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_reptile <- data.frame(ias_groups = "reptile",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_birds_sumIASthreat_type2 <- rbind(watch_birds_sumIASthreat_type2, row_reptile)
}

if (!(check_unspecified_spp %in% watch_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_unspecified_spp <- data.frame(ias_groups = "unspecified_spp",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_birds_sumIASthreat_type2 <- rbind(watch_birds_sumIASthreat_type2, row_unspecified_spp)
}

if (!(check_fungus %in% watch_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_fungus <- data.frame(ias_groups = "fungus",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_birds_sumIASthreat_type2 <- rbind(watch_birds_sumIASthreat_type2, row_fungus)
}

if (!(check_introduced %in% watch_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_introduced <- data.frame(ias_groups = "introduced_genetic_material",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_birds_sumIASthreat_type2 <- rbind(watch_birds_sumIASthreat_type2, row_introduced)
}



watch_mammals_sumIASthreat_type2 <- aggregate(NoIAS ~ ias_groups, watch_mammals_sumIASthreat_type, sum)

if (!(check_amphibian %in% watch_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_amphibian <- data.frame(ias_groups = "amphibian",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_mammals_sumIASthreat_type2 <- rbind(watch_mammals_sumIASthreat_type2, row_amphibian)
}

if (!(check_bird %in% watch_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_bird <- data.frame(ias_groups = "bird",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_mammals_sumIASthreat_type2 <- rbind(watch_mammals_sumIASthreat_type2, row_bird)
}

if (!(check_invertebrate %in% watch_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_invertebrate <- data.frame(ias_groups = "invertebrate",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_mammals_sumIASthreat_type2 <- rbind(watch_mammals_sumIASthreat_type2, row_invertebrate)
}

if (!(check_mammal %in% watch_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_mammal <- data.frame(ias_groups = "mammal",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_mammals_sumIASthreat_type2 <- rbind(watch_mammals_sumIASthreat_type2, row_mammal)
}

if (!(check_plant %in% watch_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_plant <- data.frame(ias_groups = "plant",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_mammals_sumIASthreat_type2 <- rbind(watch_mammals_sumIASthreat_type2, row_plant)
}

if (!(check_problematic_native_diseases %in% watch_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_problematic_native_diseases <- data.frame(ias_groups = "problematic_native_diseases",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_mammals_sumIASthreat_type2 <- rbind(watch_mammals_sumIASthreat_type2, row_problematic_native_diseases)
}

if (!(check_ctenophora %in% watch_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_ctenophora <- data.frame(ias_groups = "ctenophora",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_mammals_sumIASthreat_type2 <- rbind(watch_mammals_sumIASthreat_type2, row_ctenophora)
}

if (!(check_diseases_unknown_cause %in% watch_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_diseases_unknown_cause <- data.frame(ias_groups = "diseases_unknown_cause",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_mammals_sumIASthreat_type2 <- rbind(watch_mammals_sumIASthreat_type2, row_diseases_unknown_cause)
}

if (!(check_fish %in% watch_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_fish <- data.frame(ias_groups = "fish",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_mammals_sumIASthreat_type2 <- rbind(watch_mammals_sumIASthreat_type2, row_fish)
}

if (!(check_parasite %in% watch_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_parasite <- data.frame(ias_groups = "parasite",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_mammals_sumIASthreat_type2 <- rbind(watch_mammals_sumIASthreat_type2, row_parasite)
}

if (!(check_reptile %in% watch_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_reptile <- data.frame(ias_groups = "reptile",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_mammals_sumIASthreat_type2 <- rbind(watch_mammals_sumIASthreat_type2, row_reptile)
}

if (!(check_unspecified_spp %in% watch_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_unspecified_spp <- data.frame(ias_groups = "unspecified_spp",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_mammals_sumIASthreat_type2 <- rbind(watch_mammals_sumIASthreat_type2, row_unspecified_spp)
}

if (!(check_fungus %in% watch_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_fungus <- data.frame(ias_groups = "fungus",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_mammals_sumIASthreat_type2 <- rbind(watch_mammals_sumIASthreat_type2, row_fungus)
}

if (!(check_introduced %in% watch_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_introduced <- data.frame(ias_groups = "introduced_genetic_material",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_mammals_sumIASthreat_type2 <- rbind(watch_mammals_sumIASthreat_type2, row_introduced)
}




watch_reptiles_sumIASthreat_type2 <- aggregate(NoIAS ~ ias_groups, watch_reptiles_sumIASthreat_type, sum)

if (!(check_amphibian %in% watch_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_amphibian <- data.frame(ias_groups = "amphibian",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_reptiles_sumIASthreat_type2 <- rbind(watch_reptiles_sumIASthreat_type2, row_amphibian)
}

if (!(check_bird %in% watch_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_bird <- data.frame(ias_groups = "bird",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_reptiles_sumIASthreat_type2 <- rbind(watch_reptiles_sumIASthreat_type2, row_bird)
}

if (!(check_invertebrate %in% watch_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_invertebrate <- data.frame(ias_groups = "invertebrate",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_reptiles_sumIASthreat_type2 <- rbind(watch_reptiles_sumIASthreat_type2, row_invertebrate)
}

if (!(check_mammal %in% watch_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_mammal <- data.frame(ias_groups = "mammal",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_reptiles_sumIASthreat_type2 <- rbind(watch_reptiles_sumIASthreat_type2, row_mammal)
}

if (!(check_plant %in% watch_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_plant <- data.frame(ias_groups = "plant",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_reptiles_sumIASthreat_type2 <- rbind(watch_reptiles_sumIASthreat_type2, row_plant)
}

if (!(check_problematic_native_diseases %in% watch_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_problematic_native_diseases <- data.frame(ias_groups = "problematic_native_diseases",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_reptiles_sumIASthreat_type2 <- rbind(watch_reptiles_sumIASthreat_type2, row_problematic_native_diseases)
}

if (!(check_ctenophora %in% watch_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_ctenophora <- data.frame(ias_groups = "ctenophora",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_reptiles_sumIASthreat_type2 <- rbind(watch_reptiles_sumIASthreat_type2, row_ctenophora)
}

if (!(check_diseases_unknown_cause %in% watch_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_diseases_unknown_cause <- data.frame(ias_groups = "diseases_unknown_cause",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_reptiles_sumIASthreat_type2 <- rbind(watch_reptiles_sumIASthreat_type2, row_diseases_unknown_cause)
}

if (!(check_fish %in% watch_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_fish <- data.frame(ias_groups = "fish",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_reptiles_sumIASthreat_type2 <- rbind(watch_reptiles_sumIASthreat_type2, row_fish)
}

if (!(check_parasite %in% watch_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_parasite <- data.frame(ias_groups = "parasite",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_reptiles_sumIASthreat_type2 <- rbind(watch_reptiles_sumIASthreat_type2, row_parasite)
}

if (!(check_reptile %in% watch_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_reptile <- data.frame(ias_groups = "reptile",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_reptiles_sumIASthreat_type2 <- rbind(watch_reptiles_sumIASthreat_type2, row_reptile)
}

if (!(check_unspecified_spp %in% watch_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_unspecified_spp <- data.frame(ias_groups = "unspecified_spp",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_reptiles_sumIASthreat_type2 <- rbind(watch_reptiles_sumIASthreat_type2, row_unspecified_spp)
}

if (!(check_fungus %in% watch_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_fungus <- data.frame(ias_groups = "fungus",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_reptiles_sumIASthreat_type2 <- rbind(watch_reptiles_sumIASthreat_type2, row_fungus)
}

if (!(check_introduced %in% watch_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_introduced <- data.frame(ias_groups = "introduced_genetic_material",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_reptiles_sumIASthreat_type2 <- rbind(watch_reptiles_sumIASthreat_type2, row_introduced)
}




watch_amphibians_sumIASthreat_type2 <- aggregate(NoIAS ~ ias_groups, watch_amphibians_sumIASthreat_type, sum)

if (!(check_amphibian %in% watch_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_amphibian <- data.frame(ias_groups = "amphibian",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_amphibians_sumIASthreat_type2 <- rbind(watch_amphibians_sumIASthreat_type2, row_amphibian)
}

if (!(check_bird %in% watch_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_bird <- data.frame(ias_groups = "bird",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_amphibians_sumIASthreat_type2 <- rbind(watch_amphibians_sumIASthreat_type2, row_bird)
}

if (!(check_invertebrate %in% watch_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_invertebrate <- data.frame(ias_groups = "invertebrate",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_amphibians_sumIASthreat_type2 <- rbind(watch_amphibians_sumIASthreat_type2, row_invertebrate)
}

if (!(check_mammal %in% watch_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_mammal <- data.frame(ias_groups = "mammal",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_amphibians_sumIASthreat_type2 <- rbind(watch_amphibians_sumIASthreat_type2, row_mammal)
}

if (!(check_plant %in% watch_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_plant <- data.frame(ias_groups = "plant",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_amphibians_sumIASthreat_type2 <- rbind(watch_amphibians_sumIASthreat_type2, row_plant)
}

if (!(check_problematic_native_diseases %in% watch_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_problematic_native_diseases <- data.frame(ias_groups = "problematic_native_diseases",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_amphibians_sumIASthreat_type2 <- rbind(watch_amphibians_sumIASthreat_type2, row_problematic_native_diseases)
}

if (!(check_ctenophora %in% watch_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_ctenophora <- data.frame(ias_groups = "ctenophora",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_amphibians_sumIASthreat_type2 <- rbind(watch_amphibians_sumIASthreat_type2, row_ctenophora)
}

if (!(check_diseases_unknown_cause %in% watch_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_diseases_unknown_cause <- data.frame(ias_groups = "diseases_unknown_cause",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_amphibians_sumIASthreat_type2 <- rbind(watch_amphibians_sumIASthreat_type2, row_diseases_unknown_cause)
}

if (!(check_fish %in% watch_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_fish <- data.frame(ias_groups = "fish",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_amphibians_sumIASthreat_type2 <- rbind(watch_amphibians_sumIASthreat_type2, row_fish)
}

if (!(check_parasite %in% watch_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_parasite <- data.frame(ias_groups = "parasite",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_amphibians_sumIASthreat_type2 <- rbind(watch_amphibians_sumIASthreat_type2, row_parasite)
}

if (!(check_reptile %in% watch_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_reptile <- data.frame(ias_groups = "reptile",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_amphibians_sumIASthreat_type2 <- rbind(watch_amphibians_sumIASthreat_type2, row_reptile)
}

if (!(check_unspecified_spp %in% watch_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_unspecified_spp <- data.frame(ias_groups = "unspecified_spp",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_amphibians_sumIASthreat_type2 <- rbind(watch_amphibians_sumIASthreat_type2, row_unspecified_spp)
}

if (!(check_fungus %in% watch_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_fungus <- data.frame(ias_groups = "fungus",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_amphibians_sumIASthreat_type2 <- rbind(watch_amphibians_sumIASthreat_type2, row_fungus)
}

if (!(check_introduced %in% watch_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_introduced <- data.frame(ias_groups = "introduced_genetic_material",NoIAS = 0)
  # Add the new row to the existing data frame
  watch_amphibians_sumIASthreat_type2 <- rbind(watch_amphibians_sumIASthreat_type2, row_introduced)
}


watch_sumIASthreat_type2 <- merge(merge(merge(watch_birds_sumIASthreat_type2,watch_mammals_sumIASthreat_type2, by = "ias_groups",suffixes = c("_birds", "_mammals"),all = TRUE),watch_reptiles_sumIASthreat_type2,by = "ias_groups",suffixes = c("_", "_reptiles"),all = TRUE),watch_amphibians_sumIASthreat_type2,by = "ias_groups",suffixes = c("_", "_amphibians"),all = TRUE)
colnames(watch_sumIASthreat_type2)[colnames(watch_sumIASthreat_type2) == "NoIAS_"] <- "NoIAS_reptiles"

watch_sumIASthreat_type2$ias_groups <- factor(watch_sumIASthreat_type2$ias_groups, levels = c("introduced_genetic_material","diseases_unknown_cause","problematic_native_diseases","unspecified_spp","plant","mammal","bird","reptile","amphibian","fish","invertebrate","ctenophora","fungus","parasite"))

watch_sumIASthreat_type2_birds <- ggplot(watch_sumIASthreat_type2, aes(x=ias_groups, y=NoIAS_birds)) + geom_bar(stat = "identity") + geom_text(aes(label = NoIAS_birds), hjust = -0.5, size = 3) + coord_flip() + ggtitle("watch birds")
watch_sumIASthreat_type2_mammals <- ggplot(watch_sumIASthreat_type2, aes(x=ias_groups, y=NoIAS_mammals)) + geom_bar(stat = "identity") + geom_text(aes(label = NoIAS_mammals), hjust = -0.5, size = 3) + coord_flip() + ggtitle("watch mammals")
watch_sumIASthreat_type2_reptiles <- ggplot(watch_sumIASthreat_type2, aes(x=ias_groups, y=NoIAS_reptiles)) + geom_bar(stat = "identity") + geom_text(aes(label = NoIAS_reptiles), hjust = -0.5, size = 3) +  coord_flip() + ggtitle("watch reptiles")
watch_sumIASthreat_type2_amphibians <- ggplot(watch_sumIASthreat_type2, aes(x=ias_groups, y=NoIAS_amphibians)) + geom_bar(stat = "identity") + geom_text(aes(label = NoIAS_amphibians), hjust = -0.5, size = 3) +  coord_flip() + ggtitle("watch amphibians")

grid.arrange(watch_sumIASthreat_type2_birds,watch_sumIASthreat_type2_mammals,watch_sumIASthreat_type2_reptiles,watch_sumIASthreat_type2_amphibians,layout_matrix = layout_matrix2) # 1000*800




border_birds_sumIASthreat_type2 <- aggregate(NoIAS ~ ias_groups, border_birds_sumIASthreat_type, sum)

if (!(check_amphibian %in% border_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_amphibian <- data.frame(ias_groups = "amphibian",NoIAS = 0)
  # Add the new row to the existing data frame
  border_birds_sumIASthreat_type2 <- rbind(border_birds_sumIASthreat_type2, row_amphibian)
}

if (!(check_bird %in% border_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_bird <- data.frame(ias_groups = "bird",NoIAS = 0)
  # Add the new row to the existing data frame
  border_birds_sumIASthreat_type2 <- rbind(border_birds_sumIASthreat_type2, row_bird)
}

if (!(check_invertebrate %in% border_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_invertebrate <- data.frame(ias_groups = "invertebrate",NoIAS = 0)
  # Add the new row to the existing data frame
  border_birds_sumIASthreat_type2 <- rbind(border_birds_sumIASthreat_type2, row_invertebrate)
}

if (!(check_mammal %in% border_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_mammal <- data.frame(ias_groups = "mammal",NoIAS = 0)
  # Add the new row to the existing data frame
  border_birds_sumIASthreat_type2 <- rbind(border_birds_sumIASthreat_type2, row_mammal)
}

if (!(check_plant %in% border_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_plant <- data.frame(ias_groups = "plant",NoIAS = 0)
  # Add the new row to the existing data frame
  border_birds_sumIASthreat_type2 <- rbind(border_birds_sumIASthreat_type2, row_plant)
}

if (!(check_problematic_native_diseases %in% border_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_problematic_native_diseases <- data.frame(ias_groups = "problematic_native_diseases",NoIAS = 0)
  # Add the new row to the existing data frame
  border_birds_sumIASthreat_type2 <- rbind(border_birds_sumIASthreat_type2, row_problematic_native_diseases)
}

if (!(check_ctenophora %in% border_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_ctenophora <- data.frame(ias_groups = "ctenophora",NoIAS = 0)
  # Add the new row to the existing data frame
  border_birds_sumIASthreat_type2 <- rbind(border_birds_sumIASthreat_type2, row_ctenophora)
}

if (!(check_diseases_unknown_cause %in% border_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_diseases_unknown_cause <- data.frame(ias_groups = "diseases_unknown_cause",NoIAS = 0)
  # Add the new row to the existing data frame
  border_birds_sumIASthreat_type2 <- rbind(border_birds_sumIASthreat_type2, row_diseases_unknown_cause)
}

if (!(check_fish %in% border_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_fish <- data.frame(ias_groups = "fish",NoIAS = 0)
  # Add the new row to the existing data frame
  border_birds_sumIASthreat_type2 <- rbind(border_birds_sumIASthreat_type2, row_fish)
}

if (!(check_parasite %in% border_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_parasite <- data.frame(ias_groups = "parasite",NoIAS = 0)
  # Add the new row to the existing data frame
  border_birds_sumIASthreat_type2 <- rbind(border_birds_sumIASthreat_type2, row_parasite)
}

if (!(check_reptile %in% border_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_reptile <- data.frame(ias_groups = "reptile",NoIAS = 0)
  # Add the new row to the existing data frame
  border_birds_sumIASthreat_type2 <- rbind(border_birds_sumIASthreat_type2, row_reptile)
}

if (!(check_unspecified_spp %in% border_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_unspecified_spp <- data.frame(ias_groups = "unspecified_spp",NoIAS = 0)
  # Add the new row to the existing data frame
  border_birds_sumIASthreat_type2 <- rbind(border_birds_sumIASthreat_type2, row_unspecified_spp)
}

if (!(check_fungus %in% border_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_fungus <- data.frame(ias_groups = "fungus",NoIAS = 0)
  # Add the new row to the existing data frame
  border_birds_sumIASthreat_type2 <- rbind(border_birds_sumIASthreat_type2, row_fungus)
}

if (!(check_introduced %in% border_birds_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_introduced <- data.frame(ias_groups = "introduced_genetic_material",NoIAS = 0)
  # Add the new row to the existing data frame
  border_birds_sumIASthreat_type2 <- rbind(border_birds_sumIASthreat_type2, row_introduced)
}



border_mammals_sumIASthreat_type2 <- aggregate(NoIAS ~ ias_groups, border_mammals_sumIASthreat_type, sum)

if (!(check_amphibian %in% border_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_amphibian <- data.frame(ias_groups = "amphibian",NoIAS = 0)
  # Add the new row to the existing data frame
  border_mammals_sumIASthreat_type2 <- rbind(border_mammals_sumIASthreat_type2, row_amphibian)
}

if (!(check_bird %in% border_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_bird <- data.frame(ias_groups = "bird",NoIAS = 0)
  # Add the new row to the existing data frame
  border_mammals_sumIASthreat_type2 <- rbind(border_mammals_sumIASthreat_type2, row_bird)
}

if (!(check_invertebrate %in% border_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_invertebrate <- data.frame(ias_groups = "invertebrate",NoIAS = 0)
  # Add the new row to the existing data frame
  border_mammals_sumIASthreat_type2 <- rbind(border_mammals_sumIASthreat_type2, row_invertebrate)
}

if (!(check_mammal %in% border_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_mammal <- data.frame(ias_groups = "mammal",NoIAS = 0)
  # Add the new row to the existing data frame
  border_mammals_sumIASthreat_type2 <- rbind(border_mammals_sumIASthreat_type2, row_mammal)
}

if (!(check_plant %in% border_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_plant <- data.frame(ias_groups = "plant",NoIAS = 0)
  # Add the new row to the existing data frame
  border_mammals_sumIASthreat_type2 <- rbind(border_mammals_sumIASthreat_type2, row_plant)
}

if (!(check_problematic_native_diseases %in% border_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_problematic_native_diseases <- data.frame(ias_groups = "problematic_native_diseases",NoIAS = 0)
  # Add the new row to the existing data frame
  border_mammals_sumIASthreat_type2 <- rbind(border_mammals_sumIASthreat_type2, row_problematic_native_diseases)
}

if (!(check_ctenophora %in% border_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_ctenophora <- data.frame(ias_groups = "ctenophora",NoIAS = 0)
  # Add the new row to the existing data frame
  border_mammals_sumIASthreat_type2 <- rbind(border_mammals_sumIASthreat_type2, row_ctenophora)
}

if (!(check_diseases_unknown_cause %in% border_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_diseases_unknown_cause <- data.frame(ias_groups = "diseases_unknown_cause",NoIAS = 0)
  # Add the new row to the existing data frame
  border_mammals_sumIASthreat_type2 <- rbind(border_mammals_sumIASthreat_type2, row_diseases_unknown_cause)
}

if (!(check_fish %in% border_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_fish <- data.frame(ias_groups = "fish",NoIAS = 0)
  # Add the new row to the existing data frame
  border_mammals_sumIASthreat_type2 <- rbind(border_mammals_sumIASthreat_type2, row_fish)
}

if (!(check_parasite %in% border_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_parasite <- data.frame(ias_groups = "parasite",NoIAS = 0)
  # Add the new row to the existing data frame
  border_mammals_sumIASthreat_type2 <- rbind(border_mammals_sumIASthreat_type2, row_parasite)
}

if (!(check_reptile %in% border_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_reptile <- data.frame(ias_groups = "reptile",NoIAS = 0)
  # Add the new row to the existing data frame
  border_mammals_sumIASthreat_type2 <- rbind(border_mammals_sumIASthreat_type2, row_reptile)
}

if (!(check_unspecified_spp %in% border_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_unspecified_spp <- data.frame(ias_groups = "unspecified_spp",NoIAS = 0)
  # Add the new row to the existing data frame
  border_mammals_sumIASthreat_type2 <- rbind(border_mammals_sumIASthreat_type2, row_unspecified_spp)
}

if (!(check_fungus %in% border_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_fungus <- data.frame(ias_groups = "fungus",NoIAS = 0)
  # Add the new row to the existing data frame
  border_mammals_sumIASthreat_type2 <- rbind(border_mammals_sumIASthreat_type2, row_fungus)
}

if (!(check_introduced %in% border_mammals_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_introduced <- data.frame(ias_groups = "introduced_genetic_material",NoIAS = 0)
  # Add the new row to the existing data frame
  border_mammals_sumIASthreat_type2 <- rbind(border_mammals_sumIASthreat_type2, row_introduced)
}




border_reptiles_sumIASthreat_type2 <- aggregate(NoIAS ~ ias_groups, border_reptiles_sumIASthreat_type, sum)

if (!(check_amphibian %in% border_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_amphibian <- data.frame(ias_groups = "amphibian",NoIAS = 0)
  # Add the new row to the existing data frame
  border_reptiles_sumIASthreat_type2 <- rbind(border_reptiles_sumIASthreat_type2, row_amphibian)
}

if (!(check_bird %in% border_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_bird <- data.frame(ias_groups = "bird",NoIAS = 0)
  # Add the new row to the existing data frame
  border_reptiles_sumIASthreat_type2 <- rbind(border_reptiles_sumIASthreat_type2, row_bird)
}

if (!(check_invertebrate %in% border_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_invertebrate <- data.frame(ias_groups = "invertebrate",NoIAS = 0)
  # Add the new row to the existing data frame
  border_reptiles_sumIASthreat_type2 <- rbind(border_reptiles_sumIASthreat_type2, row_invertebrate)
}

if (!(check_mammal %in% border_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_mammal <- data.frame(ias_groups = "mammal",NoIAS = 0)
  # Add the new row to the existing data frame
  border_reptiles_sumIASthreat_type2 <- rbind(border_reptiles_sumIASthreat_type2, row_mammal)
}

if (!(check_plant %in% border_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_plant <- data.frame(ias_groups = "plant",NoIAS = 0)
  # Add the new row to the existing data frame
  border_reptiles_sumIASthreat_type2 <- rbind(border_reptiles_sumIASthreat_type2, row_plant)
}

if (!(check_problematic_native_diseases %in% border_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_problematic_native_diseases <- data.frame(ias_groups = "problematic_native_diseases",NoIAS = 0)
  # Add the new row to the existing data frame
  border_reptiles_sumIASthreat_type2 <- rbind(border_reptiles_sumIASthreat_type2, row_problematic_native_diseases)
}

if (!(check_ctenophora %in% border_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_ctenophora <- data.frame(ias_groups = "ctenophora",NoIAS = 0)
  # Add the new row to the existing data frame
  border_reptiles_sumIASthreat_type2 <- rbind(border_reptiles_sumIASthreat_type2, row_ctenophora)
}

if (!(check_diseases_unknown_cause %in% border_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_diseases_unknown_cause <- data.frame(ias_groups = "diseases_unknown_cause",NoIAS = 0)
  # Add the new row to the existing data frame
  border_reptiles_sumIASthreat_type2 <- rbind(border_reptiles_sumIASthreat_type2, row_diseases_unknown_cause)
}

if (!(check_fish %in% border_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_fish <- data.frame(ias_groups = "fish",NoIAS = 0)
  # Add the new row to the existing data frame
  border_reptiles_sumIASthreat_type2 <- rbind(border_reptiles_sumIASthreat_type2, row_fish)
}

if (!(check_parasite %in% border_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_parasite <- data.frame(ias_groups = "parasite",NoIAS = 0)
  # Add the new row to the existing data frame
  border_reptiles_sumIASthreat_type2 <- rbind(border_reptiles_sumIASthreat_type2, row_parasite)
}

if (!(check_reptile %in% border_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_reptile <- data.frame(ias_groups = "reptile",NoIAS = 0)
  # Add the new row to the existing data frame
  border_reptiles_sumIASthreat_type2 <- rbind(border_reptiles_sumIASthreat_type2, row_reptile)
}

if (!(check_unspecified_spp %in% border_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_unspecified_spp <- data.frame(ias_groups = "unspecified_spp",NoIAS = 0)
  # Add the new row to the existing data frame
  border_reptiles_sumIASthreat_type2 <- rbind(border_reptiles_sumIASthreat_type2, row_unspecified_spp)
}

if (!(check_fungus %in% border_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_fungus <- data.frame(ias_groups = "fungus",NoIAS = 0)
  # Add the new row to the existing data frame
  border_reptiles_sumIASthreat_type2 <- rbind(border_reptiles_sumIASthreat_type2, row_fungus)
}

if (!(check_introduced %in% border_reptiles_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_introduced <- data.frame(ias_groups = "introduced_genetic_material",NoIAS = 0)
  # Add the new row to the existing data frame
  border_reptiles_sumIASthreat_type2 <- rbind(border_reptiles_sumIASthreat_type2, row_introduced)
}




border_amphibians_sumIASthreat_type2 <- aggregate(NoIAS ~ ias_groups, border_amphibians_sumIASthreat_type, sum)

if (!(check_amphibian %in% border_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_amphibian <- data.frame(ias_groups = "amphibian",NoIAS = 0)
  # Add the new row to the existing data frame
  border_amphibians_sumIASthreat_type2 <- rbind(border_amphibians_sumIASthreat_type2, row_amphibian)
}

if (!(check_bird %in% border_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_bird <- data.frame(ias_groups = "bird",NoIAS = 0)
  # Add the new row to the existing data frame
  border_amphibians_sumIASthreat_type2 <- rbind(border_amphibians_sumIASthreat_type2, row_bird)
}

if (!(check_invertebrate %in% border_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_invertebrate <- data.frame(ias_groups = "invertebrate",NoIAS = 0)
  # Add the new row to the existing data frame
  border_amphibians_sumIASthreat_type2 <- rbind(border_amphibians_sumIASthreat_type2, row_invertebrate)
}

if (!(check_mammal %in% border_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_mammal <- data.frame(ias_groups = "mammal",NoIAS = 0)
  # Add the new row to the existing data frame
  border_amphibians_sumIASthreat_type2 <- rbind(border_amphibians_sumIASthreat_type2, row_mammal)
}

if (!(check_plant %in% border_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_plant <- data.frame(ias_groups = "plant",NoIAS = 0)
  # Add the new row to the existing data frame
  border_amphibians_sumIASthreat_type2 <- rbind(border_amphibians_sumIASthreat_type2, row_plant)
}

if (!(check_problematic_native_diseases %in% border_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_problematic_native_diseases <- data.frame(ias_groups = "problematic_native_diseases",NoIAS = 0)
  # Add the new row to the existing data frame
  border_amphibians_sumIASthreat_type2 <- rbind(border_amphibians_sumIASthreat_type2, row_problematic_native_diseases)
}

if (!(check_ctenophora %in% border_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_ctenophora <- data.frame(ias_groups = "ctenophora",NoIAS = 0)
  # Add the new row to the existing data frame
  border_amphibians_sumIASthreat_type2 <- rbind(border_amphibians_sumIASthreat_type2, row_ctenophora)
}

if (!(check_diseases_unknown_cause %in% border_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_diseases_unknown_cause <- data.frame(ias_groups = "diseases_unknown_cause",NoIAS = 0)
  # Add the new row to the existing data frame
  border_amphibians_sumIASthreat_type2 <- rbind(border_amphibians_sumIASthreat_type2, row_diseases_unknown_cause)
}

if (!(check_fish %in% border_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_fish <- data.frame(ias_groups = "fish",NoIAS = 0)
  # Add the new row to the existing data frame
  border_amphibians_sumIASthreat_type2 <- rbind(border_amphibians_sumIASthreat_type2, row_fish)
}

if (!(check_parasite %in% border_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_parasite <- data.frame(ias_groups = "parasite",NoIAS = 0)
  # Add the new row to the existing data frame
  border_amphibians_sumIASthreat_type2 <- rbind(border_amphibians_sumIASthreat_type2, row_parasite)
}

if (!(check_reptile %in% border_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_reptile <- data.frame(ias_groups = "reptile",NoIAS = 0)
  # Add the new row to the existing data frame
  border_amphibians_sumIASthreat_type2 <- rbind(border_amphibians_sumIASthreat_type2, row_reptile)
}

if (!(check_unspecified_spp %in% border_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_unspecified_spp <- data.frame(ias_groups = "unspecified_spp",NoIAS = 0)
  # Add the new row to the existing data frame
  border_amphibians_sumIASthreat_type2 <- rbind(border_amphibians_sumIASthreat_type2, row_unspecified_spp)
}

if (!(check_fungus %in% border_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_fungus <- data.frame(ias_groups = "fungus",NoIAS = 0)
  # Add the new row to the existing data frame
  border_amphibians_sumIASthreat_type2 <- rbind(border_amphibians_sumIASthreat_type2, row_fungus)
}

if (!(check_introduced %in% border_amphibians_sumIASthreat_type2$ias_groups)) {
  # Add a new row with the specified value
  row_introduced <- data.frame(ias_groups = "introduced_genetic_material",NoIAS = 0)
  # Add the new row to the existing data frame
  border_amphibians_sumIASthreat_type2 <- rbind(border_amphibians_sumIASthreat_type2, row_introduced)
}


border_sumIASthreat_type2 <- merge(merge(merge(border_birds_sumIASthreat_type2,border_mammals_sumIASthreat_type2, by = "ias_groups",suffixes = c("_birds", "_mammals"),all = TRUE),border_reptiles_sumIASthreat_type2,by = "ias_groups",suffixes = c("_", "_reptiles"),all = TRUE),border_amphibians_sumIASthreat_type2,by = "ias_groups",suffixes = c("_", "_amphibians"),all = TRUE)
colnames(border_sumIASthreat_type2)[colnames(border_sumIASthreat_type2) == "NoIAS_"] <- "NoIAS_reptiles"

border_sumIASthreat_type2$ias_groups <- factor(border_sumIASthreat_type2$ias_groups, levels = c("introduced_genetic_material","diseases_unknown_cause","problematic_native_diseases","unspecified_spp","plant","mammal","bird","reptile","amphibian","fish","invertebrate","ctenophora","fungus","parasite"))

border_sumIASthreat_type2_birds <- ggplot(border_sumIASthreat_type2, aes(x=ias_groups, y=NoIAS_birds)) + geom_bar(stat = "identity") + geom_text(aes(label = NoIAS_birds), hjust = -0.5, size = 3) + coord_flip() + ggtitle("border birds")
border_sumIASthreat_type2_mammals <- ggplot(border_sumIASthreat_type2, aes(x=ias_groups, y=NoIAS_mammals)) + geom_bar(stat = "identity") + geom_text(aes(label = NoIAS_mammals), hjust = -0.5, size = 3) + coord_flip() + ggtitle("border mammals")
border_sumIASthreat_type2_reptiles <- ggplot(border_sumIASthreat_type2, aes(x=ias_groups, y=NoIAS_reptiles)) + geom_bar(stat = "identity") + geom_text(aes(label = NoIAS_reptiles), hjust = -0.5, size = 3) +  coord_flip() + ggtitle("border reptiles")
border_sumIASthreat_type2_amphibians <- ggplot(border_sumIASthreat_type2, aes(x=ias_groups, y=NoIAS_amphibians)) + geom_bar(stat = "identity") + geom_text(aes(label = NoIAS_amphibians), hjust = -0.5, size = 3) +  coord_flip() + ggtitle("border amphibians")

grid.arrange(border_sumIASthreat_type2_birds,border_sumIASthreat_type2_mammals,border_sumIASthreat_type2_reptiles,border_sumIASthreat_type2_amphibians,layout_matrix = layout_matrix2) # 1000*800


# check if the fungus for amphibians is the chytrid
table((core_amphibians_IASthreat %>% filter(ias_groups == "fungus"))$ias)




# Distribution of FUSE-IAS scores per IAS group
# core_birds_sumIASthreat_score2 <- merge(core_birds_sumIASthreat2, core_list_birds, by = "binomial", all.x = TRUE)
# core_birds_sumIASthreat_score2$ias_groups <- factor(core_birds_sumIASthreat_score2$ias_groups, levels = c("DiseasesUnknownCause","ProblematicNativeDiseases","UnspecifiedSpp","mammal","bird","plant","invertebrate","parasite","reptile","fish","fungus"))
# ggplot(core_birds_sumIASthreat_score2, aes(x=as.factor(ias_groups), y=FUSE_IAS_med)) + 
#   geom_boxplot(fill="gray", alpha=0.2) + 
#   xlab("IAS group") + geom_jitter(color="black", size=1.5, alpha=1) + coord_flip()




### 4. No. and identity of other threats (besides IAS) associated with CORE species 

## Keep only rows related to other threats
core_birds_otherthreats <- core_birds_threats[core_birds_threats$threat %in% c("1","2","3","4","5","6","7","9","10","11","12"), ]
core_mammals_otherthreats <- core_mammals_threats[core_mammals_threats$threat %in% c("1","2","3","4","5","6","7","9","10","11","12"), ]
core_reptiles_otherthreats <- core_reptiles_threats[core_reptiles_threats$threat %in% c("1","2","3","4","5","6","7","9","10","11","12"), ]
core_amphibians_otherthreats <- core_amphibians_threats[core_amphibians_threats$threat %in% c("1","2","3","4","5","6","7","9","10","11","12"), ]

watch_birds_otherthreats <- watch_birds_threats[watch_birds_threats$threat %in% c("1","2","3","4","5","6","7","9","10","11","12"), ]
watch_mammals_otherthreats <- watch_mammals_threats[watch_mammals_threats$threat %in% c("1","2","3","4","5","6","7","9","10","11","12"), ]
watch_reptiles_otherthreats <- watch_reptiles_threats[watch_reptiles_threats$threat %in% c("1","2","3","4","5","6","7","9","10","11","12"), ]
watch_amphibians_otherthreats <- watch_amphibians_threats[watch_amphibians_threats$threat %in% c("1","2","3","4","5","6","7","9","10","11","12"), ]

border_birds_otherthreats <- border_birds_threats[border_birds_threats$threat %in% c("1","2","3","4","5","6","7","9","10","11","12"), ]
border_mammals_otherthreats <- border_mammals_threats[border_mammals_threats$threat %in% c("1","2","3","4","5","6","7","9","10","11","12"), ]
border_reptiles_otherthreats <- border_reptiles_threats[border_reptiles_threats$threat %in% c("1","2","3","4","5","6","7","9","10","11","12"), ]
border_amphibians_otherthreats <- border_amphibians_threats[border_amphibians_threats$threat %in% c("1","2","3","4","5","6","7","9","10","11","12"), ]


# keep only one row per Other Threat
# right now core_birds_otherthreats has more because some species are threatened by 2 or more subthreats of the same threat (11.1 & 11.2)
core_birds_otherthreats2 <- core_birds_otherthreats %>% distinct(threat, binomial, .keep_all = TRUE)
core_mammals_otherthreats2 <- core_mammals_otherthreats %>% distinct(threat, binomial, .keep_all = TRUE)
core_reptiles_otherthreats2 <- core_reptiles_otherthreats %>% distinct(threat, binomial, .keep_all = TRUE)
core_amphibians_otherthreats2 <- core_amphibians_otherthreats %>% distinct(threat, binomial, .keep_all = TRUE)

watch_birds_otherthreats2 <- watch_birds_otherthreats %>% distinct(threat, binomial, .keep_all = TRUE)
watch_mammals_otherthreats2 <- watch_mammals_otherthreats %>% distinct(threat, binomial, .keep_all = TRUE)
watch_reptiles_otherthreats2 <- watch_reptiles_otherthreats %>% distinct(threat, binomial, .keep_all = TRUE)
watch_amphibians_otherthreats2 <- watch_amphibians_otherthreats %>% distinct(threat, binomial, .keep_all = TRUE)

border_birds_otherthreats2 <- border_birds_otherthreats %>% distinct(threat, binomial, .keep_all = TRUE)
border_mammals_otherthreats2 <- border_mammals_otherthreats %>% distinct(threat, binomial, .keep_all = TRUE)
border_reptiles_otherthreats2 <- border_reptiles_otherthreats %>% distinct(threat, binomial, .keep_all = TRUE)
border_amphibians_otherthreats2 <- border_amphibians_otherthreats %>% distinct(threat, binomial, .keep_all = TRUE)


## Count no. other threats of each species
core_birds_sumotherthreat <- core_birds_otherthreats2 %>% count(binomial)
names(core_birds_sumotherthreat)[names(core_birds_sumotherthreat) == "n"] <- "NoOtherThreats"
# Histogram no. species x no. other threats 
n_distinct(core_birds_sumotherthreat$binomial) # 315 species
core_birds_sumotherthreat_hist <- ggplot(data = core_birds_sumotherthreat, aes(x = NoOtherThreats)) + 
  geom_histogram(binwidth = 0.5, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) +  
  labs(title = "CORE birds", x = "No. other threats", y = "No. bird species") + coord_flip()

core_mammals_sumotherthreat <- core_mammals_otherthreats2 %>% count(binomial)
names(core_mammals_sumotherthreat)[names(core_mammals_sumotherthreat) == "n"] <- "NoOtherThreats"
# Histogram no. species x no. other threats 
n_distinct(core_mammals_sumotherthreat$binomial) 
core_mammals_sumotherthreat_hist <- ggplot(data = core_mammals_sumotherthreat, aes(x = NoOtherThreats)) + 
  geom_histogram(binwidth = 0.5, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) +  
  labs(title = "CORE mammals", x = "No. other threats", y = "No. bird species") + coord_flip()

core_reptiles_sumotherthreat <- core_reptiles_otherthreats2 %>% count(binomial)
names(core_reptiles_sumotherthreat)[names(core_reptiles_sumotherthreat) == "n"] <- "NoOtherThreats"
# Histogram no. species x no. other threats 
n_distinct(core_reptiles_sumotherthreat$binomial) 
core_reptiles_sumotherthreat_hist <- ggplot(data = core_reptiles_sumotherthreat, aes(x = NoOtherThreats)) + 
  geom_histogram(binwidth = 0.5, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) +  
  labs(title = "CORE reptiles", x = "No. other threats", y = "No. bird species") + coord_flip()

core_amphibians_sumotherthreat <- core_amphibians_otherthreats2 %>% count(binomial)
names(core_amphibians_sumotherthreat)[names(core_amphibians_sumotherthreat) == "n"] <- "NoOtherThreats"
# Histogram no. species x no. other threats 
n_distinct(core_amphibians_sumotherthreat$binomial) 
core_amphibians_sumotherthreat_hist <- ggplot(data = core_amphibians_sumotherthreat, aes(x = NoOtherThreats)) + 
  geom_histogram(binwidth = 0.5, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) +  
  labs(title = "CORE amphibians", x = "No. other threats", y = "No. bird species") + coord_flip()

grid.arrange(core_birds_sumotherthreat_hist,core_mammals_sumotherthreat_hist,core_reptiles_sumotherthreat_hist,core_amphibians_sumotherthreat_hist,layout_matrix = layout_matrix2) # 1000*800



watch_birds_sumotherthreat <- watch_birds_otherthreats2 %>% count(binomial)
names(watch_birds_sumotherthreat)[names(watch_birds_sumotherthreat) == "n"] <- "NoOtherThreats"
# Histogram no. species x no. other threats 
n_distinct(watch_birds_sumotherthreat$binomial) # 16 species
watch_birds_sumotherthreat_hist <- ggplot(data = watch_birds_sumotherthreat, aes(x = NoOtherThreats)) + 
  geom_histogram(binwidth = 0.5, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 1)) +  
  labs(title = "watch birds", x = "No. other threats", y = "No. bird species") + coord_flip()

watch_mammals_sumotherthreat <- watch_mammals_otherthreats2 %>% count(binomial)
names(watch_mammals_sumotherthreat)[names(watch_mammals_sumotherthreat) == "n"] <- "NoOtherThreats"
# Histogram no. species x no. other threats 
n_distinct(watch_mammals_sumotherthreat$binomial) 
watch_mammals_sumotherthreat_hist <- ggplot(data = watch_mammals_sumotherthreat, aes(x = NoOtherThreats)) + 
  geom_histogram(binwidth = 0.5, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 1)) +  
  labs(title = "watch mammals", x = "No. other threats", y = "No. bird species") + coord_flip()

watch_reptiles_sumotherthreat <- watch_reptiles_otherthreats2 %>% count(binomial)
names(watch_reptiles_sumotherthreat)[names(watch_reptiles_sumotherthreat) == "n"] <- "NoOtherThreats"
# Histogram no. species x no. other threats 
n_distinct(watch_reptiles_sumotherthreat$binomial) 
watch_reptiles_sumotherthreat_hist <- ggplot(data = watch_reptiles_sumotherthreat, aes(x = NoOtherThreats)) + 
  geom_histogram(binwidth = 0.5, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 1)) +  
  labs(title = "watch reptiles", x = "No. other threats", y = "No. bird species") + coord_flip()

watch_amphibians_sumotherthreat <- watch_amphibians_otherthreats2 %>% count(binomial)
names(watch_amphibians_sumotherthreat)[names(watch_amphibians_sumotherthreat) == "n"] <- "NoOtherThreats"
# Histogram no. species x no. other threats 
n_distinct(watch_amphibians_sumotherthreat$binomial) 
watch_amphibians_sumotherthreat_hist <- ggplot(data = watch_amphibians_sumotherthreat, aes(x = NoOtherThreats)) + 
  geom_histogram(binwidth = 0.5, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 1)) +  
  labs(title = "watch amphibians", x = "No. other threats", y = "No. bird species") + coord_flip()

grid.arrange(watch_birds_sumotherthreat_hist,watch_mammals_sumotherthreat_hist,watch_reptiles_sumotherthreat_hist,watch_amphibians_sumotherthreat_hist,layout_matrix = layout_matrix2) # 1000*800


border_birds_sumotherthreat <- border_birds_otherthreats2 %>% count(binomial)
names(border_birds_sumotherthreat)[names(border_birds_sumotherthreat) == "n"] <- "NoOtherThreats"
# Histogram no. species x no. other threats 
n_distinct(border_birds_sumotherthreat$binomial) # 315 species
border_birds_sumotherthreat_hist <- ggplot(data = border_birds_sumotherthreat, aes(x = NoOtherThreats)) + 
  geom_histogram(binwidth = 0.5, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) +  
  labs(title = "border birds", x = "No. other threats", y = "No. bird species") + coord_flip()

border_mammals_sumotherthreat <- border_mammals_otherthreats2 %>% count(binomial)
names(border_mammals_sumotherthreat)[names(border_mammals_sumotherthreat) == "n"] <- "NoOtherThreats"
# Histogram no. species x no. other threats 
n_distinct(border_mammals_sumotherthreat$binomial) 
border_mammals_sumotherthreat_hist <- ggplot(data = border_mammals_sumotherthreat, aes(x = NoOtherThreats)) + 
  geom_histogram(binwidth = 0.5, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) +  
  labs(title = "border mammals", x = "No. other threats", y = "No. bird species") + coord_flip()

border_reptiles_sumotherthreat <- border_reptiles_otherthreats2 %>% count(binomial)
names(border_reptiles_sumotherthreat)[names(border_reptiles_sumotherthreat) == "n"] <- "NoOtherThreats"
# Histogram no. species x no. other threats 
n_distinct(border_reptiles_sumotherthreat$binomial) 
border_reptiles_sumotherthreat_hist <- ggplot(data = border_reptiles_sumotherthreat, aes(x = NoOtherThreats)) + 
  geom_histogram(binwidth = 0.5, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) +  
  labs(title = "border reptiles", x = "No. other threats", y = "No. bird species") + coord_flip()

border_amphibians_sumotherthreat <- border_amphibians_otherthreats2 %>% count(binomial)
names(border_amphibians_sumotherthreat)[names(border_amphibians_sumotherthreat) == "n"] <- "NoOtherThreats"
# Histogram no. species x no. other threats 
n_distinct(border_amphibians_sumotherthreat$binomial) 
border_amphibians_sumotherthreat_hist <- ggplot(data = border_amphibians_sumotherthreat, aes(x = NoOtherThreats)) + 
  geom_histogram(binwidth = 0.5, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) +  
  labs(title = "border amphibians", x = "No. other threats", y = "No. bird species") + coord_flip()

grid.arrange(border_birds_sumotherthreat_hist,border_mammals_sumotherthreat_hist,border_reptiles_sumotherthreat_hist,border_amphibians_sumotherthreat_hist,layout_matrix = layout_matrix2) # 1000*800


## Hypothesis: species with a higher FUSE-IAS score tend to be threatened by a higher number of threats
# core_birds_sumotherthreat_score <- merge(core_birds_sumotherthreat, core_list_birds, by = "binomial", all.x = TRUE)
# ggplot(core_birds_sumotherthreat_score, aes(x=as.factor(NoOtherThreats), y=FUSE_IAS_med)) + 
#   geom_boxplot(fill="gray", alpha=0.2) + 
#   xlab("No other threats") + geom_jitter(color="black", size=1.5, alpha=1) + coord_flip()


## How many species exist for each "other threat"
core_birds_otherthreats3 <- data.frame(table(core_birds_otherthreats2$threat))
names(core_birds_otherthreats3)[names(core_birds_otherthreats3) == "Var1"] <- "OtherThreats"
names(core_birds_otherthreats3)[names(core_birds_otherthreats3) == "Freq"] <- "NoSpp"
core_birds_otherthreats3 <- core_birds_otherthreats3 %>%
  mutate(OtherThreats = case_when(
    OtherThreats == "1" ~ "Residential_and _commercial_development",
    OtherThreats == "2" ~ "Agriculture_and_aquaculture",
    OtherThreats == "3" ~ "Energy_production_and_mining",
    OtherThreats == "4" ~ "Transportation_and_service_corridors",
    OtherThreats == "5" ~ "Biological_resource_use",
    OtherThreats == "6" ~ "Human_intrusions_and_disturbance",
    OtherThreats == "7" ~ "Natural_system_modifications",
    OtherThreats == "8" ~ "Invasive_and_other_problematic_species_genes_diseases",
    OtherThreats == "9" ~ "Pollution",
    OtherThreats == "10" ~ "Geological_events",
    OtherThreats == "11" ~ "Climate_change_and_severe_weather",
    OtherThreats == "12" ~ "Other_options",
    TRUE ~ OtherThreats))

core_birds_otherthreats_barplot <- ggplot(core_birds_otherthreats3, aes(x=OtherThreats, y=NoSpp)) + geom_bar(stat = "identity") + geom_text(aes(label = NoSpp), hjust = -0.5, size = 3) + coord_flip() + ggtitle("CORE birds")

core_mammals_otherthreats3 <- data.frame(table(core_mammals_otherthreats2$threat))
names(core_mammals_otherthreats3)[names(core_mammals_otherthreats3) == "Var1"] <- "OtherThreats"
names(core_mammals_otherthreats3)[names(core_mammals_otherthreats3) == "Freq"] <- "NoSpp"
core_mammals_otherthreats3 <- core_mammals_otherthreats3 %>%
  mutate(OtherThreats = case_when(
    OtherThreats == "1" ~ "Residential_and _commercial_development",
    OtherThreats == "2" ~ "Agriculture_and_aquaculture",
    OtherThreats == "3" ~ "Energy_production_and_mining",
    OtherThreats == "4" ~ "Transportation_and_service_corridors",
    OtherThreats == "5" ~ "Biological_resource_use",
    OtherThreats == "6" ~ "Human_intrusions_and_disturbance",
    OtherThreats == "7" ~ "Natural_system_modifications",
    OtherThreats == "8" ~ "Invasive_and_other_problematic_species_genes_diseases",
    OtherThreats == "9" ~ "Pollution",
    OtherThreats == "10" ~ "Geological_events",
    OtherThreats == "11" ~ "Climate_change_and_severe_weather",
    OtherThreats == "12" ~ "Other_options",
    TRUE ~ OtherThreats))

core_mammals_otherthreats_barplot <- ggplot(core_mammals_otherthreats3, aes(x=OtherThreats, y=NoSpp)) + geom_bar(stat = "identity") + geom_text(aes(label = NoSpp), hjust = -0.5, size = 3) + coord_flip() + ggtitle("CORE mammals")


core_reptiles_otherthreats3 <- data.frame(table(core_reptiles_otherthreats2$threat))
names(core_reptiles_otherthreats3)[names(core_reptiles_otherthreats3) == "Var1"] <- "OtherThreats"
names(core_reptiles_otherthreats3)[names(core_reptiles_otherthreats3) == "Freq"] <- "NoSpp"
core_reptiles_otherthreats3 <- core_reptiles_otherthreats3 %>%
  mutate(OtherThreats = case_when(
    OtherThreats == "1" ~ "Residential_and _commercial_development",
    OtherThreats == "2" ~ "Agriculture_and_aquaculture",
    OtherThreats == "3" ~ "Energy_production_and_mining",
    OtherThreats == "4" ~ "Transportation_and_service_corridors",
    OtherThreats == "5" ~ "Biological_resource_use",
    OtherThreats == "6" ~ "Human_intrusions_and_disturbance",
    OtherThreats == "7" ~ "Natural_system_modifications",
    OtherThreats == "8" ~ "Invasive_and_other_problematic_species_genes_diseases",
    OtherThreats == "9" ~ "Pollution",
    OtherThreats == "10" ~ "Geological_events",
    OtherThreats == "11" ~ "Climate_change_and_severe_weather",
    OtherThreats == "12" ~ "Other_options",
    TRUE ~ OtherThreats))

core_reptiles_otherthreats_barplot <- ggplot(core_reptiles_otherthreats3, aes(x=OtherThreats, y=NoSpp)) + geom_bar(stat = "identity") + geom_text(aes(label = NoSpp), hjust = -0.5, size = 3) + coord_flip() + ggtitle("CORE reptiles")


core_amphibians_otherthreats3 <- data.frame(table(core_amphibians_otherthreats2$threat))
names(core_amphibians_otherthreats3)[names(core_amphibians_otherthreats3) == "Var1"] <- "OtherThreats"
names(core_amphibians_otherthreats3)[names(core_amphibians_otherthreats3) == "Freq"] <- "NoSpp"
core_amphibians_otherthreats3 <- core_amphibians_otherthreats3 %>%
  mutate(OtherThreats = case_when(
    OtherThreats == "1" ~ "Residential_and _commercial_development",
    OtherThreats == "2" ~ "Agriculture_and_aquaculture",
    OtherThreats == "3" ~ "Energy_production_and_mining",
    OtherThreats == "4" ~ "Transportation_and_service_corridors",
    OtherThreats == "5" ~ "Biological_resource_use",
    OtherThreats == "6" ~ "Human_intrusions_and_disturbance",
    OtherThreats == "7" ~ "Natural_system_modifications",
    OtherThreats == "8" ~ "Invasive_and_other_problematic_species_genes_diseases",
    OtherThreats == "9" ~ "Pollution",
    OtherThreats == "10" ~ "Geological_events",
    OtherThreats == "11" ~ "Climate_change_and_severe_weather",
    OtherThreats == "12" ~ "Other_options",
    TRUE ~ OtherThreats))

core_amphibians_otherthreats_barplot <- ggplot(core_amphibians_otherthreats3, aes(x=OtherThreats, y=NoSpp)) + geom_bar(stat = "identity") + geom_text(aes(label = NoSpp), hjust = -0.5, size = 3) + coord_flip() + ggtitle("CORE amphibians")

grid.arrange(core_birds_otherthreats_barplot,core_mammals_otherthreats_barplot,core_reptiles_otherthreats_barplot,core_amphibians_otherthreats_barplot,layout_matrix = layout_matrix2) # 1200*800



watch_birds_otherthreats3 <- data.frame(table(watch_birds_otherthreats2$threat))
names(watch_birds_otherthreats3)[names(watch_birds_otherthreats3) == "Var1"] <- "OtherThreats"
names(watch_birds_otherthreats3)[names(watch_birds_otherthreats3) == "Freq"] <- "NoSpp"
watch_birds_otherthreats3 <- watch_birds_otherthreats3 %>%
  mutate(OtherThreats = case_when(
    OtherThreats == "1" ~ "Residential_and _commercial_development",
    OtherThreats == "2" ~ "Agriculture_and_aquaculture",
    OtherThreats == "3" ~ "Energy_production_and_mining",
    OtherThreats == "4" ~ "Transportation_and_service_corridors",
    OtherThreats == "5" ~ "Biological_resource_use",
    OtherThreats == "6" ~ "Human_intrusions_and_disturbance",
    OtherThreats == "7" ~ "Natural_system_modifications",
    OtherThreats == "8" ~ "Invasive_and_other_problematic_species_genes_diseases",
    OtherThreats == "9" ~ "Pollution",
    OtherThreats == "10" ~ "Geological_events",
    OtherThreats == "11" ~ "Climate_change_and_severe_weather",
    OtherThreats == "12" ~ "Other_options",
    TRUE ~ OtherThreats))

watch_birds_otherthreats_barplot <- ggplot(watch_birds_otherthreats3, aes(x=OtherThreats, y=NoSpp)) + geom_bar(stat = "identity") + geom_text(aes(label = NoSpp), hjust = -0.5, size = 3) + coord_flip() + ggtitle("watch birds")

watch_mammals_otherthreats3 <- data.frame(table(watch_mammals_otherthreats2$threat))
names(watch_mammals_otherthreats3)[names(watch_mammals_otherthreats3) == "Var1"] <- "OtherThreats"
names(watch_mammals_otherthreats3)[names(watch_mammals_otherthreats3) == "Freq"] <- "NoSpp"
watch_mammals_otherthreats3 <- watch_mammals_otherthreats3 %>%
  mutate(OtherThreats = case_when(
    OtherThreats == "1" ~ "Residential_and _commercial_development",
    OtherThreats == "2" ~ "Agriculture_and_aquaculture",
    OtherThreats == "3" ~ "Energy_production_and_mining",
    OtherThreats == "4" ~ "Transportation_and_service_corridors",
    OtherThreats == "5" ~ "Biological_resource_use",
    OtherThreats == "6" ~ "Human_intrusions_and_disturbance",
    OtherThreats == "7" ~ "Natural_system_modifications",
    OtherThreats == "8" ~ "Invasive_and_other_problematic_species_genes_diseases",
    OtherThreats == "9" ~ "Pollution",
    OtherThreats == "10" ~ "Geological_events",
    OtherThreats == "11" ~ "Climate_change_and_severe_weather",
    OtherThreats == "12" ~ "Other_options",
    TRUE ~ OtherThreats))

watch_mammals_otherthreats_barplot <- ggplot(watch_mammals_otherthreats3, aes(x=OtherThreats, y=NoSpp)) + geom_bar(stat = "identity") + geom_text(aes(label = NoSpp), hjust = -0.5, size = 3) + coord_flip() + ggtitle("watch mammals")


watch_reptiles_otherthreats3 <- data.frame(table(watch_reptiles_otherthreats2$threat))
names(watch_reptiles_otherthreats3)[names(watch_reptiles_otherthreats3) == "Var1"] <- "OtherThreats"
names(watch_reptiles_otherthreats3)[names(watch_reptiles_otherthreats3) == "Freq"] <- "NoSpp"
watch_reptiles_otherthreats3 <- watch_reptiles_otherthreats3 %>%
  mutate(OtherThreats = case_when(
    OtherThreats == "1" ~ "Residential_and _commercial_development",
    OtherThreats == "2" ~ "Agriculture_and_aquaculture",
    OtherThreats == "3" ~ "Energy_production_and_mining",
    OtherThreats == "4" ~ "Transportation_and_service_corridors",
    OtherThreats == "5" ~ "Biological_resource_use",
    OtherThreats == "6" ~ "Human_intrusions_and_disturbance",
    OtherThreats == "7" ~ "Natural_system_modifications",
    OtherThreats == "8" ~ "Invasive_and_other_problematic_species_genes_diseases",
    OtherThreats == "9" ~ "Pollution",
    OtherThreats == "10" ~ "Geological_events",
    OtherThreats == "11" ~ "Climate_change_and_severe_weather",
    OtherThreats == "12" ~ "Other_options",
    TRUE ~ OtherThreats))

watch_reptiles_otherthreats_barplot <- ggplot(watch_reptiles_otherthreats3, aes(x=OtherThreats, y=NoSpp)) + geom_bar(stat = "identity") + geom_text(aes(label = NoSpp), hjust = -0.5, size = 3) + coord_flip() + ggtitle("watch reptiles")


watch_amphibians_otherthreats3 <- data.frame(table(watch_amphibians_otherthreats2$threat))
names(watch_amphibians_otherthreats3)[names(watch_amphibians_otherthreats3) == "Var1"] <- "OtherThreats"
names(watch_amphibians_otherthreats3)[names(watch_amphibians_otherthreats3) == "Freq"] <- "NoSpp"
watch_amphibians_otherthreats3 <- watch_amphibians_otherthreats3 %>%
  mutate(OtherThreats = case_when(
    OtherThreats == "1" ~ "Residential_and _commercial_development",
    OtherThreats == "2" ~ "Agriculture_and_aquaculture",
    OtherThreats == "3" ~ "Energy_production_and_mining",
    OtherThreats == "4" ~ "Transportation_and_service_corridors",
    OtherThreats == "5" ~ "Biological_resource_use",
    OtherThreats == "6" ~ "Human_intrusions_and_disturbance",
    OtherThreats == "7" ~ "Natural_system_modifications",
    OtherThreats == "8" ~ "Invasive_and_other_problematic_species_genes_diseases",
    OtherThreats == "9" ~ "Pollution",
    OtherThreats == "10" ~ "Geological_events",
    OtherThreats == "11" ~ "Climate_change_and_severe_weather",
    OtherThreats == "12" ~ "Other_options",
    TRUE ~ OtherThreats))

watch_amphibians_otherthreats_barplot <- ggplot(watch_amphibians_otherthreats3, aes(x=OtherThreats, y=NoSpp)) + geom_bar(stat = "identity") + geom_text(aes(label = NoSpp), hjust = -0.5, size = 3) + coord_flip() + ggtitle("watch amphibians")

grid.arrange(watch_birds_otherthreats_barplot,watch_mammals_otherthreats_barplot,watch_reptiles_otherthreats_barplot,watch_amphibians_otherthreats_barplot,layout_matrix = layout_matrix2) # 1200*800


border_birds_otherthreats3 <- data.frame(table(border_birds_otherthreats2$threat))
names(border_birds_otherthreats3)[names(border_birds_otherthreats3) == "Var1"] <- "OtherThreats"
names(border_birds_otherthreats3)[names(border_birds_otherthreats3) == "Freq"] <- "NoSpp"
border_birds_otherthreats3 <- border_birds_otherthreats3 %>%
  mutate(OtherThreats = case_when(
    OtherThreats == "1" ~ "Residential_and _commercial_development",
    OtherThreats == "2" ~ "Agriculture_and_aquaculture",
    OtherThreats == "3" ~ "Energy_production_and_mining",
    OtherThreats == "4" ~ "Transportation_and_service_corridors",
    OtherThreats == "5" ~ "Biological_resource_use",
    OtherThreats == "6" ~ "Human_intrusions_and_disturbance",
    OtherThreats == "7" ~ "Natural_system_modifications",
    OtherThreats == "8" ~ "Invasive_and_other_problematic_species_genes_diseases",
    OtherThreats == "9" ~ "Pollution",
    OtherThreats == "10" ~ "Geological_events",
    OtherThreats == "11" ~ "Climate_change_and_severe_weather",
    OtherThreats == "12" ~ "Other_options",
    TRUE ~ OtherThreats))

border_birds_otherthreats_barplot <- ggplot(border_birds_otherthreats3, aes(x=OtherThreats, y=NoSpp)) + geom_bar(stat = "identity") + geom_text(aes(label = NoSpp), hjust = -0.5, size = 3) + coord_flip() + ggtitle("border birds")

border_mammals_otherthreats3 <- data.frame(table(border_mammals_otherthreats2$threat))
names(border_mammals_otherthreats3)[names(border_mammals_otherthreats3) == "Var1"] <- "OtherThreats"
names(border_mammals_otherthreats3)[names(border_mammals_otherthreats3) == "Freq"] <- "NoSpp"
border_mammals_otherthreats3 <- border_mammals_otherthreats3 %>%
  mutate(OtherThreats = case_when(
    OtherThreats == "1" ~ "Residential_and _commercial_development",
    OtherThreats == "2" ~ "Agriculture_and_aquaculture",
    OtherThreats == "3" ~ "Energy_production_and_mining",
    OtherThreats == "4" ~ "Transportation_and_service_corridors",
    OtherThreats == "5" ~ "Biological_resource_use",
    OtherThreats == "6" ~ "Human_intrusions_and_disturbance",
    OtherThreats == "7" ~ "Natural_system_modifications",
    OtherThreats == "8" ~ "Invasive_and_other_problematic_species_genes_diseases",
    OtherThreats == "9" ~ "Pollution",
    OtherThreats == "10" ~ "Geological_events",
    OtherThreats == "11" ~ "Climate_change_and_severe_weather",
    OtherThreats == "12" ~ "Other_options",
    TRUE ~ OtherThreats))

border_mammals_otherthreats_barplot <- ggplot(border_mammals_otherthreats3, aes(x=OtherThreats, y=NoSpp)) + geom_bar(stat = "identity") + geom_text(aes(label = NoSpp), hjust = -0.5, size = 3) + coord_flip() + ggtitle("border mammals")


border_reptiles_otherthreats3 <- data.frame(table(border_reptiles_otherthreats2$threat))
names(border_reptiles_otherthreats3)[names(border_reptiles_otherthreats3) == "Var1"] <- "OtherThreats"
names(border_reptiles_otherthreats3)[names(border_reptiles_otherthreats3) == "Freq"] <- "NoSpp"
border_reptiles_otherthreats3 <- border_reptiles_otherthreats3 %>%
  mutate(OtherThreats = case_when(
    OtherThreats == "1" ~ "Residential_and _commercial_development",
    OtherThreats == "2" ~ "Agriculture_and_aquaculture",
    OtherThreats == "3" ~ "Energy_production_and_mining",
    OtherThreats == "4" ~ "Transportation_and_service_corridors",
    OtherThreats == "5" ~ "Biological_resource_use",
    OtherThreats == "6" ~ "Human_intrusions_and_disturbance",
    OtherThreats == "7" ~ "Natural_system_modifications",
    OtherThreats == "8" ~ "Invasive_and_other_problematic_species_genes_diseases",
    OtherThreats == "9" ~ "Pollution",
    OtherThreats == "10" ~ "Geological_events",
    OtherThreats == "11" ~ "Climate_change_and_severe_weather",
    OtherThreats == "12" ~ "Other_options",
    TRUE ~ OtherThreats))

border_reptiles_otherthreats_barplot <- ggplot(border_reptiles_otherthreats3, aes(x=OtherThreats, y=NoSpp)) + geom_bar(stat = "identity") + geom_text(aes(label = NoSpp), hjust = -0.5, size = 3) + coord_flip() + ggtitle("border reptiles")


border_amphibians_otherthreats3 <- data.frame(table(border_amphibians_otherthreats2$threat))
names(border_amphibians_otherthreats3)[names(border_amphibians_otherthreats3) == "Var1"] <- "OtherThreats"
names(border_amphibians_otherthreats3)[names(border_amphibians_otherthreats3) == "Freq"] <- "NoSpp"
border_amphibians_otherthreats3 <- border_amphibians_otherthreats3 %>%
  mutate(OtherThreats = case_when(
    OtherThreats == "1" ~ "Residential_and _commercial_development",
    OtherThreats == "2" ~ "Agriculture_and_aquaculture",
    OtherThreats == "3" ~ "Energy_production_and_mining",
    OtherThreats == "4" ~ "Transportation_and_service_corridors",
    OtherThreats == "5" ~ "Biological_resource_use",
    OtherThreats == "6" ~ "Human_intrusions_and_disturbance",
    OtherThreats == "7" ~ "Natural_system_modifications",
    OtherThreats == "8" ~ "Invasive_and_other_problematic_species_genes_diseases",
    OtherThreats == "9" ~ "Pollution",
    OtherThreats == "10" ~ "Geological_events",
    OtherThreats == "11" ~ "Climate_change_and_severe_weather",
    OtherThreats == "12" ~ "Other_options",
    TRUE ~ OtherThreats))

border_amphibians_otherthreats_barplot <- ggplot(border_amphibians_otherthreats3, aes(x=OtherThreats, y=NoSpp)) + geom_bar(stat = "identity") + geom_text(aes(label = NoSpp), hjust = -0.5, size = 3) + coord_flip() + ggtitle("border amphibians")

grid.arrange(border_birds_otherthreats_barplot,border_mammals_otherthreats_barplot,border_reptiles_otherthreats_barplot,border_amphibians_otherthreats_barplot,layout_matrix = layout_matrix2) # 1200*800


## Distribution of FUSE-IAS scores per threat
# core_birds_otherthreats2$threat <- factor(core_birds_otherthreats2$threat, levels = c("1","2","3","4","5","6","7","9","10","11","12"))
# ggplot(core_birds_otherthreats2, aes(x=as.factor(threat), y=FUSE_IAS_med)) + 
#   geom_boxplot(fill="gray", alpha=0.2) + 
#   xlab("Other Threats") + geom_jitter(color="black", size=1.5, alpha=1) + coord_flip()




### 5. Conservation actions in place of CORE species

table(birds_conservation$name) # how many conservations actions exist
core_birds_conservation <- merge(core_list_birds, birds_conservation, by.x = "binomial", by.y = "scientificName", all.x = TRUE)
nrow(core_birds_conservation)
table(core_birds_conservation$name, useNA = "always") # 18 species for which there are no conservation actions
core_birds_conservation2 <- core_birds_conservation %>% drop_na(name) # remove all species that have no conservation actions
core_birds_conservation3 <- core_birds_conservation2 %>% distinct(binomial, code, name, note, .keep_all = TRUE) # remove duplicates based on conservation action name and code, and notes


## Count no. conservation actions of each CORE species
core_birds_conservation_sum <- core_birds_conservation3 %>% count(binomial)
nrow(core_birds_conservation_sum) # 325 species
names(core_birds_conservation_sum)[names(core_birds_conservation_sum) == "n"] <- "NoConservationActions"
# Histogram no. species x no. conservation actions 
n_distinct(core_birds_conservation_sum$binomial) # 325
ggplot(data = core_birds_conservation_sum, aes(NoConservationActions)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  labs(title = "", x = "No. conservation actions", y = "No. bird species") + coord_flip()
table(core_birds_conservation_sum$NoConservationActions, useNA = "always")



## Hypothesis: species with a higher FUSE-IAS score tend to have a higher number of conservation actions
# core_birds_conservation_sum_score <- merge(core_birds_conservation_sum, core_list_birds, by = "binomial", all.x = TRUE)
# ggplot(core_birds_conservation_sum_score, aes(x=as.factor(NoConservationActions), y=FUSE_IAS_med)) + 
#   geom_boxplot(fill="gray", alpha=0.2) + 
#   xlab("No conservation actions") + geom_jitter(color="black", size=1.5, alpha=1) + coord_flip()


## How many species exist for each type of conservation action
table(core_birds_conservation3$code)
# Extract the second level of the conservation actions classification scheme
core_birds_conservation3$action2 <- stringr::str_extract(core_birds_conservation3$code, "^.{3}")
view(core_birds_conservation3[,c("code","action2")])
# no. conservation actions per species per second level conservation action
core_birds_conservation_sum2 <- core_birds_conservation3 %>% count(action2, binomial)
# no. species per second level conservation action
core_birds_conservation_sum3 <- core_birds_conservation_sum2 %>% count(action2)
names(core_birds_conservation_sum3)[names(core_birds_conservation_sum3) == "n"] <- "NoCOREbirds"

ggplot(core_birds_conservation_sum3, aes(x=action2, y=NoCOREbirds)) + 
  geom_bar(stat = "identity") +
  coord_flip()

table(core_birds_conservation_sum3$NoCOREbirds)

# if I use the first level of conservation actions
core_birds_conservation3$action1 <- stringr::str_extract(core_birds_conservation3$code, "^.{2}")
core_birds_conservation_sum4 <- core_birds_conservation3 %>% count(action1, binomial)
core_birds_conservation_sum5 <- core_birds_conservation_sum4 %>% count(action1)
names(core_birds_conservation_sum5)[names(core_birds_conservation_sum5) == "n"] <- "NoCOREbirds"
ggplot(core_birds_conservation_sum5, aes(x=action1, y=NoCOREbirds)) + 
  geom_bar(stat = "identity") +
  coord_flip()



## Distribution of FUSE-IAS scores per conservation action
# core_birds_conservation_sum_score2 <- merge(core_birds_conservation_sum2, core_list_birds, by = "binomial", all.x = TRUE)
# 
# ggplot(core_birds_conservation_sum_score2, aes(x=as.factor(action2), y=FUSE_IAS_med)) + 
#   geom_boxplot(fill="gray", alpha=0.2) + 
#   xlab("") + ylab("FUSE-IAS score") geom_jitter(color="black", size=1.5, alpha=1) + coord_flip()
# 
# # with the first level of conservation actions
# core_birds_conservation_sum_score3 <- merge(core_birds_conservation_sum4, core_list_birds, by = "binomial", all.x = TRUE)
# ggplot(core_birds_conservation_sum_score3, aes(x=as.factor(action1), y=FUSE_IAS_med)) + 
#   geom_boxplot(fill="gray", alpha=0.2) + xlab("") + ylab("FUSE-IAS score") + geom_jitter(color="black", size=1.5, alpha=1) + coord_flip()



## 6. Analyse TOP50 birds

core_birds_threats # database with all 334 species that have threats (already removed the 9 with no threats)
n_distinct(core_birds_threats$binomial)
core_birds_threats_species <- core_birds_threats %>% distinct(binomial, .keep_all = TRUE)
nrow(core_birds_threats_species) # 334

# identify the TOP50 
core_birds_threats_species_top50 <- core_birds_threats_species %>% arrange(desc(FUSE_IAS_med)) %>% slice_head(n = 50)  
nrow(core_birds_threats_species_top50) # 50 species

# subset these species in the dataframe with threats
core_birds_threats_top50 <- core_birds_threats %>% filter(binomial %in% core_birds_threats_species_top50$binomial)
nrow(core_birds_threats_top50) # 331

# which species are island endemic?
# information obtained from datazone birdlife 
core_birds_threats_top50$endemism <- "NA"
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Strigops habroptila", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Phoebastria irrorata", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Diomedea dabbenena", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Ardeotis nigriceps", "no", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Pareudiastes silvestris", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Anas wyvilliana", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Macrocephalon maleo", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Puffinus newelli", "no", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Ara glaucogularis", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Hypotaenidia sylvestris", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Pterodroma phaeopygia", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Puffinus myrtae", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Pelecanoides whenuahouensis", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Puffinus bryani", "no", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Podiceps taczanowskii", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Thalasseus bernsteini", "no", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Hypotaenidia owstoni", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Cyanolimnas cerverai", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Eurostopodus exul", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Aythya innotata", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Zanda baudinii", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Leptotila wellsi", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Puffinus mauretanicus", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Pezoporus occidentalis", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Rynchops albicollis", "no", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Fregetta maoriana", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Haliaeetus leucoryphus", "no", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Gyps bengalensis", "no", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Papasula abbotti", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Pterodroma caribbaea", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Charadrius obscurus", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Sterna acuticauda", "no", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Pseudobulweria aterrima", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Pterodroma magentae", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Pterodroma sandwichensis", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Porphyrio hochstetteri", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Hydrobates macrodactylus", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Turnix novaecaledoniae", "no", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Puffinus auricularis", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Neophema chrysogaster", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Pachyptila macgillivrayi", "no", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Oxyura maccoa", "no", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Heteromirafra archeri", "no", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Didunculus strigirostris", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Rollandia microptera", "no", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Nestor notabilis", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Sypheotides indicus", "no", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Himantopus novaezelandiae", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Rhynochetos jubatus", "yes", endemism))
core_birds_threats_top50 <- core_birds_threats_top50 %>% mutate(endemism = ifelse(binomial == "Alauda razae", "yes", endemism))

TOP50_birds_endemism <- core_birds_threats_top50 %>% distinct(binomial, .keep_all = TRUE) %>% group_by(endemism) %>% summarise(Count = n())
print(TOP50_birds_endemism)




# how many identified IAS per species (separate by IAS group)?
core_birds_sumIASthreat_type
levels(as.factor(core_birds_sumIASthreat_type$ias_groups))
# "bird"                        "diseases_unknown_cause"      "fish"                        "fungus"                     
# "invertebrate"                "mammal"                      "parasite"                    "plant"                      
# "problematic_native_diseases" "reptile"                     "unspecified_spp"            

TOP50_birds_noIAS <- core_birds_sumIASthreat_type %>% filter(binomial %in% core_birds_threats_species_top50$binomial)
n_distinct(TOP50_birds_noIAS$binomial) # 50 species
# check if they have the same species
TOP50_birds_noIAS_spp <- TOP50_birds_noIAS %>% distinct(binomial, .keep_all = TRUE)
all(sort(core_birds_threats_species_top50$binomial) == sort(TOP50_birds_noIAS_spp$binomial))

TOP50_birds_averagenoIAS_spp <- TOP50_birds_noIAS %>% count(binomial)
ggplot(data = TOP50_birds_averagenoIAS_spp, aes(x = n)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) + 
  labs(title = "TOP 50 Core birds - no. IAS", x = "No. IAS", y = "No. bird species") + 
  coord_flip()

# create new database to organize results
TOP50_birds_noIAS_spp <- TOP50_birds_noIAS_spp %>% select(-NoIAS, -ias_groups)

TOP50_birds_noIASbirds <- TOP50_birds_noIAS %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(ias_groups == "bird", NoIAS, 0)))
TOP50_birds_noIAS_spp <- merge(TOP50_birds_noIAS_spp, TOP50_birds_noIASbirds, by = "binomial", all.x = TRUE)
TOP50_birds_noIAS_spp <- TOP50_birds_noIAS_spp %>% rename(NoIAS_birds = SumResult)

TOP50_birds_noIASmammals <- TOP50_birds_noIAS %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(ias_groups == "mammal", NoIAS, 0)))
TOP50_birds_noIAS_spp <- merge(TOP50_birds_noIAS_spp, TOP50_birds_noIASmammals, by = "binomial", all.x = TRUE)
TOP50_birds_noIAS_spp <- TOP50_birds_noIAS_spp %>% rename(NoIAS_mammals = SumResult)

TOP50_birds_noIASreptiles <- TOP50_birds_noIAS %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(ias_groups == "reptile", NoIAS, 0)))
TOP50_birds_noIAS_spp <- merge(TOP50_birds_noIAS_spp, TOP50_birds_noIASreptiles, by = "binomial", all.x = TRUE)
TOP50_birds_noIAS_spp <- TOP50_birds_noIAS_spp %>% rename(NoIAS_reptiles = SumResult)

TOP50_birds_noIASfish <- TOP50_birds_noIAS %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(ias_groups == "fish", NoIAS, 0)))
TOP50_birds_noIAS_spp <- merge(TOP50_birds_noIAS_spp, TOP50_birds_noIASfish, by = "binomial", all.x = TRUE)
TOP50_birds_noIAS_spp <- TOP50_birds_noIAS_spp %>% rename(NoIAS_fish = SumResult)

TOP50_birds_noIASplant <- TOP50_birds_noIAS %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(ias_groups == "plant", NoIAS, 0)))
TOP50_birds_noIAS_spp <- merge(TOP50_birds_noIAS_spp, TOP50_birds_noIASplant, by = "binomial", all.x = TRUE)
TOP50_birds_noIAS_spp <- TOP50_birds_noIAS_spp %>% rename(NoIAS_plant = SumResult)

TOP50_birds_noIASinvertebrate <- TOP50_birds_noIAS %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(ias_groups == "invertebrate", NoIAS, 0)))
TOP50_birds_noIAS_spp <- merge(TOP50_birds_noIAS_spp, TOP50_birds_noIASinvertebrate, by = "binomial", all.x = TRUE)
TOP50_birds_noIAS_spp <- TOP50_birds_noIAS_spp %>% rename(NoIAS_invertebrate = SumResult)

TOP50_birds_noIASfungus <- TOP50_birds_noIAS %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(ias_groups == "fungus", NoIAS, 0)))
TOP50_birds_noIAS_spp <- merge(TOP50_birds_noIAS_spp, TOP50_birds_noIASfungus, by = "binomial", all.x = TRUE)
TOP50_birds_noIAS_spp <- TOP50_birds_noIAS_spp %>% rename(NoIAS_fungus = SumResult)

TOP50_birds_noIASparasite <- TOP50_birds_noIAS %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(ias_groups == "parasite", NoIAS, 0)))
TOP50_birds_noIAS_spp <- merge(TOP50_birds_noIAS_spp, TOP50_birds_noIASparasite, by = "binomial", all.x = TRUE)
TOP50_birds_noIAS_spp <- TOP50_birds_noIAS_spp %>% rename(NoIAS_parasite = SumResult)

TOP50_birds_noIASunspecified_spp <- TOP50_birds_noIAS %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(ias_groups == "unspecified_spp", NoIAS, 0)))
TOP50_birds_noIAS_spp <- merge(TOP50_birds_noIAS_spp, TOP50_birds_noIASunspecified_spp, by = "binomial", all.x = TRUE)
TOP50_birds_noIAS_spp <- TOP50_birds_noIAS_spp %>% rename(NoIAS_unspecified_spp = SumResult)

TOP50_birds_noIASdiseases_unknown_cause <- TOP50_birds_noIAS %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(ias_groups == "diseases_unknown_cause", NoIAS, 0)))
TOP50_birds_noIAS_spp <- merge(TOP50_birds_noIAS_spp, TOP50_birds_noIASdiseases_unknown_cause, by = "binomial", all.x = TRUE)
TOP50_birds_noIAS_spp <- TOP50_birds_noIAS_spp %>% rename(NoIAS_diseases_unknown_cause = SumResult)

TOP50_birds_noIASproblematic_native_diseases <- TOP50_birds_noIAS %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(ias_groups == "problematic_native_diseases", NoIAS, 0)))
TOP50_birds_noIAS_spp <- merge(TOP50_birds_noIAS_spp, TOP50_birds_noIASproblematic_native_diseases, by = "binomial", all.x = TRUE)
TOP50_birds_noIAS_spp <- TOP50_birds_noIAS_spp %>% rename(NoIAS_problematic_native_diseases = SumResult)

write.csv(TOP50_birds_noIAS_spp, file = "C:/Users/fmacedocoutinhodeoli/OneDrive - MNHN/Outros/FUSE-IAS/TOP50_birds_noIAS_spp.csv", row.names = FALSE)

# count the no. of species threatened by birds
sum(TOP50_birds_noIAS_spp$NoIAS_birds > 0) # 11 0.22
sum(TOP50_birds_noIAS_spp$NoIAS_mammals > 0) # 28 0.56
sum(TOP50_birds_noIAS_spp$NoIAS_reptiles > 0) # 2 0.04
sum(TOP50_birds_noIAS_spp$NoIAS_fish > 0) # 4 0.08
sum(TOP50_birds_noIAS_spp$NoIAS_plant > 0) # 6 0.12
sum(TOP50_birds_noIAS_spp$NoIAS_invertebrate > 0) # 5 0.10
sum(TOP50_birds_noIAS_spp$NoIAS_fungus > 0) # 0 
sum(TOP50_birds_noIAS_spp$NoIAS_parasite > 0) # 2 0.04
sum(TOP50_birds_noIAS_spp$NoIAS_unspecified_spp > 0) # 15 0.3
sum(TOP50_birds_noIAS_spp$NoIAS_diseases_unknown_cause > 0) # 0
sum(TOP50_birds_noIAS_spp$NoIAS_problematic_native_diseases > 0) # 1 0.02


# how many other threats per species?
levels(as.factor(core_birds_otherthreats2$threat))

TOP50_birds_noOtherThreats <- core_birds_otherthreats2 %>% filter(binomial %in% core_birds_threats_species_top50$binomial)
n_distinct(TOP50_birds_noOtherThreats$binomial) # 46 species, so not all 50 species have other threats

# create new database to organize results
TOP50_birds_noOtherThreats_spp <- TOP50_birds_noOtherThreats %>% distinct(binomial, .keep_all = TRUE)
TOP50_birds_noOtherThreats_spp <- TOP50_birds_noOtherThreats_spp %>% select(binomial)

TOP50_birds_sumOtherThreats <- TOP50_birds_noOtherThreats %>% count(binomial,threat)
TOP50_birds_averageOtherThreats_spp <- TOP50_birds_sumOtherThreats %>% count(binomial)

ggplot(data = TOP50_birds_averageOtherThreats_spp, aes(x = n)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) + 
  labs(title = "TOP 50 Core birds - no. other threats", x = "No. other threats", y = "No. bird species") + 
  coord_flip()


TOP50_birds_noThreat1 <- TOP50_birds_sumOtherThreats %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(threat == "1", n, 0)))
TOP50_birds_noOtherThreats_spp <- merge(TOP50_birds_noOtherThreats_spp, TOP50_birds_noThreat1, by = "binomial", all.x = TRUE)
TOP50_birds_noOtherThreats_spp <- TOP50_birds_noOtherThreats_spp %>% rename(NoThreat1 = SumResult)

TOP50_birds_noThreat2 <- TOP50_birds_sumOtherThreats %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(threat == "2", n, 0)))
TOP50_birds_noOtherThreats_spp <- merge(TOP50_birds_noOtherThreats_spp, TOP50_birds_noThreat2, by = "binomial", all.x = TRUE)
TOP50_birds_noOtherThreats_spp <- TOP50_birds_noOtherThreats_spp %>% rename(NoThreat2 = SumResult)

TOP50_birds_noThreat3 <- TOP50_birds_sumOtherThreats %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(threat == "3", n, 0)))
TOP50_birds_noOtherThreats_spp <- merge(TOP50_birds_noOtherThreats_spp, TOP50_birds_noThreat3, by = "binomial", all.x = TRUE)
TOP50_birds_noOtherThreats_spp <- TOP50_birds_noOtherThreats_spp %>% rename(NoThreat3 = SumResult)

TOP50_birds_noThreat4 <- TOP50_birds_sumOtherThreats %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(threat == "4", n, 0)))
TOP50_birds_noOtherThreats_spp <- merge(TOP50_birds_noOtherThreats_spp, TOP50_birds_noThreat4, by = "binomial", all.x = TRUE)
TOP50_birds_noOtherThreats_spp <- TOP50_birds_noOtherThreats_spp %>% rename(NoThreat4 = SumResult)

TOP50_birds_noThreat5 <- TOP50_birds_sumOtherThreats %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(threat == "5", n, 0)))
TOP50_birds_noOtherThreats_spp <- merge(TOP50_birds_noOtherThreats_spp, TOP50_birds_noThreat5, by = "binomial", all.x = TRUE)
TOP50_birds_noOtherThreats_spp <- TOP50_birds_noOtherThreats_spp %>% rename(NoThreat5 = SumResult)

TOP50_birds_noThreat6 <- TOP50_birds_sumOtherThreats %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(threat == "6", n, 0)))
TOP50_birds_noOtherThreats_spp <- merge(TOP50_birds_noOtherThreats_spp, TOP50_birds_noThreat6, by = "binomial", all.x = TRUE)
TOP50_birds_noOtherThreats_spp <- TOP50_birds_noOtherThreats_spp %>% rename(NoThreat6 = SumResult)

TOP50_birds_noThreat7 <- TOP50_birds_sumOtherThreats %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(threat == "7", n, 0)))
TOP50_birds_noOtherThreats_spp <- merge(TOP50_birds_noOtherThreats_spp, TOP50_birds_noThreat7, by = "binomial", all.x = TRUE)
TOP50_birds_noOtherThreats_spp <- TOP50_birds_noOtherThreats_spp %>% rename(NoThreat7 = SumResult)

TOP50_birds_noThreat8 <- TOP50_birds_sumOtherThreats %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(threat == "8", n, 0)))
TOP50_birds_noOtherThreats_spp <- merge(TOP50_birds_noOtherThreats_spp, TOP50_birds_noThreat8, by = "binomial", all.x = TRUE)
TOP50_birds_noOtherThreats_spp <- TOP50_birds_noOtherThreats_spp %>% rename(NoThreat8 = SumResult)

TOP50_birds_noThreat9 <- TOP50_birds_sumOtherThreats %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(threat == "9", n, 0)))
TOP50_birds_noOtherThreats_spp <- merge(TOP50_birds_noOtherThreats_spp, TOP50_birds_noThreat9, by = "binomial", all.x = TRUE)
TOP50_birds_noOtherThreats_spp <- TOP50_birds_noOtherThreats_spp %>% rename(NoThreat9 = SumResult)

TOP50_birds_noThreat10 <- TOP50_birds_sumOtherThreats %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(threat == "10", n, 0)))
TOP50_birds_noOtherThreats_spp <- merge(TOP50_birds_noOtherThreats_spp, TOP50_birds_noThreat10, by = "binomial", all.x = TRUE)
TOP50_birds_noOtherThreats_spp <- TOP50_birds_noOtherThreats_spp %>% rename(NoThreat10 = SumResult)

TOP50_birds_noThreat11 <- TOP50_birds_sumOtherThreats %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(threat == "11", n, 0)))
TOP50_birds_noOtherThreats_spp <- merge(TOP50_birds_noOtherThreats_spp, TOP50_birds_noThreat11, by = "binomial", all.x = TRUE)
TOP50_birds_noOtherThreats_spp <- TOP50_birds_noOtherThreats_spp %>% rename(NoThreat11 = SumResult)

TOP50_birds_noThreat12 <- TOP50_birds_sumOtherThreats %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(threat == "12", n, 0)))
TOP50_birds_noOtherThreats_spp <- merge(TOP50_birds_noOtherThreats_spp, TOP50_birds_noThreat12, by = "binomial", all.x = TRUE)
TOP50_birds_noOtherThreats_spp <- TOP50_birds_noOtherThreats_spp %>% rename(NoThreat12 = SumResult)

write.csv(TOP50_birds_noOtherThreats_spp, file = "C:/Users/fmacedocoutinhodeoli/OneDrive - MNHN/Outros/FUSE-IAS/TOP50_birds_noOtherThreats_spp.csv", row.names = FALSE)


# most common threatt
sum(TOP50_birds_noOtherThreats_spp$NoThreat1 > 0) # 15 0.3
sum(TOP50_birds_noOtherThreats_spp$NoThreat2 > 0) # 24 0.48
sum(TOP50_birds_noOtherThreats_spp$NoThreat3 > 0) # 14 0.28
sum(TOP50_birds_noOtherThreats_spp$NoThreat4 > 0) # 8 0.16
sum(TOP50_birds_noOtherThreats_spp$NoThreat5 > 0) # 35 0.7
sum(TOP50_birds_noOtherThreats_spp$NoThreat6 > 0) # 15 0.3
sum(TOP50_birds_noOtherThreats_spp$NoThreat7 > 0) # 19 0.38
sum(TOP50_birds_noOtherThreats_spp$NoThreat8 > 0) # 0
sum(TOP50_birds_noOtherThreats_spp$NoThreat9 > 0) # 17 0.34
sum(TOP50_birds_noOtherThreats_spp$NoThreat10 > 0) # 1 0.02
sum(TOP50_birds_noOtherThreats_spp$NoThreat11 > 0) # 28 0.56
sum(TOP50_birds_noOtherThreats_spp$NoThreat12 > 0) # 2 0.04


# how many identified conservation actions (separate by group)?
core_birds_conservation # this is the data that we should look

TOP50_birds_spp <- TOP50_birds_noIAS_spp %>% select(binomial)
head(TOP50_birds_spp) # dataframe with just the TOP 50 species 

TOP50_birds_conservationactions <- core_birds_conservation %>% filter(binomial %in% TOP50_birds_spp$binomial)
n_distinct(TOP50_birds_conservationactions$binomial) 

TOP50_birds_conservationactions <- TOP50_birds_conservationactions[complete.cases(TOP50_birds_conservationactions[, "code", drop = FALSE]), ]
n_distinct(TOP50_birds_conservationactions$binomial) # 2 species without conservations actions

# extract the second level of the conservation actions classification scheme
TOP50_birds_conservationactions$action2 <- stringr::str_extract(TOP50_birds_conservationactions$code, "^.{3}")
TOP50_birds_conservationactions$action1 <- stringr::str_extract(TOP50_birds_conservationactions$code, "^.{2}")

TOP50_birds_conservationactions2 <- TOP50_birds_conservationactions %>% distinct(binomial,action2, .keep_all = TRUE)

# count the no. of conservations actions per species based on the second level
TOP50_birds_sumconservationactions2 <- TOP50_birds_conservationactions2 %>% count(binomial)
ggplot(data = TOP50_birds_sumconservationactions2, aes(x = n)) + 
  geom_histogram(binwidth = 1, color = "black", fill = "grey") + 
  geom_text(stat = "count", aes(label = after_stat(count)), position = position_nudge(y = 3)) + 
  labs(title = "TOP 50 Core birds - no. conservations actions", x = "No. conservation actions", y = "No. bird species") + 
  coord_flip()


# looking at each conservation action
TOP50_birds_sumconservationactions2_type <- TOP50_birds_conservationactions2 %>% count(binomial,action2)
TOP50_birds_sumconservationactions2_type_spp <- TOP50_birds_sumconservationactions2_type %>% count(binomial)
table(TOP50_birds_sumconservationactions2_type$action2)
# 1.1 1.2 2.1 2.2 2.3 3.1 3.2 3.3 3.4 4.2 4.3 5.1 5.2 5.3 5.4 6.1 6.2 6.4 6.5

TOP50_birds_Action1_1 <- TOP50_birds_sumconservationactions2_type %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(action2 == "1.1", n, 0)))
TOP50_birds_Action1_1 <- TOP50_birds_Action1_1 %>% rename(Action1_1 = SumResult)

TOP50_birds_Action1_2 <- TOP50_birds_sumconservationactions2_type %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(action2 == "1.2", n, 0)))
TOP50_birds_Action1_2 <- TOP50_birds_Action1_2 %>% rename(Action1_2 = SumResult)

TOP50_birds_Action2_1 <- TOP50_birds_sumconservationactions2_type %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(action2 == "2.1", n, 0)))
TOP50_birds_Action2_1 <- TOP50_birds_Action2_1 %>% rename(Action2_1 = SumResult)

TOP50_birds_Action2_2 <- TOP50_birds_sumconservationactions2_type %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(action2 == "2.2", n, 0)))
TOP50_birds_Action2_2 <- TOP50_birds_Action2_2 %>% rename(Action2_2 = SumResult)

TOP50_birds_Action2_3 <- TOP50_birds_sumconservationactions2_type %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(action2 == "2.3", n, 0)))
TOP50_birds_Action2_3 <- TOP50_birds_Action2_3 %>% rename(Action2_3 = SumResult)

TOP50_birds_Action3_1 <- TOP50_birds_sumconservationactions2_type %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(action2 == "3.1", n, 0)))
TOP50_birds_Action3_1 <- TOP50_birds_Action3_1 %>% rename(Action3_1 = SumResult)

TOP50_birds_Action3_2 <- TOP50_birds_sumconservationactions2_type %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(action2 == "3.2", n, 0)))
TOP50_birds_Action3_2 <- TOP50_birds_Action3_2 %>% rename(Action3_2 = SumResult)

TOP50_birds_Action3_3 <- TOP50_birds_sumconservationactions2_type %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(action2 == "3.3", n, 0)))
TOP50_birds_Action3_3 <- TOP50_birds_Action3_3 %>% rename(Action3_3 = SumResult)

TOP50_birds_Action3_4 <- TOP50_birds_sumconservationactions2_type %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(action2 == "3.4", n, 0)))
TOP50_birds_Action3_4 <- TOP50_birds_Action3_4 %>% rename(Action3_4 = SumResult)

TOP50_birds_Action4_2 <- TOP50_birds_sumconservationactions2_type %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(action2 == "4.2", n, 0)))
TOP50_birds_Action4_2 <- TOP50_birds_Action4_2 %>% rename(Action4_2 = SumResult)

TOP50_birds_Action4_3 <- TOP50_birds_sumconservationactions2_type %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(action2 == "4.3", n, 0)))
TOP50_birds_Action4_3 <- TOP50_birds_Action4_3 %>% rename(Action4_3 = SumResult)

TOP50_birds_Action5_1 <- TOP50_birds_sumconservationactions2_type %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(action2 == "5.1", n, 0)))
TOP50_birds_Action5_1 <- TOP50_birds_Action5_1 %>% rename(Action5_1 = SumResult)

TOP50_birds_Action5_2 <- TOP50_birds_sumconservationactions2_type %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(action2 == "5.2", n, 0)))
TOP50_birds_Action5_2 <- TOP50_birds_Action5_2 %>% rename(Action5_2 = SumResult)

TOP50_birds_Action5_3 <- TOP50_birds_sumconservationactions2_type %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(action2 == "5.3", n, 0)))
TOP50_birds_Action5_3 <- TOP50_birds_Action5_3 %>% rename(Action5_3 = SumResult)

TOP50_birds_Action5_4 <- TOP50_birds_sumconservationactions2_type %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(action2 == "5.4", n, 0)))
TOP50_birds_Action5_4 <- TOP50_birds_Action5_4 %>% rename(Action5_4 = SumResult)

TOP50_birds_Action6_1 <- TOP50_birds_sumconservationactions2_type %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(action2 == "6.1", n, 0)))
TOP50_birds_Action6_1 <- TOP50_birds_Action6_1 %>% rename(Action6_1 = SumResult)

TOP50_birds_Action6_2 <- TOP50_birds_sumconservationactions2_type %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(action2 == "6.2", n, 0)))
TOP50_birds_Action6_2 <- TOP50_birds_Action6_2 %>% rename(Action6_2 = SumResult)

TOP50_birds_Action6_4 <- TOP50_birds_sumconservationactions2_type %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(action2 == "6.4", n, 0)))
TOP50_birds_Action6_4 <- TOP50_birds_Action6_4 %>% rename(Action6_4 = SumResult)

TOP50_birds_Action6_5 <- TOP50_birds_sumconservationactions2_type %>% group_by(binomial) %>% summarise(SumResult = sum(ifelse(action2 == "6.5", n, 0)))
TOP50_birds_Action6_5 <- TOP50_birds_Action6_5 %>% rename(Action6_5 = SumResult)

# merge all based on column species
TOP50_birds_Action <- merge(TOP50_birds_Action1_1, TOP50_birds_Action1_2, by = "binomial", all.x = TRUE) 
TOP50_birds_Action <- merge(TOP50_birds_Action, TOP50_birds_Action2_1, by = "binomial", all.x = TRUE)  
TOP50_birds_Action <- merge(TOP50_birds_Action, TOP50_birds_Action2_2, by = "binomial", all.x = TRUE)  
TOP50_birds_Action <- merge(TOP50_birds_Action, TOP50_birds_Action2_3, by = "binomial", all.x = TRUE)  
TOP50_birds_Action <- merge(TOP50_birds_Action, TOP50_birds_Action3_1, by = "binomial", all.x = TRUE)  
TOP50_birds_Action <- merge(TOP50_birds_Action, TOP50_birds_Action3_2, by = "binomial", all.x = TRUE)  
TOP50_birds_Action <- merge(TOP50_birds_Action, TOP50_birds_Action3_3, by = "binomial", all.x = TRUE)  
TOP50_birds_Action <- merge(TOP50_birds_Action, TOP50_birds_Action3_4, by = "binomial", all.x = TRUE)  
TOP50_birds_Action <- merge(TOP50_birds_Action, TOP50_birds_Action4_2, by = "binomial", all.x = TRUE)  
TOP50_birds_Action <- merge(TOP50_birds_Action, TOP50_birds_Action4_3, by = "binomial", all.x = TRUE)  
TOP50_birds_Action <- merge(TOP50_birds_Action, TOP50_birds_Action5_1, by = "binomial", all.x = TRUE)  
TOP50_birds_Action <- merge(TOP50_birds_Action, TOP50_birds_Action5_2, by = "binomial", all.x = TRUE)  
TOP50_birds_Action <- merge(TOP50_birds_Action, TOP50_birds_Action5_3, by = "binomial", all.x = TRUE)  
TOP50_birds_Action <- merge(TOP50_birds_Action, TOP50_birds_Action5_4, by = "binomial", all.x = TRUE)  
TOP50_birds_Action <- merge(TOP50_birds_Action, TOP50_birds_Action6_1, by = "binomial", all.x = TRUE)  
TOP50_birds_Action <- merge(TOP50_birds_Action, TOP50_birds_Action6_2, by = "binomial", all.x = TRUE)  
TOP50_birds_Action <- merge(TOP50_birds_Action, TOP50_birds_Action6_4, by = "binomial", all.x = TRUE)  
TOP50_birds_Action <- merge(TOP50_birds_Action, TOP50_birds_Action6_5, by = "binomial", all.x = TRUE)  
colnames(TOP50_birds_Action)

write.csv(TOP50_birds_Action, file = "C:/Users/fmacedocoutinhodeoli/OneDrive - MNHN/Outros/FUSE-IAS/TOP50_birds_Action.csv", row.names = FALSE)

# most common conservation action
# count the no. of species with 2.2 conservation actions
sum(TOP50_birds_Action$Action1_1 > 0) # 18 36%
sum(TOP50_birds_Action$Action1_2 > 0) # 10 20%
sum(TOP50_birds_Action$Action2_1 > 0) # 22 44%
sum(TOP50_birds_Action$Action2_2 > 0) # 32 64%
sum(TOP50_birds_Action$Action2_3 > 0) # 12 24%
sum(TOP50_birds_Action$Action3_1 > 0) # 3 6%
sum(TOP50_birds_Action$Action3_2 > 0) # 12 24%
sum(TOP50_birds_Action$Action3_3 > 0) # 13 26%
sum(TOP50_birds_Action$Action3_4 > 0) # 15 30%
sum(TOP50_birds_Action$Action4_2 > 0) # 2 4%
sum(TOP50_birds_Action$Action4_3 > 0) # 28 56%
sum(TOP50_birds_Action$Action5_1 > 0) # 17 34%
sum(TOP50_birds_Action$Action5_2 > 0) # 1 2%
sum(TOP50_birds_Action$Action5_3 > 0) # 1 2%
sum(TOP50_birds_Action$Action5_4 > 0) # 9 18%
sum(TOP50_birds_Action$Action6_1 > 0) # 6 12%
sum(TOP50_birds_Action$Action6_2 > 0) # 2 4%
sum(TOP50_birds_Action$Action6_4 > 0) # 1 2%
sum(TOP50_birds_Action$Action6_5 > 0) # 1 2%





