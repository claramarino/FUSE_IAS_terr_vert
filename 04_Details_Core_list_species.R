# Diving into Core list species 

rm(list=ls())

library(tidyverse)

# load threat information
threat <- openxlsx::read.xlsx("Data/Species_threat_IAS_terr_vert.xlsx") 
nb_threat <- threat %>% dplyr::distinct(binomial, nb_tot_threat) %>%
  filter(!is.na(nb_tot_threat))

# load lists
core_list <- readRDS("Output/03_FUSIAS_core_list_ABMR.rds")
border_list <- readRDS("Output/03_FUSIAS_borderline_list_ABMR.rds")
watch_list <- readRDS("Output/03_FUSIAS_watch_list_ABMR.rds")

all_lists <- core_list
for (i in 1:4){
  all_lists[[i]] <- bind_rows(
    core_list[[i]] %>% mutate(list = "Core"), 
    watch_list[[i]] %>% mutate(list = "Watch"),
    border_list[[i]] %>% mutate(list = "Borderline"))
}


# count nb of threats per species in the core list
nb_th_core <- lapply(core_list, function(x){
  left_join(x, nb_threat)
})

lapply(nb_th_core, function(x){mean(x$nb_tot_threat)})
lapply(nb_th_core, function(x){sd(x$nb_tot_threat)})

mean(bind_rows(nb_th_core)$nb_tot_threat)
sd(bind_rows(nb_th_core)$nb_tot_threat)

# do some species have only IAS threat?
core_ias_only <- lapply(nb_th_core, function (x){
  x %>% filter(nb_tot_threat < 2)
})

lapply(core_ias_only, nrow)
# only a few species per group
# 11 amph, 19 birds, 8 mam, 26 rept

# do those species have a high score?
lapply(core_ias_only, function(x){
  print(mean(x$FUSE_IAS_med))
  # print(sd(x$FUSE_IAS_med))
})

# save species list with only IAS threat
# for all lists (not only Core)
nb_th_all <- lapply(all_lists, function(x){
  left_join(x, nb_threat) %>% filter(nb_tot_threat < 2)
})

for (i in 1:length(nb_th_all)){
  nb_th_all[[i]]$class <- names(nb_th_all)[i]
}
nb_th_all_tb <- bind_rows(nb_th_all) %>%
  select(class, binomial, cate_p_ext, severity, p_med, FUSE_IAS_med, FUSE_IAS_sd, list) %>%
  mutate(class = case_when(
    class == "amph" ~ "Amphibians",
    class == "bird" ~ "Birds",
    class == "mam" ~ "Mammals",
    class == "rept" ~ "Lizards" )) %>%
  mutate(severity = if_else(severity=="DD","NA", severity))

openxlsx::write.xlsx(nb_th_all_tb, file = 'Output/04_Species_threatened_by_IAS_only.xlsx')

core_ias_only

####### TOP 50 birds #########


# select top 50
ranked_core_list <- lapply(core_list, function(x){
  x %>% mutate(rank = rank(-FUSE_IAS_med)) %>%
    filter(rank<=50)
})
# select birds only
rank_b <- ranked_core_list$bird %>%
  mutate(sev_info = if_else(severity=="DD", "DD","info"))

# bind with Fsp and Fun
std_fun_fsp <- readRDS("Output/02_standardized_FUn_FSp_ABMR.rds")
rank_fig <- left_join(rank_b, std_fun_fsp$bird %>% select(binomial, FUn_5NN, FSp_7D))


# load data 

# conservation action
cons <- read.csv("Data/TOP50_birds_Action.csv")
thr <- read.csv("Data/TOP50_birds_noOtherThreats_spp.csv")
ias <- read.csv("Data/TOP50_birds_noIAS_spp.csv")

thr2 <- thr %>% column_to_rownames("binomial")
thr2$is_threat <- rowSums(thr2)

thr_glob <- thr %>%
  mutate(
    Habitat = NoThreat1+NoThreat2+NoThreat3+NoThreat4+NoThreat6+NoThreat7,
    Overexpl = NoThreat5,
    Pollution = NoThreat9,
    CC = NoThreat11,
    other = NoThreat10 + NoThreat12) %>%
  select(binomial, Habitat:other) %>%
  column_to_rownames("binomial")

thr_glob[thr_glob>0] <- 1

rank_fig <- left_join(
  left_join(rank_fig, cons %>% select(binomial, Action2_2)),
  thr_glob %>% rownames_to_column("binomial"))
rank_fig[is.na(rank_fig)] <- 0 

library(ggpattern)

pdf("Fig/04_Fig3_barplot_rank_top50.pdf", 8, 8)

ggplot(rank_fig)+
  geom_bar_pattern(aes(x = -rank, y = FUSE_IAS_med, 
                       fill = cate_p_ext, alpha = severity,
                       pattern = sev_info), stat = "identity",
                   pattern_fill = "white", pattern_color = NA,
                   pattern_density = 0.1, pattern_spacing = .02)+
  geom_errorbar(aes(x = -rank, 
                    ymin = FUSE_IAS_med-FUSE_IAS_sd, 
                    ymax = FUSE_IAS_med+FUSE_IAS_sd),
                color = "grey")+
  scale_fill_manual(values = c("NT" = "mediumseagreen", 
                               "LC" = "olivedrab2", 
                               "VU" = "gold1", 
                               "EN" = "darkorange2", 
                               "CR" = "firebrick2"))+
  scale_alpha_manual(values = c("0" = .5, "1" = 1)) +
  scale_pattern_manual(values = c(DD = "stripe", info = "none"))+
  geom_text(aes(x = -rank, y = -0.50, label=binomial), hjust=0, size = 3.5, fontface = "italic")+
  xlab("")+ ylab("FUSE-IAS score")+
  coord_flip()+
  theme_classic()+
  theme(strip.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank())
dev.off()


colnames(rank_fig)
# habitat
h <- ggplot(rank_fig)+
  geom_point(aes(x=-rank, y = 1, fill = as.character(Habitat)), 
             shape = 22, color = "chocolate4", size = 2.5)+
  scale_fill_manual(values = c("0" = "white", "1" = "chocolate4"))+
  coord_flip()+
  theme_classic()+
  xlab("")+ ylab("")+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = "none")
# Overexploitation
o <- ggplot(rank_fig)+  
  geom_point(aes(x=-rank, y = 1, fill = as.character(Overexpl)), 
             shape = 22, color = "forestgreen", size = 2.5)+
  scale_fill_manual(values = c("0" = "white", "1" = "forestgreen"))+
  coord_flip()+
  theme_classic()+
  xlab("")+ ylab("")+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = "none")
# Climate change
c <- ggplot(rank_fig)+  
  geom_point(aes(x=-rank, y = 1, fill = as.character(CC)), 
             shape = 22, color = "darkorange1", size = 2.5)+
  scale_fill_manual(values = c("0" = "white", "1" = "darkorange1"))+
  coord_flip()+
  theme_classic()+
  xlab("")+ ylab("")+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = "none")
# Pollution
p<- ggplot(rank_fig)+  
  geom_point(aes(x=-rank, y = 1, fill = as.character(Pollution)), 
             shape = 22, color = "firebrick1", size = 2.5)+
  scale_fill_manual(values = c("0" = "white", "1" = "firebrick1"))+
  coord_flip()+
  theme_classic()+
  xlab("")+ ylab("")+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = "none")
# Other
ot <- ggplot(rank_fig)+  
  geom_point(aes(x=-rank, y = 1, fill = as.character(other)), 
             shape = 22, color = "grey50", size = 2.5)+
  scale_fill_manual(values = c("0" = "white", "1" = "grey50"))+
  coord_flip()+
  theme_classic()+
  xlab("")+ ylab("")+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = "none")

cons <- ggplot(rank_fig)+  
  geom_point(aes(x=-rank, y = 1, fill = as.character(Action2_2)), 
             shape = 22, color = "grey10", size = 2.5)+
  scale_fill_manual(values = c("0" = "white", "1" = "grey10"))+
  coord_flip()+
  theme_classic()+
  xlab("")+ ylab("")+
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = "none")

ot
o
h
p
c

library(ggpubr)

colSums(rank_fig %>% select(Habitat:other))

pdf("Fig/04_Fig3_threats_top50.pdf", 8, 8)
ggarrange(o, h, c, p, ot, cons, nrow = 1, ncol = 6)
dev.off()

# supplementary figure 
pdf("Fig/04_SupplFig_top50_components.pdf", 6, 8)

ggplot(rank_fig)+
  geom_text(aes(x = -rank, y = -0.5, label=binomial), hjust=0, size = 4, fontface = "italic")+
  geom_line(aes(x=-rank, y = FUn_5NN), color = "grey70", lty=3)+
  geom_line(aes(x=-rank, y = FSp_7D), color = "grey30", lty=3)+
  geom_line(aes(x=-rank, y = p_med), color = "red", lty=3)+
  geom_point(aes(x=-rank, y = FUn_5NN), color = "grey70", shape = 1)+
  geom_point(aes(x=-rank, y = FSp_7D), color = "grey30", shape = 1)+
  geom_point(aes(x=-rank, y = p_med), color = "red", shape=4)+
  xlab("")+ ylab("FUSE-IAS components")+
  coord_flip()+
  theme_classic()+
  theme(strip.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank())

dev.off()
