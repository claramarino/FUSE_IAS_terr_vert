# Compute FUSE-IAS score and define priority lists

rm(list=ls())

library(tidyverse)


#### Load data ####

# load proba extinction
GE2_list <- readRDS("Output/01_Proba_extinction_IAS_ABMR.rds")
# load standardized FUn et FSpe metrics
std_fun_fsp <- readRDS("Output/02_standardized_FUn_FSp_ABMR.rds")

# bind databases

lapply(GE2_list, dim)
lapply(std_fun_fsp, dim)

ge_fun_fsp <- list()
for (i in names(GE2_list)){
  ge_fun_fsp[[i]] <- inner_join(GE2_list[[i]], std_fun_fsp[[i]])
}



# chose nb dim functional space & nb nearest neighbors 
# for each taxonomic group (following script 01)
nD = c("amph" = 3, "bird" = 7, "mam" = 5,"rept" = 5)
NN = 5

# check ranges 
lapply(ge_fun_fsp, function(x) range(x$p))
lapply(ge_fun_fsp, function(x) range(x$FUn_5NN))
lapply(ge_fun_fsp, function(x) range(x$FSp_3D))


#### Calculate FUSE-IAS score ####

# Formula from Griffin et al. 2020 ; Gouhier & Pillai 2020
# FUSE = ln(1 + FUn*GE + Fsp*GE)
# FUn, FSp and GE are the standardized FUn, FSp and GE (0-1)

fusias <- list()
for (i in names(nD)){
  dim = nD[i]
  db_fus <- ge_fun_fsp[[i]] %>% 
    select(c(binomial, cate_p_ext, severity, p, 
             contains(paste0(dim, "D")),
             contains(paste0(NN, "NN"))))
  colnames(db_fus) <- c("binomial", "cate_p_ext","severity", "p","FSp","FUn")
  fusias[[i]] <- db_fus %>%
    mutate(FUSE_IAS = log(1 + FUn*p + FSp*p))
}
# potential maximum ~= log(3)
log(1+0.99999+0.99999)

lapply(fusias, function(x) round(range(x$FUSE_IAS), 4))
# max is never reached

# save FUSE-IAS scores
saveRDS(fusias, "Output/03_FUSE_IAS_scores_ABMR.rds")


#### Define FUSE-IAS priority lists ####

# get the total median per class
tot_median <- lapply(fusias, function(x) median(x$FUSE_IAS))

# count for each sp the number of fusias score above the median
fusias_over_med <- list()
for(k in names(fusias)){
  temp <- fusias[[k]] %>%
    mutate(fus_over_med = FUSE_IAS-tot_median[[k]]) %>%
    mutate(is_over_med = if_else(fus_over_med>0, "yes","no"))
  fusias_over_med[[k]] <- temp %>%
    group_by(binomial, is_over_med) %>%
    summarize(nb = n()) %>% ungroup() %>%
    filter(is_over_med=="yes")
}

# pull species names for all sp 95% above median (for fusias lists)
fusias_95 <- lapply(fusias_over_med, function(x){
  x %>%
    filter(is_over_med=="yes") %>%
    filter(nb>=950) %>%
    pull(binomial)
})


# pull species names for all sp 80% above median (for fusias borderline lists)
fusias_80_border<- lapply(fusias_over_med, function(x){
  x %>%
    filter(is_over_med=="yes") %>%
    filter(nb>=800 & nb<950) %>%
    pull(binomial)
})


# separate fusias_95 into fusias list, fusias research list & fusias watch list
# get median fusias score per species + sd
# fusias core list = fusias95 + CR, EN, VU
# fusias super core list = fusias95 + CR, EN, VU + severity == 1
# fusias research list = fusias95 + DD
# fusias watch list = fusias95 + LC, NT
# get fusias borderline list = fusias80 + CR, EN, VU

sp_median <- lapply(fusias, function(x) {
  x %>% group_by(binomial, cate_p_ext, severity) %>%
    summarize(p_med = median(p),
              FUSE_IAS_med = median(FUSE_IAS),
              FUSE_IAS_sd = sd(FUSE_IAS)) %>%
    ungroup()
})


fusias_core_list <- fusias_research_list <- fusias_watch_list <- 
  fusias_border_list <- list()

for(k in names(sp_median)){
  
  fusias_core_list[[k]] <- sp_median[[k]] %>%
    filter(cate_p_ext %in% c("CR","EN","VU")) %>%
    filter(binomial %in% fusias_95[[k]])
  
  fusias_research_list[[k]] <- sp_median[[k]] %>%
    filter(cate_p_ext == "DD") %>%
    filter(binomial %in% fusias_95[[k]])
  
  fusias_watch_list[[k]] <- sp_median[[k]] %>%
    filter(cate_p_ext %in% c("LC","NT")) %>%
    filter(binomial %in% fusias_95[[k]])
  
  fusias_border_list[[k]] <- sp_median[[k]] %>%
    filter(cate_p_ext %in% c("CR","EN","VU")) %>%
    filter(binomial %in% fusias_80_border[[k]])
  
}


# how many species in each list
lapply(fusias_core_list, nrow)
lapply(fusias_research_list, nrow) # pas d'esp en research list
lapply(fusias_watch_list, nrow)
lapply(fusias_border_list, nrow)


# save lists 
saveRDS(fusias_core_list, "Output/03_FUSIAS_core_list_ABMR.rds")
saveRDS(fusias_watch_list, "Output/03_FUSIAS_watch_list_ABMR.rds")
saveRDS(fusias_border_list, "Output/03_FUSIAS_borderline_list_ABMR.rds")



#### Plot for Fig. 2 nb of species per list ####

library(ggpubr)

# load species lists
core_list <- readRDS("Output/03_FUSIAS_core_list_ABMR.rds")
watch_list <- readRDS("Output/03_FUSIAS_watch_list_ABMR.rds")
border_list <- readRDS("Output/03_FUSIAS_borderline_list_ABMR.rds")

hsize <- 3

donut_list <- function(class, list){
  
  if(list == "core"){to_apply = core_list}
  if(list == "border"){to_apply = border_list}
  if(list == "watch"){to_apply = watch_list}
  
  df = data.frame(
    value = as.numeric(lapply(to_apply, function(x){table(x$cate_p_ext)})[[class]]),
    group = names(lapply(to_apply, function(x){table(x$cate_p_ext)})[[class]]),
    x = hsize)
  
  p <- ggplot(df, aes(x = hsize, y = value, fill = group)) +
    geom_col(color = NA) +
    geom_text(aes(label = value),
              position = position_stack(vjust = 0.5), 
              color="grey20") +
    coord_polar(theta = "y") +
    scale_fill_manual(values = c("LC" = "mediumseagreen", 
                                 "NT" = "olivedrab2", 
                                 "VU" = "gold1", 
                                 "EN" = "darkorange2", 
                                 "CR" = "firebrick2"))+
    xlim(c(0.2, hsize + 0.5)) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank())
  
  return(p)
  
}

ac <- donut_list("amph","core")
bc <- donut_list("bird","core")
lc <- donut_list("rept","core")
mc <- donut_list("mam","core")

aw <- donut_list("amph","watch")
bw <- donut_list("bird","watch")
lw <- donut_list("rept","watch")
mw <- donut_list("mam","watch")

ab <- donut_list("amph","border")
bb <- donut_list("bird","border")
lb <- donut_list("rept","border")
mb <- donut_list("mam","border")


allcore <- ggarrange(ac, bc, lc, mc, ncol=4, nrow=1, 
                     common.legend = T, legend = "right")
allwatch <- ggarrange(aw, bw, lw, mw, ncol=4, nrow=1, 
                      common.legend = T, legend = "right")
allborder <- ggarrange(ab, bb, lb, mb, ncol=4, nrow=1, 
                       common.legend = T, legend = "right")


pdf("Fig/03_Fig2_donut_lists.pdf")
ggarrange(allcore, allwatch, allborder,
          nrow = 3, ncol = 1)
dev.off()



