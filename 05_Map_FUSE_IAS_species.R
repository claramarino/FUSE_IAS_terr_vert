# map all fusias core list species
rm(list=ls())

library(tidyverse)
library(sf)
library(ggpubr)

# load species lists
core_list <- readRDS("Output/05_FUSIAS_core_list_all_groups")
watch_list <- readRDS("Output/05_FUSIAS_watch_list_all_groups")
border_list <- readRDS("Output/05_FUSIAS_borderline_list_all_groups")


# load species distribution in cells 55km
res = "110" # 110 or 55
proj = "Z:/THESE/6_Projects/predict_vulnerability/"

# load grid 
grid_terr <- readRDS(paste0(proj, "Output/RISK_32_grid_", res, "km"))


######### Extract SR and fusias score per cell ########

# load sp presence per cell
sp_fold = "Output/Sensitivity/Cells_nat_all_bmr/"
sp_b1 <- readRDS(paste0(proj, sp_fold, "RISK_32_cells_nat_all_BIRD_110_55_chunk1"))
sp_b2 <- readRDS(paste0(proj, sp_fold, "RISK_32_cells_nat_all_BIRD_110_55_chunk2"))
sp_b3 <- readRDS(paste0(proj, sp_fold, "RISK_32_cells_nat_all_BIRD_110_55_chunk3"))
sp_b4 <- readRDS(paste0(proj, sp_fold, "RISK_32_cells_nat_all_BIRD_110_55_chunk4"))
sp_m <- readRDS(paste0(proj, sp_fold, "RISK_32_cells_nat_all_MAM_110_55"))
sp_r <- readRDS(paste0(proj, sp_fold, "RISK_32_cells_nat_all_REPT_110_55"))
sp_a <- readRDS(paste0(proj, sp_fold, "RISK_32_cells_nat_all_AMPH_110_55"))

# load species id
path_poly <- "Output/Sensitivity/Polygons_native_all_bmr/"
mam_nat <- readRDS(paste0(proj, path_poly, "RISK_32_Polygon_nat_all_MAM"))
mam_IDs <- mam_nat %>% st_drop_geometry() %>%
  dplyr::mutate(objectid = 1:nrow(mam_nat)) %>%
  select(objectid, binomial)
rm(mam_nat)
b1_nat <- readRDS(paste0(proj, path_poly, "RISK_32_Polygon_nat_all_BIRD1"))
b1_IDs <- b1_nat %>% st_drop_geometry() %>%
  dplyr::mutate(objectid = 1:nrow(b1_nat)) %>%
  select(objectid, binomial)
rm(b1_nat)
b2_nat <- readRDS(paste0(proj, path_poly, "RISK_32_Polygon_nat_all_BIRD2"))
b2_IDs <- b2_nat %>% st_drop_geometry()  %>%
  dplyr::mutate(objectid = 1:nrow(b2_nat)) %>%
  select(objectid, binomial)
rm(b2_nat)
b3_nat <- readRDS(paste0(proj, path_poly, "RISK_32_Polygon_nat_all_BIRD3"))
b3_IDs <- b3_nat %>% st_drop_geometry()  %>%
  dplyr::mutate(objectid = 1:nrow(b3_nat)) %>%
  select(objectid, binomial)
rm(b3_nat)
b4_nat <- readRDS(paste0(proj, path_poly, "RISK_32_Polygon_nat_all_BIRD4"))
b4_IDs <- b4_nat %>% st_drop_geometry()  %>%
  dplyr::mutate(objectid = 1:nrow(b4_nat)) %>%
  select(objectid, binomial)
rm(b4_nat)
bird_IDs <- bind_rows(b1_IDs, b2_IDs, b3_IDs, b4_IDs)
rept_nat <- readRDS(paste0(proj, path_poly, "RISK_32_Polygon_nat_all_REPT"))
rept_IDs <- rept_nat %>% st_drop_geometry()  %>%
  dplyr::mutate(objectid = 1:nrow(rept_nat)) %>%
  select(objectid, binomial)
rm(rept_nat)
amph_nat <- readRDS(paste0(proj, path_poly, "RISK_32_Polygon_nat_all_AMPH"))
amph_IDs <- amph_nat %>% st_drop_geometry()  %>%
  dplyr::mutate(objectid = 1:nrow(amph_nat)) %>%
  select(objectid, binomial)
rm(amph_nat)

# count total number of species
length(unique(mam_IDs$binomial))
length(unique(rept_IDs$binomial))
length(unique(bird_IDs$binomial))
length(unique(amph_IDs$binomial))

# extract cells ids for each sp 
# sp_ids = b1_IDs
# sp_cells = sp_b1

get_cell_ids <- function(sp_ids, sp_cells){
  df <- data.frame()
  for(i in sp_ids$objectid){
    cells = sp_cells[[i]][[paste0("cells",res)]]
    if(is_empty(cells)){
      sp_df = data.frame(
        binomial = sp_ids$binomial[i],
        cell_id =  NA)
    } else {
      sp_df = data.frame(
        binomial = sp_ids$binomial[i],
        cell_id =  cells)
    }
    df <- bind_rows(df, sp_df)
  }
  return(df %>% distinct())
}

cell_ids_m <- get_cell_ids(sp_ids = mam_IDs, sp_cells = sp_m)
sr_tot_m <- cell_ids_m %>% 
  group_by(cell_id) %>%
  summarise(SR_tot_mam = n())
hist(sr_tot_m$SR_tot_mam)

cell_ids_b1 <- get_cell_ids(sp_ids = b1_IDs, sp_cells = sp_b1)
cell_ids_b2 <- get_cell_ids(sp_ids = b2_IDs, sp_cells = sp_b2)
cell_ids_b3 <- get_cell_ids(sp_ids = b3_IDs, sp_cells = sp_b3)
cell_ids_b4 <- get_cell_ids(sp_ids = b4_IDs, sp_cells = sp_b4)
cell_ids_r <- get_cell_ids(sp_ids = rept_IDs, sp_cells = sp_r)
cell_ids_a <- get_cell_ids(sp_ids = amph_IDs, sp_cells = sp_a)


cell_ids <- list(
  amph = cell_ids_a,
  bird = bind_rows(cell_ids_b1, cell_ids_b2, cell_ids_b3, cell_ids_b4) %>% distinct(),
  mam = cell_ids_m,
  rept = cell_ids_r
)

saveRDS(cell_ids, paste0("Output/06_species_in_cells_all_groups", res))


###### Calculate SR ######

cell_ids <- readRDS(paste0("Output/06_species_in_cells_all_groups", res))

# calculate sr_tot
sr_tot <- lapply(cell_ids, function(x){
  x %>% 
    group_by(cell_id) %>%
    summarise(SR_tot = n())
})

# calculate sr fuse-ias core list 
core_list_bino <- bind_rows(core_list)
watch_list_bino <- bind_rows(watch_list)
border_list_bino <- bind_rows(border_list)

# check if all sp from core list have range info
setdiff(core_list$amph$binomial, unique(cell_ids$amph$binomial)) # manque 7 amph / 600
setdiff(core_list$bird$binomial, unique(cell_ids$bird$binomial)) # manque 14 birds /343
setdiff(core_list$mam$binomial, unique(cell_ids$mam$binomial)) # tous les mammals sont présents
setdiff(core_list$rept$binomial, unique(cell_ids$rept$binomial)) # manque 40 rept/ 199
# parce qu'il n'ont pas de range "présent" (only "possibly extinct" ranges)
# should I include those ranges?

str(watch_list)

# # check if all sp from core list have range info
# setdiff(watch_list$amph$binomial, unique(cell_ids$amph$binomial)) # manque 7 amph / 600
# setdiff(watch_list$bird$binomial, unique(cell_ids$bird$binomial)) # manque 14 birds /343
# setdiff(watch_list$mam$binomial, unique(cell_ids$mam$binomial)) # tous les mammals sont présents
# setdiff(watch_list$rept$binomial, unique(cell_ids$rept$binomial)) # manque 40 rept/ 199
# # parce qu'il n'ont pas de range "présent" (only "possibly extinct" ranges)
# # should I include those ranges?

# core list
sr_core_list <- lapply(cell_ids, function(x){
  x %>% 
    filter(binomial %in% core_list_bino$binomial) %>%
    group_by(cell_id) %>%
    summarise(SR_core_list = n())
})

# sum fusias core list
fusias_sum_core <- lapply(cell_ids, function(x){
  left_join(x %>% filter(binomial %in% core_list_bino$binomial), 
                core_list_bino) %>%
    group_by(cell_id) %>%
    summarise(tot_fusias = sum(FUSE_IAS_med))
})


# watch list
sr_watch_list <- lapply(cell_ids, function(x){
  x %>% 
    filter(binomial %in% watch_list_bino$binomial) %>%
    group_by(cell_id) %>%
    summarise(SR_watch_list = n())
})

# border list 
sr_border_list <- lapply(cell_ids, function(x){
  x %>% 
    filter(binomial %in% border_list_bino$binomial) %>%
    group_by(cell_id) %>%
    summarise(SR_border_list = n())
})


sr_all <- mapply(left_join,
      (mapply(left_join,
          mapply(full_join, sr_tot, sr_core_list, SIMPLIFY = FALSE),
          mapply(full_join, sr_watch_list, sr_border_list, SIMPLIFY = FALSE),
          SIMPLIFY = FALSE)),
  fusias_sum_core, SIMPLIFY = FALSE)



colnames(sr_all$mam)


str(sr_all)


# sr_all <- lapply(sr_all, function(x){
#   x %>% replace_na(list(SR_core_list = 0, tot_fusias = 0)) %>%
#     filter(!is.na(cell_id))
# })

saveRDS(sr_all, paste0("Output/06_SR_FUSIAS_all_groups_", res))






######## MAPS #########

sr_all <- readRDS(paste0("Output/06_SR_FUSIAS_all_groups_", res))
grid_terr$cell_id = 1:nrow(grid_terr)



plot_sr_core_list <- function(class){
  dat <- sr_all[[class]] 
  dat_sf <- left_join(grid_terr, dat) %>%
    mutate(SR_core_list = if_else(SR_core_list == 0, NA, SR_core_list))
  p <- ggplot(dat_sf)+
    geom_sf(aes(fill = SR_core_list), color = NA) +
    scale_fill_viridis_c(option="plasma", direction = -1, na.value = "grey85")+
    theme_classic()
  return(p)
}

sra <- plot_sr_core_list("amph")
srb <- plot_sr_core_list("bird")
srl <- plot_sr_core_list("rept")
srm <- plot_sr_core_list("mam")


pdf(paste0("Fig/Chap6_Fig2_map_SR_core_list_", res, ".pdf"))
ggarrange(sra, srb, srl, srm, ncol = 2, nrow=2, legend = "top")
dev.off()

# watch list
plot_sr_watch_list <- function(class){
  dat <- sr_all[[class]] 
  dat_sf <- left_join(grid_terr, dat) %>%
    mutate(SR_watch_list = if_else(SR_watch_list == 0, NA, SR_watch_list))
  p <- ggplot(dat_sf)+
    geom_sf(aes(fill = SR_watch_list), color = NA) +
    scale_fill_viridis_c(option="viridis", direction = -1, na.value = "grey85")+
    theme_classic()
  return(p)
}

wa <- plot_sr_watch_list("amph")
wb <- plot_sr_watch_list("bird")
wl <- plot_sr_watch_list("rept")
wm <- plot_sr_watch_list("mam")

pdf(paste0("Fig/Chap6_Suppl_Fig2_map_SR_watch_list_", res, ".pdf"))
ggarrange(wa, wb, wl, wm, ncol = 2, nrow=2, legend = "top")
dev.off()

# border list
# sr too low, plot only cells with at least one sp (max SR border = 2)
plot_sr_border_list <- function(class){
  dat <- sr_all[[class]] 
  dat_sf <- left_join(grid_terr, dat) %>%
    mutate(SR_border_list = if_else(SR_border_list > 0, 1, NA))
  p <- ggplot(dat_sf)+
    geom_sf(aes(fill = SR_border_list), color = NA) +
    scale_fill_viridis_c(option="rocket", direction = -1, na.value = "grey85")+
    theme_classic()
  return(p)
}

ba <- plot_sr_border_list("amph")
bb <- plot_sr_border_list("bird")
bl <- plot_sr_border_list("rept")
bm <- plot_sr_border_list("mam")

pdf(paste0("Fig/Chap6_Suppl_Fig2_map_SR_border_list_", res, ".pdf"))
ggarrange(ba, bb, bl, bm, ncol = 2, nrow=2, legend = "top")
dev.off()

# cb de cell contiennent des core et des watch
lapply(sr_all, function(x){
  nrow(x %>% filter(SR_watch_list>0 & SR_core_list>0))
})
# cb de cells continennt de watch et pas des core
lapply(sr_all, function(x){
  nrow(x %>% filter(SR_watch_list>0 & is.na(SR_core_list)))
})


# sum total fusias 
plot_sum_fusias_core_list <- function(class){
  dat <- sr_all[[class]] 
  dat_sf <- left_join(grid_terr, dat) %>%
    mutate(tot_fusias = if_else(tot_fusias == 0, NA, tot_fusias))
  p <- ggplot(dat_sf)+
    geom_sf(aes(fill = tot_fusias), color = NA) +
    scale_fill_viridis_c(option="plasma", direction = -1, na.value = "grey85")+
    theme_classic()
  return(p)
}

sa <- plot_sum_fusias_core_list("amph")
sb <- plot_sum_fusias_core_list("bird")
sl <- plot_sum_fusias_core_list("rept")
sm <- plot_sum_fusias_core_list("mam")

ggarrange(sa, sb, sl, sm, ncol = 2, nrow=2)



# realtionship between sr and tot fusias

for(i in 1:length(sr_all)){
  sr_all[[i]] <- sr_all[[i]] %>%
    mutate(class = names(sr_all)[i])
}

df_sr <- bind_rows(sr_all) %>%
  mutate(Group = case_when(
    class=="amph" ~ "Amphibians",
    class=="bird" ~ "Birds",
    class=="mam" ~ "Mammals",
    class=="rept" ~ "Lizards"
  ))


pdf("Fig/Chap6_Suppl_Fig1_lm_sum_fusias_sr.pdf", 7,5)
ggplot(df_sr %>% filter(SR_core_list>0))+
  geom_point(aes(x = SR_core_list, y = tot_fusias))+
  geom_smooth(aes(x = SR_core_list, y = tot_fusias), method = "lm")+
  facet_wrap(~Group, scale = "free") +
  theme_classic() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 15))+
  xlab("Species richness (FUSE-IAS core list)")+
  ylab("Sum of FUSE-IAS scores")
dev.off()


cor.test(df_sr %>% filter(SR_core_list>0) %>% pull(SR_core_list),
         df_sr %>% filter(SR_core_list>0) %>% pull(tot_fusias))

cor.test(df_sr %>% filter(SR_core_list>0 & Group =="Amphibians") %>% pull(SR_core_list),
         df_sr %>% filter(SR_core_list>0 & Group =="Amphibians") %>% pull(tot_fusias))
cor.test(df_sr %>% filter(SR_core_list>0 & Group =="Birds") %>% pull(SR_core_list),
         df_sr %>% filter(SR_core_list>0 & Group =="Birds") %>% pull(tot_fusias))
cor.test(df_sr %>% filter(SR_core_list>0 & Group =="Lizards") %>% pull(SR_core_list),
         df_sr %>% filter(SR_core_list>0 & Group =="Lizards") %>% pull(tot_fusias))
cor.test(df_sr %>% filter(SR_core_list>0 & Group =="Mammals") %>% pull(SR_core_list),
         df_sr %>% filter(SR_core_list>0 & Group =="Mammals") %>% pull(tot_fusias))

