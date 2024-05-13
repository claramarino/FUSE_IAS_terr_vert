rm(list=ls())

# Compute distance matrix and PCoA
# Calculate functional originality and specialization

library(tidyverse)
library(tibble)
library(ggpubr)
library(ggExtra)

#### Distance matrix and functional axes ####

# Load trait data 
path <- "Data/Species_traits_terr_vert.xlsx"
sheets <- openxlsx::getSheetNames(path) 
data <- lapply(sheets, openxlsx::read.xlsx, xlsxFile=path) 
names(data) <- c("amph","bird","mam","rept")


# Clean trait data for computing distance matrix
# remove insular endemic trait (useful for analyzing the FUSE score after)
# make sure all categorical traits are factors
# Habitat as ordered factor
str(data)
data_ft <- lapply(data, function(x){
  x %>%
    select(-insular_endemic) %>%
    mutate(Habitat=factor(Habitat, ordered = T,
                          levels = c("1","2","3","4","5+"))) %>%
    mutate_if(is.character, as.factor)
})
str(data_ft)


# distance and pcoa using the mFD package

# define the type of traits
traits_cat <- list(
  amph = data.frame(
    trait_name = colnames(data_ft$amph)[2:5],
    trait_type = c("O","N", "N", "Q")),
  bird = data.frame(
    trait_name = colnames(data_ft$bird)[2:11],
    trait_type = c("Q","Q","Q","N","N","Q","Q","Q","Q","O")),
  mam = data.frame(
    trait_name = colnames(data_ft$mam)[2:6],
    trait_type = c("O","N", "N","N", "Q")),
  rept = data.frame(
    trait_name = colnames(data_ft$rept)[2:6],
    trait_type = c("O","N", "N","N", "Q")))

# Functional distance based on the traits
names(data_ft)
sp_dist <- traits_cat
for(i in names(traits_cat)){
  sp_dist[[i]] <- mFD::funct.dist(
    sp_tr    = data_ft[[i]] %>% column_to_rownames("binomial"),
    tr_cat        = traits_cat[[i]],
    metric        = "gower",
    scale_euclid  = "scale_center",
    ordinal_var   = "classic",
    weight_type   = "equal",
    stop_if_NA    = T)
  print(i)
}

saveRDS(sp_dist, "Output/02_sp_dist_mFD_ABMR.rds")
for(i in names(sp_dist)){
  saveRDS(sp_dist[[i]], paste0("Output/02_sp_dist_mFD_", i, ".rds"))
  print(i)
}


# the mfd::fspaces_quality function does not function for birds
# so 1. run classic pcoa on the sp_dist matrix, for all groups to be consistent
# and 2. calculate mean deviation as in the mfd::fspaces_quality function 

pcoa_out <- traits_cat
for(i in names(pcoa_out)){
  sp_dist <- readRDS(paste0("Output/02_sp_dist_mFD_", i, ".rds"))
  pcoa_out[[i]] <- ape::pcoa(sp_dist)
  print(i)
}
saveRDS(pcoa_out, "Output/02_fsp_pcoa_out_ABMR.rds")

# calculate mean deviation
pcoa_out <- readRDS("Output/02_fsp_pcoa_out_ABMR.rds")

nbdim = 10
quality_fsp <- traits_cat

for(i in names(quality_fsp)){
  mat_coord <- as.data.frame(pcoa_out[[i]]$vectors[,1:nbdim])
  row.names(mat_coord)<-data_ft[[i]]$binomial
  colnames(mat_coord)<-paste("PC",1:nbdim,sep="")
  
  mad <- rmsd <- mad_scaled <- rmsd_scaled <- c()
  
  # initial distances
  sp_dist <- readRDS(paste0("Output/02_sp_dist_mFD_", i, ".rds"))
  print(i)
  
  for (n in 1:nbdim){
    # Distances recalculated from the n first axes of the PCoA
    new <- stats::dist(mat_coord[,1:n])
    
    new_mat <- as.matrix(new)
    ini_mat <- as.matrix(sp_dist)
    # absolute distances
    diff <- mean(abs(ini_mat - new_mat))
    diff2 <- sqrt(mean((ini_mat - new_mat)^2))
    mad <- c(mad, diff)
    rmsd <- c(rmsd, diff2)
    # standardize distances
    std_new_dist <- new_mat*max(ini_mat)/max(new_mat)
    # scaled mean absolute deviation + mean square deviation
    sdiff <- mean(abs(ini_mat - std_new_dist))
    sdiff2 <- sqrt(mean((ini_mat - std_new_dist)^2))
    mad_scaled <- c(mad_scaled, sdiff)
    rmsd_scaled <- c(rmsd_scaled, sdiff2)
    print(n)

  }
  
  quality_fsp[[i]] <- data.frame(
    axes = paste0("pcoa_", 1:nbdim),
    mad = mad,
    rmsd = rmsd,
    mad_scaled = mad_scaled,
    rmsd_scaled = rmsd_scaled)
} 

quality_fsp

lapply(quality_fsp, function(x){
  apply(x[,-1], 2, which.min)
})


#### Variance explained in the selected functional spaces ####
# with rmsd as metric
# 3D for amphibians
# 7D for birds
# 5D for mammals
# 5D for reptiles

var_abmr <- lapply(pcoa_out, function(x){
  tot_pos <- sum(x$values$Eigenvalues[x$values$Eigenvalues>0])
  var_b <- data.frame(
    egv = x$values$Eigenvalues[1:10]/tot_pos,
    egv_cum = cumsum(x$values$Eigenvalues[1:10]/tot_pos))
  return(var_b)
})

var_abmr

#### Functional Specialization & Uniqueness ####

nbdim=10
mat_dis <- mat_coord <- list()
pcoa_out <- readRDS("Output/02_fsp_pcoa_out_ABMR.rds")
for(i in names(data_ft)){
  mat_dis[[i]] <- as.matrix(readRDS(paste0("Output/02_sp_dist_mFD_", i, ".rds")))
  mat_coord[[i]] <- as.data.frame(pcoa_out[[i]]$vectors[,1:nbdim])
  row.names(mat_coord[[i]])<-data_ft[[i]]$binomial
  colnames(mat_coord[[i]])<-paste("PC",1:nbdim,sep="")
}

# find k closest species

k_closest_dist <- lapply(mat_dis, function(x){
  name_sp <- attributes(x)$dimnames[[1]]
  
  k_closest <- data.frame(matrix(ncol = 10, nrow = 0))
  colnames(k_closest) <- paste("closest_", 1:10, sep="")
  
  mat=as.matrix(x)
  
  for(i in 1:length(name_sp)){
    col1 <- mat[i,]
    sorted <- sort(col1)
    
    #select the 10 first sp but no zero (dist with the sp itself)
    k10 <- sorted[2:11]
    names(k10) = names(k_closest)
    
    k_closest <- bind_rows(k_closest, k10)
  }
  rownames(k_closest) = name_sp
  
  return(k_closest)  
})


# calculate FUn as in Pimiento et al 2021 and 
# https://www.pnas.org/doi/10.1073/pnas.1716643115

funi <- lapply(k_closest_dist, function(x){
  # take different numbers of nearest neighbors for sensitivity analyses
  data.frame(binomial = rownames(x),
             FUn_3NN = rowSums(x[,1:3]),
             FUn_4NN = rowSums(x[,1:4]),
             FUn_5NN = rowSums(x[,1:5]),
             FUn_6NN = rowSums(x[,1:6]),
             FUn_7NN = rowSums(x[,1:7]),
             FUn_8NN = rowSums(x[,1:8]))
})


plot(funi$amph$FUn_5NN,funi$amph$FUn_4NN)
hist(funi$bird$FUn_5NN)

#### Specialization ####

# distance of each sp to the center of multidimensional functional space
# calculate distance for different number of dimensions 
# for further sensitivity analyses

fsp <- lapply(mat_coord, function(x){
  # define output df
  dist_all <- data.frame(binomial = row.names(x))
  # compute fspe for each dimension 
  for (n in 3:10){
    coord = x[1:n]
    center_coord = colMeans(coord)
    all_d <- c()
    for(i in 1:nrow(coord)){
      d = dist(bind_rows(coord[i,], center_coord))
      all_d <- c(all_d, as.numeric(d))
    }
    # store each dim in 1 df
    dist_n = data.frame(
      binomial = row.names(x),
      dist = all_d)
    colnames(dist_n) = c("binomial", paste0("FSp_", n,"D"))
    # join all dimensions
    dist_all <- left_join(dist_all, dist_n, by ="binomial")
  }
  return(dist_all)
})


#### save Fun & FSp ####

fun_fsp <- list()
for(i in names(fsp)){
  fun_fsp[[i]] <- left_join(funi[[i]], fsp[[i]])
}

saveRDS(fun_fsp, "Output/02_FUn_FSp_ABMR2.rds")

# standardize FUn & FSp 
fun_fsp <- readRDS("Output/02_FUn_FSp_ABMR2.rds")

std_fun_fsp <- lapply(fun_fsp, function(x){
  
  y <- x %>% column_to_rownames("binomial")
  # normalize by max min linear
  maxcol <- apply(y, 2, max)
  mincol <- apply(y, 2, min)
  
  std <- y
  for (i in 1:length(mincol)){
    std[,i] <- (std[,i]-mincol[i])/(maxcol[i]-mincol[i])
  }
  
  std$binomial = rownames(std)
  
  return(std)
})
hist(std_fun_fsp$amph$FSp_3D)
hist(std_fun_fsp$rept$FUn_5NN)
plot(std_fun_fsp$amph$FSp_3D, std_fun_fsp$amph$FSp_5D)

saveRDS(std_fun_fsp, "Output/02_standardized_FUn_FSp_ABMR2.rds")

fun_fsp <- readRDS("Output/02_standardized_FUn_FSp_ABMR.rds")
fun_fsp2 <- readRDS("Output/02_standardized_FUn_FSp_ABMR2.rds")

plot(fun_fsp$bird$FSp_3D, fun_fsp2$bird$FSp_3D)
#change between 6 and 7D for birds
plot(fun_fsp2$bird$FSp_6D, fun_fsp2$bird$FSp_7D)
#change between 5 and 4D for reptiles
plot(fun_fsp2$rept$FSp_5D, fun_fsp2$rept$FSp_4D)



#### Supplementary figures ####

# Uniqueness value plotted on functional space

# Plot functional space with FSp/FUn color 

# function to apply to each class 
# corresponding title
title = list("amph" = "Amphibians", 
             "bird" = "Birds",
             "rept" = "Lizards",
             "mam" = "Mammals")
plot_FUn <- function(tax){
  a <- left_join(
    mat_coord[[tax]] %>%
      rownames_to_column("binomial"),
    std_fun_fsp[[tax]])
  pa <- ggplot(a, aes(PC1, PC2, color = FUn_5NN))+
    geom_point(size = 2, alpha = .7)+
    scale_color_viridis_c(direction = -1)+
    labs(title = title[[tax]])
  return(pa)
}


pamph <- plot_FUn("amph")
pbird <- plot_FUn("bird")
pliz <- plot_FUn("rept")
pmam <- plot_FUn("mam")

ggarrange(pamph, pbird, pliz, pmam, nrow=2, ncol = 2,
          common.legend = T, legend = "top")

# PC3 PC4
plot_FUn34 <- function(tax){
  a <- left_join(
    mat_coord[[tax]] %>%
      rownames_to_column("binomial"),
    std_fun_fsp[[tax]])
  pa <- ggplot(a, aes(PC3, PC4, color = FUn_5NN))+
    geom_point(size = 2, alpha = .7)+
    scale_color_viridis_c(direction = -1)+
    labs(title = title[[tax]])
  return(pa)
}

pbird34 <- plot_FUn34("bird")
pliz34 <- plot_FUn34("rept")
pmam34 <- plot_FUn34("mam")

ggarrange(NULL, pbird34, pliz34, pmam34, nrow=2, ncol = 2,
          common.legend = T, legend = "top")

# save fig with all dim
pdf("Fig/02_SupplFig2_FUn_funct_space.pdf",9,6)
ggarrange(pamph, pbird, pliz, pmam,
          NULL, pbird34, pliz34, pmam34, nrow=2, ncol = 4,
          common.legend = T, legend = "top")
dev.off()


# FSp plotted on functional space

# nb dim for each taxon
nD = c("amph" = 3, "bird" = 7, "mam" = 5,"rept" = 5)

ge_fun_fsp[[i]] %>% 
  select(c(binomial, cate_p_ext, severity, p, 
           contains(paste0(dim, "D")),
           contains(paste0(NN, "NN"))))
colnames(db_fus) <- c("binomial", "cate_p_ext","severity", "p","FSp","FUn")

plot_FSp <- function(tax, dima, dimb){
  dim = nD[tax]
  coord <- mat_coord[[tax]] %>%
    rownames_to_column("binomial") %>%
    select(-PC10) %>%
    select(c(binomial, 
             contains(paste0("PC", dima)), 
             contains(paste0("PC", dimb))))
  colnames(coord) <- c("binomial","PCa", "PCb")

  db_fsp <- std_fun_fsp[[tax]] %>% 
    select(c(binomial, contains(paste0(dim, "D"))))
  colnames(db_fsp) <- c("binomial", "FSp")

  a <- left_join(coord, db_fsp)
  
  pa <- ggplot(a, aes(PCa, PCb, color = FSp))+
    geom_point(size = 2, alpha = .7)+
    scale_color_viridis_c(direction = -1)+
    labs(title = title[[tax]])+
    xlab(paste0("PC", dima)) + ylab(paste0("PC", dimb))
  return(pa)
}

spamph <- plot_FSp("amph", 1,2)
spbird <- plot_FSp("bird", 1,2)
spliz <- plot_FSp("rept", 1,2)
spmam <- plot_FSp("mam", 1,2)

ggarrange(spamph, spbird, spliz, spmam, nrow=2, ncol = 2,
          common.legend = T, legend = "top")

spbird34 <- plot_FSp("bird", 3,4)
spliz34 <- plot_FSp("rept", 3,4)
spmam34 <- plot_FSp("mam", 3,4)

ggarrange(NULL, spbird34, spliz34, spmam34, nrow=2, ncol = 2,
          common.legend = T, legend = "top")

# save fig with all dim
pdf("Fig/02_SupplFig1_FSp_funct_space.pdf",9,6)
ggarrange(spamph, spbird, spliz, spmam,
          NULL, spbird34, spliz34, spmam34, nrow=2, ncol = 4,
          common.legend = T, legend = "top")
dev.off()


### Correlation between FUn & FSpe

# check correlation between FUn, FSp for amph
plot_fspfun <- function (tax){
  if(tax=="amph"){
    p = ggplot(std_fun_fsp$amph)+ 
      geom_point(aes(x = FUn_5NN, y = FSp_3D), color = "grey20", alpha=.3)}
  if(tax=="bird"){
    p = ggplot(std_fun_fsp$bird)+ 
      geom_point(aes(x = FUn_5NN, y = FSp_7D), color = "grey20", alpha=.3)}
  if(tax=="rept"){
    p = ggplot(std_fun_fsp$rept)+ 
      geom_point(aes(x = FUn_5NN, y = FSp_5D), color = "grey20", alpha=.3)}
  if(tax=="mam"){
    p = ggplot(std_fun_fsp$mam)+ 
      geom_point(aes(x = FUn_5NN, y = FSp_5D), color = "grey20", alpha=.3)}
  ph <- ggMarginal(p,type = "histogram", margins = "both", size = 4)
  return(ph)
}

ca <- plot_fspfun("amph")
cb <- plot_fspfun("bird")
cr <- plot_fspfun("rept")
cm <- plot_fspfun("mam")


pdf("Fig/02_SupplFig3_cor_Fsp_Fun.pdf")
ggarrange(ca, cb, cr, cm, nrow=2, ncol=2, 
          labels = c("Amphibians", "Birds","Lizards","Mammals"), label.x=.3)
dev.off()

