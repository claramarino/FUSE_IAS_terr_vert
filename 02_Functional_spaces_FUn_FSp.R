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


# Compute distance matrix for each group 

mat_dis <- lapply(data_ft, function(x){
  ft_ok <- x %>% column_to_rownames("binomial")
  mat <- cluster::daisy(ft_ok, metric = "gower")
  return(mat)
})

saveRDS(mat_dis, "Output/02_ft_dist_matrix_ABMR.rds")

mat_dis <- readRDS("Output/02_ft_dist_matrix_ABMR.rds")

# Apply PCoA to distance matrix
# /!\ long time ~1h20min
Sys.time()
mat_pcoa <- lapply(mat_dis, ape::pcoa)
Sys.time()
saveRDS(mat_pcoa, "Output/02_pcoa_ABMR.rds")
object.size(mat_pcoa)

mat_pcoa <- readRDS("Output/02_pcoa_ABMR.rds")

# keeping species coordinates on the 'nbdim' axes
nbdim = 10
mat_coord <- list()
for(i in names(mat_pcoa)){
  mat_coord[[i]] <- as.data.frame(mat_pcoa[[i]]$vectors[,1:nbdim])
  row.names(mat_coord[[i]])<-data_ft[[i]]$binomial
  colnames(mat_coord[[i]])<-paste("PC",1:nbdim,sep="")
}

# get the eigenvectors for the explaining variance with the pcoa
lapply(mat_pcoa, function(x){sum(x$values$Eigenvalues)})

# take positive and negative egv using sum of absolute values
egv <- lapply(mat_pcoa, function(x){
  e1 <- x$values$Eigenvalues[1]/sum(abs(x$values$Eigenvalues))
  e2 <- x$values$Eigenvalues[2]/sum(abs(x$values$Eigenvalues))
  e3 <- x$values$Eigenvalues[3]/sum(abs(x$values$Eigenvalues))
  e4 <- x$values$Eigenvalues[4]/sum(abs(x$values$Eigenvalues))
  e5 <- x$values$Eigenvalues[5]/sum(abs(x$values$Eigenvalues))
  e6 <- x$values$Eigenvalues[6]/sum(abs(x$values$Eigenvalues))
  e7 <- x$values$Eigenvalues[7]/sum(abs(x$values$Eigenvalues))
  e8 <- x$values$Eigenvalues[8]/sum(abs(x$values$Eigenvalues))
  e9 <- x$values$Eigenvalues[9]/sum(abs(x$values$Eigenvalues))
  e10 <- x$values$Eigenvalues[10]/sum(abs(x$values$Eigenvalues))
  vect <- c(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10)
  return(vect)
})

egv


# cumulative values of egv values for all groups
# how many axes do we have to take to have >50% of variance explained?
egv_cum <- bind_rows(egv) %>%
  mutate(axis = paste0("PC", 1:10)) %>%
  mutate(amph_cum = cumsum(amph),
         bird_cum = cumsum(bird),
         mam_cum = cumsum(mam), 
         rept_cum = cumsum(rept))

egv_cum

# selecting the number of axes for explaining >50 % of variance 
#   3 axes for amphibians
#   7 axes for birds
#   5 axes for mammals
#   5 axes for reptiles



#### Functional Specialization & Uniqueness ####

# find k closest species

k_closest_dist <- lapply(mat_dis, function(x){
  name_sp <- attributes(x)$Labels
  
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


plot(funi$amph$FUn_5NN,funi$amph$FUn_3NN)
hist(funi$amph$FUn_5NN)

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

saveRDS(fun_fsp, "Output/02_FUn_FSp_ABMR.rds")

# standardize FUn & FSp 
fun_fsp <- readRDS("Output/02_FUn_FSp_ABMR.rds")

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

saveRDS(std_fun_fsp, "Output/02_standardized_FUn_FSp_ABMR.rds")



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

