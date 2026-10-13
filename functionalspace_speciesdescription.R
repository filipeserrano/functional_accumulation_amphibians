library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggplot2)
library(mFD)
library(tidyr)
library(textshape)
library(rphylopic)
library(car)
library(funspace)
library(ape)
library(phytools)
library(ggpubr)
library(geometry)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

amphibians_morph = read.csv("Anura.csv")  

# checked description to see if F is larger  

amphibians_morph2 = amphibians_morph  %>% 
  dplyr::mutate(Sex = if_else(Species == "Nidirana hainanensis" & SVL == 44.40000,"F", Sex),
                Sex = if_else(Species == "Choerophryne microps" & SVL != 13.83333,"F", Sex),
                Sex = if_else(Species == "Cophixalus ateles" & HL != 4.522,"F", Sex),
                Sex = if_else(Species == "Cophixalus timidus" & SVL != 14.7,"F", Sex),
                Sex = if_else(Species == "Cophixalus variabilis" & SVL != 15.500,"F", Sex),
                Sex = if_else(is.na(Sex), "M", Sex)) %>% 
  dplyr::filter(Sex == "M")

#### amphibians #####
  
  # getting the conservation status and biogeo realm
  amphibians_ass =  read.csv("redlist_species_data_1c65d3ee-8b9d-4a40-900a-f2d30bd39a98/assessments.csv") %>% 
    dplyr::select(scientificName, redlistCategory, realm) 
  
  # getting the taxonomy (and year of description)
  amphibians_tax2 = read.csv("redlist_species_data_1c65d3ee-8b9d-4a40-900a-f2d30bd39a98/taxonomy.csv") %>% 
    dplyr::mutate(authority = dplyr::recode(authority, "(Gundlach &amp; Peters)" = "(Gundlach & Peters 1864)"),
                  authority = gsub(")*", "",authority),
                  year_description = str_sub(authority, start= -5),
                  year_description = as.numeric(gsub(",", "",year_description))) %>% 
    dplyr::mutate(period = case_when(year_description <= 1850 ~ "p_1850",
                                     year_description > 1850 & year_description <= 1900 ~ "p_1900",
                                     year_description > 1900 & year_description <= 1950 ~ "p_1950",
                                     year_description > 1950 & year_description <= 1975 ~ "p_1975",
                                     year_description > 1975 & year_description <= 2000 ~ "p_2000",
                                     year_description > 2000 & year_description <= 2020 ~ "p_2020"
    )) 
  
  expert_traits = readxl::read_excel("amphibiantraits_taxonomy_Leandro.xlsx") %>% 
    dplyr::filter(CLASSIFICAÇÃO == "BASE" | CLASSIFICAÇÃO == "PODE SOMAR") %>% 
    dplyr::select(Code) %>% 
    as.vector()
  
  # correcting the Pseudophilautus genus
  
  pseudophilautus_data = read.csv("Pseudophilautus_data_corrected.csv") %>% 
    dplyr::mutate(Species = gsub("P.", "Pseudophilautus", Species)) %>% 
    dplyr::rename(TD = TYE,
                  HAL = PAL,
                  TL = FEL,
                  THL = TBL,
                  UAL = UAW,
                  FL = FOL,
                  FLL = LAL,
                  Fin4L = FL.IV,
                  IOD = DFE,
                  IND = IN,
                  UEW = UEW,
                  EN = EN,
                  SL = ES,
                  NS = NS,
                  IMT = IML,
                  Fin1L = FL.I,
                  Fin2L = FL.II,
                  Toe1L = TL.I,
                  Toe2L = TL.II,
                  Toe3L = TL.III,
                  Toe5L = TL.V,
                  Fin3DW = DL,
                  ) %>% 
    dplyr::select(Species, SVL, HL, HW, ED, TD, HAL, FLL, UAL, FL, TL, THL, Fin4L,
                  IOD, IND, UEW, EN, SL, NS, IMT, Fin1L, Fin2L, Toe1L, Toe2L,
                  Toe3L, Toe5L, Fin3DW) %>% 
    dplyr::mutate(across(-Species, ~ifelse(. %in% c("?", "ind", "nc", "damaged", "nd"), NA, .))) 
  
  traits = pseudophilautus_data %>% 
    dplyr::select(-Species) %>% 
    colnames() %>% 
    as.vector()
    
  
  # extracting year of description
  amphibians_tax = amphibians_tax2 %>% 
    dplyr::select(familyName, genusName, scientificName, authority, year_description,period) %>% 
    dplyr::left_join(amphibians_ass) %>% 
    dplyr::left_join(amphibians_morph2,join_by(scientificName == Species))  %>% 
    dplyr::filter(!is.na(Family)) %>% 
    dplyr::mutate(SVL = case_when(genusName == "Pseudophilautus" ~ NA,.default = SVL ))  %>% 
    dplyr::left_join(pseudophilautus_data %>% select(Species, SVL_new = SVL, HL_new = HL, HW_new = HW,
                                                     ED_ew = ED, TD_new = TD, HAL_new = HAL, FLL_new = FLL,
                                                     UAL_new = UAL, FL_new = FL, TL_new = TL,
                                                     THL_new = THL, Fin4L_new = Fin4L, IOD_new = IOD, IND_new = IND,  
                                                     UEW_new = UEW, EN_new = EN, SL_new = SL, NS_new = NS, IMT_new = IMT,
                                                     Fin1L_new = Fin1L, Fin2L_new = Fin2L, Toe1L_new = Toe1L, Toe2L_new = Toe2L,
                                                     Toe3L_new = Toe3L, Toe5L_new = Toe5L, Fin3DW_new = Fin3DW),
              by = c("scientificName" = "Species")) %>% 
    dplyr::filter(!is.na(SVL)) %>% ### REMOVED ALL SPECIES WITHOUT SVL since this is the most trustable measurement
    dplyr::mutate(ED = case_when(scientificName == "Papurana daemeli" ~NA, .default = ED),
                  HL = coalesce(HL_new, HL),
                  HW = coalesce(HW_new, HW),
                  SVL = coalesce(SVL_new, SVL),
                  SVL = case_when(scientificName == "Amolops australis" ~ 31.04, .default = SVL),
                  SVL = case_when(scientificName == "Xenorhina similis" ~ 44.9, .default = SVL),
                  SVL = case_when(scientificName == "Cycloramphus carvalhoi" ~ 62, .default = SVL),
                  SVL = case_when(scientificName == "Microhyla karunaratnei" ~ 16.5, .default = SVL),
                  SVL = case_when(scientificName == "Pristimantis esmeraldas" ~ 16.1, .default = SVL),
                  SVL = case_when(scientificName == "Scinax montivagus" ~ 28.9, .default = SVL),
                  HL = case_when(scientificName == "Scinax montivagus" ~ 9.9, .default = HL),
                  HW = case_when(scientificName == "Scinax montivagus" ~ 9.3, .default = HW),
                  ED = case_when(scientificName == "Scinax montivagus" ~ 2.7, .default = ED),
                  TD = case_when(scientificName == "Scinax montivagus" ~ 1.4, .default = TD),
                  FL = case_when(scientificName == "Scinax montivagus" ~ 19.1, .default = FL),
                  TL = case_when(scientificName == "Scinax montivagus" ~ 13.9, .default = TL),
                  FL = case_when(scientificName == "Scinax montivagus" ~ 19.1, .default = FL),
                  THL = case_when(scientificName == "Scinax montivagus" ~ 12.87, .default = THL),
                  HL = case_when(scientificName == "Choerophryne darlingtoni" ~ NA, .default = HL),
                  HW = case_when(scientificName == "Boana stellae" ~ 15.6, .default = HW),
                  HL = case_when(scientificName == "Boana stellae" ~ 14.5, .default = HL),
                  HL = case_when(scientificName == "Pristimantis erythros" ~ 10.8, .default = HL),
                  HL = case_when(scientificName == "Tomopterna tandyi" ~ NA, .default = HL),
                  HW = case_when(scientificName == "Tomopterna tandyi" ~ NA, .default = HW),
                  ED = case_when(scientificName == "Tomopterna tandyi" ~ 4.8, .default = ED),
                  TD = case_when(scientificName == "Tomopterna tandyi" ~ 1, .default = TD),
                  FL = case_when(scientificName == "Amolops assamensis" ~ NA, .default = FL),
                  FL = case_when(scientificName == "Pelophylax saharicus" ~ NA, .default = FL),
                  TL = case_when(scientificName == "Amolops albispinus" ~ 19.4, .default = TL),
                  TL = case_when(scientificName == "Dryophytes japonicus" ~ NA, .default = TL),
                  IMT = case_when(scientificName == "Boophis quasiboehmei" ~ NA, .default = IMT),
                  UEW = case_when(scientificName == "Oreolalax liangbeiensis" ~ NA, .default = UEW),
                  SVL =  case_when(scientificName == "Amolops chunganensis" ~ 38.9, .default = SVL),
                  HL =  case_when(scientificName == "Amolops chunganensis" ~ 14.5, .default = HL),
                  HW =  case_when(scientificName == "Amolops chunganensis" ~ 13.2, .default = HW),
                  SL =  case_when(scientificName == "Amolops chunganensis" ~ 6.1, .default = SL),
                  IND =  case_when(scientificName == "Amolops chunganensis" ~ 4.6, .default = IND),
                  IOD =  case_when(scientificName == "Amolops chunganensis" ~ 4, .default = IOD),
                  ED =  case_when(scientificName == "Amolops chunganensis" ~ 5, .default = ED),
                  UEW =  case_when(scientificName == "Amolops chunganensis" ~ 3.7, .default = UEW),
                  TD =  case_when(scientificName == "Amolops chunganensis" ~ 2.7, .default = TD),
                  HAL =  case_when(scientificName == "Amolops chunganensis" ~ 12, .default = HAL),
                  Fin3DW =  case_when(scientificName == "Amolops chunganensis" ~ 1.9, .default = Fin3DW),
                  THL =  case_when(scientificName == "Amolops chunganensis" ~ 20.5, .default = THL),
                  TL =  case_when(scientificName == "Amolops chunganensis" ~ 22.5, .default = TL),
                  FL =  case_when(scientificName == "Amolops chunganensis" ~ 20.4, .default = FL),
                  TD = case_when(TD == 0 ~ NA, .default = TD)) %>% 
    dplyr::filter(scientificName != "Dryophytes japonicus",
                  scientificName != "Hyla savignyi") %>% 
    dplyr::select(-HL_new, -HW_new,  -SVL_new, -ED_ew, -TD_new, -HAL_new, 
                  -FLL_new, -UAL_new, -FL_new, -TL_new, -THL_new, -Fin4L_new,
                  -IOD_new, -IND_new, -UEW_new, -EN_new, -SL_new, -NS_new, -IMT_new,
                  -Fin1L_new, -Fin2L_new, -Toe1L_new, -Toe2L_new, -Toe3L_new, -Toe5L_new, -Fin3DW_new  ) %>% 
    dplyr::select(1:11,all_of(traits)) %>% 
    dplyr::filter(if_any(HL:Fin3DW, ~ !is.na(.))) # remove all species for which only SVL was available

  table(amphibians_tax$realm)
    
  na_spp = amphibians_tax %>% 
    dplyr::filter(is.na(Family)) %>% 
    dplyr::select(scientificName)
  
  # dividing species into periods of species description
  amphibians_tax_weights = amphibians_tax %>% 
    dplyr::mutate(present = 1) %>% 
    tidyr::pivot_wider(names_from =  scientificName,       
                       values_from = present,    
                     id_cols = period, 
                     values_fill = 0      
  ) %>%
  remove_rownames() %>% column_to_rownames( "period") %>% 
  as.matrix()

  rowSums(amphibians_tax_weights)



##### amphibians functional space ####

amphibians_morph3 = amphibians_tax 

row.names(amphibians_morph3) = amphibians_morph3$scientificName

amphibians_morph3 = amphibians_morph3 %>% 
  dplyr::select(all_of(traits)) %>% 
  dplyr::select(-Fin4L, -Toe2L, -Toe3L, -Toe5L, - Toe1L)


### phylogeny ####
# reads phylogenetic tree
phy = ape::read.tree("RAxML_Rooted_Anura_bestTree_support.tre") # loading the phylogeny 
phy$tip.label = gsub("_", " ", phy$tip.label)
head(phy)
phy = phytools::force.ultrametric(phy) # making the tree ultrametric

phy_species = as.data.frame(phy$tip.label) %>%
  dplyr::rename(SCI_NAME = 1)

amphibians_morph3_imp = funspace::impute(
  amphibians_morph3,
  phylo = phy
)

amphibians_morph3_imp2 = as.data.frame(amphibians_morph3_imp$imputed) %>% 
  dplyr::mutate(across(-"SVL", ~ . / .data[["SVL"]])) 

amphibians_morph3_imp2_df = amphibians_morph3_imp2 %>%   rownames_to_column("Species") 

cor_matrix = cor(amphibians_morph3_imp2) # continuous variables not correlated

high_cor = which(abs(cor_matrix) > 0.85 & row(cor_matrix) < col(cor_matrix), arr.ind = TRUE)
(redundant_pairs = data.frame(
  trait1 = rownames(cor_matrix)[high_cor[,1]],
  trait2 = colnames(cor_matrix)[high_cor[,2]],
  cor = cor_matrix[high_cor]
))

# Toe5L highly correlated with FinL and ToeL
# Fin4L and Fin2L correlated, keep Fin2L
# Toe1L highly correlated with others ToeL
# Fin1L and Toe1L correlated

# creating the traits database (and assigning "Quantitative" to each trait)
amphibians_morph3_traits =  colnames(amphibians_morph3_imp2) %>% 
  as.data.frame() %>% 
  dplyr::rename(trait_name = 1) %>% 
  dplyr::mutate(trait_type = "Q") %>% 
  dplyr::mutate(trait_weight = case_when(trait_name %in% c("UEW", "Fin1L", "Fin2L", "Fin3DW") ~ 0.5,TRUE ~1))


# running mFD
amphibians_traits_summ <- mFD::sp.tr.summary(
  tr_cat     = amphibians_morph3_traits,   
  sp_tr      = amphibians_morph3_imp2, 
  stop_if_NA = F)

head(amphibians_traits_summ)


sp_dist_amphibians <- mFD::funct.dist(
  sp_tr         = amphibians_morph3_imp2,
  tr_cat        = amphibians_morph3_traits,
  metric        = "gower",
  scale_euclid  = "scale_center",
  ordinal_var   = "classic",
  weight_type   = "user",
  stop_if_NA    = F)

class(sp_dist_amphibians)

# write_rds(sp_dist_amphibians, "sp_dist_amphibians_20052026_filtered.RDS") # _filtered if species with only SVL were excluded

sp_dist_amphibians = read_rds("sp_dist_amphibians_20052026_filtered.RDS")


fspaces_quality_amphibians = mFD::quality.fspaces(
  sp_dist             = sp_dist_amphibians,
  maxdim_pcoa         = 10,
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE,
  fdendro             = "average")

#checking the optimal nr of dimensions
fspaces_quality_amphibians$quality_fspaces #6 with the lowest, cutoff point

fspaces_quality_amphibians$details_fspaces

sp_faxes_coord_amphibians = fspaces_quality_amphibians$"details_fspaces"$"sp_pc_coord"[,1:4] 

sp_faxes_coord_amphibians_all = fspaces_quality_amphibians$"details_fspaces"$"sp_pc_coord" %>% 
  as.data.frame() %>% 
  dplyr::mutate(scientificName = rownames(.)) %>% 
  dplyr::left_join(amphibians_tax) 

eigen_df = fspaces_quality_amphibians$details_fspaces$pc_eigenvalues

# 1. Isolate the individual percentages (Relative_eig * 100)
pc1_var <- round(eigen_df$Relative_eig[1] * 100, 2)
pc2_var <- round(eigen_df$Relative_eig[2] * 100, 2)
pc3_var <- round(eigen_df$Relative_eig[3] * 100, 2)
pc4_var <- round(eigen_df$Relative_eig[4] * 100, 2)

# 2. Calculate the Cumulative Variance Explained
# (Or pull directly from a cumulative sum of Relative_eig)
cumulative_4d <- round(sum(eigen_df$Relative_eig[1:4]) * 100, 2)


# write.csv(sp_faxes_coord_amphibians_all, "sp_faxes_coord_amphibians_all_20052026_filtered.csv")
sp_faxes_coord_amphibians_all = read.csv("sp_faxes_coord_amphibians_all_20052026_filtered.csv")

sp_faxes_coord_amphibians0 = read.csv("sp_faxes_coord_amphibians_all_20052026_filtered.csv") %>% 
  dplyr::select(2:6) 

row.names(sp_faxes_coord_amphibians0) = sp_faxes_coord_amphibians0$scientificName

# will be filtered downstream to compare with ecological traits
sp_faxes_coord_amphibians = sp_faxes_coord_amphibians0 %>% 
  dplyr::select(-scientificName) %>% 
  as.matrix()


big_plot = mFD::funct.space.plot(
  sp_faxes_coord  = sp_faxes_coord_amphibians[ , c("PC1", "PC2", "PC3", "PC4")],
  faxes           = c("PC1", "PC2", "PC3", "PC4"),
  name_file       = NULL,
  faxes_nm        = NULL,
  range_faxes     = c(NA, NA),
  color_bg        = "grey99",
  color_pool      = "black",
  fill_pool       = "green4",
  shape_pool      = 21,
  size_pool       = 1.6,
  plot_ch         = TRUE,
  color_ch        = "black",
  fill_ch         = "white",
  alpha_ch        = 0.5,
  plot_vertices   = F, #change to plot vertices
  color_vert      = "blueviolet",
  fill_vert       = "blueviolet",
  shape_vert      = 23,
  size_vert       = 1,
  plot_sp_nm      = NULL,
  nm_size         = 3,
  nm_color        = "black",
  nm_fontface     = "plain",
  check_input     = TRUE)

big_plot[1]


alpha_fd_indices_amphibians = mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_amphibians[ , c("PC1", "PC2", "PC3", "PC4")], #changed in 13/10
  asb_sp_w         = amphibians_tax_weights,
  ind_vect         = c("fric"), #"fdis", "feve", "fric", "fdiv"
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

alpha_fd_indices_amphibians$functional_diversity_indices

trait_axes_corr_all = mFD::traits.faxes.cor(
  sp_faxes_coord = sp_faxes_coord_amphibians[, c("PC1", "PC2","PC3", "PC4")], 
  sp_tr          = amphibians_morph3_imp2             # Your raw trait data
)


# direction of axes
cor(amphibians_morph3_imp2$Fin1L, sp_faxes_coord_amphibians[, "PC1"], method = "pearson")
cor(amphibians_morph3_imp2$TL, sp_faxes_coord_amphibians[, "PC1"], method = "pearson")
cor(amphibians_morph3_imp2$HL, sp_faxes_coord_amphibians[, "PC1"], method = "pearson")
cor(amphibians_morph3_imp2$HAL, sp_faxes_coord_amphibians[, "PC1"], method = "pearson")

cor(amphibians_morph3_imp2$SVL, sp_faxes_coord_amphibians[, "PC2"], method = "pearson")
cor(amphibians_morph3_imp2$IOD, sp_faxes_coord_amphibians[, "PC2"], method = "pearson")



trait_axes_corr = mFD::traits.faxes.cor(
  sp_faxes_coord = sp_faxes_coord_amphibians[, c("PC1", "PC2")], 
  sp_tr          = amphibians_morph3_imp2             # Your raw trait data
)


trait_axes_corr2 = mFD::traits.faxes.cor(
  sp_faxes_coord = sp_faxes_coord_amphibians[, c("PC1", "PC2", "PC3", "PC4")], 
  sp_tr          = amphibians_morph3_imp2             # Your raw trait data
)

col_asb1 = "orange3"
col_asb2 = "#DCE319FF"

plots_alpha = mFD::alpha.multidim.plot(
  output_alpha_fd_multidim = alpha_fd_indices_amphibians,
  plot_asb_nm              = c("p_2000", "p_2020"), # first one orange
  ind_nm                   = c( "fric"),
  faxes                    = NULL,
  faxes_nm                 = NULL,
  range_faxes              = c(NA, NA),
  color_bg                 = "white",
  shape_sp                 = c(pool = 3, asb1 = 21, asb2 = 21),
  size_sp                  = c(pool = 0.7, asb1 = .7, asb2 = .2),
  color_sp                 = c(pool = "grey50", asb1 = col_asb1, asb2 = "black"),
  color_vert               = c(pool = "grey50", asb1 = col_asb1, asb2 = "black"),
  fill_sp                  = c(pool = NA, asb1 = col_asb1, asb2 = col_asb2),
  fill_vert                = c(pool = NA, asb1 = col_asb1, asb2 = col_asb2),
  color_ch                 = c(pool = NA, asb1 = col_asb1, asb2 = col_asb2),
  fill_ch                  = c(pool = "grey86", asb1 = col_asb1, asb2 = col_asb2),
  alpha_ch                 = c(pool = 1, asb1 = 0.4, asb2 = 0.4),
  shape_centroid_fdis      = c(asb1 = 22,  asb2 = 24),
  shape_centroid_fdiv      = c(asb1 = 22,  asb2 = 24),
  shape_centroid_fspe      = 23,
  color_centroid_fspe      = "black",
  size_sp_nm               = 5, 
  color_sp_nm              = "black",
  plot_sp_nm               = NULL,
  fontface_sp_nm           = "normal",
  save_file                = FALSE,
  check_input              = TRUE) 

plots_alpha$"fric"[[1]]
plots_alpha$"fric"$"patchwork"


alpha_fd_indices_amphibians2 = alpha_fd_indices_amphibians$functional_diversity_indices %>% 
  as.data.frame() %>% 
  dplyr::mutate(period = rownames(alpha_fd_indices_amphibians$functional_diversity_indices)) %>% 
  dplyr::mutate(year = as.integer(gsub("p_", "", period))) %>%
  dplyr::arrange(year) 

ggplot(alpha_fd_indices_amphibians2, aes(x = year, y = fric)) +
  geom_line(color = "#D55E00", linewidth = 1.2) +
  geom_point(color = "#D55E00", size = 4) +
  geom_text(aes(label = round(sp_richn, 3)), vjust = -1.5, size = 3.5) +
  labs(
    x = "Time Period (Year)",
    y = "Functional Richness"
  ) +
  # scale_x_continuous(breaks = unique(alpha_fd_indices_amphibians_cum$year)) + 
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ylim(c(0.1,1.01))


ggsave("results/fric_year_SuppMat.jpeg")

plots_alpha$"fdiv"$"patchwork"

#### cumulative Fdiversity ####
amphibians_tax_weights_cum = amphibians_tax_weights %>% 
  dplyr::as_data_frame() %>% 
  dplyr::mutate(period = rownames(amphibians_tax_weights)) %>% 
  dplyr::relocate(period) %>% 
  dplyr::mutate(year = as.integer(gsub("p_", "", period))) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(across(-c(period, year), cummax)) %>% 
  dplyr::select(-year)


rowSums(amphibians_tax_weights_cum_del[,-1])

head(amphibians_tax_weights_cum[,1:10])

amphibians_tax_weights_cum2 = amphibians_tax_weights_cum%>%
  remove_rownames() %>% column_to_rownames() %>% 
  as.matrix()


alpha_fd_indices_amphibians = mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_amphibians[ , c("PC1", "PC2")],
  asb_sp_w         = amphibians_tax_weights_cum2,
  ind_vect         = c( "fric"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)


# write_rds(alpha_fd_indices_amphibians, "alpha_fd_indices_amphibians_20052026.RDS")

alpha_fd_indices_amphibians = readRDS("alpha_fd_indices_amphibians_20052026.RDS")

alpha_fd_indices_amphibians_cum = alpha_fd_indices_amphibians$functional_diversity_indices %>% 
  as.data.frame() %>% 
  dplyr::mutate(period = rownames(alpha_fd_indices_amphibians$functional_diversity_indices)) %>% 
  dplyr::mutate(year = as.integer(gsub("p_", "", period))) %>%
  dplyr::arrange(year) 


### something wrong
ggplot(alpha_fd_indices_amphibians_cum, aes(x = year, y = fric)) +
  geom_line(color = "black", linewidth = 1.2) +
  geom_point(color = "#D55E00", size = 4) +
  geom_text(aes(label = round(sp_richn, 3)), vjust = -1.5, size = 3.5) +
  labs(
    x = "Time Period (Year)",
    y = "Cumulative Functional Richness"
  ) +
  scale_x_continuous(breaks = unique(alpha_fd_indices_amphibians_cum$year)) + 
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.y = element_text(size = 17),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ylim(c(0.1,1.01))

ggsave("results/cum_fric_morph.jpeg", width = 10)

n_stand = min(alpha_fd_indices_amphibians_cum$sp_richn) 
n_iter = 99 #same with 999

cum_sp_list <- amphibians_tax_weights_cum %>%
  as.data.frame() %>%
  pivot_longer(cols = -period, names_to = "species", values_to = "present") %>%
  filter(present == 1) %>%
  group_split(period)

periods <- unique(amphibians_tax_weights_cum$period)
year_vals <- as.numeric(gsub("p_", "", periods))
period_order <- periods[order(year_vals)]

rarefied_cum_final <- map_dfr(period_order, function(p) {
  

  current_row <- amphibians_tax_weights_cum %>% filter(period == p)
  
  # Identify column names (species) that are present (value == 1)
  # We exclude the 'period' column itself
  cumulative_spp <- names(current_row)[current_row == 1]
  cumulative_spp <- setdiff(cumulative_spp, "period")
  
  n_available <- length(cumulative_spp)
  
  # Rarefaction: Draw 62 species from the entire "accumulated" pool
  res <- replicate(n_iter, {
    sample_spp <- sample(cumulative_spp, n_stand)
    
    asb_matrix <- matrix(1, nrow = 1, ncol = n_stand, 
                         dimnames = list("temp", sample_spp))
    
    mFD::alpha.fd.multidim(
      sp_faxes_coord = sp_faxes_coord_amphibians[, c("PC1", "PC2")],
      asb_sp_w       = asb_matrix,
      ind_vect       = "fric",
      scaling        = TRUE
    )$functional_diversity_indices$fric
  })
  
  return(data.frame(
    period = p, 
    fric_mean = mean(res), 
    fric_lower = quantile(res, 0.025),
    fric_upper = quantile(res, 0.975),
    year = as.numeric(gsub("p_", "", p))
  ))
})

(rarefied_cumulative = ggplot(rarefied_cum_final, aes(x = year, y = fric_mean)) +
  geom_ribbon(aes(ymin = fric_lower, ymax = fric_upper), fill = "#2C3E50", alpha = 0.2) +
  geom_line(color = "#2C3E50", linewidth = 1.2) +
  geom_point(color = "#D55E00", size = 4) +
  labs(
    y = "Rarefied Functional Richness (n=61)",
    x = "Time Period (Year)"
  ) +
  theme_classic(base_size = 18) +
  scale_x_continuous(breaks = period_order %>% gsub("p_", "", .) %>% as.numeric()) +
    annotate("text", label = "B", x = 1851, y = 1, size = 8, colour = "black", fontface = "bold") +
    ylim(0,1))

significance_results <- alpha_fd_indices_amphibians_cum %>%
  dplyr::mutate(year = as.numeric(gsub("p_", "", year))) %>%
  dplyr::select(year, fric_obs = fric) %>%
  left_join(rarefied_cum_final, by = "year") %>%
  mutate(
    # Calculate SES
    ses = (fric_obs - fric_mean) / ((fric_upper - fric_lower) / 3.92), 
    # Note: 3.92 is the spread of a 95% CI in SD units. 
    # Better yet, use the SD directly from your replicate loop if you saved it.
    
    # Simple Rank-based P-value
    # If Obs is higher than 95% of nulls, p < 0.05
    p_value = case_when(
      fric_obs > fric_upper ~ 0.05,
      fric_obs < fric_lower ~ 0.05,
      TRUE ~ 0.99 # Non-significant
    )
  )

ggplot(significance_results, aes(x = year)) +
  # The Null Model "Cloud"
  geom_ribbon(aes(ymin = fric_lower, ymax = fric_upper), fill = "grey80", alpha = 0.5) +
  geom_line(aes(y = fric_mean), linetype = "dashed", color = "grey40") +
  
  # The Observed Data
  geom_line(aes(y = fric_obs), color = "black", linewidth = 1.2) +
  geom_point(aes(y = fric_obs), color = "#D55E00", size = 4) +
  
  labs(
    x = "Year",
    y = "Functional Richness (FRic)"
  ) +
  theme_classic()

significance_results


##### CREATION OF NULL MODELS ####
N_ITERATIONS = 999
null_fric_results <- list()

# Define the set of column names to be shuffled
original_species_names_amph <- colnames(amphibians_tax_weights_cum) %>% 
  # Exclude the row identifier 'period'
  setdiff("period")

period_col <- amphibians_tax_weights_cum$period
species_data_matrix <- amphibians_tax_weights_cum %>% dplyr::select(-period) 

for (i in 1:N_ITERATIONS) {
  
  # 1. Randomize the species names
  shuffled_names <- sample(original_species_names_amph)
  
  # 2. Create the null matrix by assigning shuffled names to the data
  amphibians_matrix_null <- species_data_matrix
  colnames(amphibians_matrix_null) <- shuffled_names
  
  # 3. Re-attach the period column and convert to tibble for mFD compatibility
  amphibians_matrix_null <- amphibians_matrix_null %>%
    tibble::add_column(period = period_col, .before = 1) %>%
    # Convert period column to row names for mFD input
    tibble::column_to_rownames("period") %>% 
    as.matrix()
  
  
  # 4. RUN mFD: Pass the randomized matrix as the assemblage data (asb_sp_w)
  alpha_fd_null = mFD::alpha.fd.multidim(
    sp_faxes_coord = sp_faxes_coord_amphibians[, c("PC1", "PC2")],
    asb_sp_w       = amphibians_matrix_null,
    ind_vect       = c("fric"),
    scaling        = TRUE,
    check_input    = TRUE
  )
  
  # 5. STORE RESULTS: Extract the FRic values and store them in the list
  null_fric_results[[i]] <- alpha_fd_null$functional_diversity_indices %>%
    tibble::rownames_to_column(var = "asb_nm") %>%
    # 2. Select only the assemblage names and the FRic values (the 'fric' column)
    dplyr::select(asb_nm, value = fric) %>%
    # 3. Rename the 'value' column uniquely for this iteration
    dplyr::rename(!!paste0("fric_", i) := value)
}

# 6. COMBINE RESULTS: Convert the list of dataframes into a single dataframe
null_fric_df <- purrr::reduce(null_fric_results, dplyr::full_join, by = "asb_nm")

null_tidy = null_fric_df %>%
  # Gather all the iteration columns into two: a key (fric_1, fric_2, etc.) and a value (the FRic number)
  tidyr::pivot_longer(
    cols = starts_with("fric_"),
    names_to = "iteration",
    values_to = "fric"
  ) %>%
  # Extract the year from the assemblage name (e.g., "p_1900" -> 1900)
  dplyr::mutate(
    period = as.numeric(gsub("p_", "", asb_nm))
  )


null_envelope = null_tidy %>%
  dplyr::group_by(period) %>%
  dplyr::summarise(
    # Lower bound (2.5th percentile)
    lower_95 = quantile(fric, 0.025, na.rm = TRUE),
    # Upper bound (97.5th percentile)
    upper_95 = quantile(fric, 0.975, na.rm = TRUE),
    # Mean null curve (for reference)
    mean_null = mean(fric, na.rm = TRUE)
  ) %>%
  dplyr::ungroup()


observed_fric = alpha_fd_indices_amphibians_cum %>%
  dplyr::mutate(
    period = as.numeric(gsub("p_", "", year))
  ) 


(cum_functional_richness =ggplot(observed_fric, aes(x = period, y = fric)) +
  
  # 1. Null Envelope (Shaded Area)
  geom_ribbon(
    data = null_envelope,
    aes(x = period, ymin = lower_95, ymax = upper_95),
    fill = "grey20",
    alpha = 0.2,
    inherit.aes = FALSE # Prevents inheriting the observed aesthetic
  ) +
  # 3. Observed Curve (Solid Line and Points)
  geom_line(color = "black", linewidth = 1, alpha = .8) +
  geom_point(color = "black", size = 4, alpha = .4) +
  geom_text(aes(label = round(sp_richn, 3)), vjust = -1.5, size = 3.5) +
  
  # 2. Mean Null Curve (Dashed Line)
  geom_line(
    data = null_envelope,
    aes(x = period, y = mean_null),
    color = "grey50",
    alpha = .5,
    inherit.aes = FALSE,
    linetype="dotted"
  ) +
  # 4. Labels and Theme (rest of the code is correct)
  labs(
    x = "",
    y = "Cumulative Functional Richness"
  ) +
  scale_x_continuous(breaks = unique(observed_fric$period)) +
  theme_classic(base_size = 18) +
    annotate("text", label = "A", x = 1851, y = 1.06,  size = 8, colour = "black", fontface = "bold") +
  ylim(0, 1.06))

ggsave("results/cum_fric_morph_null.jpeg", width = 10)

(amphibians_cumulative = ggarrange(
  cum_functional_richness,  rarefied_cumulative,
  ncol = 1, nrow = 2, 
  labels = c("", ""),  # Empty label for the blank space
  widths = c(1, 1),     # Ensures both halves are equal size
  heights = c(1, 1)
))

ggsave("results/Fig_cumulative_morphological3.jpeg", width = 9, height = 9.5)


# testing
null_stats <- null_tidy %>%
  group_by(period) %>%
  summarise(
    null_mean = mean(fric),
    null_sd = sd(fric),
    .groups = "drop"
  )

results <- observed_fric %>%
  left_join(null_stats, by = "period") %>%
  mutate(
    # Standardized Effect Size: (Obs - Mean_Null) / SD_Null
    ses = (fric - null_mean) / null_sd
  )

results

ggplot() +
  # 1. The Null Distribution (The 'Expected' background)
  geom_boxplot(data = null_tidy, aes(x = as.factor(period), y = fric), fill = "gray90") +
  # 2. Your Observed Values (The 'Reality')
  geom_point(data = observed_fric, aes(x = as.factor(period), y = fric), 
             color = "red", size = 4) +
  labs(
    title = "Functional Richness Accumulation",
    subtitle = "Red points = Observed; Gray boxes = Null Expectation",
    x = "Period",
    y = "Functional Richness"
  ) +
  theme_classic()


#### cumulative funspace ####
library(tidyverse)
library(png)
library(ggpmisc)
library(grid)

# 1. Reshape the cumulative weights to a long format
weights_long = amphibians_tax_weights_cum %>%
  pivot_longer(cols = -period, names_to = "scientificName", values_to = "present") %>%
  filter(present == 1) # Keep only species present in that period

plot_data = weights_long %>%
  left_join(sp_faxes_coord_amphibians0, by = "scientificName") %>%
  mutate(year = as.numeric(gsub("p_", "", period)))

hull_data = plot_data %>%
  group_by(period, year) %>%
  slice(chull(PC1, PC2))%>%
  arrange(desc(year))

global_hull_indices <- chull(sp_faxes_coord_amphibians0$PC1, sp_faxes_coord_amphibians0$PC2)
global_hull_data <- sp_faxes_coord_amphibians0[global_hull_indices, ]

# loading png
species_SVL = sp_faxes_coord_amphibians_all %>% 
  dplyr::filter(scientificName %in% c("Rhinella diptycha", "Hoplobatrachus tigerinus", 
                                      "Pristimantis mallii", "Hemiphractus panamensis",
                                      "Stumpffia pygmaea", "Hyla arborea",
                                      "Elachistocleis corumbaensis", "Ameerega panguana")) %>% 
  dplyr::select(scientificName, SVL, year_description, realm)

#https://commons.wikimedia.org/wiki/File:Rhinella_diptycha_-_Alessandher_Piva_-_489825971.jpeg
rdiptycha = readPNG("photos/Rhinella_diptycha3b_CC_openverse.png")
rdiptycha2 = rasterGrob(rdiptycha, interpolate = TRUE)


htigerinus = readPNG("photos/hoplobatrachus_tigerinus1_CC.png")
htigerinus2 = rasterGrob(htigerinus, interpolate = TRUE)

#https://openverse.org/image/b88c479f-0244-4086-a7e9-79027510ee04?q=Pristimantis+mallii&p=1
pmallii = readPNG("photos/Pristimantis_malliib_CC_openverse.png")
pmalliis2 = rasterGrob(pmallii, interpolate = TRUE)

#https://openverse.org/image/63ad53a3-e465-4a97-b004-dead5e4c2718?q=Hemiphractus+panamensis&p=1
hpanamensis = readPNG("photos/Hemiphractus_panamensis_CC_openverse.png")
hpanamensis2 = rasterGrob(hpanamensis, interpolate = TRUE)

#https://openverse.org/image/657677cb-9241-43be-b524-4acae6fb8590?q=stumpffia&p=9
spygmaea = readPNG("photos/Stumpffia_pygmaea_CC_openverse.png")
spygmaea2 = rasterGrob(spygmaea, interpolate = TRUE)

#https://openverse.org/image/d40e8f46-d487-456c-81bf-2f022162b63e?q=hyla+arborea&p=22
harborea = readPNG("photos/Hyla_arborea2_CC_openverse.png")
harborea2 = rasterGrob(harborea, interpolate = TRUE)

#https://openverse.org/image/c5f12d7b-1611-4c43-acb5-34d87fff96a3?q=Elachistocleis+corumbaensis&p=6
ecorumbaensis = readPNG("photos/Elachistocleis_corumbaensis2_CC_openverse.png")
ecorumbaensis2 = rasterGrob(ecorumbaensis, interpolate = TRUE)

#https://openverse.org/image/8b7ef0aa-9f4b-448c-a1d7-3ac808f7e23b?q=Ameerega+panguana&p=3
apanguana = readPNG("photos/Ameerega_panguanab_CC_openverse.png")
apanguana2 = rasterGrob(apanguana, interpolate = TRUE)


arrow_data = data.frame(
  species = c("rdiptycha", "pmalliis", "hpanamensis", "spygmaea", 
              "htigerinus", "apanguana", "ecorumbaensis", "harborea"),
  
  x_start = c(mean(c(0.077, 0.17)),    # rdiptycha
              mean(c(-0.15, -0.175)),  # pmalliis
              mean(c(-0.19, -0.11)),   # hpanamensis
              mean(c(-0.02, -0.05)),   # spygmaea
              mean(c(-0.05, 0.025)),   # htigerinus
              mean(c(0.0195, 0.075)),    # apanguana (index 6)
              mean(c(0.11, 0.145)),    # ecorumbaensis
              mean(c(-0.085, -0.12))), # harborea (index 8)
  
  y_start = c(mean(c(0.055, 0.14)), 
              mean(c(-0.06, -0.08)), 
              mean(c(0.03, 0.065)), 
              mean(c(-0.1, -0.13)), 
              mean(c(0.075, 0.15)), 
              mean(c(-0.0495, -0.12)),   
              mean(c(-0.02, -0.07)), 
              mean(c(0.075, 0.1))),    
  
  x_end = c(0.127788961, -0.15311710, -0.123922, -0.03496657, 0.01997248, -0.01127268, 0.1253435, 0.01122709), 
  y_end = c(0.046665553, -0.049490361, -0.01523356, -0.08751127, 0.07800946, -0.005591748, -0.003254583, -0.02963529)
)


 ggplot() +
  geom_polygon(data = global_hull_data, 
               aes(x = PC1, y = PC2), 
               fill = NA, 
               color = "#2C3E50", # Using a neutral dark slate color
               linewidth = 0.9) +
  geom_point(data = sp_faxes_coord_amphibians0,
             aes(x = PC1, y = PC2),
             color = "black",
             size = 2.3,
             alpha = .7) +
  geom_point(data = sp_faxes_coord_amphibians0 %>% 
               dplyr::left_join(amphibians_tax %>% 
                                 dplyr::select(scientificName, year_description)) %>%
               dplyr::arrange(desc(year_description)),
             aes(x = PC1, y = PC2, fill = year_description), 
             shape = 21,
             size = 2, 
             alpha = 0.75) +
  scale_fill_viridis_c(option = "A", name = "Year of\nDescription", direction = 1)  +
  annotation_custom(
    grob = rdiptycha2, 
    xmin = 0.077,  # West boundary (left side)
    xmax = 0.17,  # East boundary (right side)
    ymin = 0.055,  # South boundary (bottom)
    ymax = 0.14   # North boundary (top) 
  ) +
  annotation_custom(
    grob = pmalliis2,
    xmin = -0.15,  # West boundary (left side)
    xmax = -0.175,  # East boundary (right side)
    ymin = -0.06,  # South boundary (bottom)
    ymax = -0.08   # North boundary (top)
  ) +
   annotation_custom(
     grob = hpanamensis2, 
     xmin = -0.19,  # West boundary (left side)
     xmax = -0.11,  # East boundary (right side)
     ymin = 0.03,  # South boundary (bottom)
     ymax = 0.065   # North boundary (top) 
   ) +
   annotation_custom(
     grob = spygmaea2, 
     xmin = -0.02,  # West boundary (left side)
     xmax = -0.05,  # East boundary (right side)
     ymin = -0.1,  # South boundary (bottom)
     ymax = -0.13   # North boundary (top) 
   ) +
   annotation_custom(
     grob = htigerinus2, 
     xmin = -0.05,  # West boundary (left side)
     xmax = 0.025,  # East boundary (right side)
     ymin = 0.075,  # South boundary (bottom)
     ymax = 0.15   # North boundary (top) 
   ) +
   annotation_custom(
     grob = apanguana2, 
     xmin = 0.02,  # West boundary (left side)
     xmax = 0.075,  # East boundary (right side)
     ymin = -0.05,  # South boundary (bottom)
     ymax = -0.12   # North boundary (top) 
   ) +
   annotation_custom(
     grob = ecorumbaensis2, 
     xmin = 0.11,  # West boundary (left side)
     xmax = 0.145,  # East boundary (right side)
     ymin = -0.02,  # South boundary (bottom)
     ymax = -0.07   # North boundary (top) 
   ) +
   annotation_custom(
     grob = harborea2, 
     xmin = -0.075,  # West boundary (left side)
     xmax = -0.12,  # East boundary (right side)
     ymin = 0.055,  # South boundary (bottom)
     ymax = 0.1   # North boundary (top) 
   ) +
   geom_segment(data = arrow_data,
                aes(x = x_start + 0.1*(x_end - x_start), 
                    y = y_start + 0.1*(y_end - y_start), 
                    xend = x_end - 0.05*(x_end - x_start), 
                    yend = y_end - 0.05*(y_end - y_start)),
                arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
                color = "grey40", linewidth = 0.4) +

  labs(
    x = "PC1",
    y = "PC2"
  ) +
   
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.84, 0.02),
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) +
  xlim(c(-0.17, 0.15)) +
  ylim(c(-0.15, 0.12))

 ggsave("results/Fig_all_funspace.jpeg", width = 9, height = 9.5)




#cum funspace - SupMat
ggplot() +
  # Background: All species in light grey to show the potential total space
  
  geom_point(data = sp_faxes_coord_amphibians0, aes(x = PC1, y = PC2),
             color = "black", size = 2, alpha = 0.9) +
  geom_point(data = plot_data, aes(x = PC1, y = PC2, color = factor(year)), size = 1.5, alpha = .6) +
  
  geom_polygon(data = hull_data, aes(x = PC1, y = PC2, color = factor(year)), fill = NA,
               alpha = 0.3, linewidth = 1.8) +
  # geom_point(data = sp_faxes_coord_amphibians0, aes(x = PC1, y = PC2), 
  #            color = "grey90", size = 0.5, alpha = 0.3) +

  scale_fill_viridis_d(option = "F", direction =-1, name = "Year") +
  scale_color_viridis_d(option = "F",name = "Year") +
  labs(
    x = "PCA1",
    y = "PCA2"
  ) +
  theme_minimal() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.84, 0.02),
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) 

ggsave("results/Fig_cumulative_funspace_SupMat.jpeg", width = 9, height = 9.5)




##### spatial patterns ####

unique(amphibians_tax$realm)

amphibians_tax_weights_spatial = amphibians_tax %>% 
  dplyr::select(scientificName, realm) %>% 
  tidyr::separate_longer_delim(realm, delim = "|") %>%
  dplyr::filter(realm != "Oceanian",
                realm != "Nearctic",
                realm != "") %>% 
  dplyr::mutate(present = 1) %>% 
  tidyr::pivot_wider(names_from =  scientificName,       
                     values_from = present,    
                     id_cols = realm, 
                     values_fill = 0      
  ) %>%
  remove_rownames() %>% column_to_rownames( "realm") #%>%   as.matrix()



amphibians_tax_weights_spatial2 = amphibians_tax_weights_spatial %>% 
  as.matrix()


alpha_fd_indices_amphibians_spatial = mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_amphibians[ , c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w         = amphibians_tax_weights_cum2,
  ind_vect         = c( "fric"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

(alpha_fd_indices_amphibians_spatial_df = alpha_fd_indices_amphibians_spatial$functional_diversity_indices)



table(amphibians_tax$realm)

realm_colors = c(
  "Afrotropical" = "#0072B2",
  "Australasian" = "#E69F00",
  "Indomalayan"  = "#009E73",
  "Palearctic"   = "#D55E00",
  "Neotropical"  = "#CC79A7"
)

realm_name = "Neotropical"
#### automatic loop beginning ####
# Loop through each realm name
for (realm_name in names(realm_colors)) {
  
  # 1. Filter morphometric data for the current realm
  amphibians_morph3_realm = amphibians_morph3_imp2 %>% 
    rownames_to_column("Species") %>% 
    dplyr::left_join(amphibians_tax %>% 
                       dplyr::rename(Species = scientificName) %>% 
                       dplyr::select(Species, realm), by = "Species") %>% 
    dplyr::filter(grepl(realm_name, realm)) %>% 
    dplyr::select(-realm) %>% 
    column_to_rownames("Species") 
  
  # 2. Setup trait weights
  amphibians_morph3_traits_realm = colnames(amphibians_morph3_realm) %>% 
    as.data.frame() %>% 
    dplyr::rename(trait_name = 1) %>% 
    dplyr::mutate(trait_type = "Q",
                  trait_weight = case_when(trait_name %in% c("UEW", "Fin1L", "Fin2L", "Fin3DW") ~ 0.5, TRUE ~ 1))
  
  # 3. Calculate functional distances and quality
  sp_dist_amphibians_realm = mFD::funct.dist(
    sp_tr = amphibians_morph3_realm,
    tr_cat = amphibians_morph3_traits_realm,
    metric = "gower",
    weight_type = "user",
    stop_if_NA = F)
  
  fspaces_quality_amphibians_realm = mFD::quality.fspaces(
    sp_dist = sp_dist_amphibians_realm,
    maxdim_pcoa = 4,
    deviation_weighting = "absolute")
  
  sp_faxes_coord_amphibians_realm = fspaces_quality_amphibians_realm$details_fspaces$sp_pc_coord 
  
  sp_faxes_coord_realm_df = as.data.frame(sp_faxes_coord_amphibians_realm)
  
  # Calculate the Convex Hull coordinates for the background shaded area
  hull_indices = chull(sp_faxes_coord_realm_df$PC1, sp_faxes_coord_realm_df$PC2)
  hull_data = sp_faxes_coord_realm_df[hull_indices, ]
  
  x_axis_color = ifelse(realm_name == "Palearctic", "black", "transparent")
  
  # Generate Functional Space Plot with ggplot2
  p_fspace = ggplot(sp_faxes_coord_realm_df, aes(x = PC1, y = PC2)) +
    # Shaded Convex Hull
    geom_polygon(data = hull_data,color = "black", fill = realm_colors[realm_name], alpha = 0.1) +
    # Species points
    geom_point(color = "black", fill = realm_colors[realm_name], shape = 21, size = 2, stroke = 0.6) +
    labs(
      title = "",
      x = "PC1",
      y = "PC2"
    ) +
    theme_classic(base_size = 40) +
    theme(
      panel.grid.minor = element_blank(), 
      plot.title = element_text(face = "bold"),
      axis.title.x = element_text(color = x_axis_color)
    ) + 
    ylim(c(-0.15, 0.16)) +
    xlim(c(-.25, .35)) 
  
  ggsave(paste0("results/funspace_", realm_name, ".jpeg"), plot = p_fspace, width = 8, height = 8.5)
  
  # 4. Prepare weights matrix (Occurrence over time)
  amphibians_tax_weights_realm = amphibians_tax %>% 
    dplyr::filter(grepl(realm_name, realm)) %>% 
    dplyr::mutate(present = 1) %>% 
    tidyr::pivot_wider(names_from = scientificName, values_from = present, id_cols = period, values_fill = 0) %>%
    na.omit(period) %>% column_to_rownames("period") %>% 
    dplyr::mutate(year = as.integer(gsub("p_", "", rownames(.)))) %>%
    dplyr::arrange(year) %>%
    dplyr::select(-year) %>% 
    as.matrix() %>%
    apply(2, cummax) # Ensure cumulative presence
  
  # 5. Calculate Observed Functional Richness
  alpha_fd_indices_amphibians_realm = mFD::alpha.fd.multidim(
    sp_faxes_coord = sp_faxes_coord_amphibians_realm[ , c("PC1", "PC2")],
    asb_sp_w = amphibians_tax_weights_realm,
    ind_vect = c("fric"),
    scaling = TRUE)
  
  observed_fric_realm = alpha_fd_indices_amphibians_realm$functional_diversity_indices %>% 
    dplyr::mutate(period = as.numeric(gsub("p_", "", rownames(.))))
  
  # 6. Null Model Loop
  N_ITERATIONS = 99
  null_fric_results_realm = list()
  original_species_names = colnames(amphibians_tax_weights_realm)
  
  for (i in 1:N_ITERATIONS) {
    shuffled_matrix = amphibians_tax_weights_realm
    colnames(shuffled_matrix) = sample(original_species_names)
    
    alpha_fd_null = mFD::alpha.fd.multidim(
      sp_faxes_coord = sp_faxes_coord_amphibians_realm[, c("PC1", "PC2")],
      asb_sp_w = shuffled_matrix,
      ind_vect = c("fric"),
      scaling = TRUE)
    
    null_fric_results_realm[[i]] = alpha_fd_null$functional_diversity_indices %>%
      rownames_to_column("asb_nm") %>%
      dplyr::select(asb_nm, fric) %>%
      dplyr::rename(!!paste0("fric_", i) := fric)
  }
  
  # 7. Calculate Null Envelope
  null_envelope_realm = purrr::reduce(null_fric_results_realm, full_join, by = "asb_nm") %>%
    tidyr::pivot_longer(-asb_nm, names_to = "iteration", values_to = "fric") %>%
    dplyr::mutate(period = as.numeric(gsub("p_", "", asb_nm))) %>%
    dplyr::group_by(period) %>%
    dplyr::summarise(lower_95 = quantile(fric, 0.025, na.rm = T),
                     upper_95 = quantile(fric, 0.975, na.rm = T),
                     mean_null = mean(fric, na.rm = T))

  x_axis_color = ifelse(realm_name == "Palearctic", "black", "transparent")
  
  # 8. Plot and Save
  p = ggplot(observed_fric_realm, aes(x = period, y = fric)) +
    geom_ribbon(data = null_envelope_realm, aes(x = period, ymin = lower_95, ymax = upper_95), 
                fill = realm_colors[realm_name], alpha = 0.2,
                inherit.aes = FALSE) +
    geom_line(color = "black", linewidth = 1) +
    geom_point(color = realm_colors[realm_name], size = 3) +
    geom_line(data = null_envelope_realm, aes(y = mean_null), linetype = "dotted") +
    theme_classic(base_size = 40) +
    labs(title = "", x = "Year", y = "")+ 
    theme(axis.title.x = element_text(color = x_axis_color)) +
    ylim(c(0,1)) +
    scale_y_continuous(
      limits = c(0, 1), 
      breaks = c(0, 0.5, 1), 
      labels = c("0", "50", "100")) 
    
  
  ggsave(paste0("results/cum_fric_", realm_name, ".jpeg"), plot = p,   width = 8, height = 8.5)
}

#### automatic loop end ####

library(dplyr)
library(tidyr)
library(ggplot2)
library(geometry)
library(vegan)

ref_val <- global_coords["Bokermannohyla itapoty", "PC2"]
if (ref_val < 0) {
  global_coords$PC2 <- global_coords$PC2 * -1
}

# Secure clean inputs
global_coords = as.data.frame(sp_faxes_coord_amphibians0[, c("PC1", "PC2", "PC3", "PC4")])
periods = sort(unique(amphibians_tax$period))
vertices_indices = geometry::convhulln(global_coords, options = "Tv")
### direction corrected ####


# 2. Join with Taxonomy
vertex_species = unique(as.vector(vertices_indices))
vertex_data = amphibians_tax[vertex_species, ]

# 3. Table of Realms
vertex_table = table(vertex_data$realm)


period_results = list()
periods = sort(unique(amphibians_tax$period))



for (p in periods) {
  
  # 1. Subset taxonomy and coordinates to only those described UP TO period 'p'
  current_tax = amphibians_tax %>% filter(period <= p)
  current_coords = sp_faxes_coord_amphibians0[rownames(sp_faxes_coord_amphibians0) %in% current_tax$scientificName, ]
  
  # 2. Identify global vertices for this specific time slice
  v_idx = unique(as.vector(geometry::convhulln(as.matrix(current_coords[,1:4]), options = "Tv")))
  v_names = rownames(current_coords)[v_idx]
  
  # 3. Count how many vertices each realm contributes
  period_results[[p]] = current_tax %>%
    dplyr::filter(scientificName %in% v_names) %>%
    group_by(realm) %>%
    summarise(vertex_count = n(), .groups = 'drop') %>%
    dplyr::mutate(period = p)
}

vertex_trends = bind_rows(period_results) %>%
  mutate(year = as.numeric(gsub("p_", "", period)))

main_realms = c("Afrotropical", "Neotropical", "Palearctic", "Indomalayan", "Australasian")
plot_data = vertex_trends %>% dplyr::filter(realm %in% main_realms) %>%
  group_by(realm) %>%
  arrange(year) %>% # Ensure years are in order before summing
  mutate(vertex_cum = cumsum(vertex_count)) %>%
  ungroup()

(vertice_plot = ggplot(plot_data, aes(x = year, y = vertex_cum, color = realm)) +
    geom_line(linewidth = 1) +
    geom_point() +
    scale_color_manual(values = realm_colors) +
    labs(title = "",
         y = "Number of species in vertices",
         x = "Time period") +
    theme_classic(base_size = 16) +
    theme(legend.title = element_blank(), legend.position = c(.15, .9)))

ggsave("results/SupMat_vertice_plot.jpeg", plot = vertice_plot)


# Initialize containers for tracking history
period_counts_list <- list()
period_compass_list <- list()
period_vertices_list <- list()

for (p in periods) {
  
  # 1. Gather all species described WITHIN specific period 'p' (Not cumulative)
  current_tax <- amphibians_tax %>% dplyr::filter(period == p) 
  current_coords <- global_coords[rownames(global_coords) %in% current_tax$scientificName, ]
  
  # 2. Extract true 4-dimensional global vertices for this era
  v_idx <- unique(as.vector(convhulln(as.matrix(current_coords[, 1:4]), options = "Tv")))
  v_names <- rownames(current_coords)[v_idx]
  
  # Isolate boundary species data and cleanly split multi-realm strings
  era_stretchers <- current_tax %>%
    dplyr::filter(scientificName %in% v_names) %>%
    dplyr::select(scientificName, realm, year_description) %>%
    tidyr::separate_rows(realm, sep = "\\|") %>%
    dplyr::filter(realm %in% names(realm_colors)) %>%
    dplyr::left_join(tibble::rownames_to_column(current_coords, "scientificName"), by = "scientificName")
  
  # Calculates the single "hub" coordinate where all arrows will start in this facet
  era_global_center <- era_stretchers %>%
    dplyr::summarise(
      global_mean_PC1 = mean(PC1, na.rm = TRUE), 
      global_mean_PC2 = mean(PC2, na.rm = TRUE)  
    )
  
  # 3. Store counts for this era
  period_counts_list[[p]] <- era_stretchers %>%
    dplyr::group_by(realm) %>%
    dplyr::summarise(vertex_count = n(), .groups = "drop") %>%
    dplyr::mutate(period = p, year = as.numeric(gsub("p_", "", p)))
  
  # 4. Store directional centroids (arrows) starting from the Global Era Center
  period_compass_list[[p]] <- era_stretchers %>%
    dplyr::group_by(realm) %>%
    dplyr::summarise(
      # Baseline starting points
      x_start       = era_global_center$global_mean_PC1, 
      y_start       = era_global_center$global_mean_PC2, 
      
      # Directional targets (Tips of the arrows)
      x_end         = mean(PC1, na.rm = TRUE),           
      y_end         = mean(PC2, na.rm = TRUE),           
      
      pull_strength = sqrt(mean(PC1, na.rm = TRUE)^2 + mean(PC2, na.rm = TRUE)^2),
      .groups = "drop"
    ) %>%
    dplyr::mutate(period = p, year = as.numeric(gsub("p_", "", p)))
  
  # Extract outer 2D boundary vertex pins using chull
  outer_hull_indices <- chull(era_stretchers$PC1, era_stretchers$PC2)
  
  period_vertices_list[[p]] <- era_stretchers[outer_hull_indices, ] %>%
    dplyr::select(scientificName, realm, PC1, PC2) %>% 
    dplyr::mutate(period = p)
}

# Combine lists into operational dataframes
historical_counts           <- dplyr::bind_rows(period_counts_list)
historical_compass          <- dplyr::bind_rows(period_compass_list)
historical_compass_vertices <- dplyr::bind_rows(period_vertices_list) 


pc1_trend_test = lm(x_end ~ -1 + year * realm, data = historical_compass)
summary(pc1_trend_test)


pc2_trend_test = lm(y_end ~ -1 + year * realm, data = historical_compass)
summary(pc2_trend_test)



period_hulls <- amphibians_tax %>%
  tidyr::separate_rows(realm, sep = "\\|") %>%
  dplyr::filter(realm %in% names(realm_colors)) %>%
  group_by(period) %>%
  do({
    sub_coords <- global_coords[rownames(global_coords) %in% .$scientificName, c("PC1", "PC2")]
    if(nrow(sub_coords) >= 3) {
      hull_rows <- chull(sub_coords$PC1, sub_coords$PC2)
      sub_coords[hull_rows, ]
    } else {
      data.frame()
    }
  }) %>%
  ungroup()

facet_labels_per = c(
  "p_1850" = "Until 1850",
  "p_1900" = "1851 - 1900",
  "p_1950" = "1901 - 1950",
  "p_1975" = "1951 - 1975",
  "p_2000" = "1976 - 2000",
  "p_2020" = "2001 - 2020"
)

arrow_annotation1 = data.frame(
  period = "p_1850",      
  x_pos  = -0.16,          
  y_start = 0.00,          
  y_end   = -0.10          
)

arrow_annotation2 = data.frame(
  period = "p_2020",      
  y_pos  = 0.09,          
  x_start = 0.00,          
  x_end   = 0.10          
)

# =================================================================
# PLOT PRODUCTION
# =================================================================
ggplot() +
  # Background cloud of all species
  geom_point(data = global_coords, aes(x = PC1, y = PC2), color = "grey87", alpha = 0.3, size = 0.8) +
  
  # Shaded period hull per facet
  geom_polygon(data = period_hulls, aes(x = PC1, y = PC2), 
               fill = "grey50", alpha = 0.09, color = "grey40", linetype = "dashed", linewidth = 0.5) +
  
  # Outer boundary vertex dots (Crisp White Outlines to isolate overlaps)
  geom_point(data = historical_compass_vertices, aes(x = PC1, y = PC2), 
             color = "black", size = 2.1, alpha = 0.95) +
  geom_point(data = historical_compass_vertices, aes(x = PC1, y = PC2, color = realm), 
             size = 1.5, alpha = 0.95) +   
  
  # Centroid starburst vectors
  geom_segment(data = historical_compass, 
               aes(x = x_start, y = y_start, xend = x_end, yend = y_end, color = realm), 
               arrow = arrow(length = unit(0.18, "cm")), linewidth = .95) +
  
  # Vertical annotation text/arrow
  geom_segment(data = arrow_annotation1, aes(x = x_pos, y = y_start, xend = x_pos, yend = y_end),
               arrow = arrow(length = unit(0.17, "cm")), color = "grey30", linewidth = 0.7) +
  geom_text(data = arrow_annotation1, aes(x = x_pos + 0.014, y = -0.018, 
                                          label = "smaller size and larger inter-orbital distance"),
            color = "grey30", size = 2.4, angle = 90, hjust = 0.5, fontface = "bold") +
  
  # Horizontal annotation text/arrow
  geom_segment(data = arrow_annotation2, aes(y = y_pos, x = x_start, xend = x_end, yend = y_pos),
               arrow = arrow(length = unit(0.17, "cm")), color = "grey30", linewidth = 0.7) +
  geom_text(data = arrow_annotation2, aes(y = y_pos + 0.01, x = .03, 
                                          label = "shorter heads, hands, tibias and fingers"),
            color = "grey30", size = 2.4, angle = 0, hjust = 0.7, fontface = "bold") +
  
  # Layout styling
  scale_color_manual(values = realm_colors) +
  facet_wrap(~period, labeller = as_labeller(facet_labels_per)) +
  labs(x = "PC1", y = "PC2") +
  theme_classic(base_size = 14) +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.margin    = margin(t = 0, r = 0, b = 0, l = -40, unit = "pt"))

ggsave("results/compass_realm_period_SPECIFIC_vertices_v2.jpeg", height = 7, width = 8)


 #### direction corrected end ####




 #### ecological traits ####


amphibio0 = read.csv("AmphiBIO_v1.csv") %>% 
  dplyr::select(-id, -Order, - Family, -Genus, -OBS) %>% 
  dplyr::inner_join(read.csv("reproductivemodes_anurans.csv")) %>% 
  dplyr::mutate(across(where(is_character),as_factor)) %>% 
  dplyr::select(-X) %>%  
  dplyr::mutate(across(c(2:18, 31:33), ~ case_when(
    is.na(.) ~ 0, .default = .
  ))) %>%  
  dplyr::mutate(across(c(38), ~ case_when(
    is.na(.) ~ "NA", .default = .
  ))) %>% 
  dplyr::mutate(X11_larval_development = as.factor(X11_larval_development))



names(amphibio0)

amphibians_eco_imp = funspace::impute(
  amphibio0 %>% 
    textshape::column_to_rownames(1),
  phylo = phy
)

amphibians_eco_imp2_df = as.data.frame(amphibians_eco_imp$imputed) %>%   rownames_to_column("Species") 




### FAZER COMO TODAS AS ESPECIES DE ECO, independente se estao em MORPH 
amphibio3 = amphibians_eco_imp2_df %>% 
  dplyr::inner_join(amphibians_tax2 %>% 
                      dplyr::select(scientificName, year_description, period) %>% 
                      dplyr::rename(Species = scientificName)) %>% 
  dplyr::inner_join(amphibians_ass %>% 
                      dplyr::select(scientificName, realm) %>% 
                      dplyr::rename(Species = scientificName))
                      
                    

amphibians_eco_imp3_df = as.data.frame(amphibians_eco_imp$imputed) 

amphibians_eco_imp3_matrix = amphibians_eco_imp3_df %>% 
  dplyr::select(where(is.numeric))


cor_matrix = cor(amphibians_eco_imp3_matrix) # continuous variables not correlated

high_cor = which(abs(cor_matrix) > 0.9 & row(cor_matrix) < col(cor_matrix), arr.ind = TRUE)
(redundant_pairs = data.frame(
  trait1 = rownames(cor_matrix)[high_cor[,1]],
  trait2 = colnames(cor_matrix)[high_cor[,2]],
  cor = cor_matrix[high_cor]) %>% 
    arrange(desc(trait1)))


quant_tr = c("Age_at_maturity_min_y", "Age_at_maturity_max_y", "Size_at_maturity_max_mm", "Longevity_max_y" ,  
             "Litter_size_min_n", "Offspring_size_max_mm", "Litter_size_max_n","Reproductive_output_y",
             "X1_reproduction_type_Oviparity", "X2_macrohabitat_egg_Environment","X4_oviposition_substrate_Animal")

nom_tr = c( "X8_embryonic_development", "X9_embryonic_nutrition", 
           "X10a_juvenile_nutrition", "X10b_parental_feeding", "X11_larval_development")


amphibio_eco = amphibians_eco_imp3_matrix %>%  # selected non-correlated variables
  dplyr::select(-Viv, 
                -Size_at_maturity_min_mm, -Offspring_size_min_mm, -Seeds,
                -X1_reproduction_type_Viviparity, -X4_oviposition_substrate_Aquatic,
                 -X5_medium_egg_Oviduct, -X3_spawning_Non.froth, 
                -X2_macrohabitat_egg_Animal, -X4_oviposition_substrate_Non.aquatic, -Lar, -Dir,
                -Body_size_mm, -Body_mass_g) %>% 
  dplyr::bind_cols(amphibians_eco_imp3_df %>% 
                     dplyr::select(-where(is.numeric))) %>% #joining back with non-numeric columns
  dplyr::mutate(across(c(1:16, 25:38), as.numeric),
                across(all_of(nom_tr), as.factor),
                X1_reproduction_type_Oviparity = as.numeric(X1_reproduction_type_Oviparity),
                X2_macrohabitat_egg_Environment = as.numeric(X2_macrohabitat_egg_Environment),
                X4_oviposition_substrate_Animal = as.numeric(X4_oviposition_substrate_Animal))
  

str(amphibio_eco)
names(amphibio_eco)

fuzzy_name = c(
  rep("Microhabitat", 4),
  rep("Diet", 5),
  rep("Activity", 3),
  rep("Climate", 4),
  rep(NA,8),
  rep(NA,2),
  rep("Medium_egg",1),
  rep("Nest_construction",1),
  rep("Microhabitat_medium",2),
  rep("Nest_construction",1),
  rep("Medium_egg",2),
  rep("Microhabitat_medium",1),
  rep("Spawning",2),
  rep(NA,1),
  rep("Medium_egg",1),
  rep(NA,5)
)


amphibio3_traits = colnames(amphibio_eco) %>% 
  as.data.frame() %>% 
  dplyr::rename(trait_name = 1) %>% 
  dplyr::mutate(trait_type = "F") %>% 
  dplyr::mutate(trait_type = case_when(trait_name %in% quant_tr ~ "Q",
                                       trait_name %in% nom_tr ~ "N",
                                       .default = "F" ))  %>% 
  dplyr::mutate("fuzzy_name" = fuzzy_name)



amphibio_eco_traits_summ = mFD::sp.tr.summary(
  tr_cat     = amphibio3_traits,   
  sp_tr      = amphibio_eco, 
  stop_if_NA = F)


sp_dist_amphibio_eco = mFD::funct.dist(
  sp_tr         = amphibio_eco,
  tr_cat        = amphibio3_traits,
  metric        = "gower",
  scale_euclid  = "scale_center",
  ordinal_var   = "classic",
  weight_type   = "equal",
  stop_if_NA    = F)

fspaces_quality_amphibio_eco = mFD::quality.fspaces(
  sp_dist             = sp_dist_amphibio_eco,
  maxdim_pcoa         = 4,
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE,
  fdendro             = "average")


sp_faxes_coord_amphibio_eco = fspaces_quality_amphibio_eco$"details_fspaces"$"sp_pc_coord" 


sp_faxes_coord_amphibio_eco_all = sp_faxes_coord_amphibio_eco %>% 
  as.data.frame() %>% 
  dplyr::mutate(scientificName = rownames(.)) %>% 
  dplyr::inner_join(amphibio_eco %>% 
                     dplyr::mutate(scientificName = rownames(.))) 

sp_faxes_coord_amphibio_eco2 = sp_faxes_coord_amphibio_eco_all %>% 
  dplyr::select(1:11) %>% 
  column_to_rownames("scientificName") %>% 
  as.matrix()

amphibio3_eco = amphibians_eco_imp3_df %>% 
  tibble::rownames_to_column("Species") %>% 
  dplyr::inner_join(amphibio3) 

#### cumulative Fdiversity ####

# dividing species into periods of species description
amphibians_tax_weights_eco = amphibio3_eco  %>% 
  dplyr::mutate(present = 1) %>% 
  tidyr::pivot_wider(names_from =  Species,       
                     values_from = present,    
                     id_cols = period, 
                     values_fill = 0      
  ) %>%
  remove_rownames() %>% column_to_rownames( "period") %>% 
  as.matrix()


amphibians_tax_weights_cum_eco = amphibians_tax_weights_eco %>% 
  dplyr::as_data_frame() %>% 
  dplyr::mutate(period = rownames(amphibians_tax_weights_eco)) %>% 
  dplyr::relocate(period) %>% 
  dplyr::mutate(year = as.integer(gsub("p_", "", period))) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(across(-c(period, year), cummax)) %>% 
  dplyr::select(-year)


amphibians_tax_weights_cum2_eco = amphibians_tax_weights_cum_eco%>%
  remove_rownames() %>% column_to_rownames() %>% 
  as.matrix()

dim(sp_faxes_coord_amphibio_eco2)
dim(amphibians_tax_weights_cum2_eco)

alpha_fd_indices_amphibians_eco = mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_amphibio_eco2[ , c("PC1", "PC2")],
  asb_sp_w         = amphibians_tax_weights_cum2_eco,
  ind_vect         = c("fric"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)



alpha_fd_indices_amphibians_eco$functional_diversity_indices


col_asb1 = "darkblue"
col_asb2 = "#DCE319FF"

plots_alpha = mFD::alpha.multidim.plot(
  output_alpha_fd_multidim = alpha_fd_indices_amphibians_eco,
  plot_asb_nm              = c("p_1850", "p_2020"),
  ind_nm                   = c( "fric", 
                                "fdiv"),
  faxes                    = NULL,
  faxes_nm                 = NULL,
  range_faxes              = c(NA, NA),
  color_bg                 = "white",
  shape_sp                 = c(pool = 3, asb1 = 21, asb2 = 21),
  size_sp                  = c(pool = 0.7, asb1 = .7, asb2 = .2),
  color_sp                 = c(pool = "grey50", asb1 = col_asb1, asb2 = "black"),
  color_vert               = c(pool = "grey50", asb1 = col_asb1, asb2 = "black"),
  fill_sp                  = c(pool = NA, asb1 = col_asb1, asb2 = col_asb2),
  fill_vert                = c(pool = NA, asb1 = col_asb1, asb2 = col_asb2),
  color_ch                 = c(pool = NA, asb1 = col_asb1, asb2 = col_asb2),
  fill_ch                  = c(pool = "grey86", asb1 = col_asb1, asb2 = col_asb2),
  alpha_ch                 = c(pool = 1, asb1 = 0.4, asb2 = 0.4),
  shape_centroid_fdis      = c(asb1 = 22,  asb2 = 24),
  shape_centroid_fdiv      = c(asb1 = 22,  asb2 = 24),
  shape_centroid_fspe      = 23,
  color_centroid_fspe      = "black",
  size_sp_nm               = 5, 
  color_sp_nm              = "black",
  plot_sp_nm               = NULL,
  fontface_sp_nm           = "normal",
  save_file                = FALSE,
  check_input              = TRUE) 


plots_alpha$"fric"[[1]]



alpha_fd_indices_amphibio_eco2 = alpha_fd_indices_amphibians_eco$functional_diversity_indices %>% 
  as.data.frame() %>% 
  dplyr::mutate(period = rownames(alpha_fd_indices_amphibians_eco$functional_diversity_indices)) %>% 
  dplyr::mutate(year = as.integer(gsub("p_", "", period))) %>%
  dplyr::arrange(year) 

# ggplot(alpha_fd_indices_amphibio_eco2, aes(x = year, y = fric)) +
#   geom_line(color = "darkblue", linewidth = 1.2) +
#   geom_point(color = "darkblue", size = 4) +
#   geom_text(aes(label = round(sp_richn, 3)), vjust = 2, size = 3.5) +
#   labs(
#     x = "Time Period (Year)",
#     y = "Cumulative Functional Richness"
#   ) +
#   theme_classic() +
#   theme(
#     plot.title = element_text(hjust = 0.5, face = "bold"),
#     plot.subtitle = element_text(hjust = 0.5),
#     axis.title = element_text(size = 12),
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   ) +
#   geom_line(data = alpha_fd_indices_amphibians_cum, 
#             aes(x = year, y = fric), color = "black", linewidth = 1.2) +
#   geom_point(data = alpha_fd_indices_amphibians_cum, 
#              aes(x = year, y = fric), color = "#D55E00", size = 4) +
#   geom_text(data = alpha_fd_indices_amphibians_cum, 
#             aes(x = year, y = fric, label = round(sp_richn, 3)), 
#             vjust = -1, size = 3.5, color = "#D55E00") +
#   ylim(c(0,1))

(eco_morpho_all = ggplot() +
  # 1. Plot Ecological Traits (Blue)
  geom_line(data = alpha_fd_indices_amphibio_eco2, 
            aes(x = year, y = fric, color = "Ecological Traits"), linewidth = 1.2) +
  geom_point(data = alpha_fd_indices_amphibio_eco2, 
             aes(x = year, y = fric, color = "Ecological Traits"), size = 4, alpha = .5) +
  geom_text(data = alpha_fd_indices_amphibio_eco2,
            aes(x = year, y = fric, label = round(sp_richn, 3)), 
            vjust = 2, size = 3.5, color = "olivedrab") +
  
  # 2. Plot Morphological Traits (Black)
  geom_line(data = alpha_fd_indices_amphibians_cum, 
            aes(x = year, y = fric, color = "Morphological Traits"), linewidth = 1.2) +
  geom_point(data = alpha_fd_indices_amphibians_cum, 
             aes(x = year, y = fric, color = "Morphological Traits"), size = 4, alpha = .5) +
  geom_text(data = alpha_fd_indices_amphibians_cum, 
            aes(x = year, y = fric, label = round(sp_richn, 3)), 
            vjust = -1, size = 3.5, color = "black") +
  
  # 3. Define the Legend Colors Manually
  scale_color_manual(name = "", 
                     values = c("Ecological Traits" = "olivedrab", 
                                "Morphological Traits" = "black")) +
  
  # 4. Formatting
  labs(x = "Time Period (Year)", y = "Cumulative Functional Richness") +
  ylim(c(0, 1)) +
  theme_classic(base_size = 16) +
  theme(
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(.55, .2)
  ))

ggsave("results/cum_fric_ecological_morphological.jpeg", width = 9, height = 8)


### common species - 632 species
amphibio2 = amphibio_eco %>% 
  rownames_to_column("Species") %>% 
  dplyr::inner_join(amphibians_morph3_imp2_df)  %>% 
  dplyr::inner_join(amphibians_tax %>% 
                      dplyr::rename(Species = scientificName) %>% 
                      dplyr::select(Species, realm, period)) # only a fraction of species common to morphological and ecological databases

amphibio2_matrix = amphibio2 %>% 
  dplyr::select(where(is.numeric))
# Testing correlations

cor_matrix_eco = cor(amphibio2_matrix) # continuous variables not correlated

high_cor_eco = which(abs(cor_matrix_eco) > 0.9 & row(cor_matrix_eco) < col(cor_matrix_eco), arr.ind = TRUE)
(redundant_pairs = data.frame(
  trait1 = rownames(cor_matrix_eco)[high_cor[,1]],
  trait2 = colnames(cor_matrix_eco)[high_cor[,2]],
  cor = cor_matrix_eco[high_cor]) %>% 
    arrange(desc(trait1)))


### doing so for shared species across datasets 



sp_faxes_coord_amphibians_redux = sp_faxes_coord_amphibians %>% 
  as.data.frame() %>% 
  rownames_to_column("Species") %>% 
  dplyr::inner_join(amphibio_eco %>% 
                      rownames_to_column("Species") ) %>% 
  column_to_rownames("Species") %>% 
  dplyr::select(1:10) %>% 
  as.matrix()


amphibians_tax_weights_cum2_redux = amphibians_tax_weights_cum2 %>% 
  as.data.frame() %>% 
  dplyr::select(any_of(rownames(sp_faxes_coord_amphibians_redux))) %>% 
  as.matrix()


alpha_fd_indices_amphibians_redux = mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_amphibians_redux[ , c("PC1", "PC2")],
  asb_sp_w         = amphibians_tax_weights_cum2_redux,
  ind_vect         = c( "fric"),
  scaling          = F,
  check_input      = TRUE,
  details_returned = T)

alpha_fd_indices_amphibians_redux$functional_diversity_indices

alpha_fd_indices_amphibio_morph_redux = alpha_fd_indices_amphibians_redux$functional_diversity_indices %>% 
  as.data.frame() %>% 
  dplyr::mutate(period = rownames(alpha_fd_indices_amphibians_redux$functional_diversity_indices)) %>% 
  dplyr::mutate(year = as.integer(gsub("p_", "", period))) %>%
  dplyr::arrange(year) %>% 
  dplyr::mutate(fric_scaled = fric/max(fric))


sp_faxes_coord_amphibio_eco_redux = sp_faxes_coord_amphibio_eco %>% 
  as.data.frame() %>% 
  dplyr::mutate(scientificName = rownames(.)) %>% 
  dplyr::inner_join(amphibio_eco %>% 
                      dplyr::mutate(scientificName = rownames(.))) %>% 
  dplyr::filter(scientificName %in% amphibians_morph3_imp2_df$Species) %>% 
  column_to_rownames("scientificName") %>% 
  dplyr::select(1:10) %>% 
  as.matrix()


amphibians_tax_weights_cum2_redux_eco = amphibians_tax_weights_cum2_eco %>% 
  as.data.frame() %>% 
  dplyr::select(any_of(rownames(sp_faxes_coord_amphibians_redux))) %>% 
  as.matrix()


alpha_fd_indices_amphibians_redux_eco = mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_amphibio_eco_redux[ , c("PC1", "PC2")],
  asb_sp_w         = amphibians_tax_weights_cum2_redux_eco,
  ind_vect         = c( "fric"),
  scaling          = F,
  check_input      = TRUE,
  details_returned = T)

alpha_fd_indices_amphibio_eco2_redux = alpha_fd_indices_amphibians_redux_eco$functional_diversity_indices %>% 
  as.data.frame() %>% 
  dplyr::mutate(period = rownames(alpha_fd_indices_amphibians_eco$functional_diversity_indices)) %>% 
  dplyr::mutate(year = as.integer(gsub("p_", "", period))) %>%
  dplyr::arrange(year) %>% 
  dplyr::mutate(fric_scaled = fric/max(fric))


(eco_morpho_redux = ggplot() +
  # 1. Plot Ecological Traits (Blue)
  geom_line(data = alpha_fd_indices_amphibio_eco2_redux, 
            aes(x = year, y = fric_scaled * 100, color = "Ecological Traits"), linewidth = 1.2) +
  geom_point(data = alpha_fd_indices_amphibio_eco2_redux, 
             aes(x = year, y = fric_scaled * 100, color = "Ecological Traits"), size = 4, alpha = .5) +
  # geom_text(data = alpha_fd_indices_amphibio_eco2_redux,
  #           aes(x = year, y = fric_scaled, label = round(sp_richn, 3)), 
  #           vjust = 2, size = 3.5, color = "olivedrab") +
  
  # 2. Plot Morphological Traits (Black)
  geom_line(data = alpha_fd_indices_amphibio_morph_redux, 
            aes(x = year, y = fric_scaled * 100, color = "Morphological Traits"), linewidth = 1.2) +
  geom_point(data = alpha_fd_indices_amphibio_morph_redux, 
             aes(x = year, y = fric_scaled * 100, color = "Morphological Traits"), size = 4, alpha = .5) +
  geom_text(data = alpha_fd_indices_amphibio_morph_redux, 
            aes(x = year, y = fric_scaled*100, label = round(sp_richn, 3)), 
            vjust = -1, size = 3.5, color = "black") +
  
  # 3. Define the Legend Colors Manually
  scale_color_manual(name = "", 
                     values = c("Ecological Traits" = "olivedrab", 
                                "Morphological Traits" = "black")) +
  
  # 4. Formatting
  labs(x = "Time Period (Year)", y = "") +
  ylim(c(0, 100)) +
  theme_classic(base_size = 16) +
  theme(
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(.8, .2)
  ))


 ggsave("results/cum_fric_ecological_morphological_redux.jpeg")
 

 # (ecological_morphological = ggarrange(
 #   eco_morpho_all,  eco_morpho_redux,
 #   ncol = 2, nrow = 1, 
 #   labels = c("A", "B"),  
 #   widths = c(1, 1.1),    
 #   heights = c(1, 1)
 # ))
 # 
 # ggsave("results/Fig_cum_fric_ecological_morphological.jpeg", height = 7, width = 10)


#### uniqueness models ####
## FUSE

fuse_amphibians = mFD::fuse(sp_dist_amphibians,
                            sp_faxes_coord_amphibians[,1:4],
                                      GE = rep(0.001, nrow(sp_faxes_coord_amphibians))) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("Species")

fuse_amphibians0 = fuse_amphibians %>% 
  dplyr::left_join(amphibians_tax, by = c("Species" = "scientificName")) %>% 
  dplyr::select(Species, FUn_std, FSp_std, realm, year_description)

amphibians_tax
table(fuse_amphibians0$realm)

library(mgcv)
library(marginaleffects)

# Pre-process data
fuse_amphibians0 = fuse_amphibians0 %>%
  dplyr::mutate(
    realm = as.factor(realm),
    year_description = as.numeric(year_description)
  ) %>%
  # GAMs can fail with NAs in the terms
  drop_na(FUn_std, year_description, realm) %>% 
  dplyr::mutate(realm = case_when(Species == "Anomaloglossus verbeeksnyderorum" ~ "Neotropical", 
                                  Species == "Cornufer vitiensis" ~ "Australasian", 
                                  .default = realm)) %>% 
  dplyr::filter(realm %in% c("Neotropical", "Palearctic", "Afrotropical", "Indomalayan", "Australasian")) %>% 
  dplyr::mutate(realm = as.factor(realm))

n_fuse = nrow(fuse_amphibians0)
fuse_amphibians0$FUn_std_beta = (fuse_amphibians0$FUn_std * (n_fuse - 1) + 0.5) / n_fuse
fuse_amphibians0$FSp_std_beta = (fuse_amphibians0$FSp_std * (n_fuse - 1) + 0.5) / n_fuse


table(fuse_amphibians0$realm)




realm_colors = c(
  "Afrotropical" = "#0072B2",
  "Australasian" = "#E69F00",
  "Indomalayan"  = "#009E73",
  "Neotropical"  = "#CC79A7",
  "Palearctic"   = "#D55E00"
)

realm_list = fuse_amphibians0 %>% 
  group_split(realm)


fuse_amphibians0_dif = fuse_amphibians0 %>%
  mutate(
    realm_fac  = relevel(as.factor(realm), ref = "Palearctic"),
    realm_ord  = as.ordered(realm_fac))

gam_fun_diff = gam(
  FUn_std_beta ~ realm_fac + 
    s(year_description, k = 10, m = 2) + 
    s(year_description, by = realm_ord, k = 10, m = 1),
  data = fuse_amphibians0_dif,
  family = betar(link = "logit"), 
  method = "REML"                 
)

summary(gam_fun_diff)

walk(realm_list, function(df) {
    
    current_realm <- as.character(unique(df$realm))
    
    current_color <- ifelse(current_realm %in% names(realm_colors), 
                            realm_colors[current_realm], 
                            "#333333")
    
    x_axis_color = ifelse(current_realm == "Palearctic", "black", "transparent")
    
    p = plot_predictions(
      gam_fun_diff, 
      condition = list(
        "year_description", 
        realm_fac = current_realm, 
        realm_ord = current_realm  
      )
    ) +
      coord_cartesian(xlim = c(1850, 2020), ylim = c(0, 0.4)) + 
      
      geom_rug(aes(x = year_description), 
               data = df, 
               alpha = 0.3, inherit.aes = FALSE, sides = "b", length = unit(0.08, "npc")) +
      
      scale_fill_manual(values = current_color) +
      scale_color_manual(values = current_color) +
      labs(
        x = "Year of Description",
        y = ""
      ) +
      theme_classic(base_size = 40) +
      theme(
        legend.position = "none",
        strip.background = element_blank(), 
        strip.text = element_blank(),
        axis.title.x = element_text(color = x_axis_color),
      ) +
      scale_y_continuous(
        limits = c(0, .4), 
        breaks = c(0, 0.2, 0.4)) 
  
    
    # 4. Clean saving naming conventions
    file_name <- paste0("results/gam_morphological_fun_", gsub("[| ]", "_", current_realm), ".jpeg")
    ggsave(file_name, p,  width = 8, height = 8.5, dpi = 300)
    
    message("Saved: ", file_name)
  })

#functional specialization

# 
# fuse_amphibians0_dif = fuse_amphibians0 %>%
#   mutate(
#     realm_fac  = relevel(as.factor(realm), ref = "Palearctic"),
#     realm_ord  = as.ordered(realm_fac))
#     
# gam_fsp_diff = gam(FSp_std_beta ~ realm_fac + 
#                      s(year_description, m = 2) + 
#                      s(year_description, by = realm_ord, m = 1) + 
#                      s(realm_fac, bs = "re"),
#                    family = betar(link = "logit"),
#                    data = fuse_amphibians0_dif,
#                    method = "REML")
# 
# summary(gam_fsp_diff)
# 
# 
# walk(realm_list, function(df) {
#   
#   current_realm <- as.character(unique(df$realm))
#   
#   current_color <- ifelse(current_realm %in% names(realm_colors), 
#                           realm_colors[current_realm], 
#                           "#333333")
#   
#   x_axis_color = ifelse(current_realm == "Palearctic", "black", "transparent")
#   
#   p <- plot_predictions(
#     gam_fsp_diff, 
#     condition = list(
#       "year_description", 
#       realm_fac = current_realm, 
#       realm_ord = current_realm  
#     )
#   ) +
#     coord_cartesian(xlim = c(1850, 2020), ylim = c(0, 0.4)) + 
#     
#     geom_rug(aes(x = year_description), 
#              data = df %>% filter(year_description >= 1850), 
#              alpha = 0.2, inherit.aes = FALSE) +
#     
#     scale_fill_manual(values = current_color) +
#     scale_color_manual(values = current_color) +
#     labs(
#       x = "Year of Description",
#       y = ""
#     ) +
#     theme_classic(base_size = 18) +
#     theme(
#       legend.position = "none",
#       strip.background = element_blank(), 
#       strip.text = element_blank(),
#       axis.title.x = element_text(color = x_axis_color),
#       axis.text.x  = element_text(color = x_axis_color),
#       axis.ticks.x = element_line(color = x_axis_color)
#     )
# 
#   
#   # 4. Clean saving naming conventions
#   file_name <- paste0("results/gam_morphological_fsp_", gsub("[| ]", "_", current_realm), ".jpeg")
#   ggsave(file_name, p, width = 8, height = 8, dpi = 300)
#   
#   message("Saved: ", file_name)
# })


#####

# 1. Setup: identify the minimum species count (Palearctic = 278)
n_min = sum(fuse_amphibians0$realm == "Palearctic") 
n_iterations <- 99 # Number of random sub-samples

# 2. Prepare the species list by realm
realm_species_list <- fuse_amphibians0 %>% 
  group_split(realm)

# 3. Functional Rarefaction Loop
rarefaction_results <- map_dfr(realm_species_list, function(df_realm) {
  
  realm_name <- as.character(unique(df_realm$realm))
  n_total <- nrow(df_realm)
  
  # If realm is smaller than or equal to n_min, we just take it as is
  # Otherwise, we sub-sample n_min species multiple times
  if (n_total <= n_min) {
    # Calculate FRic once for the full set (Palearctic case)
    asb_matrix <- matrix(1, nrow = 1, ncol = n_total, 
                         dimnames = list(realm_name, df_realm$Species))
    
    fric_val <- mFD::alpha.fd.multidim(
      sp_faxes_coord = sp_faxes_coord_amphibians[, c("PC1", "PC2", "PC3", "PC4")],
      asb_sp_w       = asb_matrix,
      ind_vect       = "fric",
      scaling        = TRUE
    )$functional_diversity_indices$fric
    
    return(data.frame(realm = realm_name, fric = fric_val, type = "Observed"))
    
  } else {
    # Sub-sample larger realms to n_min
    sub_sample_frics <- replicate(n_iterations, {
      random_spp <- sample(df_realm$Species, n_min)
      asb_matrix <- matrix(1, nrow = 1, ncol = n_min, 
                           dimnames = list("temp", random_spp))
      
      mFD::alpha.fd.multidim(
        sp_faxes_coord = sp_faxes_coord_amphibians[, c("PC1", "PC2", "PC3", "PC4")],
        asb_sp_w       = asb_matrix,
        ind_vect       = "fric",
        scaling        = TRUE
      )$functional_diversity_indices$fric
    })
    
    return(data.frame(realm = realm_name, fric = sub_sample_frics, type = "Rarefied"))
  }
})


ggplot(rarefaction_results, aes(x = reorder(realm, -fric), y = fric, fill = realm)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = realm_colors) +
  labs(
    x = "Biogeographic Realm",
    y = "Functional Richness (FRic)"
  ) +
  theme_classic() +
  theme(legend.position = "none")

ggsave("results/SuppMat_rarefaction_realm.jpeg")


#### Figures #####
library(sf)
library(raster)
library(spatialEco)
library(purrr)
library(patchwork)
library(magick)
library(grid)

#Citation: Olson, D.M., E. Dinerstein, E.D. Wikramanayake, N.D. Burgess, G.V.N. Powell, E.C. Underwood, J.A. D'Amico, ´
#I. Itoua, H.E. Strand, J.C. Morrison, C.J. Loucks, T.F. Allnutt, T.H. Ricketts, Y. Kura, J.F. Lamoreux, W.W. Wettengel, P. Hedao, and K.R. Kassem.
#Terrestrial Ecoregions of the World: A New Map of Life on Earth (PDF, 1.1M) BioScience 51:933-938.

realms0 = read_sf("spatial/terrestrial_realms_v3.gpkg") 

realms2 = realms0  %>% 
  dplyr::filter(REALM %in% c("PA", "AT", "AA", "IM", "NT"))


realm_mapping = c(
  "PA" = "Palearctic",
  "AT" = "Afrotropical",
  "AA" = "Australasian",
  "IM" = "Indomalayan",
  "NT" = "Neotropical"
)

realms_clean = realms2 %>%
  dplyr::mutate(realm_name = realm_mapping[REALM])

walk(unique(realms_clean$realm_name), function(name) {
  
  # Filter strictly to the single realm geometry
  single_realm_sf <- realms_clean %>% dplyr::filter(realm_name == name)
  
  # Dynamically pull the exact bounding box edges for this specific realm
  bbox <- st_bbox(single_realm_sf)
  
  # Fetch your matching publication color code
  current_color <- realm_colors[name]
  
  p_map = ggplot() +
    # Render the isolated realm polygon
    geom_sf(data = single_realm_sf, fill = current_color, color = "grey25", linewidth = 0.3) +
    
    # -----------------------------------------------------------------
  # THE FIX: REMOVED EXPAND CORRECTION TO LET GEOMETRY BREATHE NATURALLY
  # -----------------------------------------------------------------
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
           ylim = c(bbox["ymin"], bbox["ymax"])) + 
    
    # Render the vertical label on the left
    labs(x = NULL, y = name) + 
    
    theme_void(base_size = 20) + 
    theme(
      # Standardize the row title placement perfectly to the left
      axis.title.y = element_text(face = "bold", size = 30, angle = 90, vjust = 0.5, hjust = 0.5),
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
    )
  
  # Save using uniform square file outputs. 
  # ggplot will pad the edges with clean whitespace, keeping the maps centered and undistorted.
  file_name <- paste0("results/map_layout_fixed_", gsub(" ", "_", name), ".jpeg")
  ggsave(file_name, plot = p_map, width = 6, height = 6, dpi = 300)
  
  message("Saved perfectly proportioned map: ", file_name)
})


# 1. Define the exact row sequence matching your target layout
realm_sequence <- c( "Afrotropical",  "Australasian","Indomalayan","Neotropical", "Palearctic")

# 2. Pre-allocate a list to store our combined rows
compiled_rows <- list()

for (realm in realm_sequence) {
  
  # Sanitize the realm name to match your file saving conventions
  sanitized_name <- gsub(" ", "_", realm)
  
  # Read the 4 individual JPEGs for the current realm row
  img_map   <- ggplotGrob(ggplot() + annotation_custom(rasterGrob(image_read(paste0("results/map_layout_fixed_", sanitized_name, ".jpeg")))))
  img_space <- ggplotGrob(ggplot() + annotation_custom(rasterGrob(image_read(paste0("results/funspace_", realm, ".jpeg")))))
  img_fric  <- ggplotGrob(ggplot() + annotation_custom(rasterGrob(image_read(paste0("results/cum_fric_", realm, ".jpeg")))))
  img_gam   <- ggplotGrob(ggplot() + annotation_custom(rasterGrob(image_read(paste0("results/gam_morphological_fun_", sanitized_name, ".jpeg")))))
  
  # Combine the 4 images horizontally into a single structured row
  # patchwork alignments ensure their inner canvas borders match exactly
  compiled_rows[[realm]] <- patchwork::wrap_plots(
    img_map, img_space, img_fric, img_gam, 
    ncol = 4
  ) 
}

# 3. Stack all 5 rows vertically using patchwork's division operator
master_panel <- (compiled_rows[[1]] / 
  compiled_rows[[2]] / 
  compiled_rows[[3]] / 
  compiled_rows[[4]] / 
  compiled_rows[[5]]) & theme(
    strip.background = element_blank(),
    strip.text       = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.border     = element_blank()
  )

master_panel

ggsave("results/figure_realm_v3.svg", 
       plot = master_panel, 
       width = 16,     # 4 columns * 4 inches per cell
       height = 20,    # 5 rows * 4 inches per cell
       dpi = 300)

##### research interest #####

# interest


amphi_interest = read.csv("amphi_interest.csv") %>% 
  dplyr::rename(scientificName = Scientific) %>% 
  dplyr::inner_join(sp_faxes_coord_amphibians_all)

ggplot(amphi_interest, aes(x = year_description, y = public)) +
  geom_point(size = .6, color ="blue", alpha = .2) +
  geom_point(aes(y=acad), color='red',size = .6, alpha = .1) +
  geom_smooth(method = "gam", fill = "dodgerblue3") +
  geom_smooth(aes(y=acad), color='red', fill = "firebrick", size = 1.3, alpha = .3,method = "gam") + 
  theme_classic(base_size = 15) +
  labs(
    x = "Time Period (Year)",
    y = "Interest"
  )


(amphibians_interest_acad = ggplot() +
    # --- NON-threatened Hulls & Points ---
    geom_polygon(data = amphi_interest[chull(amphi_interest$PC1, amphi_interest$PC2), ],
                 aes(x = PC1, y = PC2, color = acad), # Map fill to a label
                 color = "black", alpha = 0.1, size = 0.9) +
    geom_point(data = amphi_interest, 
               aes(x = PC1, y = PC2), # Map color to a label
               size = 2.2, color = "black") +
    geom_point(data = amphi_interest, 
               aes(x = PC1, y = PC2, color = acad), # Map color to a label
               size = 2, alpha = .8) +
    
    scale_color_viridis_c(option = "F", direction = -1,
                          alpha = .75,
                          breaks = c(0, 0.5, 1),
                          labels = c(0, 0.5, 1)) +
    theme_classic() +
    labs(x = "PC1", y = "PC2") +
    coord_fixed(ratio = 1) +
    theme(legend.position = c(.1,.175),
          legend.text = element_text(size = 8),
          legend.title=element_blank(),
          legend.background = element_blank()))

(amphibians_interest_public = ggplot() +
    geom_polygon(data = amphi_interest[chull(amphi_interest$PC1, amphi_interest$PC2), ],
                 aes(x = PC1, y = PC2, color = public), # Map fill to a label
                 color = "black", alpha = 0.1, size = 0.9) +
    geom_point(data = amphi_interest, 
               aes(x = PC1, y = PC2), # Map color to a label
               size = 2.2, color = "black") +
    geom_point(data = amphi_interest, 
               aes(x = PC1, y = PC2, color = public), # Map color to a label
               size = 2, alpha = .8) +
    
    scale_color_viridis_c(option = "D", direction = -1,
                          alpha = .75,
                          breaks = c(0, 0.5, 1),
                          labels = c(0, 0.5, 1)) +
    theme_classic() +
    labs(x = "PC1", y = "PC2") +
    coord_fixed(ratio = 1) +
    theme(legend.position = c(.1, .175),
          legend.text = element_text(size = 8),
          legend.title=element_blank(),
          legend.background = element_blank()))


(amphibians_interest = ggarrange(
  amphibians_interest_public,  amphibians_interest_acad,
  ncol = 1, nrow = 2, 
  labels = c("A", "B"),  # Empty label for the blank space
  widths = c(1, 1)      # Ensures both halves are equal size
))

ggsave("results/amphibians_interest_Apublic_Bacad.jpeg")

# part2

amphib_interest = read.csv("amphi_interest.csv") %>% 
  dplyr::rename("scientificName" = "Scientific") %>% 
  dplyr::left_join(amphibians_tax) %>% 
  dplyr::mutate(period = as.factor(period)) 

period_order = c("p_1850", "p_1900", "p_1950", "p_1975", "p_2000", "p_2020")

period_totals = amphib_interest %>%
  dplyr::filter(!is.na(year_description)) %>%  #filtering out species without year of description
  # 2. Sum everything within each period first
  dplyr::group_by(period) %>%
  dplyr::summarise(
    sum_public = sum(public, na.rm = TRUE),
    sum_acad   = sum(acad, na.rm = TRUE),
    .groups = "drop") %>% 
  dplyr::mutate(period_year = factor(period, levels = period_order)) %>%
  
  # 4. Arrange by the NEW factor column, then calculate cumulative sums
  dplyr::arrange(period_year) %>%
  dplyr::mutate(
    cumulative_public = cumsum(sum_public),
    cumulative_acad   = cumsum(sum_acad)
  ) %>% 
  dplyr::mutate(year = as.integer(gsub("p_", "", period))) %>%
  dplyr::arrange(year) 

ggplot(period_totals, aes(x = year, y = cumulative_public )) +
  geom_line(color = "green4", linewidth = 1.2, alpha = .6) +
  geom_point(color = "green4", size = 4) +
  # scale_x_continuous(breaks = unique(alpha_fd_indices_amphibians_cum$year)) + 
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  geom_line(aes(x = year, y = cumulative_acad),color = "purple3", linewidth = 1.2, alpha = .5) +
  geom_point(aes(x = year, y = cumulative_acad),color = "purple3", size = 4) +
  labs(x = "Year",
       y = "Interest")

amphib_interest2 = amphib_interest %>% 
  dplyr::left_join(fuse_amphibians0, by = c("scientificName" = "Species", "realm", "year_description")) %>% 
  dplyr::mutate(acad_beta = (acad * (nrow(.) - 1) + 0.5) / nrow(.)) %>% 
  dplyr::mutate(publ_beta = (public * (nrow(.) - 1) + 0.5) / nrow(.)) %>% 
  dplyr::filter(!is.na(year_description)) %>% 
  dplyr::mutate(period = case_when(year_description <= 1850 ~ "1850",
                                   year_description > 1850 & year_description <= 1900 ~ "1900",
                                   year_description > 1900 & year_description <= 1950 ~ "1950",
                                   year_description > 1950 & year_description <= 1975 ~ "1975",
                                   year_description > 1975 & year_description <= 2000 ~ "2000",
                                   year_description > 2000 & year_description <= 2020 ~ "2020"
  ),
  period = as.factor(period))  


### Functional uniqueness - interest #

glm_fun_acad = glmmTMB(acad_beta ~ FUn_std_beta + year_description +  (1|realm), family = beta_family(),
                   amphib_interest2)
summary(glm_fun_acad)


pred_grid_acad = data.frame(
  FUn_std_beta = seq(min(amphib_interest2$FUn_std_beta, na.rm = TRUE), 
                     max(amphib_interest2$FUn_std_beta, na.rm = TRUE), 
                     length.out = 100),
  year_description = mean(amphib_interest2$year_description)
)

preds = predict(glm_fun_acad, newdata = pred_grid_acad, se.fit = TRUE, re.form = NA)

pred_grid_acad$estimate  <- plogis(preds$fit)
pred_grid_acad$conf.low  <- plogis(preds$fit - 1.96 * preds$se.fit)
pred_grid_acad$conf.high <- plogis(preds$fit + 1.96 * preds$se.fit)

ggplot(pred_grid_acad, aes(x = FUn_std_beta, y = estimate)) +
  # Confidence Ribbon
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "steelblue", alpha = 0.2) +
  # Main Prediction Line
  geom_line(color = "steelblue", linewidth = 1.2) +
  # Raw data distribution
  geom_rug(data = amphib_interest2, aes(x = FUn_std_beta), inherit.aes = FALSE, alpha = 0.1) +
  labs(
    x = "Functional Uniqueness (Standardized)",
    y = "Predicted Academic Interest (0-1)"
  ) +
  theme_classic()


period_means = amphib_interest2 %>%
  group_by(period) %>%
  summarise(year_description = mean(year_description, na.rm = TRUE))

pred_grid_periods <- expand.grid(
  FUn_std_beta = seq(min(amphib_interest2$FUn_std_beta, na.rm = TRUE), 
                     max(amphib_interest2$FUn_std_beta, na.rm = TRUE), 
                     length.out = 100),
  period = levels(amphib_interest2$period)
) %>%
  left_join(period_means, by = "period")

preds_p <- predict(glm_fun_acad, newdata = pred_grid_periods, se.fit = TRUE, re.form = NA)

pred_grid_periods$estimate <- plogis(preds_p$fit)
pred_grid_periods$conf.low <- plogis(preds_p$fit - 1.96 * preds_p$se.fit)
pred_grid_periods$conf.high <- plogis(preds_p$fit + 1.96 * preds_p$se.fit)

ggplot(pred_grid_periods, aes(x = FUn_std_beta, y = estimate, color = period, fill = period)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, color = NA) +
  geom_line(linewidth = 1.2) +
  labs(
    x = "Functional Uniqueness",
    y = "Predicted Academic Interest",
    color = "Period of description",
    fill = "Period of description"
  ) +
  theme_classic(base_size = 14) +
  scale_color_viridis_d(option = "F", direction = 1) +
  scale_fill_viridis_d(option = "F", direction = 1) +
  theme(legend.position = c(0.2,0.82))
#Higher uniqueness consistently drives interest, regardless of age

ggsave("results/fun_acad_glm.jpeg")

glm_fun_publ = glmmTMB(publ_beta ~ FUn_std_beta + year_description +  (1|realm), family = beta_family(),
                       amphib_interest2)
summary(glm_fun_publ)



pred_grid_pub <- expand.grid(
  FUn_std_beta = seq(min(amphib_interest2$FUn_std_beta, na.rm = TRUE), 
                     max(amphib_interest2$FUn_std_beta, na.rm = TRUE), 
                     length.out = 100),
  period = unique(amphib_interest2$period)
) %>%
  left_join(period_means, by = "period")

# 4. Predict using native glmmTMB logic
preds_pub <- predict(glm_fun_publ, newdata = pred_grid_pub, se.fit = TRUE, re.form = NA)

pred_grid_pub$estimate <- plogis(preds_pub$fit)
pred_grid_pub$conf.low <- plogis(preds_pub$fit - 1.96 * preds_pub$se.fit)
pred_grid_pub$conf.high <- plogis(preds_pub$fit + 1.96 * preds_pub$se.fit)

# 5. Plot
ggplot(pred_grid_pub, aes(x = FUn_std_beta, y = estimate, color = period, fill = period)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, color = NA) +
  geom_line(linewidth = 1.2) +
  labs(
    x = "Functional Uniqueness",
    y = "Predicted Public Interest (0-1)"
  ) +
  theme_classic(base_size = 14) +
  scale_color_viridis_d(option = "D", direction = -1)+
  scale_fill_viridis_d(option = "D", direction = -1) +
  theme(legend.position = c(0.2,0.82))

# "The public's interest is dictated by time, not 'weirdness'"

ggsave("results/fun_publ_glm.jpeg")


# Functional specialization - interest

glm_FSp_acad = glmmTMB(acad_beta ~ FSp_std_beta + year_description +  (1|realm), family = beta_family(),
                       amphib_interest2)
summary(glm_FSp_acad)


pred_grid_acad = data.frame(
  FSp_std_beta = seq(min(amphib_interest2$FSp_std_beta, na.rm = TRUE), 
                     max(amphib_interest2$FSp_std_beta, na.rm = TRUE), 
                     length.out = 100),
  year_description = mean(amphib_interest2$year_description)
)

preds = predict(glm_FSp_acad, newdata = pred_grid_acad, se.fit = TRUE, re.form = NA)

pred_grid_acad$estimate  <- plogis(preds$fit)
pred_grid_acad$conf.low  <- plogis(preds$fit - 1.96 * preds$se.fit)
pred_grid_acad$conf.high <- plogis(preds$fit + 1.96 * preds$se.fit)

ggplot(pred_grid_acad, aes(x = FSp_std_beta, y = estimate)) +
  # Confidence Ribbon
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "steelblue", alpha = 0.2) +
  # Main Prediction Line
  geom_line(color = "steelblue", linewidth = 1.2) +
  # Raw data distribution
  geom_rug(data = amphib_interest2, aes(x = FSp_std_beta), inherit.aes = FALSE, alpha = 0.1) +
  labs(
    x = "Functional Specialization (Standardized)",
    y = "Predicted Academic Interest (0-1)"
  ) +
  theme_classic()


period_means = amphib_interest2 %>%
  group_by(period) %>%
  summarise(year_description = mean(year_description, na.rm = TRUE))

pred_grid_periods <- expand.grid(
  FSp_std_beta = seq(min(amphib_interest2$FSp_std_beta, na.rm = TRUE), 
                     max(amphib_interest2$FSp_std_beta, na.rm = TRUE), 
                     length.out = 100),
  period = levels(amphib_interest2$period)
) %>%
  left_join(period_means, by = "period")

preds_p <- predict(glm_FSp_acad, newdata = pred_grid_periods, se.fit = TRUE, re.form = NA)

pred_grid_periods$estimate <- plogis(preds_p$fit)
pred_grid_periods$conf.low <- plogis(preds_p$fit - 1.96 * preds_p$se.fit)
pred_grid_periods$conf.high <- plogis(preds_p$fit + 1.96 * preds_p$se.fit)

ggplot(pred_grid_periods, aes(x = FSp_std_beta, y = estimate, color = period, fill = period)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, color = NA) +
  geom_line(linewidth = 1.2) +
  labs(
    x = "Functional Specialization",
    y = "Predicted Academic Interest",
    color = "Period of description",
    fill = "Period of description"
  ) +
  theme_classic(base_size = 14) +
  scale_color_viridis_d(option = "F", direction = 1) +
  scale_fill_viridis_d(option = "F", direction = 1) +
  theme(legend.position = c(0.2,0.82))
#Higher uniqueness consistently drives interest, regardless of age

ggsave("results/FSp_acad_glm.jpeg")

glm_FSp_publ = glmmTMB(publ_beta ~ FSp_std_beta + year_description +  (1|realm), family = beta_family(),
                       amphib_interest2)
summary(glm_FSp_publ)



pred_grid_pub <- expand.grid(
  FSp_std_beta = seq(min(amphib_interest2$FSp_std_beta, na.rm = TRUE), 
                     max(amphib_interest2$FSp_std_beta, na.rm = TRUE), 
                     length.out = 100),
  period = unique(amphib_interest2$period)
) %>%
  left_join(period_means, by = "period")

# 4. Predict using native glmmTMB logic
preds_pub <- predict(glm_FSp_publ, newdata = pred_grid_pub, se.fit = TRUE, re.form = NA)

pred_grid_pub$estimate <- plogis(preds_pub$fit)
pred_grid_pub$conf.low <- plogis(preds_pub$fit - 1.96 * preds_pub$se.fit)
pred_grid_pub$conf.high <- plogis(preds_pub$fit + 1.96 * preds_pub$se.fit)

# 5. Plot
ggplot(pred_grid_pub, aes(x = FSp_std_beta, y = estimate, color = period, fill = period)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, color = NA) +
  geom_line(linewidth = 1.2) +
  labs(
    x = "Functional Specialization",
    y = "Predicted Public Interest"
  ) +
  theme_classic(base_size = 14) +
  scale_color_viridis_d(option = "D", direction = -1)+
  scale_fill_viridis_d(option = "D", direction = -1) +
  theme(legend.position = c(0.2,0.82))

# "The public's interest is dictated by time, not 'weirdness'"

ggsave("results/FSp_publ_glm.jpeg")

#### phylogenetic diversity #####




##### tadpole traits #####
library(xlsx)

amphibians_tax3 = read.csv("amphib_names.txt", sep = "\t") %>% 
  dplyr::mutate(Species = paste0(genus," ", species),
                authority = gsub(")*", "",spauthor),
                year_description = str_sub(authority, start= -5),
                year_description = as.numeric(gsub(",", "",year_description)),
                year_description = if_else(is.na(year_description), year_described, year_description))

tadpoles1 = openxlsx::read.xlsx("Tadpole data 2025.4.21.xlsx", 2) %>%  # from literature
  dplyr::select(Species, Stage, TL, BL, BMH, BMW, TaL, MTH, TMH, TMW, DFH, VFH, OD)%>% 
  mutate_at(c("TL", "BL", "BMH", "BMW", "TaL", "MTH", "TMH", "TMW", "DFH", "VFH", "OD"), as.numeric)
tadpoles2 = openxlsx::read.xlsx("Tadpole data 2025.4.21.xlsx", 3) %>%  #from measurements
  dplyr::select(Species, Stage, TL, BL, BMH, BMW, TaL, MTH, TMH, TMW, DFH, VFH, OD) %>% 
  mutate_at(c("TL", "BL", "BMH", "BMW", "TaL", "MTH", "TMH", "TMW", "DFH", "VFH", "OD"), as.numeric) %>%   
  bind_rows(tadpoles1) %>%
  dplyr::filter(rowSums(is.na(.)) < 11) %>% 
  group_by(Species) %>% 
  slice_max(Stage, n = 1) %>%  # selecting the most advanced gosner stage
  group_by(Species) %>% 
  summarise(TL = mean(TL),
            BL = mean(BL),
            BMH = mean(BMH),
            BMW = mean(BMW),
            TaL = mean(TaL),
            MTH = mean(MTH),
            TMH = mean(TMH),
            TMW = mean(TMW),
            DFH = mean(DFH),
            VFH = mean(VFH),
            OD = mean(OD)
  ) %>% 
  dplyr::left_join(amphibians_tax3) %>% 
  dplyr::select(-13:-31) %>% 
  dplyr::rename(scientificName = Species) %>% 
  drop_na(year_description)

intersect(tadpoles2$scientificName, amphibians_tax$scientificName)


tadpoles_weight = tadpoles2 %>% 
  dplyr::mutate(period = case_when(year_description <= 1850 ~ "p_1850",
                                   year_description > 1850 & year_description <= 1900 ~ "p_1900",
                                   year_description > 1900 & year_description <= 1950 ~ "p_1950",
                                   year_description > 1950 & year_description <= 1975 ~ "p_1975",
                                   year_description > 1975 & year_description <= 2000 ~ "p_2000",
                                   year_description > 2000 & year_description <= 2025 ~ "p_2020"
  )) %>% 
  dplyr::mutate(present = 1
  ) %>% 
  tidyr::pivot_wider(names_from =  scientificName,       
                     values_from = present,    
                     id_cols = period, 
                     values_fill = 0      
  ) %>%
  remove_rownames() %>% textshape::column_to_rownames("period") %>% 
  as.matrix()

tadpoles_morph3 = tadpoles2

tadpoles_morph3 = tadpoles_morph3 %>% 
  dplyr::select(-1, -13) %>% as.data.frame()


row.names(tadpoles_morph3) = tadpoles2$scientificName

tadpoles_morph3_traits = colnames(tadpoles_morph3) %>% 
  as.data.frame() %>% 
  dplyr::rename(trait_name = 1) %>% 
  dplyr::mutate(trait_type = "Q")


tadpoles_traits_summ = mFD::sp.tr.summary(
  tr_cat     = tadpoles_morph3_traits,   
  sp_tr      = tadpoles_morph3, 
  stop_if_NA = F)



sp_dist_tadpoles <- mFD::funct.dist(
  sp_tr         = tadpoles_morph3,
  tr_cat        = tadpoles_morph3_traits,
  metric        = "gower",
  scale_euclid  = "scale_center",
  ordinal_var   = "classic",
  weight_type   = "equal",
  stop_if_NA    = F)

fspaces_quality_tadpoles = mFD::quality.fspaces(
  sp_dist             = sp_dist_tadpoles,
  maxdim_pcoa         = 4,
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE,
  fdendro             = "average")


sp_faxes_coord_tadpoles = fspaces_quality_tadpoles$"details_fspaces"$"sp_pc_coord" %>% 
  as.matrix()


class(sp_faxes_coord_tadpoles)

sp_faxes_coord_tadpoles_all = sp_faxes_coord_tadpoles %>% 
  as.data.frame() %>% 
  dplyr::mutate(scientificName = rownames(.)) %>% 
  dplyr::left_join(tadpoles2)

del = sp_faxes_coord_tadpoles = fspaces_quality_tadpoles$"details_fspaces"$"sp_pc_coord"  %>% 
  as.data.frame()


big_plot_tadpoles = mFD::funct.space.plot(
  sp_faxes_coord  = sp_faxes_coord_tadpoles[ , c("PC1", "PC2", "PC3", "PC4")],
  faxes           = c("PC1", "PC2", "PC3", "PC4"),
  name_file       = NULL,
  faxes_nm        = NULL,
  range_faxes     = c(NA, NA),
  color_bg        = "grey99",
  color_pool      = "black",
  fill_pool       = "green4",
  shape_pool      = 21,
  size_pool       = 1.6,
  plot_ch         = TRUE,
  color_ch        = "black",
  fill_ch         = "white",
  alpha_ch        = 0.5,
  plot_vertices   = F, #change to plot vertices
  color_vert      = "blueviolet",
  fill_vert       = "blueviolet",
  shape_vert      = 23,
  size_vert       = 1,
  plot_sp_nm      = NULL,
  nm_size         = 3,
  nm_color        = "black",
  nm_fontface     = "plain",
  check_input     = TRUE)

big_plot_tadpoles[1]


xalpha_fd_indices_tadpoles = mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_tadpoles[ , c("PC1", "PC2")],
  asb_sp_w         = tadpoles_weight,
  ind_vect         = c("fdis", "feve", "fric", "fdiv"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

alpha_fd_indices_tadpoles$functional_diversity_indices


col_asb1 = "darkblue"
col_asb2 = "#DCE319FF"

plots_alpha = mFD::alpha.multidim.plot(
  output_alpha_fd_multidim = alpha_fd_indices_tadpoles,
  plot_asb_nm              = c("p_1950", "p_2020"),
  ind_nm                   = c( "fric", 
                                "fdiv"),
  faxes                    = NULL,
  faxes_nm                 = NULL,
  range_faxes              = c(NA, NA),
  color_bg                 = "white",
  shape_sp                 = c(pool = 3, asb1 = 21, asb2 = 21),
  size_sp                  = c(pool = 0.7, asb1 = .7, asb2 = .2),
  color_sp                 = c(pool = "grey50", asb1 = col_asb1, asb2 = "black"),
  color_vert               = c(pool = "grey50", asb1 = col_asb1, asb2 = "black"),
  fill_sp                  = c(pool = NA, asb1 = col_asb1, asb2 = col_asb2),
  fill_vert                = c(pool = NA, asb1 = col_asb1, asb2 = col_asb2),
  color_ch                 = c(pool = NA, asb1 = col_asb1, asb2 = col_asb2),
  fill_ch                  = c(pool = "grey86", asb1 = col_asb1, asb2 = col_asb2),
  alpha_ch                 = c(pool = 1, asb1 = 0.4, asb2 = 0.4),
  shape_centroid_fdis      = c(asb1 = 22,  asb2 = 24),
  shape_centroid_fdiv      = c(asb1 = 22,  asb2 = 24),
  shape_centroid_fspe      = 23,
  color_centroid_fspe      = "black",
  size_sp_nm               = 5, 
  color_sp_nm              = "black",
  plot_sp_nm               = NULL,
  fontface_sp_nm           = "normal",
  save_file                = FALSE,
  check_input              = TRUE) 

plots_alpha$"fric"$"patchwork"


alpha_fd_indices_tadpoles2 = alpha_fd_indices_tadpoles$functional_diversity_indices %>% 
  as.data.frame() %>% 
  dplyr::mutate(period = rownames(alpha_fd_indices_tadpoles$functional_diversity_indices)) %>% 
  dplyr::mutate(year = as.integer(gsub("p_", "", period))) %>%
  dplyr::arrange(year) 

ggplot(alpha_fd_indices_tadpoles2, aes(x = year, y = fric)) +
  geom_line(color = "#D55E00", linewidth = 1.2) +
  geom_point(color = "#D55E00", size = 4) +
  geom_text(aes(label = round(fric, 3)), vjust = -1.5, size = 3.5) +
  labs(
    x = "Time Period (Year)",
    y = "Functional Richness"
  ) +
  scale_x_continuous(breaks = unique(alpha_fd_indices_tadpoles_cum$year)) + 
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ylim(c(0.4,1))


##### tadpoles cumulative Fdiversity ####

tadpoles_tax_weights_cum = tadpoles_weight %>% 
  dplyr::as_data_frame() %>% 
  dplyr::mutate(period = rownames(tadpoles_weight)) %>% 
  dplyr::relocate(period) %>% 
  dplyr::mutate(year = as.integer(gsub("p_", "", period))) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(across(-c(period, year), cummax)) %>% 
  dplyr::select(-year)

head(tadpoles_tax_weights_cum[,1:10])

tadpoles_tax_weights_cum2 = tadpoles_tax_weights_cum%>%
  remove_rownames() %>% column_to_rownames("period") %>% 
  as.matrix()


alpha_fd_indices_tadpoles = mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_tadpoles[ , c("PC1", "PC2")],
  asb_sp_w         = tadpoles_tax_weights_cum2,
  ind_vect         = c("fdis", "feve", "fric", "fdiv"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

str(alpha_fd_indices_tadpoles)


# write_rds(alpha_fd_indices_tadpoles, "alpha_fd_indices_tadpoles.RDS")


alpha_fd_indices_tadpoles_cum = alpha_fd_indices_tadpoles$functional_diversity_indices %>% 
  as.data.frame() %>% 
  dplyr::mutate(period = rownames(alpha_fd_indices_tadpoles$functional_diversity_indices)) %>% 
  dplyr::mutate(year = as.integer(gsub("p_", "", period))) %>%
  dplyr::arrange(year) %>% 
  dplyr::mutate(year = as.numeric(gsub(2020, 2024, year)))

ggplot(alpha_fd_indices_tadpoles_cum, aes(x = year, y = fric)) +
  geom_line(data=(alpha_fd_indices_amphibians_cum), aes(x=year, y=fric), color='grey30', size = 1.3, alpha = .85) + 
  geom_point(data=(alpha_fd_indices_amphibians_cum), aes(x=year, y=fric), color = "darkblue", size = 4.8, alpha = .6) +
  geom_text(data = alpha_fd_indices_amphibians_cum,
            aes(label = round(sp_richn, 3)), vjust = -1.5, hjust = .4, size = 3.5) +
  geom_line(color = "black", linewidth = .8, linetype = 2) +
  geom_point(color = "#D55E00", size = 2.9) +
  geom_text(aes(label = round(sp_richn, 3)), vjust = 1.9, hjust = -.1, size = 3.5) +
  labs(
    x = "Time Period (Year)",
    y = "Cumulative Functional Richness"
  ) +
  scale_x_continuous(breaks = unique(alpha_fd_indices_tadpoles_cum$year)) + 
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.y = element_text(size = 17),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ylim(c(0.1,1.01)) + xlim(c(1850,2025))

alpha_fd_indices_amphibians_df = as.data.frame(alpha_fd_indices_amphibians)
class(alpha_fd_indices_amphibians)


#### creating null models ####
period_col = tadpoles_tax_weights_cum$period
species_data = tadpoles_tax_weights_cum %>% dplyr::select(-period)

original_species_names = colnames(species_data)

shuffled_names = sample(original_species_names)

amphibians_matrix_null = species_data
colnames(amphibians_matrix_null) = shuffled_names
amphibians_matrix_null = amphibians_matrix_null %>%
  tibble::add_column(period = period_col, .before = 1)


amphibians_matrix_null2 = amphibians_matrix_null%>%
  remove_rownames() %>% column_to_rownames("period") %>% 
  as.matrix()


alpha_fd_indices_tadpoles_null = mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_tadpoles[ , c("PC1", "PC2")],
  asb_sp_w         = amphibians_matrix_null2,
  ind_vect         = c("fdis", "feve", "fric", "fdiv"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

alpha_fd_indices_tadpoles_null_cum = alpha_fd_indices_tadpoles_null$functional_diversity_indices %>% 
  as.data.frame() %>% 
  dplyr::mutate(period = rownames(alpha_fd_indices_tadpoles_null$functional_diversity_indices)) %>% 
  dplyr::mutate(year = as.integer(gsub("p_", "", period))) %>%
  dplyr::arrange(year) %>% 
  dplyr::mutate(year = as.numeric(gsub(2020, 2024, year)))

ggplot(alpha_fd_indices_tadpoles_cum, aes(x = year, y = fric)) +
  geom_line(data=(alpha_fd_indices_tadpoles_null_cum), aes(x=year, y=fric), color='grey30', size = 1.3, alpha = .85) + 
  geom_point(data=(alpha_fd_indices_tadpoles_null_cum), aes(x=year, y=fric), color = "darkblue", size = 4.8, alpha = .6) +
  geom_line(color = "black", linewidth = .8, linetype = 2) +
  geom_point(color = "#D55E00", size = 2.9) +
  geom_text(aes(label = round(sp_richn, 3)), vjust = 1.9, hjust = -.1, size = 3.5) +
  labs(
    x = "Time Period (Year)",
    y = "Cumulative Functional Richness"
  ) +
  scale_x_continuous(breaks = unique(alpha_fd_indices_tadpoles_cum$year)) + 
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.y = element_text(size = 17),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ylim(c(0.1,1.01)) + xlim(c(1850,2025))


N_ITERATIONS <- 999
null_fric_results <- list()

# Define the set of column names to be shuffled
original_species_names <- colnames(tadpoles_tax_weights_cum) %>% 
  # Exclude the row identifier 'period'
  setdiff("period")

period_col <- tadpoles_tax_weights_cum$period
species_data_matrix <- tadpoles_tax_weights_cum %>% dplyr::select(-period) # Data without the 'period' column

for (i in 1:N_ITERATIONS) {
  
  # 1. Randomize the species names
  shuffled_names <- sample(original_species_names)
  
  # 2. Create the null matrix by assigning shuffled names to the data
  amphibians_matrix_null <- species_data_matrix
  colnames(amphibians_matrix_null) <- shuffled_names
  
  # 3. Re-attach the period column and convert to tibble for mFD compatibility
  amphibians_matrix_null <- amphibians_matrix_null %>%
    tibble::add_column(period = period_col, .before = 1) %>%
    # Convert period column to row names for mFD input
    tibble::column_to_rownames("period") %>% 
    as.matrix()
  
  
  # 4. RUN mFD: Pass the randomized matrix as the assemblage data (asb_sp_w)
  alpha_fd_null = mFD::alpha.fd.multidim(
    sp_faxes_coord = sp_faxes_coord_tadpoles[, c("PC1", "PC2")],
    asb_sp_w       = amphibians_matrix_null,
    ind_vect       = c("fric"),
    scaling        = TRUE,
    check_input    = TRUE
  )
  
  # 5. STORE RESULTS: Extract the FRic values and store them in the list
  null_fric_results[[i]] <- alpha_fd_null$functional_diversity_indices %>%
    tibble::rownames_to_column(var = "asb_nm") %>%
    # 2. Select only the assemblage names and the FRic values (the 'fric' column)
    dplyr::select(asb_nm, value = fric) %>%
    # 3. Rename the 'value' column uniquely for this iteration
    dplyr::rename(!!paste0("fric_", i) := value)
}

# 6. COMBINE RESULTS: Convert the list of dataframes into a single dataframe
null_fric_df <- purrr::reduce(null_fric_results, dplyr::full_join, by = "asb_nm")

null_tidy = null_fric_df %>%
  # Gather all the iteration columns into two: a key (fric_1, fric_2, etc.) and a value (the FRic number)
  tidyr::pivot_longer(
    cols = starts_with("fric_"),
    names_to = "iteration",
    values_to = "fric"
  ) %>%
  # Extract the year from the assemblage name (e.g., "p_1900" -> 1900)
  dplyr::mutate(
    period = as.numeric(gsub("p_", "", asb_nm))
  )


null_envelope = null_tidy %>%
  dplyr::group_by(period) %>%
  dplyr::summarise(
    # Lower bound (2.5th percentile)
    lower_95 = quantile(fric, 0.025, na.rm = TRUE),
    # Upper bound (97.5th percentile)
    upper_95 = quantile(fric, 0.975, na.rm = TRUE),
    # Mean null curve (for reference)
    mean_null = mean(fric, na.rm = TRUE)
  ) %>%
  dplyr::ungroup()


observed_fric = alpha_fd_indices_tadpoles_cum %>%
  dplyr::mutate(
    period = as.numeric(gsub("p_", "", year))
  ) %>% 
  dplyr::select(fric, period) 


ggplot(observed_fric, aes(x = period, y = fric)) +
  
  # 1. Null Envelope (Shaded Area)
  geom_ribbon(
    data = null_envelope,
    aes(x = period, ymin = lower_95, ymax = upper_95),
    fill = "gray80",
    alpha = 0.2,
    inherit.aes = FALSE # Prevents inheriting the observed aesthetic
  ) +
  # 3. Observed Curve (Solid Line and Points)
  geom_line(color = "black", linewidth = 1) + #,    linetype = "dashed",
  geom_point(color = "blue", size = 3) +
  
  
  # 2. Mean Null Curve (Dashed Line)
  geom_line(
    data = null_envelope,
    aes(x = period, y = mean_null),
    color = "gray30",
    alpha = .3,
    inherit.aes = FALSE
  ) +
  
  
  # 4. Labels and Theme (rest of the code is correct)
  labs(
    x = "Time Period (Year)",
    y = "Cumulative Functional Richness"
  ) +
  scale_x_continuous(breaks = unique(observed_fric$period)) +
  theme_classic(base_size = 16)

