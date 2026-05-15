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

### TESTAR CORRELACAO ECO x MORPHO


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

# write_rds(sp_dist_amphibians, "sp_dist_amphibians_14052026_filtered.RDS") # _filtered if species with only SVL were excluded

# sp_dist_amphibians = read_rds("sp_dist_amphibians_14052026_filtered.RDS")


fspaces_quality_amphibians = mFD::quality.fspaces(
  sp_dist             = sp_dist_amphibians,
  maxdim_pcoa         = 4,
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

eigenvals = fspaces_quality_amphibians$details_fspaces$pc_eigenvalues$Eigenvalues

variance_pct = (eigenvals / sum(eigenvals)) * 100
print(round(variance_pct[1:4], 2))
sum(variance_pct$Rel_corr_eig)

pc1_var <- round(variance_pct[1], 2)
pc2_var <- round(variance_pct[2], 2)

cat("PC1 explains", pc1_var, "% of the variance.\n")
cat("PC2 explains", pc2_var, "% of the variance.")

# write.csv(sp_faxes_coord_amphibians_all, "sp_faxes_coord_amphibians_all_13052026_filtered.csv")
sp_faxes_coord_amphibians_all = read.csv("sp_faxes_coord_amphibians_all_13052026_filtered.csv")

sp_faxes_coord_amphibians0 = read.csv("sp_faxes_coord_amphibians_all_13052026_filtered.csv") %>% 
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
  sp_faxes_coord   = sp_faxes_coord_amphibians[ , c("PC1", "PC2")],
  asb_sp_w         = amphibians_tax_weights,
  ind_vect         = c("fric"), #"fdis", "feve", "fric", "fdiv"
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

alpha_fd_indices_amphibians$functional_diversity_indices

trait_axes_corr = mFD::traits.faxes.cor(
  sp_faxes_coord = sp_faxes_coord_amphibians[, c("PC1", "PC2")], 
  sp_tr          = amphibians_morph3_imp2             # Your raw trait data
)

str(trait_axes_corr)



col_asb1 = "orange3"
col_asb2 = "#DCE319FF"

plots_alpha = mFD::alpha.multidim.plot(
  output_alpha_fd_multidim = alpha_fd_indices_amphibians,
  plot_asb_nm              = c("p_2000", "p_2020"),
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


ggsave("fric_year_SuppMat.jpeg")

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

amphibians_tax_weights_cum_del = amphibians_tax_weights_cum %>% 
  dplyr::filter(period == "p_1850")

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


# write_rds(alpha_fd_indices_amphibians, "alpha_fd_indices_amphibians.RDS")

alpha_fd_indices_amphibians = readRDS("alpha_fd_indices_amphibians.RDS")

alpha_fd_indices_amphibians_cum = alpha_fd_indices_amphibians$functional_diversity_indices %>% 
  as.data.frame() %>% 
  dplyr::mutate(period = rownames(alpha_fd_indices_amphibians$functional_diversity_indices)) %>% 
  dplyr::mutate(year = as.integer(gsub("p_", "", period))) %>%
  dplyr::arrange(year) 

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
n_iter = 999 #same with 999

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

ggplot() +
  # The points (all species)
  geom_point(data = sp_faxes_coord_amphibians0, 
             aes(x = PC1, y = PC2),
             color = "black", 
             size = 1, 
             alpha = 0.4) +
  
  # The single global polygon (outline only)
  geom_polygon(data = global_hull_data, 
               aes(x = PC1, y = PC2), 
               fill = NA, 
               color = "#2C3E50", # Using a neutral dark slate color
               linewidth = 1.2) +
  
  # Update labels with your Rel_corr_eig percentages
  labs(
    x = "PCA1",
    y = "PCA2"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "none", # Remove legend since there are no periods
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

ggsave("results/Fig_all_funspace.jpeg", width = 9, height = 9.5)


#general funspace
ggplot() +
  # Background: All species in light grey to show the potential total space
  
  geom_point(data = sp_faxes_coord_amphibians0, aes(x = PC1, y = PC2),
             color = "black", size = 2, alpha = 0.9) +
  geom_polygon(data = sp_faxes_coord_amphibians0, aes(x = PC1, y = PC2), fill = NA,
               alpha = 0.3, linewidth = 1.8) 

#cum funspace
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
    legend.position = "right",
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) 

ggsave("results/Fig_cumulative_funspace.jpeg", width = 9, height = 9.5)

##### spatial patterns ####

table(amphibians_tax$realm)

realm_colors = c(
  "Palearctic"   = "indianred",
  "Neotropical"  = "plum3",
  "Afrotropical" = "royalblue2",
  "Indomalayan"  = "tan",
  "Australasian" = "orange2"
)

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
  
  # 8. Plot and Save
  p = ggplot(observed_fric_realm, aes(x = period, y = fric)) +
    geom_ribbon(data = null_envelope_realm, aes(x = period, ymin = lower_95, ymax = upper_95), 
                fill = realm_colors[realm_name], alpha = 0.2,
                inherit.aes = FALSE) +
    geom_line(color = "black", linewidth = 1) +
    geom_point(color = realm_colors[realm_name], size = 3) +
    geom_line(data = null_envelope_realm, aes(y = mean_null), linetype = "dotted") +
    labs(title = "", x = "Year", y = "Functional Richness") +
    theme_classic(base_size = 18) +
    ylim(c(0,1))
  
  ggsave(paste0("results/cum_fric_", realm_name, ".jpeg"), plot = p)
}

#### automatic loop end ####



realm_name = "Palearctic"

(species_realm=  amphibians_tax %>%
  dplyr::filter(grepl(realm_name, realm)) %>%
  pull(scientificName))


amphibians_morph3_realm = amphibians_morph3_imp2 %>% 
  rownames_to_column("Species") %>% 
  dplyr::left_join(amphibians_tax %>% 
                     dplyr::rename(Species = scientificName) %>% 
                     dplyr::select(Species, realm)) %>% 
  dplyr::filter(grepl(realm_name, realm)) %>% 
  dplyr::select(-realm) %>% 
  column_to_rownames("Species") 

amphibians_morph3_traits_realm = colnames(amphibians_morph3_realm) %>% 
  as.data.frame() %>% 
  dplyr::rename(trait_name = 1) %>% 
  dplyr::mutate(trait_type = "Q")%>% 
  dplyr::mutate(trait_weight = case_when(trait_name %in% c("UEW", "Fin1L", "Fin2L", "Fin3DW") ~ 0.5,TRUE ~1))


amphibians_traits_summ_realm <- mFD::sp.tr.summary(
  tr_cat     = amphibians_morph3_traits_realm,   
  sp_tr      = amphibians_morph3_realm, 
  stop_if_NA = F)



sp_dist_amphibians_realm <- mFD::funct.dist(
  sp_tr         = amphibians_morph3_realm,
  tr_cat        = amphibians_morph3_traits_realm,
  metric        = "gower",
  scale_euclid  = "scale_center",
  ordinal_var   = "classic",
  weight_type   = "user",
  stop_if_NA    = F)

fspaces_quality_amphibians_realm = mFD::quality.fspaces(
  sp_dist             = sp_dist_amphibians_realm,
  maxdim_pcoa         = 4,
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE,
  fdendro             = "average")


sp_faxes_coord_amphibians_realm = fspaces_quality_amphibians_realm$"details_fspaces"$"sp_pc_coord" 

sp_faxes_coord_amphibians_realm_df = sp_faxes_coord_amphibians_realm %>% 
  as.data.frame()


mFD::funct.space.plot(
  sp_faxes_coord  = sp_faxes_coord_amphibians_realm[ , c("PC1", "PC2", "PC3", "PC4")],
  faxes           = c("PC1", "PC2", "PC3", "PC4"),
  name_file       = NULL,
  faxes_nm        = NULL,
  range_faxes     = c(NA, NA),
  color_bg        = "grey99",
  color_pool      = "black",
  fill_pool       = "steelblue3",
  shape_pool      = 21,
  size_pool       = 2,
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
  check_input     = TRUE)[1]

ggsave("results/funspace_Palearctic.jpeg")


amphibians_tax_weights_realm = amphibians_tax %>% 
  dplyr::filter(grepl(realm_name, realm)) %>% 
  dplyr::mutate(present = 1
  ) %>% 
  tidyr::pivot_wider(names_from =  scientificName,       
                     values_from = present,    
                     id_cols = period, 
                     values_fill = 0      
  ) %>%
  remove_rownames() %>%  na.omit(period) %>%  column_to_rownames("period") %>% 
  dplyr::mutate(period = rownames(.)) %>% 
  dplyr::relocate(period) %>% 
  dplyr::mutate(year = as.integer(gsub("p_", "", period))) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(across(-c(period, year), cummax)) %>% 
  dplyr::select(-year, -period) %>% 
  as.matrix()


alpha_fd_indices_amphibians_realm = mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_amphibians_realm[ , c("PC1", "PC2")],
  asb_sp_w         = amphibians_tax_weights_realm,
  ind_vect         = c( "fric"),
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

alpha_fd_indices_amphibians_realm$functional_diversity_indices

alpha_fd_indices_amphibians_cum_realm = alpha_fd_indices_amphibians_realm$functional_diversity_indices %>% 
  as.data.frame() %>% 
  dplyr::mutate(period = rownames(alpha_fd_indices_amphibians_realm$functional_diversity_indices)) %>% 
  dplyr::mutate(year = as.integer(gsub("p_", "", period))) %>%
  dplyr::arrange(year) 

ggplot(alpha_fd_indices_amphibians_cum_realm, aes(x = year, y = fric)) +
  geom_line(color = "black", linewidth = 1.2) +
  geom_point(color = "#D55E00", size = 4) +
  geom_text(aes(label = round(sp_richn, 3)), vjust = -1.5, size = 3.5) +
  labs(
    x = "Time Period (Year)",
    y = "Cumulative Functional Richness"
  ) +
  scale_x_continuous(breaks = unique(alpha_fd_indices_amphibians_cum_realm$year)) + 
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.y = element_text(size = 17),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ylim(c(0.00,1.03))

# ggsave("results/cum_fric_Indomalayan.jpeg")


### null models - spatial ####

N_ITERATIONS <- 99
null_fric_results_realm <- list()


amphibians_tax_weights_realm2 = amphibians_tax_weights_realm %>% 
  as.data.frame() %>% 
  rownames_to_column("period")

original_species_names_amph_realm = colnames(amphibians_tax_weights_realm2) %>% 
  setdiff("period")

period_col_realm = amphibians_tax_weights_realm2$period
species_data_matrix_realm <- amphibians_tax_weights_realm2 %>% dplyr::select(-period) 

for (i in 1:N_ITERATIONS) {
  
  # 1. Randomize the species names
  shuffled_names_realm <- sample(original_species_names_amph_realm)
  
  # 2. Create the null matrix by assigning shuffled names to the data
  amphibians_matrix_null_realm <- species_data_matrix_realm
  colnames(amphibians_matrix_null_realm) <- shuffled_names_realm
  
  # 3. Re-attach the period column and convert to tibble for mFD compatibility
  amphibians_matrix_null_realm <- amphibians_matrix_null_realm %>%
    tibble::add_column(period = period_col_realm, .before = 1) %>%
    # Convert period column to row names for mFD input
    tibble::column_to_rownames("period") %>% 
    as.matrix()
  
  
  # 4. RUN mFD: Pass the randomized matrix as the assemblage data (asb_sp_w)
  alpha_fd_null_realm = mFD::alpha.fd.multidim(
    sp_faxes_coord = sp_faxes_coord_amphibians_realm[, c("PC1", "PC2")],
    asb_sp_w       = amphibians_matrix_null_realm,
    ind_vect       = c("fric"),
    scaling        = TRUE,
    check_input    = TRUE
  )
  
  # 5. STORE RESULTS: Extract the FRic values and store them in the list
  null_fric_results_realm[[i]] <- alpha_fd_null_realm$functional_diversity_indices %>%
    tibble::rownames_to_column(var = "asb_nm") %>%
    # 2. Select only the assemblage names and the FRic values (the 'fric' column)
    dplyr::select(asb_nm, value = fric) %>%
    # 3. Rename the 'value' column uniquely for this iteration
    dplyr::rename(!!paste0("fric_", i) := value)
}

# 6. COMBINE RESULTS: Convert the list of dataframes into a single dataframe

null_fric_df_realm <- purrr::reduce(null_fric_results_realm, dplyr::full_join, by = "asb_nm")

null_tidy_realm = null_fric_df_realm %>%
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


null_envelope_realm = null_tidy_realm %>%
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


observed_fric_realm = alpha_fd_indices_amphibians_cum_realm %>%
  dplyr::mutate(
    period = as.numeric(gsub("p_", "", year))
  ) %>% 
  dplyr::select(fric, period) 


ggplot(observed_fric_realm, aes(x = period, y = fric)) +
  
  # 1. Null Envelope (Shaded Area)
  geom_ribbon(
    data = null_envelope_realm,
    aes(x = period, ymin = lower_95, ymax = upper_95),
    fill = "burlywood2",
    alpha = 0.3,
    inherit.aes = FALSE # Prevents inheriting the observed aesthetic
  ) +
  # 3. Observed Curve (Solid Line and Points)
  geom_line(color = "black", linewidth = 1, alpha = .8) +
  geom_point(color = "darkblue", size = 4, alpha = .4) +
  
  
  # 2. Mean Null Curve (Dashed Line)
  geom_line(
    data = null_envelope_realm,
    aes(x = period, y = mean_null),
    color = "burlywood4",
    alpha = .9,
    inherit.aes = FALSE,
    linetype="dotted"
  ) +
  
  
  # 4. Labels and Theme (rest of the code is correct)
  labs(
    x = "Time Period (Year)",
    y = "Cumulative Functional Richness"
  ) +
  scale_x_continuous(breaks = unique(observed_fric_realm$period)) +
  theme_classic(base_size = 16)

ggsave("results/cum_fric_Indomalayan.jpeg")



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


sp_dist_amphibio_eco <- mFD::funct.dist(
  sp_tr         = amphibio_eco,
  tr_cat        = amphibio3_traits,
  metric        = "gower",
  scale_euclid  = "scale_center",
  ordinal_var   = "classic",
  weight_type   = "equal",
  stop_if_NA    = F)

fspaces_quality_amphibio_eco = mFD::quality.fspaces(
  sp_dist             = sp_dist_amphibio_eco,
  maxdim_pcoa         = 10,
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE,
  fdendro             = "average")


sp_faxes_coord_amphibio_eco = fspaces_quality_amphibio_eco$"details_fspaces"$"sp_pc_coord" 


sp_faxes_coord_amxphibio_eco_all = sp_faxes_coord_amphibio_eco %>% 
  as.data.frame() %>% 
  dplyr::mutate(scientificName = rownames(.)) %>% 
  dplyr::inner_join(amphibio_eco %>% 
                     dplyr::mutate(scientificName = rownames(.))) 

sp_faxes_coord_amphibio_eco2 = sp_faxes_coord_amphibio_eco_all %>% 
  dplyr::select(1:11) %>% 
  column_to_rownames("scientificName") %>% 
  as.matrix()

big_plot = mFD::funct.space.plot(
  sp_faxes_coord  = sp_faxes_coord_amphibio_eco2[ , c("PC1", "PC2", "PC3", "PC4")],
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

(big_plot[[1]])

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


### common species - 645 species
amphibio2 = amphibians_eco_imp2_df %>% 
  dplyr::inner_join(amphibians_morph3_imp2_df)  %>% 
  dplyr::inner_join(amphibians_tax %>% 
                      dplyr::rename(Species = scientificName) %>% 
                      dplyr::select(Species, realm, period)) # only 645 species common to morphological and ecological databases


### doing so for the same 645 species

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
            aes(x = year, y = fric_scaled, color = "Ecological Traits"), linewidth = 1.2) +
  geom_point(data = alpha_fd_indices_amphibio_eco2_redux, 
             aes(x = year, y = fric_scaled, color = "Ecological Traits"), size = 4, alpha = .5) +
  # geom_text(data = alpha_fd_indices_amphibio_eco2_redux,
  #           aes(x = year, y = fric_scaled, label = round(sp_richn, 3)), 
  #           vjust = 2, size = 3.5, color = "olivedrab") +
  
  # 2. Plot Morphological Traits (Black)
  geom_line(data = alpha_fd_indices_amphibio_morph_redux, 
            aes(x = year, y = fric_scaled, color = "Morphological Traits"), linewidth = 1.2) +
  geom_point(data = alpha_fd_indices_amphibio_morph_redux, 
             aes(x = year, y = fric_scaled, color = "Morphological Traits"), size = 4, alpha = .5) +
  geom_text(data = alpha_fd_indices_amphibio_morph_redux, 
            aes(x = year, y = fric_scaled, label = round(sp_richn, 3)), 
            vjust = -1, size = 3.5, color = "black") +
  
  # 3. Define the Legend Colors Manually
  scale_color_manual(name = "", 
                     values = c("Ecological Traits" = "olivedrab", 
                                "Morphological Traits" = "black")) +
  
  # 4. Formatting
  labs(x = "Time Period (Year)", y = "") +
  ylim(c(0, 1)) +
  theme_classic(base_size = 16) +
  theme(
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none" #c(.8, .2)
  ))


 ggsave("results/cum_fric_ecological_morphological_redux.jpeg")
 

 (ecological_morphological = ggarrange(
   eco_morpho_all,  eco_morpho_redux,
   ncol = 2, nrow = 1, 
   labels = c("A", "B"),  
   widths = c(1, 1.1),    
   heights = c(1, 1)
 ))
 
 ggsave("results/Fig_cum_fric_ecological_morphological.jpeg", height = 7, width = 10)


#### uniqueness models ####
## FUSE

fuse_amphibians = mFD::fuse(sp_dist_amphibians,
                            sp_faxes_coord_amphibians[,1:10],
                                      GE = rep(0.001, 3042)) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column("Species")

fuse_amphibians0 = fuse_amphibians %>% 
  dplyr::left_join(amphibians_tax, by = c("Species" = "scientificName")) %>% 
  dplyr::select(Species, FUn_std, FSp_std, realm, year_description)

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

table(fuse_amphibians0$realm)

gam_beta = mgcv::gam(FUn_std_beta ~ s(year_description, by = realm) + 
                       s(realm, bs = "re"), 
                     family = mgcv::betar(link = "logit"), 
                     data = fuse_amphibians0, 
                     method = "REML")


summary(gam_beta)

plot_predictions(gam_beta, condition = c("year_description", "realm")) +
  # Create a separate panel for each realm
  facet_wrap(~realm, scales = "free_y") + 
  # Add the original data points (rug) to each panel
  geom_rug(aes(x = year_description), data = fuse_amphibians0, alpha = 0.2, inherit.aes = FALSE) +
  labs(
    x = "Year of Description",
    y = "Predicted Functional Uniqueness"
  ) +
  theme_classic() +
  theme(strip.background = element_blank(), strip.text = element_text(face = "bold")) +
  coord_cartesian(ylim = c(0, 0.4))

ggsave("results/gam_FUn_year.jpeg", width = 10)


realm_colors = c(
  "Palearctic"   = "indianred",
  "Neotropical"  = "plum3",
  "Afrotropical" = "royalblue2",
  "Indomalayan"  = "tan",
  "Australasian" = "orange2"
)

realm_list = fuse_amphibians0 %>% 
  group_split(realm)


walk(realm_list, function(df) {
  
  current_realm <- as.character(unique(df$realm))
  current_color <- ifelse(current_realm %in% names(realm_colors), 
                          realm_colors[current_realm], 
                          "#333333")
  
  # Generate plot
  p <- plot_predictions(gam_beta, 
                        condition = list("year_description", realm = current_realm)) +
    # Use coord_cartesian to zoom in without discarding data for the model fit
    coord_cartesian(xlim = c(1850, 2020), ylim = c(0, 0.4)) + 
    
    # Ensure rug plot only shows data within your chosen window
    geom_rug(aes(x = year_description), 
             data = df %>% filter(year_description >= 1850), 
             alpha = 0.2, inherit.aes = FALSE) +
    
    scale_fill_manual(values = current_color) +
    scale_color_manual(values = current_color) +
    labs(
      x = "Year of Description",
      y = "Predicted Functional Uniqueness"
    ) +
    theme_classic(base_size = 16) +
    theme(legend.position = "none")
  
  # Save
  file_name <- paste0("results/gam_morphological_fun_", gsub("[| ]", "_", current_realm), ".jpeg")
  ggsave(file_name, p, width = 8, height = 8, dpi = 300)
  
  message("Saved: ", file_name)
})


#functional specialization

fuse_amphibians0$FSp_std_beta = (fuse_amphibians0$FSp_std * (n_fuse - 1) + 0.5) / n_fuse

gam_beta_fsp = mgcv::gam(FSp_std_beta ~ s(year_description, by = realm) + 
                       s(realm, bs = "re"), 
                     family = mgcv::betar(link = "logit"), 
                     data = fuse_amphibians0, 
                     method = "REML")


summary(gam_beta_fsp)

walk(realm_list, function(df) {
  
  current_realm <- as.character(unique(df$realm))
  current_color <- ifelse(current_realm %in% names(realm_colors), 
                          realm_colors[current_realm], 
                          "#333333")
  
  # Generate plot
  p <- plot_predictions(gam_beta_fsp, 
                        condition = list("year_description", realm = current_realm)) +
    # Use coord_cartesian to zoom in without discarding data for the model fit
    coord_cartesian(xlim = c(1850, 2020), ylim = c(0, 0.4)) + 
    
    # Ensure rug plot only shows data within your chosen window
    geom_rug(aes(x = year_description), 
             data = df %>% filter(year_description >= 1850), 
             alpha = 0.2, inherit.aes = FALSE) +
    
    scale_fill_manual(values = current_color) +
    scale_color_manual(values = current_color) +
    labs(
      x = "Year of Description",
      y = "Predicted Functional Specialization"
    ) +
    theme_classic(base_size = 16) +
    theme(legend.position = "none")
  
  # Save
  file_name <- paste0("results/gam_morphological_fsp_", gsub("[| ]", "_", current_realm), ".jpeg")
  ggsave(file_name, p, width = 8, height = 8, dpi = 300)
  
  message("Saved: ", file_name)
})


# 1. Setup: identify the minimum species count (Palearctic = 278)
n_min <- 278
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
      sp_faxes_coord = sp_faxes_coord_amphibians[, c("PC1", "PC2")],
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
        sp_faxes_coord = sp_faxes_coord_amphibians[, c("PC1", "PC2")],
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

ggsave("results/SuppMat_rarefaction_realm278.jpeg")
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

amphibians_tax3 = read.csv("amphib_names.txt", sep = "\t") %>% 
  dplyr::mutate(Species = paste0(genus," ", species),
                authority = gsub(")*", "",spauthor),
                year_description = str_sub(authority, start= -5),
                year_description = as.numeric(gsub(",", "",year_description)),
                year_description = if_else(is.na(year_description), year_described, year_description))

tadpoles1 = read.xlsx("Tadpole data 2025.4.21.xlsx", 2) %>% 
  dplyr::select(Species, Stage, TL, BL, BMH, BMW, TaL, MTH, TMH, TMW, DFH, VFH, OD)%>% 
  mutate_at(c("TL", "BL", "BMH", "BMW", "TaL", "MTH", "TMH", "TMW", "DFH", "VFH", "OD"), as.numeric)
tadpoles2 = read.xlsx("Tadpole data 2025.4.21.xlsx", 3)%>% 
  dplyr::select(Species, Stage, TL, BL, BMH, BMW, TaL, MTH, TMH, TMW, DFH, VFH, OD) %>% 
  mutate_at(c("TL", "BL", "BMH", "BMW", "TaL", "MTH", "TMH", "TMW", "DFH", "VFH", "OD"), as.numeric) %>%   
  bind_rows(tadpoles1) %>%
  dplyr::filter(rowSums(is.na(.)) < 11) %>% 
  group_by(Species) %>% 
  slice_max(Stage, n = 1) %>% 
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
  maxdim_pcoa         = 10,
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE,
  fdendro             = "average")


sp_faxes_coord_tadpoles = fspaces_quality_tadpoles$"details_fspaces"$"sp_pc_coord" 


class(sp_faxes_coord_tadpoles)

sp_faxes_coord_tadpoles_all = sp_faxes_coord_tadpoles %>% 
  as.data.frame() %>% 
  dplyr::mutate(scientificName = rownames(.)) %>% 
  dplyr::left_join(tadpoles2)

del = sp_faxes_coord_tadpoles = fspaces_quality_tadpoles$"details_fspaces"$"sp_pc_coord"  %>% 
  as.data.frame()


big_plot = mFD::funct.space.plot(
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

big_plot[1]


alpha_fd_indices_tadpoles = mFD::alpha.fd.multidim(
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


# --- 1. Define a function to calculate AUC (using trapezoidal rule) ---
calculate_auc <- function(df) {
  # Sort by period to ensure correct calculation
  df <- df %>% arrange(period)
  
  # Calculate area using the trapezoidal rule: sum( (y_i + y_{i-1}) / 2 * (x_i - x_{i-1}) )
  # We use the time periods (x) and the FRic values (y)
  auc_value <- sum(
    (df$fric[-1] + df$fric[-length(df$fric)]) / 2 * (df$period[-1] - df$period[-length(df$period)])
  )
  return(auc_value)
}

# --- 2. Calculate AUC for all Null Iterations ---
null_auc_values <- null_tidy %>%
  # Group by each simulation iteration
  dplyr::group_by(iteration) %>%
  # Apply the AUC calculation function
  dplyr::summarise(auc = calculate_auc(cur_data())) %>%
  dplyr::ungroup()

# --- 3. Calculate AUC for the Observed Curve ---
# Note: You need to rename 'value' to 'fric_value' for the function to work
observed_auc = observed_fric %>%
  calculate_auc()


# --- 4. Calculate the P-value ---
# Test if the observed AUC is significantly greater than the null (early species are highly unique)
p_value_greater <- sum(null_auc_values$auc >= observed_auc) / N_ITERATIONS

# Test if the observed AUC is significantly less than the null (early species are functionally average)
p_value_lesser <- sum(null_auc_values$auc <= observed_auc) / N_ITERATIONS

# --- 5. Report Results ---
cat("\n--- AUC Permutation Test Results ---\n")
cat(paste("Observed AUC:", round(observed_auc, 2), "\n"))
cat(paste("Mean Null AUC:", round(mean(null_auc_values$auc), 2), "\n"))
cat(paste("P-value (Observed > Null):", p_value_greater, "\n"))
cat(paste("P-value (Observed < Null):", p_value_lesser, "\n"))

if (p_value_greater <= 0.05) {
  cat("Conclusion: The observed FRic accumulation rate is SIGNIFICANTLY faster than expected by random chance (early species were highly unique).\n")
} else if (p_value_lesser <= 0.05) {
  cat("Conclusion: The observed FRic accumulation rate is SIGNIFICANTLY slower than expected by random chance.\n")
} else {
  cat("Conclusion: The observed FRic accumulation rate is NOT significantly different from random.\n")
}



