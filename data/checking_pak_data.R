# Script for checking the PTI data for PAK


library(tidyverse)
library(devPTIpack)
library(sf)


# Chekcing an cleaning shape files
pak_shp <- read_rds("data/ukr_shp.rds")
#View(pak_shp)

# Converting all to consistent datum and merging country shapes.
pak_shp <- 
  pak_shp %>% 
  map(~{.x %>% st_transform(4326)})

pak_shp$admin0_Province <-
  pak_shp$admin1_Oblast %>% 
  group_by(admin0Pcod) %>% 
  summarise() %>% 
  mutate(admin0Name = "Khyber PakhtunKhwa") %>% 
  st_make_valid()

pak_shp_new <- NULL
pak_shp_new$admin0_Province  <- pak_shp$admin0_Province 
pak_shp_new$admin1_District <- pak_shp$admin1_Oblast 
pak_shp_new$admin2_Tehsil <- pak_shp$admin2_Rayon 

pak_shp_new %>% write_rds("data/pak_geometries.rds", compress = "gz")

# View(pak_shp_new)


pak_shp_new$admin0_Province %>% st_crs()
pak_shp_new$admin1_District %>% st_crs()
pak_shp_new$admin2_Tehsil %>% st_crs()

validate_geometries(pak_shp_new)


# Checking the metadata
pak_dta <- read_rds("data/ukr_mtdt_full.rds")
#*
pak_dta1 <- read_rds("data/data_for_PTI/kp_mtdt_full_orig_data.rds")

# View(pak_dta)

pak_dta_new <- NULL
pak_dta_new$general <- pak_dta$general %>% as_tibble()
pak_dta_new$metadata <- pak_dta$metadata %>% as_tibble() 
pak_dta_new$admin1_District <- pak_dta$admin1_Oblast %>% as_tibble()
pak_dta_new$admin2_Tehsil<- pak_dta$admin2_Rayon %>% as_tibble()

#*
pak_dta_new1 <- NULL

pak_dta_new1$general <- pak_dta1$general %>% as_tibble()

pak_dta_new1$metadata <- pak_dta1$metadata %>% as_tibble()

pak_dta_new1$admin1_District <- pak_dta1$admin1_Oblast %>% as_tibble() 
# %>% 
#   mutate_at(vars(everything()), ~replace(., is.na(.), "")) %>%
#   # mutate(value = as.double(value)) %>% 
#   as_tibble()

pak_dta_new1$admin2_Tehsil<- pak_dta1$admin2_Rayon %>% as_tibble()  
# %>% 
#     mutate_at(vars(everything()), ~replace(., is.na(.), "")) %>%
#     as_tibble()
# 
  
# 
pak_dta_new %>% write_rds("data/pak_mtdt.rds")
#*
pak_dta_new1 %>% write_rds("data/data_for_PTI/kp_mtdt_full_orig_data.rds")



## Checking if the PTI calculations work
shp_dta <- pak_shp_new
imp_dta <- pak_dta_new
imp_dta$indicators_list <- devPTIpack::get_indicators_list(imp_dta) 
imp_dta$weights_clean <-
imp_dta$indicators_list$var_code %>% 
  devPTIpack::get_all_weights_combs(., 2)

#*
shp_dta <- pak_shp_new
imp_dta1 <- pak_dta_new1
imp_dta1$indicators_list1 <- devPTIpack::get_indicators_list(imp_dta1) 
imp_dta1$weights_clean <-
  imp_dta1$indicators_list1$var_code %>% 
  devPTIpack::get_all_weights_combs(., 2)

long_vars <- imp_dta %>% pivot_pti_dta(imp_dta$indicators_list) 
existing_shapes <- shp_dta %>% clean_geoms() 
mt <- shp_dta %>% get_mt()
adm_lvls <- mt %>% get_adm_levels()

#*Have to replace NAs with "" otherwise pivot_long deletes them
long_vars1 <- imp_dta1 %>% pivot_pti_dta(imp_dta1$indicators_list1) 
#*This replce "" with NAs and convert
# long_vars1$admin1_District <- long_vars1$admin1_District %>%
#   as_tibble() %>% 
#   mutate(value = as.double(value))
# long_vars1$admin2_Tehsil <- long_vars1$admin2_Tehsil %>%
#   as_tibble() %>% 
#   mutate(value = as.double(value))

existing_shapes <- shp_dta %>% clean_geoms() 
mt <- shp_dta %>% get_mt()
adm_lvls <- mt %>% get_adm_levels()

calc_wght <- 
  imp_dta$weights_clean %>%
  get_weighted_data(long_vars, indicators_list = imp_dta$indicators_list) %>% 
  get_scores_data() %>% 
  imap(~ expand_adm_levels(.x, mt) %>%
         merge_expandedn_adm_levels()) %>% 
  agg_pti_scores(existing_shapes) %>% 
  label_generic_pti() %>% 
  structure_pti_data(shp_dta)

#*Names of variables shoudl be different for this to work, otherwisse error
calc_wght1 <- 
imp_dta1$weights_clean %>%
  get_weighted_data(long_vars1, indicators_list = imp_dta1$indicators_list1) %>% 
  get_scores_data() %>% 
  imap(~ expand_adm_levels(.x, mt) %>%
         merge_expandedn_adm_levels()) %>% 
  agg_pti_scores(existing_shapes) %>% 
  label_generic_pti() %>% 
  structure_pti_data(shp_dta)

# Preparing data for plotting
preplot_dta0 <- 
  calc_wght %>% 
  preplot_reshape_wghtd_dta()
#*
preplot_dta1 <- 
  calc_wght1 %>% 
  preplot_reshape_wghtd_dta()

length(preplot_dta0)
#*
length(preplot_dta1)

# Filtering admin levels which are not available
adm_to_filter <- 
  imp_dta %>% 
  get_indicators_list() %>% 
  get_vars_un_avbil(names(get_current_levels(preplot_dta0))) %>% 
  get_min_admin_wght(imp_dta$weights_clean)

#*
adm_to_filter1 <- 
  imp_dta1 %>% 
  get_indicators_list() %>% 
  get_vars_un_avbil(names(get_current_levels(preplot_dta1))) %>% 
  get_min_admin_wght(imp_dta1$weights_clean)


preplot_dta <-
  preplot_dta0 %>% 
  drop_inval_adm(adm_to_filter) %>% 
  # Placeholder for filtering data module to plot only specific admin levels
  # filter_admin_levels(c("Country", "Hexagons")) %>% 
  
  # Module on the number of bins and legend 
  add_legend_paras(nbins = 7) %>% 
  complete_pti_labels() %>% 
  rev()

#*
  preplot_dta_orig <-
  preplot_dta1 %>% 
  drop_inval_adm(adm_to_filter1) %>% 
  # Placeholder for filtering data module to plot only specific admin levels
  # filter_admin_levels(c("Country", "Hexagons")) %>% 
  
  # Module on the number of bins and legend 
   add_legend_paras(nbins = 7) %>% 
  complete_pti_labels() %>% 
  rev()


preplot_dta %>% 
  get_current_levels() 

preplot_dta_orig %>% 
  get_current_levels() 


library(leaflet)
leaflet() %>%
  plot_leaf_line_map2(shp_dta, show_adm_levels = NULL) %>%
  plot_pti_polygons(preplot_dta) %>%
  add_pti_poly_controls(preplot_dta)

#*
leaflet() %>%
  plot_leaf_line_map2(shp_dta, show_adm_levels = NULL) %>%
  plot_pti_polygons(preplot_dta_orig) %>%
  add_pti_poly_controls(preplot_dta_orig)




# # Cheking if the PTI calculations may work
# shp_dta <- pak_shp_new
# imp_dta <- pak_dta_new
# imp_dta$indicators_list <- get_indicators_list(imp_dta)
# long_vars <- imp_dta %>% pivot_pti_dta(imp_dta$indicators_list)
# existing_shapes <- shp_dta %>% clean_geoms()
# mt <- shp_dta %>% get_mt()
# adm_lvls <- mt %>% get_adm_levels()
# imp_dta$weights_clean <- imp_dta$indicators_list$var_code %>%
#   get_all_weights_combs(1)
# calc_pti_at_once <- get_weighted_data(imp_dta$weights_clean,
#                                       long_vars, imp_dta$indicators_list) %>%
#   get_scores_data() %>% imap(~ expand_adm_levels(.x,
#                                                  mt) %>% merge_expandedn_adm_levels())
# na_agg <- calc_pti_at_once %>% agg_pti_scores(existing_shapes)
# nrow_pti <- na_agg %>% map_dfr(~ .x %>% count(pti_name)) %>%
#   count(pti_name) %>% nrow()
# nrow(imp_dta$indicators_list) == nrow_pti


run_new_pti(
  pti.name = "Khyber Pakhtunkhwa",
  # shape_path = NULL,
  shape_dta = pak_shp_new, 
  # data_path = NULL,
  data_dta = pak_dta_new1,
  metadata_path = NULL,
  show_waiter = FALSE,
  
  default_adm_level = "admin1",
  show_adm_levels = c("admin1", "admin2"),
  choose_adm_levels = TRUE,
  
  explorer_choose_adm = FALSE,
  explorer_default_adm = "all",
  explorer_multiple_var = FALSE,
  
  full_ui = FALSE,
  pti_landing_page = "data/landing-page.md"
)

# devPTIpack::run_onepage_pti(
#   shape_dta = pak_shp_new, data_dta = pak_dta_new
# )

