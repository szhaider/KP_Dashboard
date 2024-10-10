#Data Prep File

library(tidyverse)
library(readxl)
library(rio)
library(janitor)
library(sf)



################################################################################
#09/12/2021
#Reading and preparing Accessibility data for KP dashboard
#District
accessibility_dis <- read_csv("data/Component1_Accessibility/C1_District_long.csv") %>% 
  select(-X1, -`District Code (Adm2_Code)`, -Column) %>%
  rename(district = `District Name (ADM2_EN)`, division = `Division Name`) %>% 
  clean_names() %>% 
  mutate(component = "accessibility") %>% 
  arrange(district) %>% 
  mutate(polygon = "district")
# %>% 
#   saveRDS("KP_Dashboard/data/accessibility_dis.RDS")

#Tehsil
accessibility_teh <- read_csv("data/Component1_Accessibility/C1_Tehsils_long.csv")%>% 
  select(-X1, -`District Code (Adm2_Code)`, -`Tehsil Code (Adm3_Code)`, -Column) %>%
  rename(district = `District Name (ADM2_EN)`, tehsil = `Tehsil Name (ADM3_EN)`, division = `Division Name`) %>% 
  clean_names()%>% 
  mutate(component = "accessibility") %>% 
  arrange(tehsil) %>% 
  mutate(polygon = "tehsil")
# %>% 
#   saveRDS("KP_Dashboard/data/accessibility_teh.RDS")

##Combining tehsil & District level accessibility data into one

accessibility_comb <- bind_rows(accessibility_dis, accessibility_teh) 

# %>% 
#   write_rds("KP_Dashboard/data/accessibility_comb.RDS")


################################################################################

#Reading in Natural Hazards data & preparing

#Districts (Old natural hazards data with 34 districts)
# hazards_dis <- read_csv("data/Component2_Natural_Hazards/C2_District_long_old.csv") %>% 
#   select(-X1, -Column) %>%
#   clean_names() %>% 
#   rename(district = district_name, division = division_name, indicator_v2 = variable) %>% 
#   mutate(component = "natural hazards") %>% 
#   arrange(district) %>% 
#   mutate(polygon = "district")

#New Natural Hazards data with 38 districts in KP
#First converting into long and make necessary changes to create a join

district_guide <- read_excel("data/Component2_Natural_Hazards/C2_District_Guide.xlsx") %>% 
  select(-Column, 
         indicator_v2 = Indicator,
         source= Source,
         unit = Unit,
         description = Description) %>% 
         slice_tail(n=24) %>% 
  mutate(indicator_v2 = str_replace_all(indicator_v2, "_", " ")) %>% 
  mutate(indicator_v2 = str_to_title(indicator_v2)) %>% 
  mutate(indicator_v2 = str_replace_all(indicator_v2, " ", "_")) 

hazards_dis <- read_excel("data/Component2_Natural_Hazards/C2_Districts.xlsx") %>% 
  # arrange(District_Name) %>%
  # select(Division_Name, District_Name) %>%
  # write_csv("data/Component2_Natural_Hazards/forls.csv")
  pivot_longer(cols = !ends_with("Name"), names_to = "indicator_v2", values_to =  "value") %>% 
  rename(district = District_Name, 
         division = Division_Name) %>% 
  mutate(indicator_v2 = str_replace_all(indicator_v2, "_", " ")) %>% 
  mutate(indicator_v2 = str_to_title(indicator_v2)) %>% 
  mutate(indicator_v2 = str_replace_all(indicator_v2, " ", "_")) %>% 
  left_join(district_guide, by= c("indicator_v2")) %>% 
  mutate(component = "natural hazards",
         polygon = "district") %>% 
  select(division, district, value, indicator_v2, everything()) %>% 
  arrange(district) %>% 
  replace_na(list(value=0)) %>%   #Replacing NAs with 0s for natural hazards
  filter(indicator_v2 !=  "Population_Density",
         indicator_v2 != "District_Area",
         indicator_v2 != "District_Population")
  
#Tehsils
hazards_teh <- read_csv("data/Component2_Natural_Hazards/C2_Tehsils_long.csv") %>%
  # arrange(Tehsil_Name) %>%
  # select(Division_Name, Tehsil_Name) %>%
  # distinct(Division_Name, Tehsil_Name) %>% 
  # write_csv("data/Component2_Natural_Hazards/forls_teh.csv")
  select(-X1, -Column) %>%
  clean_names() %>% 
  rename(district = district_name, division = division_name, tehsil= tehsil_name, indicator_v2 = variable) %>% 
  mutate(component = "natural hazards") %>% 
  arrange(tehsil) %>% 
  mutate(polygon = "tehsil") %>% 
  replace_na(list(value=0)) %>% 
  filter(indicator_v2 !=  "Population_Density",
         indicator_v2 != "Tehsil_Area",
         indicator_v2 != "Tehsil_Population")

##Combining tehsil & District level Natural Hazards data into one

hazards_comb <- bind_rows(hazards_dis, hazards_teh)

#Making a new component to pluck demogroahic estimates from natural hazards
#District
demography_dis <- read_excel("data/Component2_Natural_Hazards/C2_Districts.xlsx") %>% 
  pivot_longer(cols = !ends_with("Name"), names_to = "indicator_v2", values_to =  "value") %>% 
  rename(district = District_Name, 
         division = Division_Name) %>% 
  mutate(indicator_v2 = str_replace_all(indicator_v2, "_", " ")) %>% 
  mutate(indicator_v2 = str_to_title(indicator_v2)) %>% 
  mutate(indicator_v2 = str_replace_all(indicator_v2, " ", "_")) %>% 
  left_join(district_guide, by= c("indicator_v2")) %>% 
  mutate(component = "natural hazards",
         polygon = "district") %>% 
  select(division, district, value, indicator_v2, everything()) %>% 
  arrange(district) %>% 
  subset(indicator_v2 == "Population_Density"|
           indicator_v2 == "District_Area"|
           indicator_v2 == "District_Population") %>% 
  mutate(component = "Demography")

#Tehsil
demography_teh <- read_csv("data/Component2_Natural_Hazards/C2_Tehsils_long.csv") %>%
  select(-X1, -Column) %>%
  clean_names() %>% 
  rename(district = district_name, division = division_name, tehsil= tehsil_name, indicator_v2 = variable) %>% 
  mutate(component = "natural hazards") %>% 
  arrange(tehsil) %>% 
  mutate(polygon = "tehsil") %>% 
  subset(indicator_v2 == "Population_Density"|
         indicator_v2 == "Tehsil_Area"|
         indicator_v2 == "Tehsil_Population") %>% 
  mutate(component = "Demography")

##Combining tehsil & District level demographic data into one

demography_comb <- bind_rows(demography_dis, demography_teh)

################################################################################
#Standard of living 

#Districts Level
district_guide_ls <- read_excel("data/Component3_Living_Standards/C3_District_Guide.xlsx") %>% 
  select(-Column, 
         indicator_v2 = Indicator,
         source= Source,
         unit = Unit,
         description = Description) %>% 
  slice_tail(n=7) %>% 
  filter(indicator_v2 == "District_Mean_RWI"|
           indicator_v2 =="Mean_LSI_Weighted_Transformed"|
           indicator_v2 =="Mean_AI_Weighted_Transformed") %>% 
  mutate(indicator_v2 =
           if_else(indicator_v2 == "District_Mean_RWI", "Relative Wealth Index", indicator_v2)) %>% 
  mutate(indicator_v2 =
           if_else(indicator_v2 == "Mean_LSI_Weighted_Transformed", "Living Standard Index", indicator_v2)) %>% 
  mutate(indicator_v2 =
           if_else(indicator_v2 == "Mean_AI_Weighted_Transformed", "Accessibility Index", indicator_v2))


 
ls_dis <- read_excel("data/Component3_Living_Standards/C3_District_DashboardData.xlsx") %>% 
  pivot_longer(cols = !ends_with("Name"), names_to = "indicator_v2", values_to =  "value") %>% 
  rename(district = District_Name, 
         division = Division_Name) %>% 
  filter(indicator_v2 == "District_Mean_RWI"|
           indicator_v2 =="Mean_LSI_Weighted_Transformed"|
           indicator_v2 =="Mean_AI_Weighted_Transformed") %>% 
  mutate(indicator_v2 =
           if_else(indicator_v2 == "District_Mean_RWI", "Relative Wealth Index", indicator_v2)) %>% 
  mutate(indicator_v2 =
           if_else(indicator_v2 == "Mean_LSI_Weighted_Transformed", "Living Standard Index", indicator_v2)) %>% 
  mutate(indicator_v2 =
           if_else(indicator_v2 == "Mean_AI_Weighted_Transformed", "Accessibility Index", indicator_v2)) %>% 

  left_join(district_guide_ls, by= c("indicator_v2")) %>% 
  mutate(component = "living standards",
         polygon = "district") %>% 
  select(division, district, value, indicator_v2, everything()) %>% 
  arrange(district) %>% 
  mutate(value = as.double(value))

#TehsilS level
tehsil_guide_ls <- read_excel("data/Component3_Living_Standards/C3_Tehsil_Guide.xlsx") %>% 
  select(-Column, 
         indicator_v2 = Indicator,
         source= Source,
         unit = Unit,
         description = Description) %>% 
  # slice_tail(n=9) 
         filter(indicator_v2 == "Tehsil_Mean_RWI"|
                indicator_v2 =="Mean_LSI_Weighted_Transformed"|
                indicator_v2 =="Mean_AI_Weighted_Transformed") %>% 
  mutate(indicator_v2 =
           if_else(indicator_v2 == "Tehsil_Mean_RWI", "Relative Wealth Index", indicator_v2)) %>% 
  mutate(indicator_v2 =
           if_else(indicator_v2 == "Mean_LSI_Weighted_Transformed", "Living Standard Index", indicator_v2)) %>% 
  mutate(indicator_v2 =
           if_else(indicator_v2 == "Mean_AI_Weighted_Transformed", "Accessibility Index", indicator_v2))
  


ls_teh <- read_excel("data/Component3_Living_Standards/C3_Tehsils_DashboardData.xlsx") %>% 
  pivot_longer(cols = !ends_with("Name"), names_to = "indicator_v2", values_to =  "value") %>% 
  rename(district = District_Name, 
         division = Division_Name,
         tehsil= Tehsil_Name) %>%
  filter(indicator_v2 == "Tehsil_Mean_RWI"|
           indicator_v2 =="Mean_LSI_Weighted_Transformed"|
           indicator_v2 =="Mean_AI_Weighted_Transformed") %>% 
  mutate(indicator_v2 =
           if_else(indicator_v2 == "Tehsil_Mean_RWI", "Relative Wealth Index", indicator_v2)) %>% 
  mutate(indicator_v2 =
           if_else(indicator_v2 == "Mean_LSI_Weighted_Transformed", "Living Standard Index", indicator_v2)) %>% 
  mutate(indicator_v2 =
           if_else(indicator_v2 == "Mean_AI_Weighted_Transformed", "Accessibility Index", indicator_v2)) %>% 

  left_join(tehsil_guide_ls, by= c("indicator_v2")) %>% 
  mutate(component = "living standards",
         polygon = "tehsil") %>% 
  select(division, district, tehsil, value, indicator_v2, everything()) %>% 
  arrange(tehsil)%>% 
  mutate(value = as.double(value)) 

ls_comb <- bind_rows(ls_dis, ls_teh)
###############################################################################

#Bringing in Household Welfare estimated 2014
poverty <- readRDS("data/Pakistan_indicators/pak_ind.rds") %>%    #Using Pakistan Indicators file
  as_tibble() %>% 
  filter(year %in% c(2014),
         province == "Khyber Pakhtunkhwa",
         domain == "Household Welfare",
         indicator_1 == "Poverty Rate" | indicator_1 == "Number Of Poor") %>% 
  rename(indicator_v2 = indicator,
         unit = units,
         component = domain) %>% 
  mutate(division = NA,
         variable = indicator_v2,
         description = definition,
         polygon = "district",
         tehsil = NA) %>% 
  mutate(indicator_v2 =
           if_else(indicator_v2 == "Poverty Rate (%)", "Poverty Rate - 2014", indicator_v2)) %>% 
  mutate(indicator_v2 =
           if_else(indicator_v2 == "Number of poor (1,000s)", "Number of poor - 2014", indicator_v2)) %>%
  mutate(unit = 
           if_else(unit == "%", "Percentage (%)", unit)) %>% 
  mutate(unit =
           if_else(unit == "(000s)", "Thousands (000)", unit)) %>% 
  select(-year, -context, -year_1, -province, -indicator_1)

################################################################################
#Combining Accessibility, Hazards data, Living Standards & Demography

data <- bind_rows(accessibility_comb, 
                  hazards_comb, 
                  ls_comb, 
                  demography_comb,
                  poverty) %>% 
  write_rds("KP_Dashboard/data/data.RDS")
################################################################################


#Reading and preparing shapefiles both for tehsils and districts

#Reading in Tehsil level shapefile (Using UNOCHA shape file)
#It has 116 tehsils which matches datasets

pak_shp_tehsil <- st_read("data/Tehsils_shp_UNOCHA/pak_admbnda_adm3_ocha_pco_gaul_20181218.shp") %>% 
  st_as_sf() %>% 
  clean_names() %>% 
  filter(adm1_en == "Khyber Pakhtunkhwa") %>% 
  select(adm2_en, adm3_en, geometry) %>% 
  rename(tehsil = adm3_en, district = adm2_en) %>% 
  arrange(tehsil)%>% 
  mutate(polygon = "tehsil",
         dis_teh = tehsil)
# pak_shp_tehsil <- st_read("data/Tehsils/pak_admbnda_adm3_ocha_pco_gaul_20181218.shp", stringsAsFactors=FALSE)

#Reading in District level shapefile (Using UNOCHA shapefile)

#pak_shp_district <- st_read("data/districts_shp_UNOCHA/Districts_Projected.shp")
#UNOCHA shape file has 33 districts but accessibility data has 38 districts so trying the usual district shp file here

pak_shp_district <- st_read("data/Districts_shp_usual/pakistan_indicators.shp") %>% 
  st_as_sf() %>% 
  clean_names() %>% 
  filter(province == "Khyber Pakhtunkhwa", year==2018) %>% 
  select(district, geometry) %>% 
  arrange(district)%>% 
  mutate(polygon = "district",
         dis_teh = district)


#Combining both shapefiles 
pak_shp_comb <- list(District =pak_shp_district, Tehsil =pak_shp_tehsil)%>% 
  write_rds("KP_Dashboard/data/pak_shp_comb.RDS")

# pak_shp_comb <- bind_rows(pak_shp_district,pak_shp_tehsil) %>% 
#   write_rds("KP_Dashboard/data/pak_shp_comb.RDS")

#Saving Shape file geometries as RDS
# pak_shp_tehsil %>% 
#   write_rds("KP_Dashboard/data/pak_shp_tehsil.RDS")
# 
# pak_shp_district %>% 
#   write_rds("KP_Dashboard/data/pak_shp_district.RDS")

################################################################################

#16/01/2022
#Preparing shapefile based on the requirements of PTIdevpack
#2 Admin levels: Districts & Tehsils

# District level
dist_nums <- read_excel("data/KP_dist_no.xlsx")
pak_shp_district_PTI %>% as_tibble() %>%  select(admin1Name) %>% write.csv("data/district.csv")

pak_shp_district_PTI <- st_read("data/Districts_shp_usual/pakistan_indicators.shp") %>% 
  st_as_sf() %>% 
  clean_names() %>% 
  filter(province == "Khyber Pakhtunkhwa", year==2018) %>% 
  arrange(district) %>% 
  rename(admin1Name = district)  %>% 
  left_join(dist_nums, by= "admin1Name") %>% 
  mutate(admin0Pcod = "KP") %>% 
  mutate(admin1Pcod = paste0("KP", nums)) %>% 
  select(admin0Pcod, admin1Pcod, admin1Name, geometry)
  # mutate(admin1Pcod = paste0("KP", row_number())) %>% 

# Tehsil level

# pak_shp_tehsil_PTI %>% as_tibble() %>%  select(admin2Name, district) %>% write.csv("data/tehsil.csv")

tehsil_nums <- read_excel("data/tehsil_nums.xlsx")

pak_shp_tehsil_PTI <- st_read("data/Tehsils_shp_UNOCHA/pak_admbnda_adm3_ocha_pco_gaul_20181218.shp") %>% 
  st_as_sf() %>% 
  clean_names() %>% 
  filter(adm1_en == "Khyber Pakhtunkhwa") %>% 
  mutate(admin0Pcod = "KP") %>% 
  # mutate(admin1Pcod = paste0("KP", row_number())) %>% 
  # mutate(admin2Pcod = paste0(admin1Pcod, row_number())) %>% 
  select(admin0Pcod, adm2_en, adm3_en, geometry) %>% 
  rename(tehsil = adm3_en, district = adm2_en) %>% 
  arrange(tehsil) %>% 
  rename(admin2Name = tehsil) %>% 
  left_join(tehsil_nums ,by="admin2Name") %>% 
  select(-district.y, -district.x) %>% 
  arrange(admin1Pcod) %>% 
  mutate(admin2Pcod = paste0(admin1Pcod, admin2Pcod))

#Making a list of district and tehsil shape objects
pak_shp <- list(admin1_Oblast = pak_shp_district_PTI, admin2_Rayon = pak_shp_tehsil_PTI) 

pak_shp <- 
  pak_shp %>% 
  map(~{.x %>% st_transform(4326)})

pak_shp$admin0_Province <-
  pak_shp$admin1_Oblast %>% 
  group_by(admin0Pcod) %>% 
  summarise() %>% 
  mutate(admin0Name = "Khyber PakhtunKhwa") %>% 
  st_make_valid()

pti_shps <- NULL
pti_shps$admin0_Province  <- pak_shp$admin0_Province 
pti_shps$admin1_District <- pak_shp$admin1_Oblast 
pti_shps$admin2_Tehsil <- pak_shp$admin2_Rayon 

pti_shps %>% write_rds("data/pak_geometries.rds", compress = "gz")

pti_shps$admin0_Province %>% st_crs()
pti_shps$admin1_District %>% st_crs()
pti_shps$admin2_Tehsil %>% st_crs()

validate_geometries(pti_shps)

  
# ukr_shp %>% write_rds("data/ukr_shp.rds") 
# View(kp_shp)

#Reading in datasheet with metadata as list
# ukr_mtdt_full <- import_list("data/kp_mtdt_full.xlsx")
# ukr_mtdt_full %>% write_rds("data/ukr_mtdt_full.rds")
# View(kp_mtdt_full)
#Using the same names as in PTI dev package initially

#27/1
#Reading & Arranging Natural Hazards data in wide format for populating PTI datasheet and metadata
#District
#replaceing NAs with ""
NH_wide_dis <- read_excel("data/data_for_PTI/C2_Districts_NH.xlsx") %>% 
  arrange(District_Name) %>% 
  select(-Division_Name) %>% 
  mutate_at(vars(everything()), ~replace(., is.na(.), "")) %>% 
  write_csv("data/data_for_PTI/C2_Districts_arranged_NH.csv")

#Tehsil
NH_wide_teh <- read_csv("data/data_for_PTI/C2_Tehsils_long.csv") %>% 
  select(-X1, -Division_Name, -District_Name, -Source, -Unit, -Description, -Column) %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  mutate_at(vars(everything()), ~replace(., is.na(.), "")) %>% 
  write.csv("data/data_for_PTI/C2_Tehsils_arranged_NH.csv")

#Reading & Arranging Accessibility data in wide format for populating PTI datasheet and metadata
Access_wide_dis <- read_csv("data/data_for_PTI/C1_District_long.csv") %>% 
  select(-X1, -Division_Name, -District_Code, -Column, -variable, -Source, -Unit, -Description) %>% 
  pivot_wider(names_from = Indicator_v2 , values_from = value) %>% 
  write.csv("data/data_for_PTI/C1_Districts_arranged_access.csv")

Access_wide_teh <- read_csv("data/data_for_PTI/C1_Tehsils_long.csv") %>% 
  select(-X1, -Division_Name, -District_Name, -District_Code, -Tehsil_Code, -Column, -variable, -Source, -Unit, -Description) %>% 
  pivot_wider(names_from = Indicator_v2 , values_from = value) %>% 
  write.csv("data/data_for_PTI/C1_Tehsils_arranged_access.csv")

#Living standard data arrangement (to arrange tehsils in accordance with shape data)
tehsil_nums <- read_excel("data/tehsil_nums.xlsx")

teh_living_stand <- read_excel("data/Component3_Living_Standards/C3_Tehsils_DashboardData.xlsx") %>% 
  left_join(tehsil_nums, by= "admin2Name") %>% 
  arrange(admin1Pcod) %>% 
  mutate(admin2Pcod = paste0(admin1Pcod, admin2Pcod)) %>% 
  write.csv("data/Component3_Living_Standards/C3_Tehsils_arranged.csv")
  
#Then copying the required variables from C3_Tehsils_arranged.csv into metadata by hand


kp_mtdt_full_orig_data <- import_list("data/data_for_PTI/kp_mtdt_full_orig_data.xlsx") 
  
kp_mtdt_full_orig_data %>%  write_rds("data/data_for_PTI/kp_mtdt_full_orig_data.rds")



  







