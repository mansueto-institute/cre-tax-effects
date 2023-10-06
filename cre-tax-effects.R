
# Author: Nicholas Marchio, Mansueto Institute
# Date: October 1, 2023
# Based on vignette available here: # https://ccao-data.github.io/ptaxsim/articles/reassessment.html#future-reassessments

library(tidyverse)
library(DBI)
library(curl)
library(ptaxsim)
library(ccao)
library(assessr)
library(viridis)
library(arrow)
library(duckdb)
library(sf)
library(sfarrow)
library(patchwork)
library(scales)
library(nlme)
library(dbplyr)

# Set working directory ---------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Dictionaries ------------------------------------------------------------

class_data <- ccao::class_dict

class_data <- class_data %>%
  mutate(subgroup_label = case_when(
    major_class_type == 'Residential' ~ 'Residential',
    class_code %in% c('591') ~ '4+ story office/retail',
    class_code %in% c('592') ~ '2 to 3 story office/retail',
    # class_code %in% c('791', '792', '743', '774') ~ 'Office/retail incentive',
    major_class_type %in% c('Commercial','Commercial Incentive') ~ 'Other commercial',
    TRUE ~ 'Non-commercial / non-residential'),
    group_label = case_when(
      major_class_type == 'Residential' ~ 'Residential',
      class_code %in% c('591','592') ~ '2+ story office/retail',
      major_class_type %in% c('Commercial','Commercial Incentive') ~ 'Other commercial',
      TRUE ~ 'Non-commercial / non-residential')
    )
    
# Township data
# Source: https://datacatalog.cookcountyil.gov/GIS-Maps/Historical-ccgisdata-Political-Township-2016/uvx8-ftf4
townships <- ccao::town_shp %>% st_make_valid()

# Load data ---------------------------------------------------------------

# Townships
townships <- ccao::town_shp %>% st_make_valid()
# Filter analysis to city
township_city_list <- townships %>% filter(triad_name == 'City') %>% select(township_code) %>% st_drop_geometry() %>% pull()

# Chicago CBD
chicago_cbd <- st_read('https://data.cityofchicago.org/api/geospatial/tksj-nvsw?method=export&format=GeoJSON') %>%
  mutate(cbd = 1) %>%
  select(cbd) %>%
  st_transform(4326)

# Chicago TIF
# Download here: https://hub-cookcountyil.opendata.arcgis.com/datasets/cc516b88a47547bd974bba4ebc120ecf/explore
tif_districts <- st_read('data/Historical_Tax_Increment_Financing_(TIF)_Districts_-_2018.geojson')
tif_districts <- tif_districts %>%
  rename_all(list(tolower)) %>%
  mutate(tif_district = 1) %>%
  select(tif_district) %>%
  st_make_valid() %>%
  group_by(tif_district) %>%
  dplyr::summarize(geometry = st_union(geometry)) %>%
  ungroup() %>% 
  st_make_valid() %>%
  st_cast("MULTIPOLYGON") %>% 
  st_cast("POLYGON")  %>%
  st_transform(4326)

# Check districts
ggplot() +
  geom_sf(data = chicago_cbd, fill = 'red', alpha = .5) +
  geom_sf(data = tif_districts %>% st_intersection(., chicago_cbd), fill = 'blue', alpha = .5) + 
  theme_void()

# Read in PIN Universe ----------------------------------------------------

# Set DB connection
# Download here: https://github.com/ccao-data/ptaxsim#ptaxsim
ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), "/Users/nm/Desktop/Projects/work/cook-assessor/download.nosync/data/ptaxsim-2021.0.4.db")

# PIN geometries
ptax_data_geo <- dbGetQuery(ptaxsim_db_conn, "SELECT pin10, longitude, latitude FROM pin_geometry_raw")

ptax_data_geo <- ptax_data_geo %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>%
  group_by(pin10) %>%
  mutate(dup = row_number()) %>%
  ungroup() %>%
  filter(dup == 1) %>%
  st_join(., chicago_cbd, join = st_within) %>%
  st_join(., tif_districts, join = st_within) %>%
  st_drop_geometry() %>%
  select(pin10, cbd, tif_district) 

ptax_data_geo <- ptax_data_geo %>%
  mutate(cbd = case_when(is.na(cbd) ~ 0, TRUE ~ 1),
         tif_district = case_when(is.na(tif_district) ~ 0, TRUE ~ 1))

# Property tax bills 
# ptax_data <- dbGetQuery(ptaxsim_db_conn, "SELECT year, pin, tax_bill_total, av_mailed, av_certified, av_board, av_clerk FROM pin where year >= 2019") %>% as_tibble()

# PTaxSim -----------------------------------------------------------------

ptax_data_2021 <- dbGetQuery(ptaxsim_db_conn, "SELECT * FROM pin where year = 2021") %>% as_tibble()

pin_data <- ptax_data_2021 %>%
  mutate(pin10 = str_sub(pin, start = 1L, end = 10L),
         township_code = str_sub(tax_code_num, start = 1L, end = 2L)) %>%
  filter(township_code %in% township_city_list) %>% # filter to City PINs
  left_join(., ptax_data_geo , by = c("pin10" = "pin10")) %>% 
  left_join(., class_data, by = c('class'='class_code')) %>%
  collect() 

rm(ptax_data_geo)
gc()


# Unaltered tax bills -----------------------------------------------------

base_year = 2021
pin_universe <- unique(pin_data$pin)

# Grab unaltered components to feed to tax_bill()
base_pins_dt <- lookup_pin(base_year, pin_universe)
base_tax_codes <- lookup_tax_code(base_year, pin_universe)
base_levies <- lookup_agency(base_year, base_tax_codes)
base_tifs <- lookup_tif(base_year, base_tax_codes)

# Get unaltered, unprojected tax bills
base_bills <- tax_bill(
  year_vec = base_year,
  pin_vec = pin_universe,
  tax_code_vec = base_tax_codes,
  pin_dt = base_pins_dt,
  agency_dt = base_levies,
  tif_dt = base_tifs
)

options(scipen = 999)
base_bills %>%
  group_by(agency_num, agency_name) %>%
  tally(wt = final_tax) %>%
  ungroup() %>%
  mutate(share = n/sum(n)) %>%
  arrange(desc(share)) %>% 
  filter(share > .01)

base_bills_coded <- base_bills %>%
  left_join(., pin_data %>%
              select(pin,class,cbd,tif_district,subgroup_label,group_label),
            by = c('pin'='pin'))  %>%
  filter(!(class.x %in% c('0','192'))) %>%
  mutate(class_type =   case_when(group_label == "2+ story office/retail" & cbd == 1 ~ 'Downtown multistory office/retail',
                                  group_label == "2+ story office/retail" & cbd == 0 ~ 'Chicago multistory office/retail',
                                  group_label == "Residential" & cbd == 1 ~ 'All Chicago residential',
                                  group_label == "Residential" & cbd == 0 ~ 'All Chicago residential',
                                  group_label %in% c("Non-commercial / non-residential","Other commercial") & cbd == 1 ~ 'Downtown commercial/industrial',
                                  group_label %in% c("Non-commercial / non-residential","Other commercial") & cbd == 0 ~ 'Chicago commercial/industrial',
                                  group_label == "2+ story office/retail" ~ 'Chicago commercial/industrial',
                                  group_label == "Residential" ~ 'All Chicago residential',
                                  group_label %in% c("Non-commercial / non-residential","Other commercial") ~ 'Chicago commercial/industrial',
                                  TRUE ~ "Other"))

base_bills_sum <- base_bills_coded %>%
  mutate(pin_agency_count = 1) %>%
  group_by(pin, year, class_type) %>%
  summarize(
    av = sum(av), #  first(av)
    final_tax = sum(final_tax),
    pin_agency_count = sum(pin_agency_count),
    .groups = 'keep'
    ) %>%
  ungroup() %>%
  mutate(pin_count = 1, av = av/pin_agency_count) %>%
  group_by(year, class_type) %>%
  summarize(
    av = sum(av),
    final_tax = sum(final_tax),
    pin_count = sum(pin_count),
    .groups = 'keep') %>%
  ungroup() %>%
  mutate(average_tax = final_tax / pin_count,
         av_average = av/pin_count,
         av_share = av/sum(av),
         final_tax_share = final_tax/sum(final_tax),
         pin_count_share = pin_count/sum(pin_count)) %>%
  #mutate(reference_year = "2021 Peak") %>%
  rename_with(.cols = c("av", "final_tax", "pin_count", "average_tax", "av_average", "av_share", "final_tax_share", "pin_count_share"), 
              .fn = ~ paste0("", .x, "_2021"))

# Future scenario ---------------------------------------------------------

# Function
run_cre_projection <- function(change_fraction) {
  
  # Equalizer assumption (hold constant) ------------------------------------
  
  future_eq_fct <- DBI::dbGetQuery(
    ptaxsim_db_conn,
    "SELECT * FROM eq_factor WHERE year = 2021"
  ) %>%
    pull(eq_factor_final)
  
  # Recalculate the equalized assessed value of all City triad PINs  --------
  
  future_pins <- pin_data %>%
    as_tibble() %>%
    mutate(
      # Reduce AVs of these property types
      av = case_when(group_label %in% c("2+ story office/retail","Other commercial","Non-commercial / non-residential")  & 
                       cbd == 1 ~ av_clerk * change_fraction,
                     TRUE ~ as.numeric(av_clerk)
      ),
      eav = round(av * future_eq_fct, 0)
    )
  
  # Recalculate the tax base of each taxing district using our new AVs --------
  future_tax_codes <- future_pins %>%
    group_by(year, tax_code_num) %>%
    summarize(total_eav = sum(eav), .groups = 'keep') %>%
    ungroup()
  
  # Get the total estimated tax base for each agency for 2021
  future_agency_tots <- DBI::dbGetQuery(
    ptaxsim_db_conn,
    "SELECT * FROM tax_code WHERE year = 2021") %>%
    left_join(future_tax_codes, by = c("year", "tax_code_num")) %>%
    group_by(year, agency_num) %>%
    summarize(agency_total_eav = sum(total_eav, na.rm = TRUE),
              .groups = 'keep') %>%
    ungroup()
  
  # Recalculate levies for the city + CPS  ----------------------------------
  # Projected budgets for CPS and city
  chicago_levies <- list('year' = c(2019, 2020, 2021, 2022, 2023, 2024),
                         'cps' = c(2984300000, 3134500000, 3264900000, 3374200000, 3685300000, 3816000000),
                         'city' = c(1514102000, 1539811000, 1633162000, 1709390000, 1734390000, 1824390000)) %>% as.data.frame()
  
  chicago_levies <- chicago_levies %>% 
    pivot_wider(names_from = year, values_from = c(cps,city), values_fill = 0) %>%
    mutate(cps_growth_rate_2024_2021 = cps_2024/cps_2021,
           city_growth_rate_2024_2021 = city_2024/city_2021)
  
  future_levies <- lookup_agency(2021, unique(future_tax_codes$tax_code_num)) %>%
    mutate(agency_total_ext = case_when(agency_name == 'BOARD OF EDUCATION' ~ agency_total_ext*unique(chicago_levies$cps_growth_rate_2024_2021),
                                        agency_name == 'CITY OF CHICAGO' ~ agency_total_ext*unique(chicago_levies$city_growth_rate_2024_2021),
                                        TRUE ~ agency_total_ext))
  
  # Transform the data into the format excepted by tax_bill() ---------------
  
  future_pins_dt <- future_pins %>%
    select(year, pin, class, av, eav, starts_with("exe_")) %>%
    mutate(across(c(av, eav), as.integer)) %>%
    filter(pin %in% pin_universe) %>%
    mutate(year = 2024) %>%
    data.table::setDT(key = c("year", "pin"))
  
  # Prep agency_dt. Combine base and levy for each agency
  future_agencies_dt <- future_levies %>%
    select(-ends_with("_change"), -agency_total_eav) %>%
    left_join(future_agency_tots, by = c("year", "agency_num")) %>%
    relocate(agency_total_eav, .before = "agency_total_ext") %>%
    mutate(year = 2024) %>%
    data.table::setDT(key = c("year", "tax_code", "agency_num"))
  
  # Base year tax bills -----------------------------------------------------
  
  future_bills <- tax_bill(
    year_vec = 2024,
    pin_vec = pin_universe,
    tax_code_vec = lookup_tax_code(2021, pin_universe),
    agency_dt = future_agencies_dt,
    pin_dt = future_pins_dt
  )
  
  future_bills_coded <- future_bills %>%
    left_join(., pin_data %>% select(pin,class,cbd,tif_district,subgroup_label,group_label), 
              by = c('pin'='pin'))  %>%
    filter(!(class.x %in% c('0','192'))) %>%
    mutate(class_type =   case_when(group_label == "2+ story office/retail" & cbd == 1 ~ 'Downtown multistory office/retail',
                                    group_label == "2+ story office/retail" & cbd == 0 ~ 'Chicago multistory office/retail',
                                    
                                    group_label == "Residential" & cbd == 1 ~ 'All Chicago residential',
                                    group_label == "Residential" & cbd == 0 ~ 'All Chicago residential',
                                    
                                    group_label %in% c("Non-commercial / non-residential","Other commercial") & cbd == 1 ~ 'Downtown commercial/industrial',
                                    group_label %in% c("Non-commercial / non-residential","Other commercial") & cbd == 0 ~ 'Chicago commercial/industrial',
                                    
                                    group_label == "2+ story office/retail" ~ 'Chicago commercial/industrial',
                                    group_label == "Residential" ~ 'All Chicago residential',
                                    group_label %in% c("Non-commercial / non-residential","Other commercial") ~ 'Chicago commercial/industrial',
                                    
                                    TRUE ~ "Other"))
  
  future_bills_sum <- future_bills_coded %>%
    mutate(pin_agency_count = 1) %>%
    group_by(pin, year, class_type) %>%
    summarize(
      av = sum(av), #  first(av)
      final_tax = sum(final_tax),
      pin_agency_count = sum(pin_agency_count),
      .groups = 'keep'
    ) %>%
    ungroup() %>%
    mutate(pin_count = 1, av = av/pin_agency_count) %>%
    group_by(year, class_type) %>%
    summarize(
      av = sum(av),
      final_tax = sum(final_tax),
      pin_count = sum(pin_count),
      .groups = 'keep') %>%
    ungroup() %>%
    mutate(average_tax = final_tax / pin_count,
           av_average = av/pin_count,
           av_share = av/sum(av),
           final_tax_share = final_tax/sum(final_tax),
           pin_count_share = pin_count/sum(pin_count)) %>%
    #mutate(reference_year = "2021 Peak") %>%
    rename_with(.cols = c("av", "final_tax", "pin_count", "average_tax", "av_average", "av_share", "final_tax_share", "pin_count_share"), 
                .fn = ~ paste0("", .x, "_2024"))
  
  
}

cre_40 = run_cre_projection(change_fraction = .6)
cre_30 = run_cre_projection(change_fraction = .7)
cre_20 = run_cre_projection(change_fraction = .8)
cre_10 = run_cre_projection(change_fraction = .9)


cre_data <- rbind(
  cre_40 %>% mutate(reduction = '40%'),
  cre_30 %>% mutate(reduction = '30%'),
  cre_20 %>% mutate(reduction = '20%'),
  cre_10 %>% mutate(reduction = '10%')
)

# -------------------------------------------------------------------------

class_type_fct <- c("Downtown multistory office/retail", "Downtown commercial/industrial", "All Chicago residential", "Chicago multistory office/retail", "Chicago commercial/industrial")

combined_bills_sum <- cre_data %>%
  left_join(., base_bills_sum, by = c('class_type')) %>%
  mutate(av_diff = av_2024-av_2021,
         final_tax_diff = final_tax_2024-final_tax_2021,
         avg_tax_diff = average_tax_2024-average_tax_2021,
         av_share_diff = av_share_2024-av_share_2021,
         final_tax_share_diff = final_tax_share_2024-final_tax_share_2021) %>%
  mutate(
    av_share_diff_label = case_when(av_share_diff > 0 ~ paste0('+', label_percent(accuracy = .1)(av_share_diff)),
                                    TRUE ~ label_percent(accuracy = .1)(av_share_diff)),
    final_tax_share_diff_label = case_when(final_tax_share_diff > 0 ~ paste0('+',  round(final_tax_share_diff*100,1),'pp' ),
                                           TRUE ~ paste0(round(final_tax_share_diff*100,1),'pp')  ),
    
    av_diff_label = case_when(av_diff > 0 ~ paste0('+', label_dollar(accuracy = .1, scale_cut = cut_short_scale())(av_diff)),
                              TRUE ~ label_dollar(accuracy = .1, scale_cut = cut_short_scale())(av_diff)),
    final_tax_diff_label = case_when(final_tax_diff > 0 ~ paste0('+', label_dollar(accuracy = .1, scale_cut = cut_short_scale())(final_tax_diff)),
                                     TRUE ~ label_dollar(accuracy = .1, scale_cut = cut_short_scale())(final_tax_diff)),
    avg_tax_diff_label = case_when(avg_tax_diff > 0 ~ paste0('+', label_dollar(accuracy = .1, scale_cut = cut_short_scale())(avg_tax_diff)),
                                   TRUE ~ label_dollar(accuracy = .1, scale_cut = cut_short_scale())(avg_tax_diff))
  ) %>%
  mutate(class_type_raw = class_type, 
         class_type_rev = factor(str_wrap(class_type,20), 
                             levels = rev( str_wrap(class_type_fct,20))),
         class_type = factor(str_wrap(class_type,20), 
                                 levels = str_wrap(class_type_fct,20)))
         


# Dot chart ---------------------------------------------------------------


cre_dot_plot <- function(reduction_level) { 
  (cre_dots <- ggplot() +
     
     geom_point(data = combined_bills_sum %>% filter(reduction == reduction_level), 
                aes(x = final_tax_share_2024, 
                    y = class_type_rev, fill = class_type_rev, color = class_type_rev,  size = final_tax_2024), 
                alpha = 1) +
     
     geom_point(data = combined_bills_sum %>% filter(reduction == reduction_level), 
                aes(x = final_tax_share_2021, 
                    y = class_type_rev), 
                fill = 'black', color = 'black',  size = 1.5, alpha = 1) +
     
     geom_segment(data = combined_bills_sum %>% filter(reduction == reduction_level, abs(final_tax_share_diff) > 0.001),
                  aes(x=final_tax_share_2021, xend=final_tax_share_2024, y=class_type_rev, yend=class_type_rev), 
                  linewidth = .8, arrow = arrow(length=unit(0.15,"cm"))) +
     
     geom_text(data = combined_bills_sum %>% filter(reduction == reduction_level), 
               aes(x = final_tax_share_2024,
                   y = class_type_rev,
                   label = ifelse((abs(final_tax_share_diff) > 0.001), 
                                  paste0('  ', round(final_tax_share_2024 *100,1),'% (',final_tax_share_diff_label,')  '),
                                  '')), size =3, hjust = .5, vjust = 3.5, fontface = 'bold') + 
     
     geom_text(data = combined_bills_sum %>% filter(reduction == reduction_level), 
               aes(x = final_tax_share_2024,
                   y = class_type_rev,
                   label = ifelse((abs(final_tax_share_diff) > 0.001), 
                                  paste0('',avg_tax_diff_label,' avg. change'),
                                  '')), size =3, hjust = .5, vjust = -3, fontface = 'bold') + 
     
     scale_x_continuous(expand = c(.001, .001), breaks = c(0,.1,.2,.3,.4,.5,.6), limits = c(-.03,.63),  labels = scales::percent ) + 
     scale_fill_manual(name = '', values = rev(c('#4D96FF','#6BCB77','#FFD93D','#FF6B6B','#845EC2'))) + # '#ffa034'
     scale_color_manual(name = '', values = rev(c('#4D96FF','#6BCB77','#FFD93D','#FF6B6B','#845EC2'))) + # '#ffa034'
     scale_size_continuous(name = 'Total property taxes', breaks = c(1000000000,2000000000,3000000000),
                           range = c(5,20), labels = label_comma(accuracy = .1, scale =  0.000000001, suffix = "B") ) +
     labs(y = '', x = 'Share of property tax bill in Chicago', 
          subtitle = paste0('',reduction_level,' decline in downtown CRE')) + 
     guides(fill = "none", color = 'none') +
     theme_classic() +
     theme(panel.grid.major = element_line(colour = "#333333", linewidth = 0.1),
           plot.margin = margin(t = 10, r = 0, b = 0, l = 0, unit = "pt"),
           legend.title = element_text(hjust = .5),
           axis.text.y = element_text(size = 11, color = '#333333'),
           axis.text.x = element_text(size = 12, color = '#333333'),
           axis.title = element_text(size = 14, color = '#333333'),
           legend.position = 'bottom',
           plot.subtitle = element_text(size = 12, face="bold", hjust=.5))) 
}

(cre_40_dots <- cre_dot_plot(reduction_level = '40%'))
(cre_30_dots <- cre_dot_plot(reduction_level = '30%'))
(cre_20_dots <- cre_dot_plot(reduction_level = '20%'))
(cre_10_dots <- cre_dot_plot(reduction_level = '10%'))

(cre_grid <- cre_10_dots + cre_20_dots + cre_30_dots + cre_40_dots + 
  plot_layout( guides = 'collect', ncol = 2) &
  theme(legend.position = 'none'))


ggsave(plot = cre_grid, filename = 'outputs/cre_grid.pdf', width = 18, height = 9)


# Line chart --------------------------------------------------------------


(line_cre_share <- ggplot() +
  geom_line(data = combined_bills_sum,
            aes(x= reduction, y = final_tax_share_2024, group = class_type, color = class_type),
            linewidth = 1) + 
  geom_point(data = combined_bills_sum,
             aes(x= reduction, y = final_tax_share_2024, group = class_type, color = class_type),
             size = 1.5) +
  geom_text(data = combined_bills_sum, aes(x= reduction, y = final_tax_share_2024, label = paste0(round(final_tax_share_2024*100,1),'%')), 
            check_overlap = TRUE, vjust = -.5) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,.6), labels = scales::percent) + 
  scale_x_discrete(expand = c(0.05, 0.05)) +
  scale_color_manual(name = '', values = c('#4D96FF','#6BCB77','#FFD93D','#FF6B6B','#845EC2','#ffa034')) +
  labs(#subtitle = 'Projected 2024 residential property tax burden\ndue to declines in downtown commercial real estate value',
    x = 'Projected decline in downtown Commercial Real Estate (CRE) valuations relative to 2021 peak', 
    y = 'Share of total property tax burden in Chicago') +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme_classic() + 
  theme(legend.position = 'bottom',
        plot.margin = margin(t = 10, r = 10, b = 0, l = 10, unit = "pt"),
        axis.text = element_text(size = 11, color = 'black'),
        legend.text =  element_text(size = 11, color = 'black'),
        axis.title =   element_text(size = 11, color = 'black'),
        legend.title = element_blank(),
        plot.subtitle = element_text(hjust = .5, face = 'bold')) )

ggsave(plot = line_cre_share,
       filename = 'outputs/line_cre_share.pdf',
       width = 10, height = 6)


low_bills <- c("All Chicago residential", "Chicago commercial/industrial")
high_bills <- c("Chicago multistory office/retail", "Downtown commercial/industrial", "Downtown multistory office/retail")

(line_cre_avg <- (ggplot() +
    geom_line(data = combined_bills_sum %>% filter(class_type_raw %in% high_bills),
              aes(x= reduction, y = average_tax_2024, group = class_type, color = class_type),
              linewidth = 1) + 
    geom_point(data = combined_bills_sum %>% filter(class_type_raw %in% high_bills),
               aes(x= reduction, y = average_tax_2024, group = class_type, color = class_type),
               size = 1.5) +
    geom_text(data = combined_bills_sum %>% filter(class_type_raw %in% high_bills), aes(x= reduction, y = average_tax_2024, label = label_dollar(accuracy = 1L, scale_cut = cut_short_scale())(average_tax_2024) ), 
              check_overlap = TRUE, vjust = -.5) +
    scale_y_continuous(expand = c(0, 0), limits = c(-10, 800000), labels = dollar_format()) + 
    scale_x_discrete(expand = c(0.05, 0.05)) +
    scale_color_manual(name = '', values = c('#4D96FF','#6BCB77','#FF6B6B')) +
    labs(#subtitle = 'Projected 2024 residential property tax burden\ndue to declines in downtown commercial real estate value',
      x = 'Projected decline in downtown Commercial Real Estate (CRE) valuations relative to 2021 peak', 
      y = 'Average property tax bill in Chicago') +
    guides(color=guide_legend(nrow=1,byrow=TRUE)) +
    theme_classic() + 
    theme(legend.position = 'bottom',
          plot.margin = margin(t = 10, r = 10, b = 0, l = 10, unit = "pt"),
          axis.text = element_text(size = 11, color = 'black'),
          legend.text =  element_text(size = 11, color = 'black'),
          axis.title =   element_text(size = 11, color = 'black'),
          legend.title = element_blank(),
          plot.subtitle = element_text(hjust = .5, face = 'bold'))) + 
    (ggplot() +
    geom_line(data = combined_bills_sum %>% filter(class_type_raw %in% low_bills),
              aes(x= reduction, y = average_tax_2024, group = class_type, color = class_type),
              linewidth = 1) + 
    geom_point(data = combined_bills_sum %>% filter(class_type_raw %in% low_bills),
               aes(x= reduction, y = average_tax_2024, group = class_type, color = class_type),
               size = 1.5) +
    geom_text(data = combined_bills_sum %>% filter(class_type_raw %in% low_bills), aes(x= reduction, y = average_tax_2024, label = label_dollar(accuracy = .01, scale_cut = cut_short_scale())(average_tax_2024) ), 
              check_overlap = TRUE, vjust = -.5) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 23000) , labels = dollar_format()) + 
    scale_x_discrete(expand = c(0.05, 0.05)) +
    scale_color_manual(name = '', values = c('#FFD93D','#845EC2')) +
    labs(#subtitle = 'Projected 2024 residential property tax burden\ndue to declines in downtown commercial real estate value',
      x = 'Projected decline in downtown Commercial Real Estate (CRE) valuations relative to 2021 peak', 
      y = 'Average property tax bill in Chicago') +
    guides(color=guide_legend(nrow=1,byrow=TRUE)) +
    theme_classic() + 
    theme(legend.position = 'bottom',
          plot.margin = margin(t = 10, r = 10, b = 0, l = 10, unit = "pt"),
          axis.text = element_text(size = 11, color = 'black'),
          legend.text =  element_text(size = 11, color = 'black'),
          axis.title =   element_text(size = 11, color = 'black'),
          legend.title = element_blank(),
          plot.subtitle = element_text(hjust = .5, face = 'bold')) ) )


ggsave(plot = line_cre_avg,
       filename = 'outputs/line_cre_avg.pdf',
       width = 16, height = 7)


# Data files --------------------------------------------------------------

write_csv(combined_bills_sum %>%
            select(class_type_raw, reduction, average_tax_2021, average_tax_2024, avg_tax_diff, final_tax_share_2021, final_tax_share_2024, final_tax_share_diff, av_share_2021, av_share_2024, av_share_diff, av_2021, av_2024, av_diff, final_tax_2021, final_tax_2024, final_tax_diff, 
                   av_average_2021, av_average_2024, pin_count_2021, pin_count_2024) %>%
            rename(`Property class type`= class_type_raw, 
                   `Forecasted reduction in downtown CRE valuations`= reduction, 
                   `Average property tax bill, 2021`= average_tax_2021, 
                   `Average property tax bill, 2024`= average_tax_2024, 
                   `Change in average property tax bill, 2021-24`= avg_tax_diff, 
                   `Total property tax share, 2021`= final_tax_share_2021, 
                   `Total property tax share, 2024`= final_tax_share_2024, 
                   `Change in total property tax share, 2021-24`= final_tax_share_diff, 
                   `Assessed value share, 2021`= av_share_2021, 
                   `Assessed value share, 2024`= av_share_2024, 
                   `Change in assessed value share, 2021-24`= av_share_diff, 
                   `Assessed value, 2021 (10% of residential, 25% of commercial)`= av_2021, 
                   `Assessed value, 2024 (10% of residential, 25% of commercial)`= av_2024, 
                   `Change in assessed value, 2021-24`= av_diff,
                   `Average assessed value, 2021` = av_average_2021, 
                   `Average assessed value, 2024` = av_average_2024,
                   `PIN count, 2021` = pin_count_2021,
                   `PIN count, 2024`= pin_count_2024,
                   `Total property tax, 2021`= final_tax_2021, 
                   `Total property tax, 2024`= final_tax_2024, 
                   `Change in total property tax, 2021-24`= final_tax_diff)
          , 'outputs/cre_forecast_data.csv')

