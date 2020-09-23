library(tidyverse) ; library(sf)

# Local authority district #

# Source: Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/local-authority-districts-may-2020-boundaries-uk-bgc/geoservice
# Licence: Open Government Licence 3.0

st_read("https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Local_Authority_Districts_May_2020_Boundaries_UK_BGC/MapServer/0/query?where=UPPER(lad20nm)%20like%20'%25TRAFFORD%25'&outFields=lad20cd,lad20nm&outSR=4326&f=geojson") %>% 
  select(lad20cd, lad20nm) %>% 
  st_write("local_authority.geojson")

# ------------------------------------------------------------------------------

# Electoral wards #

# Ward boundaries
# Source: Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/wards-december-2019-boundaries-ew-bgc/geoservice
# Licence: Open Government Licence 3.0

lookup <- read_csv("https://opendata.arcgis.com/datasets/e169bb50944747cd83dcfb4dd66555b1_0.csv") %>% 
  filter(LAD19NM == "Trafford") %>% 
  pull(WD19CD)

ward_boundaries <- st_read(paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Wards_December_2019_Boundaries_EW_BGC/MapServer/0/query?where=", 
               URLencode(paste0("wd19cd IN (", paste(shQuote(lookup), collapse = ", "), ")")), 
               "&outFields=wd19cd,wd19nm,long,lat&outSR=4326&f=json")) %>% 
  rename(lon = long)

# Mid-2018 population estimates by ward
# Source: Nomis / ONS
# URL: https://www.nomisweb.co.uk/datasets/pestsyoaoa
# Licence: Open Government Licence 3.0

ward_population <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2010_1.data.csv?geography=1660945005...1660945019,1660945021,1660945020,1660945022...1660945025&date=latest&gender=0&c_age=200&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,measures_name,obs_value,obs_status_name") %>% 
  select(wd19cd = GEOGRAPHY_CODE, population = OBS_VALUE) 

# Join datasets and write results -------------------
left_join(ward_boundaries, ward_population, by = "wd19cd") %>% 
  select(wd19cd, wd19nm, population, lon, lat) %>% 
  st_write("wards.geojson")

# ------------------------------------------------------------------------------

# Middle-layer Super Output Areas #

# MSOA names
# Source: House of Commons Library 
# URL: https://visual.parliament.uk/msoanames
# Licence: Open Government Licence 3.0

msoa_names <- read_csv("https://visual.parliament.uk/msoanames/static/MSOA-Names-1.4.0.csv") %>%
  filter(Laname == "Trafford") %>% 
  select(msoa11cd, msoa11hclnm)

# MSOA boundaries
# Source: Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/middle-layer-super-output-areas-december-2011-boundaries-ew-bsc
# Licence: Open Government Licence 3.0

msoa_boundaries <- st_read("https://opendata.arcgis.com/datasets/87aa4eb6393644768a5f85929cc704c2_0.geojson") %>% 
  filter(str_detect(MSOA11NM, "Trafford")) %>%
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
  select(msoa11cd = MSOA11CD, msoa11nm = MSOA11NM, lon, lat)

# Mid-2018 population estimates by MSOA
# Source: Nomis / ONS
# URL: https://www.nomisweb.co.uk/datasets/pestsyoaoa
# Licence: Open Government Licence 3.0

msoa_population <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2010_1.data.csv?geography=1245709240...1245709350,1245715048,1245715058...1245715063,1245709351...1245709382,1245715006,1245709383...1245709577&date=latest&gender=0&c_age=200&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,measures_name,obs_value,obs_status_name") %>% 
  select(msoa11cd = GEOGRAPHY_CODE, population = OBS_VALUE) 

# Join datasets and write results -------------------
left_join(msoa_boundaries, msoa_names, by = "msoa11cd") %>% 
  left_join(msoa_population, by = "msoa11cd") %>% 
  select(msoa11cd, msoa11nm, msoa11hclnm, population, lon, lat) %>% 
  st_write("msoa.geojson")

# ------------------------------------------------------------------------------

# Lower-layer Super Output Areas #

# LSOA boundaries
# Source: Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-areas-december-2011-boundaries-ew-bgc
# Licence: Open Government Licence 3.0

lsoa_boundaries <- st_read("https://ons-inspire.esriuk.com/arcgis/rest/services/Census_Boundaries/Lower_Super_Output_Areas_December_2011_Boundaries/MapServer/2/query?where=UPPER(lsoa11nm)%20like%20'%25TRAFFORD%25'&outFields=lsoa11cd,lsoa11nm&outSR=4326&f=geojson") %>%
  select(lsoa11cd, lsoa11nm) 

# Mid-2018 population estimates by LSOA
# Source: Nomis / ONS
# URL: https://www.nomisweb.co.uk/datasets/pestsyoaoa
# Licence: Open Government Licence 3.0

lsoa_population <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2010_1.data.csv?geography=1249908541...1249908544,1249908617,1249908620,1249908548...1249908551,1249908553,1249908573,1249908618,1249908619,1249908621,1249908545...1249908547,1249908577,1249908578,1249908554...1249908556,1249908560,1249908563,1249908587,1249908589,1249908591,1249908614,1249908615,1249908557...1249908559,1249908562,1249908564,1249908588,1249908590,1249908611,1249908616,1249908630...1249908634,1249908552,1249908561,1249908565,1249908629,1249908635,1249908574...1249908576,1249908612,1249908613,1249908579,1249908581,1249908582,1249908586,1249908597,1249908598,1249908601...1249908603,1249908530,1249908531,1249908596,1249908606,1249908610,1249908529,1249908592...1249908595,1249908580,1249908583...1249908585,1249908604,1249908536...1249908540,1249908534,1249908605,1249908607...1249908609,1249908523,1249908524,1249908527,1249908599,1249908600,1249908522,1249908526,1249908528,1249908532,1249908535,1249908533,1249908622,1249908627,1249908628,1249908642,1249908636,1249908638...1249908640,1249908643,1249908525,1249908623,1249908624,1249908637,1249908641,1249908510,1249908512,1249908521,1249908625,1249908626,1249908506...1249908508,1249908511,1249908519,1249908515,1249908516,1249908520,1249908571,1249908572,1249908509,1249908513,1249908514,1249908517,1249908518,1249908566...1249908570&date=latest&gender=0&c_age=200&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,measures_name,obs_value,obs_status_name") %>% 
  select(lsoa11cd = GEOGRAPHY_CODE, population = OBS_VALUE) 

# Join datasets and write results -------------------
left_join(lsoa_boundaries, lsoa_population, by = "lsoa11cd") %>% 
  select(lsoa11cd, lsoa11nm, population) %>% 
  st_write("lsoa.geojson")

# ------------------------------------------------------------------------------

# Postcodes #

# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/ons-postcode-directory-may-2020
# Licence: Custom Licence

url <- "https://www.arcgis.com/sharing/rest/content/items/a644dd04d18f4592b7d36705f93270d8/data"
download.file(url, dest = "ONSPD_AUGUST_2020_UK.zip")
unzip("ONSPD_AUGUST_2020_UK.zip", exdir = ".")
file.remove("ONSPD_AUGUST_2020_UK.zip")

postcodes <- read_csv("Data/ONSPD_AUG_2020_UK.csv") %>% 
  filter(oslaua == "E08000009") %>% 
  select(postcode = pcds,
         oa11cd = oa11,
         lsoa11cd = lsoa01,
         msoa11cd = msoa01,
         wd19cd = osward,
         lon = long,
         lat = lat) %>% 
  mutate(postcode = str_replace_all(postcode, fixed(" "), "")) %>% 
  # join House of Commons Library MSOA names
  left_join(msoa_names, by = "msoa11cd") %>% 
  # join ward names
  left_join(select(st_drop_geometry(ward_boundaries), wd19cd, wd19nm), by = "wd19cd") %>% 
  select(postcode, oa11cd, lsoa11cd, msoa11cd, msoa11hclnm, wd19cd, wd19nm, lon, lat)

write_csv(postcodes, "postcodes.csv")
