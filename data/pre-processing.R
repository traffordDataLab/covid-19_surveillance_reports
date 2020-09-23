library(tidyverse) ; library(httr) ; library(readxl) ; library(sf)

# Mid-2019 population estimates for local authorities in England

# Source: Nomis / ONS
# URL: https://www.nomisweb.co.uk/datasets/pestsyoala
# Licence: OGL 3.0

ltla_population <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1820327937...1820328318&date=latest&gender=0&c_age=200&measures=20100&select=geography_code,obs_value") %>% 
  rename(area_code = GEOGRAPHY_CODE, population = OBS_VALUE) %>% 
  filter(str_detect(area_code, "^E")) %>% 
  # combine population estimates for Hackney and City of London / Cornwall and Isles of Scilly 
  mutate(area_code = case_when(
    as.character(area_code) %in% c("E09000012", "E09000001") ~ "E09000012", 
    as.character(area_code) %in% c("E06000052", "E06000053") ~ "E06000052", 
    TRUE ~ area_code)) %>% 
  group_by(area_code) %>% 
  summarise(population = sum(population)) 

# Mid-2019 population estimates for England and GM

# Source: ONS
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
# Licence: OGL 3.0

tmp <- tempfile(fileext = ".xls")
GET(url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid2019april2020localauthoritydistrictcodes/ukmidyearestimates20192020ladcodes.xls",
    write_disk(tmp))
ca_country_population <- read_xls(tmp, sheet = 6, skip = 4) %>% 
  filter(Name %in% c("Greater Manchester (Met County)", "ENGLAND")) %>% 
  select(area_code = Code, population = `All ages`)

bind_rows(ltla_population, ca_country_population) %>%
  write_csv("population.csv")

# Age and sex

# Source: Nomis / ONS
# URL: https://www.nomisweb.co.uk/datasets/pestsyoala
# Licence: OGL 3.0

read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=1811939363&date=latest&gender=1,2&c_age=101...191&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,measures_name,obs_value,obs_status_name") %>% 
  select(gender = GENDER_NAME, age = C_AGE_NAME, n = OBS_VALUE) %>% 
  mutate(gender = factor(gender, levels = c("Male", "Female")),
         age = parse_number(age),
         ageband = cut(age,
                       breaks = c(0,5,10,20,30,40,50,60,70,80,120),
                       labels = c("0-4","5-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+"),
                       right = FALSE)) %>% 
  group_by(gender, ageband) %>% 
  summarise(population = sum(n)) %>%
  write_csv("ageband.csv")

# Ethnicity 

# Source:  Table KS201EW, 2011 Census, ONS
# URL: https://www.nomisweb.co.uk/census/2011/ks201ew

read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_608_1.data.csv?date=latest&geography=1946157089&rural_urban=0&cell=100,200,300,400,500&measures=20100&select=date_name,geography_name,geography_code,rural_urban_name,cell_name,measures_name,obs_value,obs_status_name") %>% 
  mutate(ethnic_group = 
           case_when(
             CELL_NAME == "White" ~ "White",
             CELL_NAME == "Mixed/multiple ethnic groups" ~ "Mixed",
             CELL_NAME == "Asian/Asian British" ~ "Asian / Asian British",
             CELL_NAME == "Black/African/Caribbean/Black British" ~ "Black / Black British",
             CELL_NAME == "Other ethnic group" ~ "Other")) %>% 
  select(ethnic_group, population = OBS_VALUE) %>% 
  write_csv("ethnicity.csv")
