

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
inputdir <- "/Users/cfree/Dropbox/subnational-distributions-extended-data/all_intakes" # On Chris Free's computer
datadir <- "data"
plotdir <- "figures"

# Read nutrient key
nutr_key <- readxl::read_excel(file.path(datadir, "SPADE_nutrient_key.xlsx"))


# Merge data
################################################################################

# Files
intake_files <- list.files(inputdir)

# Build file key
file_key <- tibble(filename=intake_files) %>%
  # Break filename apart
  mutate(filename_temp=filename) %>%
  mutate(filename_temp=gsub("_h_w_", "_hw_", filename_temp),
         filename_temp=gsub("omega_", "omega", filename_temp)) %>%
  tidyr::separate(col=filename_temp, into=c("country", "sex", "nutrient"), sep="_") %>%
  # Recode country
  mutate(country=stringr::str_to_title(country),
         country=recode(country,
                        "Bang"="Bangladesh",
                        "Bulg"="Bulgaria",
                        "Phil"="Philippines",
                        "Rom"="Romania",
                        "Usa"="United States of America")) %>%
  # Recode sex
  mutate(sex=recode(sex,
                    "c"="Children",
                    "hw"="Females",
                    "m"="Males",
                    "w"="Females")) %>%
  # Recode nutrient
  mutate(nutrient=gsub(".csv", "", nutrient),
         nutrient=recode(nutrient,
                         "adsugar"="Added sugar",
                         "ala"="alpha-linolenic acid",
                         "alcohol"="Alcohol",
                         "alphacarot"="alpha-Carotene",
                         "b12"="Vitamin B12",
                         "bcarot"="beta-Carotene",
                         "betacarot"="beta-Carotene",
                         "betacrypt"="beta-cryptoxanthin",
                         "calc"="Calcium",
                         "carb"="Carbohydrates",
                         "chol"="Choline",
                         "cholest"="Cholesterol",
                         "cu"="Copper",
                         "energy"="Energy",
                         "fat"="Fat",
                         "fiber"="Fiber",
                         "fola"="Folate",
                         "iod"="Iodine",
                         "iodine"="Iodine",
                         "iron"="Iron",
                         "la"="Linoleic acid",
                         "lutzea"="Lutein and zeaxanthin",
                         "lyco"="Lycopene",
                         "mang"="Manganese",
                         "mg"="Magnesium",
                         "mufa"="Monounsaturated fatty acids",
                         "na"="Sodium",
                         "niac"="Niacin",
                         "omega3"="Omega-3 fatty acids",
                         "omega6"="Omega-6 fatty acids",
                         "phos"="Phosphate",
                         "plantomega3"="Plant-based omega-3 fatty acids",
                         "pota"="Potassium",
                         "protein"="Protein",
                         "pufa"="Polyunsaturated fatty acids",
                         "retinol"="Retinol",
                         "retol"="Retinol",
                         "ribo"="Riboflavin",
                         "satfat"="Saturated fat",
                         "se"="Selenium",
                         "sfa"="Saturated fat",
                         "sucrose"="Sucrose",
                         "sugar"="Sugar",
                         "tfa"="Trans fat",
                         "tfat"="Trans fat",
                         "theo"="Theobromine",
                         "thia"="Thiamine",
                         "vita"="Vitamin A (RAE)",
                         "vitb12"="Vitamin B12",
                         "vitb6"="Vitamin B6",
                         "vitc"="Vitamin C",
                         "vitd"="Vitamin D",
                         "vite"="Vitamin E",
                         "vitk"="Vitamin K",
                         "zinc"="Zinc")) %>%
  # Add ISO3
  mutate(country=countrycode(country, "country.name", "country.name"),
         iso3=countrycode(country, "country.name", "iso3c"),
         continent=countrycode(country, "country.name", "continent")) %>%
  # Arrange
  select(filename, continent, country, iso3, sex, nutrient, everything()) %>%
  # Remove duplicates
  filter(!grepl("_h_w_", filename) & !filename%in%c("usa_m_vitb12.csv", "usa_w_vitb12.csv")) %>%
  # Remove Theobromine
  filter(nutrient!="Theobromine") %>%
  # Add units and other names
  left_join(nutr_key, by="nutrient")


# Inspect
freeR::complete(file_key)
table(file_key$continent)
table(file_key$country)
table(file_key$sex)
sort(unique(file_key$nutrient))

# Only 1 file per country, sex, nutrient?
# must report zero rows
check1 <- file_key %>%
  group_by(country, iso3, sex, nutrient) %>%
  summarize(n=n(),
            files=paste(sort(unique(filename)), collapse = ", ")) %>%
  filter(n!=1)

# Export file key
write.csv(file_key, file=file.path(datadir, "SPADE_file_key.csv"), row.names=F)


# Plot data
################################################################################

# Plot
g <- ggplot(file_key, aes(x=country, y=nutrient)) +
  facet_grid(type1+type2~sex, space="free_y", scale="free_y") +
  geom_raster() +
  # Labels
  labs(x="", y="", title="Data coverage") +
  # Theme
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g




