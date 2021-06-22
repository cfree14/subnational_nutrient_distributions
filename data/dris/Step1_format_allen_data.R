

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
inputdir <- "data/dris/raw"
outputdir <- "data/dris/processed"

# Read ULs
ars_orig <- readxl::read_excel(file.path(inputdir, "Table2.xlsx"))
uls_orig <- readxl::read_excel(file.path(inputdir, "Table6.xlsx"))


# Format ARs
################################################################################

# Extract meta-data
ar_meta <- ars_orig %>%
  # Top two rows
  slice(1:2) %>%
  # Remove first two columns
  select(-c(Stage, Age)) %>%
  # Transpose
  t() %>%
  as.data.frame() %>%
 rownames_to_column(var="nutrient") %>%
  # Rename
  rename(units=V1, source=V2)

# Format average requirements (ARs)
ars <- ars_orig %>%
  # Remove top two rows
  slice(3:nrow(.)) %>%
  # Rename
  rename(stage=Stage, age_group=Age) %>%
  # Gather
  gather(key="nutrient", value="nrv", 3:ncol(.)) %>%
  # Format AR
  mutate(nrv_note=ifelse(nrv %in% c("Add 7.2 g/d", "Add 12.5 g/d"), nrv, NA),
         nrv=ifelse(nrv %in% c("Add 7.2 g/d", "Add 12.5 g/d"), NA, nrv),
         nrv=as.numeric(nrv)) %>%
  # Add sex
  mutate(sex=recode(stage,
                    "Lactation"="Females",
                    "Pregnancy"="Females")) %>%
  # Add units
  left_join(ar_meta) %>%
  mutate(nrv_type="Average requirement") %>%
  # Arrange
  select(nutrient, units, source, sex, stage, age_group, nrv_type, everything()) %>%
  arrange(nutrient, units, source, sex, stage, age_group)

# Inspect
str(ars)
freeR::complete(ars)
table(ars$stage)
table(ars$age_group)
table(ars$nutrient)


# Format ULss
################################################################################

# Extract meta-data
ul_meta <- uls_orig %>%
  # Top two rows
  slice(1:2) %>%
  # Remove first two columns
  select(-c(Stage, Age)) %>%
  # Transpose
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var="nutrient") %>%
  # Rename
  rename(units=V1, source=V2)

# Format data
uls <- uls_orig %>%
  # Remove top two rows
  slice(3:nrow(.)) %>%
  # Rename
  rename(stage=Stage, age_group=Age) %>%
  # Gather
  gather(key="nutrient", value="nrv", 3:ncol(.)) %>%
  # Format UL
  mutate(nrv=as.numeric(nrv)) %>%
  # Add sex
  mutate(sex=recode(stage,
                    "Lactation"="Females",
                    "Pregnancy"="Females")) %>%
  # Add units
  left_join(ul_meta) %>%
  mutate(nrv_type="Upper limit") %>%
  # Arrange
  select(nutrient, units, source, sex, stage, age_group, nrv_type, everything()) %>%
  arrange(nutrient, units, source, sex, stage, age_group)

# Inspect
str(uls)
freeR::complete(uls)
table(uls$stage)
table(uls$age_group)
table(uls$nutrient)


# Merge data
################################################################################

# Merge
data <- bind_rows(ars, uls)

# Inspect
str(data)
freeR::complete(data)
sort(unique(data$sex))
sort(unique(data$stage))
sort(unique(data$age_group))
sort(unique(data$nutrient))
sort(unique(data$units))
sort(unique(data$source))


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outputdir, "Allen_etal_2020_nrvs.Rds"))




