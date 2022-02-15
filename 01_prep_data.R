#####################################
# Emma property values analysis:
# Clean and process raw data
# Ginger Kowal February 2022
#####################################

# package imports, function definitions, and globals including project directories
source("C:/Users/ginge/Documents/Python/property_values/scripts/00_packages_functions.R")

# Parcel data
# this table contains parcels intersecting and within Emma 0.25 buffer only
parcels_df <- read.csv(paste0(wd$raw_data, "Parcels_by_intersection_Emma_and_buffer.csv"))
parcels_Emma <- subset(
  parcels_df, intersect == 'buffer_withi' | intersect == 'buffer_inter',
  select=c(PIN, Class))  # to be explicit

# isolate class codes appearing in Emma, most inclusive parcel list (intersects 0.25 mile buffer)
class_codes <- read.csv(paste0(wd$processed_data, "Emma_unique_class_codes.csv"))
parcels_Emma_with_class_codes <- merge(parcels_Emma, class_codes, by='Class', all.x=TRUE)

# Values data
values_df <- read.csv(paste0(wd$raw_data, "Real_Estate_Appraisal_Tax_History_2019.csv"))
colnames(values_df)[1] <- 'PIN'

# values in tax appraisal years
val_by_year <- values_df[values_df$TaxYear %in% appraisal_years, ]  # isolate to appraisal years only (2009, 2013, 2017)
values_appraisal_years_unique <- val_by_year[
  !duplicated(val_by_year[ , c("PIN", "TaxYear")]), ]

# restrict to parcels in Emma only
values_Emma_appraisal_years <- merge(
  parcels_Emma_with_class_codes, values_appraisal_years_unique, by='PIN')

# restrict to residential parcels only
values_Emma_residential_appraisal_years <- subset(
  values_Emma_appraisal_years, Ginger_class == 'residential')
write.csv(
  values_Emma_residential_appraisal_years,
  paste0(wd$processed_data, "values_tax_years_residential_parcels_intersect_Emma_0.25mi_buffer.csv"))

join_df <- read.csv(paste0(wd$processed_data, "parcels_zoning_intersection.csv"))
parcel_area_df <- join_df[!duplicated(join_df$PIN), c('PIN', 'par_area')]

# Join zoning data with parcel data
# identify parcels well covered by zoning data
# criteria: >=80% covered by zoning data
sum_by_parcel <- group_by(
  join_df,PIN) %>%
  summarize(sum_covered_area = sum(sub_area, na.omit=TRUE))
perc_covered_calc <- merge(sum_by_parcel, parcel_area_df, all=TRUE)
perc_covered_calc$perc_covered <- perc_covered_calc$sum_covered_area / perc_covered_calc$par_area
well_covered_by_zoning <- subset(perc_covered_calc, perc_covered >= 0.8, c('PIN'))

# identify unambiguous zoning code per parcel
# criteria: one code covers >73% of parcel area
sum_by_code_parcel <- group_by(
  join_df, ZoningCode, PIN) %>%
  summarize(sum_code_area = sum(sub_area, na.omit=TRUE))
perc_calc <- merge(sum_by_code_parcel, parcel_area_df, all=TRUE)
perc_calc$perc_of_parcel_area <- perc_calc$sum_code_area / perc_calc$par_area
unambiguous_code <- perc_calc[perc_calc$perc_of_parcel_area > 0.73, c('PIN', 'ZoningCode')]

parcel_code_df <- merge(well_covered_by_zoning, unambiguous_code, all=FALSE)
write.csv(
  parcel_code_df, paste0(wd$processed_data, 'Emma_parcels_join_zoning_code_2021.csv'),
  row.names=FALSE)
