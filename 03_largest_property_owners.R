#####################################
# Emma property values analysis:
# Summarize largest property owners in Emma
# Ginger Kowal February 2022
#####################################

source("C:/Users/ginge/Documents/Python/property_values/scripts/00_packages_functions.R")

# identify vacant land zoned for mobile homes
Emma_parcels_zone_codes <- read.csv(
  paste0(wd$processed_data, 'Emma_parcels_join_zoning_code_2021.csv'))
# parcels in Emma that are vacant, zoned for manufactured home parks
vacant_mfg_park <- subset(
  Emma_parcels_zone_codes,
  (Ginger_class == 'vacant' & (ZoningCode == 'R-3' | ZoningCode == 'OU')))
# join with parcel data to get owner, etc
parcels_df <- read.csv(paste0(wd$raw_data, "Parcels_by_intersection_Emma_and_buffer.csv"))
vacant_mfg_park_info <- merge(vacant_mfg_park, parcels_df, all.x=TRUE)
cols_to_keep <- c(
  'PIN', 'Class', 'ZoningCode', 'Description', 'Owner', 'HouseNumbe', 'StreetName',
  'StreetType', 'Acreage', 'CareOf', 'Address', 'CityName', 'State', 'Zipcode',
  'TaxValue', 'PropCard')
vacant_mfg_park_info <- vacant_mfg_park_info[, cols_to_keep]
write.csv(
  vacant_mfg_park_info, paste0(wd$output, 'Emma_vacant_zoned_mobile_home_parks.csv'),
  row.names=FALSE)

# top 10 owners of property in Emma, by area
# properties within Emma proper, no buffer (2156 parcels)
# TODO move this into 01_prep_data.R
useful_cols <- c(
  'objectid', 'PIN', 'Owner', 'Acreage', 'Class', 'HouseNumbe', 'StreetName',
  'CityName', 'CareOf', 'Address')
parcels_Emma_df <- parcels_df[
  parcels_df$Emma_nobuf == 'Emma_within', useful_cols]
class_codes <- read.csv(paste0(wd$processed_data, "Emma_unique_class_codes.csv"))
parcels_codes <- merge(parcels_Emma_df, class_codes, by='Class')

# investment properties: commercial + vacant + residential with LLC type name
invest_prop <- rbind(
  parcels_codes[
    (parcels_codes$Ginger_class == 'commercial') |
      (parcels_codes$Ginger_class == 'vacant'), ],
  subset(parcels_codes[(parcels_codes$Ginger_class == 'residential'), ], grepl('LLC', Owner)))

# duplicate owners identified inexactly, by eye
invest_prop['Owner'][invest_prop['Owner'] == 'AOC HOLDING LLC'] <- 'AOC HOLDINGS LLC'
invest_prop['Owner'][invest_prop['Owner'] == 'C2 INVESTMENTS LLC'] <- 'C2 HOLDING GROUP LLC'
invest_prop['Owner'][invest_prop['Owner'] == 'C2 INVESTMENTS LLC'] <- 'C2 HOLDING GROUP LLC'
invest_prop['Owner'][invest_prop['Owner'] == 'DJ3 LLC'] <- 'DJ3 DELAWARE LLC'
invest_prop['Owner'][invest_prop['Owner'] == 'DR & DA INVESTMENTS INC BECKY I MARTIN & WINSTON T'] <- 'DR & DA INVESTMENTS INC'
invest_prop['Owner'][invest_prop['Owner'] == 'DR & DA INVESTMENTS LLC;MARTIN BECKY IVESTER'] <- 'DR & DA INVESTMENTS INC'
invest_prop['Owner'][invest_prop['Owner'] == 'WINSTON MARTIN (ETAL) DR&DA INVESTMENTS LLC'] <- 'DR & DA INVESTMENTS INC'
invest_prop['Owner'][invest_prop['Owner'] == 'GREEN HILLS CEM ASSC INC'] <- 'GREEN HILL CEMETERY ASSOCIATION'
invest_prop['Owner'][invest_prop['Owner'] == 'KISER INVESTMENTS OF ASHEVILLE LLC'] <- 'KISER INVESTMENTS'
invest_prop['Owner'][invest_prop['Owner'] == 'LASHER PROPERTIES LLC DBA CREST MOUNTAIN COMMERCIAL PROP'] <- 'LASHER PROPERTIES LLC'
invest_prop['Owner'][invest_prop['Owner'] == 'QUENTIN K MILLER ET AL'] <- 'MILLER QUENTIN K'
invest_prop['Owner'][invest_prop['Owner'] == 'RICKER CARL H JR;RICKER JANIS L'] <- 'RICKER CARL H JR'
invest_prop['Owner'][invest_prop['Owner'] == 'THE ROBERSON GROUP LLCD'] <- 'THE ROBERSON GROUP LLC'

# sum up acreage by owner
acreage_by_owner <- aggregate(Acreage~Owner, data=invest_prop, FUN=sum)

# sum up value by owner
values_2019 <- subset(
  values_df, (TaxYear == 2019 & PIN %in% invest_prop$PIN),
  select=c(PIN, Acres, LandVal, BldgVal))
invest_prop_values <- merge(invest_prop, values_2019, by='PIN', all=TRUE)
invest_prop_values$totalVal <- invest_prop_values$BldgVal + invest_prop_values$LandVal
totalval_by_owner <- aggregate(totalVal~Owner, data=invest_prop_values, FUN=sum)
landval_by_owner <- aggregate(LandVal~Owner, data=invest_prop_values, FUN=sum)

# calculate rank by owner for acreage, total val, and land val
biggest_owners <- merge(acreage_by_owner, totalval_by_owner, all=TRUE)
biggest_owners <- merge(biggest_owners, landval_by_owner, all=TRUE)
biggest_owners$acreage_rank <- rank(-biggest_owners$Acreage, ties.method='min')
biggest_owners$landval_rank <- rank(-biggest_owners$LandVal, ties.method='min')
biggest_owners$totalval_rank <- rank(-biggest_owners$totalVal, ties.method='min')
write.csv(
  biggest_owners, "C:/Users/ginge/Documents/PODER_Emma/Results/biggest_owners_commercial_vacant_residential_LLC.csv",
  row.names=FALSE)
