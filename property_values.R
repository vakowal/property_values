# Data analysis for PODER Emma


# years when properties were appraised (values in other years are the same, or reflect sale)
appraisal_years <- c(2017, 2013, 2009, 2005)

# property values in Emma over time
values_df <- read.csv("C:/Users/ginge/Documents/PODER_Emma/Property_values/Real_Estate_Appraisal_Tax_History_2019.csv")
colnames(values_df)[1] <- 'PIN'

# parcel PINs by intersection with Emma neighborhood and buffer
parcels_df <- read.csv("C:/Users/ginge/Documents/PODER_Emma/Property_values/Parcels_by_intersection_Emma_and_buffer.csv")

# properties inside Emma 1/4 mile buffer
parcels_subs <- parcels_df[, c(
  'objectid', 'PIN', 'Class', 'Owner', 'intersect', 'Emma_nobuf')]
# parcel intersects buffer (4320 parcels)
intersect_buf_list <- parcels_subs[, 'PIN']
# parcel within buffer (3994 parcels)
within_buf_list <- parcels_subs[parcels_subs$intersect == 'buffer_withi', 'PIN']

# change in property values over time
# isolate to appraisal years only (2009, 2013, 2017)
val_by_year <- values_df[values_df$TaxYear %in% appraisal_years, ]
# one distinct record per year
values_appraisal_years_unique <- val_by_year[
  !duplicated(val_by_year[ , c("PIN", "TaxYear")]), ]
prop_val_PIN_list <- within_buf_list  # TODO test intersect_buf_list
values_subs <- values_appraisal_years_unique[
  values_appraisal_years_unique$PIN %in% prop_val_PIN_list, ]
values_subs$total_val <- values_subs$LandVal + values_subs$BldgVal

# residential property value quartiles
codes_df <- read.csv("C:/Users/ginge/Documents/PODER_Emma/Property_values/Emma_unique_class_codes.csv")
parcels_codes <- merge(parcels_subs, codes_df, by='Class')
residential_parcels <- parcels_codes[
  parcels_codes$Ginger_class == 'residential', 'PIN']
residential_vals <- values_subs[values_subs$PIN %in% residential_parcels, ]

# change in number of parcels in bins defined by HUD affordable housing definitions for Buncombe
# HUD income and housing affordability data in this spreadsheet here:
# https://docs.google.com/spreadsheets/d/1y34G2pcEYJn3iwk071wHLL__ImYuxq_Ncqsd9UrIg6A/edit?usp=sharing
# bins <- quantile(residential_vals$total_val, probs=c(0.25, 0.5, 0.75, 1))
bins <- c(70980, 101400, 162240, 23100000) # 23100000 = maximum residential value in 2019
df_list = list()
for(year in appraisal_years) {
  q1 <- length(residential_vals[
    (residential_vals$TaxYear == year) &
      (residential_vals$total_val <= bins[1]), 'PIN'])
  q2 <- length(residential_vals[
    (residential_vals$TaxYear == year) &
      (residential_vals$total_val <= bins[1]), 'PIN'])
  q3 <- length(residential_vals[
    (residential_vals$TaxYear == year) &
      (residential_vals$total_val > bins[2]) &
      (residential_vals$total_val <= bins[3]), 'PIN'])
  q4 <- length(residential_vals[
    (residential_vals$TaxYear == year) &
      (residential_vals$total_val > bins[3]), 'PIN'])
  df <- data.frame(
    'q1'=q1, 'q2'=q2, 'q3'=q3, 'q4'=q4, 'year'=year)
  df_list[[paste(year)]] <- df
}
num_prop_wide <- do.call(rbind, df_list)
num_prop_df <- reshape(
  num_prop_wide, varying=c('q1', 'q2', 'q3', 'q4'), v.names='num',
  timevar='quartile', times=c('q1', 'q2', 'q3', 'q4'), idvar='year',
  direction='long')

num_prop_df$Parcel_value <- factor(
  num_prop_df$quartile, levels=c('q4', 'q3', 'q2', 'q1'),
  labels=c('Not Low Income ($162,240 - $23,100,000)', 'Low Income ($101,400 - $162,240)',
           'Very Low Income ($70,980 - $101,400)', 'Extremely Low Income ($0 - $70,980)'))
p <- ggplot(num_prop_df, aes(fill=Parcel_value, y=num, x=year))
p <- p + geom_bar(position="fill", stat="identity")  # position="stack" for number, position="fill" for percent
p <- p + xlab("Year") + ylab("% parcels")  # "# parcels"
p <- p + scale_x_continuous(breaks=c(2005, 2009, 2013, 2017))
print(p)
pngname <- "C:/Users/ginge/Documents/PODER_Emma/Results/percent_residential_parcel_by_HUD_limits_by_year.png"
png(filename=pngname, width=6, height=3, units='in', res=300)
print(p)
dev.off()

# calculate change in average parcel value, for different residential categories
parcels_codes <- merge(parcels_subs, codes_df, by='Class')
residential_parcels <- subset(
  parcels_codes, Ginger_class == 'residential', select=PIN)$PIN

residential_vals <- values_subs[values_subs$PIN %in% residential_parcels, ]
residential_vals_codes <- merge(residential_vals, parcels_codes, by='PIN')
mean_by_taxyear_class <- group_by(
  residential_vals_codes, TaxYear, Class) %>%
  summarize(mean_total_val = mean(total_val, na.rm=TRUE))
df_list <- list()
for(class in mean_by_taxyear_class$Class) {
  if(class %in% c(120, 312, 635, 301, 173, 311)) {  # skip condos, 'low income housing', and non-dwelling categories
    next
  }
  years <- filter(mean_by_taxyear_class, Class == class) %>%
    pull(TaxYear)
  baseline_val <- filter(
    mean_by_taxyear_class, TaxYear == 2005 & Class == class) %>%
    pull(mean_total_val)
  yearly_vals <- filter(mean_by_taxyear_class, Class == class) %>%
    pull(mean_total_val)
  change_vec <- (yearly_vals - baseline_val) / abs(baseline_val) * 100
  df <- data.frame('TaxYear'=years, 'percent_change_from_2005'=change_vec)
  df$Class <- class
  df_list[[class]] <- df
}
change_df <- do.call(rbind, df_list)
change_df <- merge(change_df, codes_df)
# num_by_class <- group_by(
#   residential_vals_codes, TaxYear, Description) %>%
#   summarize(num=n()) %>%
#   group_by(Description) %>%
#   summarize(mean_num=mean(num, na.rm=TRUE))
# num_by_class <- num_by_class[
#   (num_by_class$Description != 'CONDO') &
#   () &
#   () &
#   (), ]

p <- ggplot(change_df, aes(x=TaxYear, y=percent_change_from_2005))
p <- p + geom_point() + facet_wrap(~Description, scales='free')
# p <- p + geom_text(data=num_by_class, aes(x=2014, y=0, label=mean_num))
p <- p + scale_x_continuous(breaks=c(2005, 2013))
p <- p + ylab("Percent change from 2005")
pngname <- "C:/Users/ginge/Documents/PODER_EmmaResults/percent_change_total_val_residential_by_type.png"
png(filename=pngname, width=7, height=6, units='in', res=300)
print(p)
dev.off()

# look at change for mobile home parks with >= 3 units: what's driving this trend?
mh_parks <- subset(residential_vals_codes, Class == 416)

res_values <- subset(residential_vals_codes, Class == 100 | Class == 101)
# extract residential parcels values with a value in 2005
res_parcels_2005 <- subset(res_values, TaxYear == 2005, select=PIN)$PIN
res_values_since_2005 <- subset(res_values, PIN %in% res_parcels_2005)
write.csv(res_values_since_2005, "C:/Users/ginge/Desktop/res_values.csv")
# NEXT continue here

# calculate percent change in value for building and land separately
# land value only
subs_2005 <- values_subs[values_subs$TaxYear == 2005, 'LandVal']
landval_quantiles <- quantile(subs_2005, probs=c(0.33, 0.67, 1))
lower_third_PIN_list <- values_subs[
  (values_subs$TaxYear == 2005) &
    (values_subs$LandVal < totalval_quantiles[1]), 'PIN']
middle_third_PIN_list <- values_subs[
  (values_subs$TaxYear == 2005) &
    (values_subs$LandVal >= totalval_quantiles[1]) &
    (values_subs$LandVal < totalval_quantiles[2]), 'PIN']
upper_third_PIN_list <- values_subs[
  (values_subs$TaxYear == 2005) &
    (values_subs$LandVal >= totalval_quantiles[2]), 'PIN']
lower_third_df <- values_subs[values_subs$PIN %in% lower_third_PIN_list, ]
middle_third_df <- values_subs[values_subs$PIN %in% middle_third_PIN_list, ]
upper_third_df <- values_subs[values_subs$PIN %in% upper_third_PIN_list, ]

lower_third_summary <- data.frame(
  do.call(
    rbind, tapply(lower_third_df$LandVal, lower_third_df$TaxYear, summ_fun)))
lower_third_sum_df <- data.frame(
  cbind(TaxYear=row.names(lower_third_summary), lower_third_summary))
lower_third_sum_df$segment <- 'lower 33%'
lower_third_pc_df <- relative_to_2005(lower_third_sum_df)
middle_third_summary <- data.frame(
  do.call(
    rbind, tapply(middle_third_df$LandVal, middle_third_df$TaxYear, summ_fun)))
middle_third_sum_df <- data.frame(
  cbind(TaxYear=row.names(middle_third_summary), middle_third_summary))
middle_third_sum_df$segment <- 'middle 33%'
middle_third_pc_df <- relative_to_2005(middle_third_sum_df)
upper_third_summary <- data.frame(
  do.call(
    rbind, tapply(upper_third_df$LandVal, upper_third_df$TaxYear, summ_fun)))
upper_third_sum_df <- data.frame(
  cbind(TaxYear=row.names(upper_third_summary), upper_third_summary))
upper_third_sum_df$segment <- 'upper 33%'
upper_third_pc_df <- relative_to_2005(upper_third_sum_df)

segmented_df <- rbind(lower_third_pc_df, middle_third_pc_df, upper_third_pc_df)
segmented_resh <- reshape(
  segmented_df,
  varying=c(
    'perc_change_min', 'perc_change_max', 'perc_change_mean',
    'perc_change_median', 'perc_change_std'),
  v.names='value', timevar='metric', idvar=c('TaxYear', 'segment'),
  times=c(
    'perc_change_min', 'perc_change_max', 'perc_change_mean',
    'perc_change_median', 'perc_change_std'),
  drop=c('count'),
  direction='long')

segmented_resh$metric <- factor(
  segmented_resh$metric,
  levels=c("perc_change_min", "perc_change_max", "perc_change_mean",
           "perc_change_median", "perc_change_std"),
  labels=c('min', 'max', 'mean', 'median', 'std'))
p <- ggplot(segmented_resh, aes(x=TaxYear, y=value))
p <- p + geom_point() + facet_grid(metric~segment, scales='free')
p <- p + ylab("Percent change from 2005 (LAND VAL)")
print(p)

# top 10 owners of property in Emma, by area
# properties within Emma proper, no buffer (2156 parcels)
useful_cols <- c(
  'objectid', 'PIN', 'Owner', 'Acreage', 'Class', 'HouseNumbe', 'StreetName',
  'CityName', 'CareOf', 'Address')
parcels_Emma_df <- parcels_df[
  parcels_df$Emma_nobuf == 'Emma_within', useful_cols]
class_codes <- read.csv("C:/Users/ginge/Documents/PODER_Emma/Property_values/Emma_unique_class_codes.csv")
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

# obsolete snippets
# isolate class codes appearing in Emma
class_codes <- read.csv("C:/Users/ginge/Documents/PODER_Emma/Property_values/Buncombe_class_codes.csv")
parcels_Emma <- merge(
  parcels_df, class_codes, by.x='Class', by.y='CODE', all.x=TRUE)
class_codes_Emma <- parcels_Emma[
  !duplicated(parcels_Emma$Class), c('Class', 'Description', 'Ginger_class')]
# class codes appearing in Emma, most inclusive parcel list (intersects 0.25 mile buffer)
write.csv(class_codes_Emma, "C:/Users/ginge/Documents/PODER_Emma/Property_values/Emma_unique_class_codes.csv")

# change in quartile bins of residential values over time
df_list = list()
for(year in appraisal_years) {
  subs <- residential_vals[residential_vals$TaxYear == year, 'total_val']
  quant_tv_df <- data.frame(year=quantile(subs, probs=c(0.25, 0.5, 0.75, 1)))
  colnames(quant_tv_df) <- year
  df_list[[paste(year)]] <- quant_tv_df
}
quant_allyears_df <- data.frame(
  '2005-2017'=quantile(residential_vals$total_val, probs=c(0.25, 0.5, 0.75, 1)))
df_list[['allyears']] <- quant_allyears_df
combined_df <- do.call(cbind, df_list)
write.csv(combined_df, "C:/Users/ginge/Documents/PODER_EmmaResults/total_val_quantiles_residential_by_year.csv")
