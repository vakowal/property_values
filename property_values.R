# Data analysis for PODER Emma
library(ggplot2)
library(dplyr)

summ_fun <- function(x) {
  c(min = min(x), max = max(x), 
    mean = mean(x), median = median(x), 
    std = sd(x), count=length(x))
}

relative_to_2005 <- function(df) {
  for (metric in c('min', 'max', 'mean', 'median', 'std')) {
    baseline_val <- df[df$TaxYear == 2005, metric]
    yearly_vals <- df[, metric]
    change_vec <- (yearly_vals - baseline_val) / abs(baseline_val) * 100
    new_col_name <- paste('perc_change_', metric, sep='')
    df[, new_col_name] <- change_vec
  }
  return(df)
}

# years when properties were appraised (values in other years are the same, or reflect sale)
appraisal_years <- c(2017, 2013, 2009, 2005)

# property values in Emma over time
values_df <- read.csv("D:/PODER_Emma/Property_values/Real_Estate_Appraisal_Tax_History_2019.csv")
colnames(values_df)[1] <- 'PIN'

# parcel PINs by intersection with Emma neighborhood and buffer
parcels_df <- read.csv("D:/PODER_Emma/Property_values/Parcels_by_intersection_Emma_and_buffer.csv")

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
codes_df <- read.csv("D:/PODER_Emma/Property_values/Emma_unique_class_codes.csv")
parcels_codes <- merge(parcels_subs, codes_df, by='Class')
residential_parcels <- parcels_codes[
  parcels_codes$Ginger_class == 'residential', 'PIN']
residential_vals <- values_subs[values_subs$PIN %in% residential_parcels, ]

# change in number of parcels in overall quantile bins
bins <- quantile(residential_vals$total_val, probs=c(0.25, 0.5, 0.75, 1))
df_list = list()
for(year in appraisal_years) {
  q1 <- length(values_subs[
    (values_subs$TaxYear == year) &
      (values_subs$total_val <= bins[1]), 'PIN'])
  q2 <- length(values_subs[
    (values_subs$TaxYear == year) &
      (values_subs$total_val <= bins[1]), 'PIN'])
  q3 <- length(values_subs[
    (values_subs$TaxYear == year) &
      (values_subs$total_val > bins[2]) &
      (values_subs$total_val <= bins[3]), 'PIN'])
  q4 <- length(values_subs[
    (values_subs$TaxYear == year) &
      (values_subs$total_val > bins[3]), 'PIN'])
  df <- data.frame(
    'q1'=q1, 'q2'=q2, 'q3'=q3, 'q4'=q4, 'year'=year)
  df_list[[paste(year)]] <- df
}
num_prop_wide <- do.call(rbind, df_list)
num_prop_df <- reshape(
  num_prop_wide, varying=c('q1', 'q2', 'q3', 'q4'), v.names='num',
  timevar='quartile', times=c('q1', 'q2', 'q3', 'q4'), idvar='year',
  direction='long')

num_prop_df$quartile <- factor(
  num_prop_df$quartile, levels=c('q1', 'q2', 'q3', 'q4'),
  labels=c('q1 ($0 - $53,100)', 'q2 ($53,100 - $100,300)',
           'q3 ($100,300 - $147,300)', 'q4 ($147,300 - $23,100,000)'))
p <- ggplot(num_prop_df, aes(fill=quartile, y=num, x=year))
p <- p + geom_bar(position="fill", stat="identity")
p <- p + xlab("Year") + ylab("% parcels")
p <- p + scale_x_continuous(breaks=c(2005, 2009, 2013, 2017))
print(p)
pngname <- "D:/PODER_Emma/Results/percent_residential_parcel_in_quantile_by_year.png"
png(filename=pngname, width=5, height=3, units='in', res=300)
print(p)
dev.off()

# calculate change in average parcel value, for different residential categories
parcels_codes <- merge(parcels_subs, codes_df, by='Class')
residential_parcels <- parcels_codes[
  parcels_codes$Ginger_class == 'residential', 'PIN']
residential_vals <- values_subs[values_subs$PIN %in% residential_parcels, ]
residential_vals_codes <- merge(residential_vals, parcels_codes, by='PIN')
mean_by_taxyear_class <- group_by(
  residential_vals_codes, TaxYear, Class) %>%
  summarize(mean_total_val = mean(total_val, na.rm=TRUE))
df_list <- list()
for(class in mean_by_taxyear_class$Class) {
  if(class == 120) {
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
num_by_class <- group_by(
  residential_vals_codes, TaxYear, Description) %>%
  summarize(num=n()) %>%
  group_by(Description) %>%
  summarize(mean_num=mean(num, na.rm=TRUE))
num_by_class <- num_by_class[num_by_class$Description != 'CONDO', ]

p <- ggplot(change_df, aes(x=TaxYear, y=percent_change_from_2005))
p <- p + geom_point() + facet_wrap(~Description, scales='free')
p <- p + geom_text(data=num_by_class, aes(x=2014, y=0, label=mean_num))
p <- p + scale_x_continuous(breaks=c(2005, 2013))
p <- p + ylab("Percent change from 2005")
pngname <- "D:/PODER_Emma/Results/percent_change_total_val_residential_by_type.png"
png(filename=pngname, width=7, height=6, units='in', res=300)
print(p)
dev.off()

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
class_codes <- read.csv("D:/PODER_Emma/Property_values/Buncombe_class_codes.csv")

# TODO identify investment properties
# commercial properties -> Ginger_class == 'commercial'
# vacant properties -> Ginger_class == 'vacant'
# rental properties -> Ginger_class == ??
# residential with an LLC type name -> Ginger_class == 'residential' && Owner.endswith('LLC')

# obsolete snippets
# isolate class codes appearing in Emma
class_codes <- read.csv("D:/PODER_Emma/Property_values/Buncombe_class_codes.csv")
parcels_Emma <- merge(
  parcels_df, class_codes, by.x='Class', by.y='CODE', all.x=TRUE)
class_codes_Emma <- parcels_Emma[
  !duplicated(parcels_Emma$Class), c('Class', 'Description', 'Ginger_class')]
# class codes appearing in Emma, most inclusive parcel list (intersects 0.25 mile buffer)
write.csv(class_codes_Emma, "D:/PODER_Emma/Property_values/Emma_unique_class_codes.csv")

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
write.csv(combined_df, "D:/PODER_Emma/Results/total_val_quantiles_residential_by_year.csv")
