#####################################
# Emma property values analysis:
# Analyze change in values of residential properties over time
# Ginger Kowal February 2022
#####################################

source("C:/Users/ginge/Documents/Python/property_values/scripts/00_packages_functions.R")

# values for residential parcels inside or intersecting Emma 0.25 mile buffer
residential_vals <- read.csv(paste0(wd$processed_data, "values_tax_years_residential_parcels_intersect_Emma_0.25mi_buffer.csv"))
residential_vals$total_val <- residential_vals$BldgVal + residential_vals$LandVal

# change in number of residential parcels in bins
# defined by HUD affordable housing definitions for Buncombe
# HUD income and housing affordability data in this spreadsheet here:
# https://docs.google.com/spreadsheets/d/1y34G2pcEYJn3iwk071wHLL__ImYuxq_Ncqsd9UrIg6A/edit?usp=sharing
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

# number of parcels in each affordability bin
p <- ggplot(num_prop_df, aes(fill=Parcel_value, y=num, x=year))
p <- p + geom_bar(position="stack", stat="identity")
p <- p + xlab("Year") + ylab("# parcels")
p <- p + scale_x_continuous(breaks=c(2005, 2009, 2013, 2017))
print(p)
pngname <- paste0(wd$output, "num_residential_parcel_by_HUD_limits_by_year.png")
png(filename=pngname, width=6, height=3, units='in', res=300)
print(p)
dev.off()

# percent of parcels in each affordability bin
p <- ggplot(num_prop_df, aes(fill=Parcel_value, y=num, x=year))
p <- p + geom_bar(position="fill", stat="identity")
p <- p + xlab("Year") + ylab("% parcels")
p <- p + scale_x_continuous(breaks=c(2005, 2009, 2013, 2017))
print(p)
pngname <- paste0(wd$output, "percent_residential_parcel_by_HUD_limits_by_year.png")
png(filename=pngname, width=6, height=3, units='in', res=300)
print(p)
dev.off()

# change in average parcel value, for different residential categories
mean_by_taxyear_class <- group_by(
  residential_vals, TaxYear, Description) %>%
  summarize(mean_total_val = mean(total_val, na.rm=TRUE))
df_list <- list()
leave_out_list <- c(
  'CONDO', 'LOW I/C HOUSING', 'NON-DWG IMPV',
  'SUBSTANDARD LOT', 'PP MH(S) OR SITE', 'RES BLDG LOT')
for(desc in mean_by_taxyear_class$Description) {
  if(desc %in% leave_out_list) {  # skip condos, 'low income housing', and non-dwelling categories
    next
  }
  years <- filter(mean_by_taxyear_class, Description == desc) %>%
    pull(TaxYear)
  baseline_val <- filter(
    mean_by_taxyear_class, TaxYear == 2005 & Description == desc) %>%
    pull(mean_total_val)
  yearly_vals <- filter(mean_by_taxyear_class, Description == desc) %>%
    pull(mean_total_val)
  change_vec <- (yearly_vals - baseline_val) / abs(baseline_val) * 100
  df <- data.frame('TaxYear'=years, 'percent_change_from_2005'=change_vec)
  df$Description <- desc
  df_list[[desc]] <- df
}
change_df <- do.call(rbind, df_list)

p <- ggplot(change_df, aes(x=TaxYear, y=percent_change_from_2005))
p <- p + geom_point() + geom_line() + facet_wrap(~Description)  #, scales='free')
p <- p + scale_x_continuous(breaks=c(2005, 2013))
p <- p + ylab("Percent change from 2005")
pngname <- paste0(wd$output, "percent_change_total_val_residential_by_type_fixedy.png")
png(filename=pngname, width=7, height=6, units='in', res=300)
print(p)
dev.off()

# change in land value per acre
residential_vals$landval_per_acre <- residential_vals$LandVal / residential_vals$Acres
mean_by_taxyear_class <- group_by(
  residential_vals, TaxYear, Description) %>%
  summarize(mean_land_val_per_acre = mean(landval_per_acre, na.rm=TRUE))
df_list <- list()
leave_out_list <- c(
  'CONDO', 'LOW I/C HOUSING', 'NON-DWG IMPV', 'TOWNHOME',
  'SUBSTANDARD LOT', 'PP MH(S) OR SITE', 'RES BLDG LOT')  # skip townhomes for val per acre
for(desc in mean_by_taxyear_class$Description) {
  if(desc %in% leave_out_list) {
    next
  }
  years <- filter(mean_by_taxyear_class, Description == desc) %>%
    pull(TaxYear)
  baseline_val <- filter(
    mean_by_taxyear_class, TaxYear == 2005 & Description == desc) %>%
    pull(mean_land_val_per_acre)
  yearly_vals <- filter(mean_by_taxyear_class, Description == desc) %>%
    pull(mean_land_val_per_acre)
  change_vec <- (yearly_vals - baseline_val) / abs(baseline_val) * 100
  df <- data.frame('TaxYear'=years, 'percent_change_from_2005'=change_vec)
  df$Description <- desc
  df_list[[desc]] <- df
}
landval_per_acre_change_df <- do.call(rbind, df_list)

p <- ggplot(landval_per_acre_change_df, aes(x=TaxYear, y=percent_change_from_2005))
p <- p + geom_point() + geom_line() + facet_wrap(~Description)  #, scales='free')
p <- p + scale_x_continuous(breaks=c(2005, 2013))
p <- p + ylab("Percent change from 2005")
print(p)
pngname <- paste0(wd$output, "percent_change_landval_per_acre_residential_by_type_fixedy.png")
png(filename=pngname, width=5.5, height=4, units='in', res=300)
print(p)
dev.off()
