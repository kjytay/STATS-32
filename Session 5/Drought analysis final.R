# This script produces an area plot of drought levels for one county.
# Data is taken from US Drought Monitor's Comprehensive Statistics page
# (http://droughtmonitor.unl.edu/Data/DataDownload/ComprehensiveStatistics.aspx).

# PARAMETERS (to change if necessary)
infile <- "CA Drought Data 20150925-20170925.csv"  # dataset file
county <- "Monterey County"

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# read in drought dataset
df <- read_csv(infile, 
               col_types = cols(ReleaseDate = col_date(format = "%Y%m%d"), 
                                ValidEnd = col_date(format = "%Y-%m-%d"), 
                                ValidStart = col_date(format = "%Y-%m-%d")))

df

# Sanity check: should have all 58 counties
df %>% summarize(distinct = n_distinct(County))

# Sanity check: each county should have ~104 observations
df %>% group_by(County) %>%
    summarize(count = n()) %>%
    distinct(count)

# select important rows and gather drought levels
df_levels <- df %>% select(County, Date = ValidStart, D0:D4) %>%
    gather(D0:D4, key = "Drought level", value = "Percent of land area")

# filter for just the county we want
df_county <- df_levels %>% filter(County == county)

# make plot
ggplot(data = df_county) + 
    geom_area(mapping = aes(x = Date, y = `Percent of land area`, 
                            fill = `Drought level`)) +
    scale_fill_brewer(palette = "YlOrRd") + 
    labs(title = paste("Drought levels in", county)) +
    theme(plot.title = element_text(size = rel(1.5), face = "bold", hjust = 0.5),
          axis.title = element_text(size = rel(1.2)))