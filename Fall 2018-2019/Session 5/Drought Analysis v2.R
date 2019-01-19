# This script takes a drought level dataset from the United States Drought
# Monitor website and plots an area plot of % land area at each drought level
# vs. time for a particular county.

# import packages
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

# read in drought dataset
df <- read_csv("CA Drought Data 20150924-20180924.csv", 
               col_types = cols(ReleaseDate = col_date(format = "%Y%m%d"), 
                                ValidEnd = col_date(format = "%Y-%m-%d"), 
                                ValidStart = col_date(format = "%Y-%m-%d")))

# Sanity check: should have all 58 counties
df %>% summarize(distinct = n_distinct(County))

# Sanity check: each county should have ~156 observations
df %>% group_by(County) %>%
    summarize(count = n()) %>%
    distinct(count)

# select important rows
df_levels <- df %>% select(County, Date = ValidStart, D0:D4)


# filter for just Santa Clara County
df_county <- df_levels %>% 
    filter(County == "Santa Clara County") %>%
    gather(D0:D4, key = "Drought level", 
           value = "Percent of land area")

# make area plot
ggplot(data = df_county) + 
    geom_area(mapping = aes(x = Date, y = `Percent of land area`, 
                            fill = `Drought level`)) + 
    labs(title = "Drought levels in Santa Clara County") +
    scale_fill_brewer(palette = "YlOrRd") +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
