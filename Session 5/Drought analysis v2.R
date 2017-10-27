library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# read in drought dataset
df <- read_csv("CA Drought Data 20150925-20170925.csv", 
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

# filter for just Santa Clara County
df_county <- df_levels %>% filter(County == "Santa Clara County")

# make plot
ggplot(data = df_county) + 
    geom_area(mapping = aes(x = Date, y = `Percent of land area`, 
                            fill = `Drought level`)) +
    scale_fill_brewer(palette = "YlOrRd")