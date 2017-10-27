# read in drought dataset
library(readr)
df <- read_csv("CA Drought Data 20150925-20170925.csv", 
               col_types = cols(ReleaseDate = col_date(format = "%Y%m%d"), 
                                ValidEnd = col_date(format = "%Y-%m-%d"), 
                                ValidStart = col_date(format = "%Y-%m-%d")))

df

# Sanity check: should have all 58 counties
library(dplyr)
df %>% summarize(distinct = n_distinct(County))

# Sanity check: each county should have ~104 observations
df %>% group_by(County) %>%
    summarize(count = n()) %>%
    distinct(count)

# select important rows
df_levels <- df %>% select(County, Date = ValidStart, D0:D4)

# dplyr practice
# 1. Select the rows such that D4 is 100
df_levels %>% filter(D4 == 100)

# 2. Which counties experienced 100% land area in D4 at any time?
df_levels %>% filter(D4 == 100) %>%
    distinct(County)

# 3. Which county experienced the most number of weeks with 100% land area in D4?
df_levels %>% 
    filter(D4 == 100) %>% 
    group_by(County) %>% 
    summarize(count = n()) %>%
    arrange(desc(count))

# 4. For the date "2017-07-18", return the entries in descending order of D0.
df_levels %>% filter(Date == "2017-07-18") %>% arrange(desc(D0))

# filter for just Santa Clara County
df_county <- df_levels %>% filter(County == "Santa Clara County")

# make histogram for D0
library(ggplot2)
ggplot(data = df_county) +
    geom_histogram(aes(x = D0))

# make line plot of D0 vs. Date
ggplot(data = df_county) +
    geom_line(mapping = aes(x = Date, y = D0))

# make area plot of D0 vs. Date
ggplot(data = df_county) +
    geom_line(mapping = aes(x = Date, y = D0))

# gather drought levels
library(tidyr)
df_county <- df_county %>% gather(D0:D4, key = "Drought level", 
                                  value = "Percent of land area")

# make area plot
ggplot(data = df_county) + 
    geom_area(mapping = aes(x = Date, y = `Percent of land area`, 
                            fill = `Drought level`)) +
    scale_fill_brewer(palette = "YlOrRd")