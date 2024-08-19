


library(tidyverse)
library(here)
library(janitor)

wb_indicators <- read_csv(here::here("data", "wb_indicators.csv"), na=c("..", ""))
wb_metadata <- read_csv(here::here("data", "wb_indicators_metadata.csv"))

wb_indicators_long <- wb_indicators %>%
  pivot_longer(cols='2001 [YR2001]':'2020 [YR2020]',
               names_to = "year",
               values_to = "indicator_values")

wb_data_clean <- wb_indicators_long %>%
  tidyr::separate(col=year, into=c("year", "year_chr"), sep = " ") %>%
  dplyr::select(-year_chr, -"Country Code", -"Series Code")

# here the separate function first needs to know which columns to separate (col=), then what to separate them into (two arguments listed in a vector, in quotes), and then how to separate them (sep=). In our case, we want to separate the column titles from 2001 [YR2001] into two columns, 2001 and [YR2001]. These items are separated by a space, and so we put the space in quotes to indicate we want to separate the column name where the space is (sep= " ").

# we also do some more data cleaning with the dplyr::select() function. There we indicate that we want to remove those three columns by putting a '-' in front of their names.

wb_data_tidy <- wb_data_clean %>%
  tidyr::drop_na('Series Name') %>%
  tidyr::pivot_wider(names_from= 'Series Name',
                     values_from = indicator_values)

# now we change all the column names to way shorter titles, because the titles are super clunky.

names(wb_data_tidy) <- c("country", "year", "access_clean_fuels_pp", "access_electricity_pp", "co2_emissions_kt", "fossil_fuels_cons_pt", "water_stress")

# that's better!
# now we can filter out the country that we want to see, in this case the United States

us_wb <- wb_data_tidy %>%
  dplyr::filter(country=="United States")

# now let's say we're only interested in seeing the CO2 emissions from Nicaragua:

nicaragua_co2 <- wb_data_tidy %>%
  dplyr::filter(country=="Nicaragua") %>%
  select(year, co2_emissions_kt)

# we can also eliminate columns from the whole dataset:

wb_subset <- wb_data_tidy %>%
  select(-c(water_stress, access_electricity_pp))

# now we get a smaller dataset.

# now looking at the mutate() function

class(wb_data_tidy$year)

# here we can see our year column is designated as characters. We want to make them numbers

wb_data_tidy <- wb_data_tidy %>%
  mutate(year=as.numeric(year))

# now we check the class of the year column...

class(wb_data_tidy$year)

# and now it's numeric! The mutate function is great for doing functions to variables, making changes to the actual data.

# now looking at the group_by() function, really good at aggregating data!

c02_total <- wb_data_tidy %>%
  group_by(country) %>%
  summarize(total_co2_kt = sum(co2_emissions_kt, na.rm = TRUE))

c02_annual <- wb_data_tidy %>%
  group_by(year) %>%
  summarise(annual_total_c02_kt = sum(co2_emissions_kt, na.rm=TRUE))

ggplot(data=c02_annual, aes(x=year, y=annual_total_c02_kt)) + geom_line()


























