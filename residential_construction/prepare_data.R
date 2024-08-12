# This code is used to preprocess data for the residential construction exercise -
# you don't need to run it.

library(tidyverse)
library(readxl)
library(lubridate)

all_data = lapply(Sys.glob("data/co*c.csv"), function (fn) {
  return(read_csv(fn, skip=3,
                 col_names = c(
                   "survey_date",
                   "state_fips",
                   "county_fips",
                   "census_region",
                   "census_division",
                   "county_name",
                   "buildings_1unit",
                   "units_1unit",
                   "value_1unit",
                   "buildings_2unit",
                   "units_2unit",
                   "value_2unit",
                   "buildings_3or4unit",
                   "units_3or4unit",
                   "value_3or4unit",
                   "buildings_5plusunit",
                   "units_5plusunit",
                   "buildings_1unit_reported",
                   "units_1unit_reported",
                   "value_1unit_reported",
                   "buildings_2unit_reported",
                   "units_2unit_reported",
                   "value_2unit_reported",
                   "buildings_3or4unit_reported",
                   "units_3or4unit_reported",
                   "value_3or4unit_reported",
                   "buildings_5plusunit_reported",
                   "units_5plusunit_reported",
                   "value_5plusunit_reported"
                 )))})


all_data = bind_rows(all_data) |> mutate(svd=ym(survey_date)) |> select(-ends_with("_reported"))
all_data = mutate(all_data, total_buildings = buildings_1unit + buildings_2unit + buildings_3or4unit + buildings_5plusunit)

filter(all_data, year(svd) < 2010) |>
  select(-svd) |>
  write_csv("data/res_permits_2000s.csv")

filter(all_data, (year(svd) >= 2010) & (year(svd) < 2020)) |>
  select(-svd) |>
  write_csv("data/res_permits_2010s.csv")

filter(all_data, year(svd) >= 2020) |>
  select(-svd) |>
  write_csv("data/res_permits_2020s.csv")

# read data on county typologies
countycodes = read_excel("data/all_final_codes.xls", sheet = "all_final_codes")
countycodes = mutate(countycodes, econdep=recode(econdep,
                                                `1`="Farming",
                                                `2`="Mining",
                                                `3`="Manufacturing",
                                                `4`="Government",
                                                `5`="Service",
                                                `6`="Nonspecialized"),
                     state_fips=str_sub(FIPSTXT, 1, 2),
                     county_fips=str_sub(FIPSTXT, 3, 5))

select(countycodes, state_fips, county_fips, econdep) |>
  write_csv("data/county_types.csv")

totalhu = read_csv("data/original_housing_units.csv") |> filter(row_number() != 1)
totalhu = mutate(totalhu, state_fips=str_sub(GEO_ID, 10, 11), county_fips=str_sub(GEO_ID, 12, 14)) |>
  rename(total_housing_units=H001001)

totalhu |> select(-GEO_ID) |> write_csv("data/housing_units.csv")
