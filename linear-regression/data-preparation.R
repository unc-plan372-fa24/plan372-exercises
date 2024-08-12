library(tidyverse)
library(foreign)
library(sf)
library(tigris)

data = read.dbf("linear-regression/data/Parcel_SalesData.dbf") |> as_tibble()
since_2010 = filter(data, year(dte_dateof) >= 2010 & 
                      amt_price > 10000 & # reasonable prices only
                      is.na(txt_disqua) & # not disqualified
                      cde_typeof %in% c("WD", "SW"))

all_taxdata = map(2010:2021, function (fileyear) {
  read_sf(paste0("linear-regression/data/Taxdata_", fileyear, "/data.shp")) |>
    mutate(year=fileyear) |>
    rename_with(str_to_lower) |>
    return()
})
for (i in 6:12) {
  all_taxdata[[i]] = mutate(all_taxdata[[i]], ownerno=as.factor(ownerno))
}
taxdata = list_rbind(all_taxdata)

since_2010$taxdatayear = pmin(year(since_2010$dte_dateof), 2021)

# https://stackoverflow.com/questions/56602968
single_taxdata = group_by(taxdata, pid, year) |>
  filter(n() == 1) |>
  ungroup()

modeldata = inner_join(since_2010, single_taxdata, by=c(id_pid="pid",taxdatayear="year")) |>
  filter(units==1)

# summarize the siding for modeling
modeldata$siding = if_else(
  modeldata$extwall %in% c("ALUM,VINYL", "FACE BRICK", "MASONITE", "WOOD ON SHTG", "HARDIPLANK", "HARDIPLK/DSGN VINYL"),
  modeldata$extwall, "Other") |>
  recode("HARDIPLK/DSGN VINYL"="HARDIPLANK")

# modeldata = mutate(modeldata, taxamt=case_when(
#   taxmun=="CHARLOTTE"~totalvalue * .9650,
#   taxmun=="MATTHEWS"~totalvalue * .9119,
#   taxmun=="MECKLENBURG COUNTY-UNINCORPORATED"~totalvalue * .6169,
#   taxmun=="HUNTERSVILLE"~totalvalue * .8569,
#   taxmun=="CORNELIUS"~totalvalue * 
# )

modeldata$zip = str_sub(modeldata$zipcode, 1, 5)

set.seed(312)
modelsample = select(modeldata, dte_dateof, amt_price, houseno, houseunit, stdir, stname, sttype, stsuffix,
       heatedarea, yearbuilt, fullbaths, halfbaths, bedrooms, yearbuilt, actype, vacantorim, totalac, siding, geometry) |>
      slice_sample(n=1500)

modelsample = st_as_sf(modelsample) |> st_transform(32119)
meck_tracts = pumas(state="NC") |> st_transform(32119)
ggplot(meck_tracts) + geom_sf()

modelsample = st_join(modelsample, meck_tracts)

select(modelsample, dte_dateof, amt_price, houseno, houseunit, stdir, stname, sttype, stsuffix,
       heatedarea, yearbuilt, fullbaths, halfbaths, bedrooms, yearbuilt, actype, vacantorim, totalac, siding, PUMACE10) |>
  rename(area="PUMACE10") |>
  write_csv("linear-regression/data/charlotte_home_sales.csv")

ggplot(modeldata, aes(x=heatedarea, y=amt_price)) +
  geom_point(size=0.1) +
  geom_smooth(method="lm") +
  xlim(0, 5000) +
  ylim(0, 2000000)

model_fit = lm(amt_price~heatedarea, modeldata)
summary(model_fit)
