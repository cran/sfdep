## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library(sfdep)
library(dplyr)

## -----------------------------------------------------------------------------
# replicate the guerry dataset 10 times
x <- purrr::map_dfr(1:10, ~guerry) |> 
  select(code_dept, crime_pers) |> 
         # create an indicator for time period
  mutate(time_period = sort(rep(1:10, 85)), 
         # add some noise 
         crime_pers = crime_pers * runif(850, max = 2))

x

## -----------------------------------------------------------------------------
spt <- as_spacetime(x, "code_dept", "time_period")

## -----------------------------------------------------------------------------
df <- sf::st_drop_geometry(x)
geo <- select(guerry, code_dept)

head(df)

## -----------------------------------------------------------------------------
spt <- spacetime(
  .data = df, 
  .geometry = geo, 
  .loc_col = "code_dept", 
  .time_col = "time_period"
  ) 

spt

## -----------------------------------------------------------------------------
activate(spt, "data")

## -----------------------------------------------------------------------------
spt |> 
  activate("geometry") 

## ---- echo=FALSE--------------------------------------------------------------
knitr::include_graphics("https://pro.arcgis.com/en/pro-app/2.8/tool-reference/space-time-pattern-mining/GUID-0FEECE1A-6B54-44B4-AE49-05E7EA849A8B-web.png")

## -----------------------------------------------------------------------------
is_spacetime_cube(spt)

## -----------------------------------------------------------------------------
sparse_spt <- dplyr::slice_sample(spt, n = 800)

is_spacetime_cube(sparse_spt)

## -----------------------------------------------------------------------------
spt_complete <- complete_spacetime_cube(sparse_spt)

is_spacetime_cube(spt_complete)

## ---- error = TRUE------------------------------------------------------------
set.seed(0)
sparse_spt <- dplyr::slice_sample(spt, n = 800, replace = TRUE)

complete_spacetime_cube(sparse_spt)

## -----------------------------------------------------------------------------
dplyr::count(sparse_spt, time_period, code_dept)

## -----------------------------------------------------------------------------
emerging_hotspot_analysis(spt, "crime_pers", threshold = 0.05)


