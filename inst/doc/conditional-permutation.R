## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(sfdep)

nb <- st_contiguity(guerry)
wt <- st_weights(nb)
x <- guerry$crime_pers


## -----------------------------------------------------------------------------
permutes <- purrr::map_dbl(1:299, ~{
 p_nb <- cond_permute_nb(nb)
 p_wt <- st_weights(p_nb)
 global_moran(x, p_nb, p_wt)[["I"]]
})

permutes[1:10]

## -----------------------------------------------------------------------------
# the observed global moran
observed <- global_moran(x, nb, wt)
observed

## -----------------------------------------------------------------------------
# simulated p-value
(sum(observed[["I"]] <= permutes) + 1) / (299 + 1)

