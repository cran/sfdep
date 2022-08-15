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
 p_nb <- cond_permute_nb(nb)
 p_wt <- st_weights(p_nb)
 observed <- global_c(x, p_nb, p_wt)
 
 observed

## -----------------------------------------------------------------------------
reps <- replicate(199, {
  p_nb <- cond_permute_nb(nb)
  p_wt <- st_weights(p_nb)
  global_c(x, p_nb, p_wt)[["C"]]
})


## -----------------------------------------------------------------------------
library(ggplot2)

ggplot(data.frame(sim_val = reps), 
       aes(sim_val)) +
  geom_histogram(bins = 20) +
  geom_vline(xintercept = observed[["C"]], 
             color = "#6fb381", 
             lty = 2,
             ) +
  theme_light()

## -----------------------------------------------------------------------------
# simulated p-value
(sum(observed[["C"]] <= reps) + 1) / (199 + 1)

