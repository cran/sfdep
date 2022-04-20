## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  echo = FALSE,
  comment = "#>"
)
library(magrittr)
fp <- system.file("extdata", "lookups.yml", package = "sfdep")
df <- yaml::read_yaml(fp, error.label = "")

## -----------------------------------------------------------------------------
tibble::enframe(df$global_stats) %>% 
  tidyr::unnest_wider(value) %>% 
  dplyr::mutate(pysal = ifelse(is.na(pysal), "", pysal)) %>%
  knitr::kable()

## -----------------------------------------------------------------------------
df$lisas %>% 
  tibble::enframe() %>% 
  tidyr::unnest_wider(value) %>% 
  dplyr::mutate(pysal = purrr::map_chr(pysal, ~stringr::str_c(.x, collapse = ", "))) %>% 
  knitr::kable()

## -----------------------------------------------------------------------------
df[["not implemented"]] %>% 
  tibble::as_tibble() %>% 
  dplyr::rename(`function` = value) %>% 
  knitr::kable()

