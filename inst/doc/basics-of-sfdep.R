## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "600px"
)

## ----setup, message = FALSE---------------------------------------------------
library(sfdep)
library(dplyr)
library(ggplot2)

## ---- echo = FALSE------------------------------------------------------------
chess_board <- expand.grid(x = 1:8, y = 1:8) %>% 
  mutate(z = ifelse((x + y) %% 2 == 0, TRUE, FALSE))

board <- chess_board %>% 
  ggplot(aes(x, y, fill = z)) + 
  geom_tile() +
  scale_fill_manual(values = c("white", "black")) +
  theme_void() +
  coord_fixed() +
  theme(legend.position = "none")

board

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
library(patchwork)
library(spdep)
# create chess spatial object
chess_sf <- chess_board %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_make_grid(n = 8) %>% 
  st_sf() %>% 
  mutate(color = pull(chess_board, z))


# Create chess board neighbors
chess_nb_q <- poly2nb(chess_sf)
chess_nb_r <- poly2nb(chess_sf, queen = FALSE)

neighbors_tidy <- nb2lines(chess_nb_q, coords = st_geometry(chess_sf), as_sf = TRUE)
neighbors_tidy_r <- nb2lines(chess_nb_r, coords = st_geometry(chess_sf), as_sf = TRUE)

queen_gg <- ggplot() +
  geom_sf(data = chess_sf, aes(fill = color)) + 
  geom_sf(data = neighbors_tidy, color = "#528672") +
  scale_fill_manual(values = c("white", "black")) +
  labs(title = "Queen Contiguities") +
  theme_void() +
  theme(legend.position = "none")
  

rook_gg <- ggplot() +
  geom_sf(data = chess_sf, aes(fill = color)) + 
  geom_sf(data = neighbors_tidy_r, color = "#528672") +
  scale_fill_manual(values = c("white", "black")) +
  labs(title = "Rook Contiguities") +
  theme_void() +
  theme(legend.position = "none")


queen_gg + rook_gg 

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------

geo <- st_geometry(guerry)
ggplot() +
  geom_sf(data = geo[st_knn(geo, k = 20)[[47]]],
          fill = NA, color = "black", lwd = 1/4) +
  geom_sf(data = geo[guerry_nb$nb[[47]]],
          fill = "grey50", lwd = .25, color = "black") +
  geom_sf(data = geo[47], 
          fill = "black", lwd = 0.25, color = "black") +
  theme_void()


## ---- echo = FALSE------------------------------------------------------------
chess_nb_q <- poly2nb(chess_sf)

board +
  geom_point(data = slice(chess_board, chess_nb_q[[28]]), color= "red") +
  geom_point(data = slice(chess_board, 28), color = "blue") 


## -----------------------------------------------------------------------------
(d4_nbs <- rep(1, 8))

d4_nbs / length(d4_nbs)

## -----------------------------------------------------------------------------
guerry_nb <- guerry %>% 
  mutate(nb = st_contiguity(geometry),
         wt = st_weights(nb),
         .before = 1) # to put them in the front

guerry_nb

## -----------------------------------------------------------------------------
lisa <- guerry_nb %>% 
  mutate(local_moran = local_moran(crime_pers, nb, wt, nsim = 199),
         .before = 1)

lisa

## -----------------------------------------------------------------------------
lisa %>% 
  tidyr::unnest(local_moran)

## -----------------------------------------------------------------------------
guerry_nb %>% 
  mutate(local_c = local_c_perm(list(crime_pers, wealth), nb, wt), 
         .before = 1) %>% 
  tidyr::unnest(local_c)

