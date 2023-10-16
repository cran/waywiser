## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = rlang::is_installed("ggplot2")
)

## ----message=FALSE------------------------------------------------------------
# waywiser itself, of course:
library(waywiser)
# For the %>% pipe and mutate:
library(dplyr)

## -----------------------------------------------------------------------------
guerry %>%
  mutate(pred = predict(lm(Crm_prs ~ Litercy, .))) %>%
  ww_local_moran_i(Crm_prs, pred)

## -----------------------------------------------------------------------------
ww_build_neighbors(guerry)

ww_build_weights(guerry)

## -----------------------------------------------------------------------------
weights <- guerry %>%
  sf::st_geometry() %>%
  sf::st_centroid() %>%
  spdep::dnearneigh(0, 97000) %>%
  spdep::nb2listw()

weights

guerry %>%
  mutate(pred = predict(lm(Crm_prs ~ Litercy, .))) %>%
  ww_local_moran_i(Crm_prs, pred, weights)

## -----------------------------------------------------------------------------
weights_function <- function(data) {
  data %>%
    sf::st_geometry() %>%
    sf::st_centroid() %>%
    spdep::dnearneigh(0, 97000) %>%
    spdep::nb2listw()
}

guerry %>%
  mutate(pred = predict(lm(Crm_prs ~ Litercy, .))) %>%
  ww_local_moran_i(Crm_prs, pred, weights_function)

## ----2022_06_29-guerry, fig.width=8-------------------------------------------
library(ggplot2)

weights <- ww_build_weights(guerry)

guerry %>%
  mutate(
    pred = predict(lm(Crm_prs ~ Litercy, .)),
    .estimate = ww_local_moran_i_vec(Crm_prs, pred, weights)
  ) %>%
  sf::st_as_sf() %>%
  ggplot(aes(fill = .estimate)) +
  geom_sf() +
  scale_fill_gradient2(
    "Local Moran",
    low = "#018571",
    mid = "white",
    high = "#A6611A"
  )

## -----------------------------------------------------------------------------
moran <- yardstick::metric_set(
  ww_global_moran_i,
  ww_global_moran_pvalue
)

guerry %>%
  mutate(pred = predict(lm(Crm_prs ~ Litercy, .))) %>%
  moran(Crm_prs, pred)

## ----2023_02_21-guerryp, fig.width=8------------------------------------------
guerry %>%
  mutate(
    pred = predict(lm(Crm_prs ~ Litercy, .)),
    .estimate = ww_local_moran_pvalue_vec(Crm_prs, pred, weights)
  ) %>%
  sf::st_as_sf() %>%
  ggplot(aes(fill = .estimate < 0.01)) +
  geom_sf() +
  scale_fill_discrete("Local Moran p-value < 0.01?") +
  theme(legend.position = "bottom")

