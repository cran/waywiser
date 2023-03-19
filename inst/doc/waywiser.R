## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = rlang::is_installed("vip") && rlang::is_installed("ggplot2")
)

## ----setup--------------------------------------------------------------------
library(waywiser)
set.seed(1107)

worldclim_training <- sample(nrow(worldclim_simulation) * 0.8)
worldclim_testing <- worldclim_simulation[-worldclim_training, ]
worldclim_training <- worldclim_simulation[worldclim_training, ]


worldclim_model <- lm(
  response ~ bio2 + bio10 + bio13 + bio19,
  worldclim_training
)

worldclim_testing$predictions <- predict(
  worldclim_model,
  worldclim_testing
)

head(worldclim_testing)

## -----------------------------------------------------------------------------
ww_agreement_coefficient(
  worldclim_testing,
  truth = response,
  estimate = predictions
)

ww_agreement_coefficient_vec(
  truth = worldclim_testing$response,
  estimate = worldclim_testing$predictions
)

## -----------------------------------------------------------------------------
ww_global_geary_c(
  worldclim_testing,
  truth = response,
  estimate = predictions
)

## -----------------------------------------------------------------------------
ww_global_geary_c(
  worldclim_testing,
  truth = response,
  estimate = predictions,
  wt = ww_build_weights(worldclim_testing)
)

ww_global_geary_c(
  worldclim_testing,
  truth = response,
  estimate = predictions,
  wt = ww_build_weights
)

## -----------------------------------------------------------------------------
yardstick::metric_set(
  ww_agreement_coefficient,
  ww_global_geary_c
)(worldclim_testing,
  truth = response,
  estimate = predictions)

## -----------------------------------------------------------------------------
ww_multi_scale(
  worldclim_testing,
  truth = response,
  estimate = predictions,
  metrics = list(ww_agreement_coefficient, yardstick::rmse),
  n = list(c(2, 4))
)

## -----------------------------------------------------------------------------
grid <- sf::st_make_grid(worldclim_testing, n = c(2, 4))
ww_multi_scale(
  worldclim_testing,
  truth = response,
  estimate = predictions,
  metrics = list(ww_agreement_coefficient, yardstick::rmse),
  grids = list(grid)
)

## -----------------------------------------------------------------------------
worldclim_aoa <- ww_area_of_applicability(
  response ~ bio2 + bio10 + bio13 + bio19,
  worldclim_training,
  importance = vip::vi_model(worldclim_model)
)

worldclim_aoa

## -----------------------------------------------------------------------------
worldclim_testing <- cbind(
  worldclim_testing,
  predict(worldclim_aoa, worldclim_testing)
)

head(worldclim_testing)

## -----------------------------------------------------------------------------
library(ggplot2)

ggplot(worldclim_testing, aes(di, abs(response - predictions), color = aoa)) +
  geom_point(alpha = 0.6)

