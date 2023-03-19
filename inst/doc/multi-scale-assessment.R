## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = rlang::is_installed("ggplot2") && rlang::is_installed("tigris")
)

## ----setup, include=FALSE-----------------------------------------------------
ggplot2::theme_set(ggplot2::theme_minimal())

## -----------------------------------------------------------------------------
library(sf)
library(tidyr)
library(dplyr)
library(waywiser)

invisible(sf_proj_network(TRUE))

## -----------------------------------------------------------------------------
library(ggplot2)

ny_trees %>%
  ggplot() +
  geom_sf(aes(color = agb), alpha = 0.4) +
  scale_color_distiller(palette = "Greens", direction = 1)

## -----------------------------------------------------------------------------
agb_lm <- lm(agb ~ n_trees, ny_trees)
ny_trees$predicted <- predict(agb_lm, ny_trees)

## -----------------------------------------------------------------------------
cell_sizes <- seq(10, 100, 10) * 1000
ny_multi_scale <- ww_multi_scale(
  ny_trees,
  agb,
  predicted,
  cellsize = cell_sizes
)

ny_multi_scale

## -----------------------------------------------------------------------------
ny_multi_scale %>%
  unnest(.grid_args) %>%
  ggplot(aes(x = cellsize, y = .estimate, color = .metric)) +
  geom_line()

## -----------------------------------------------------------------------------
ny_multi_scale$.grid[[9]] %>%
  filter(!is.na(.estimate)) %>%
  ggplot(aes(fill = .estimate)) +
  geom_sf() +
  scale_fill_distiller(palette = "Greens", direction = 1)

## ----message=FALSE, results='hide'--------------------------------------------
suppressPackageStartupMessages(library(tigris))

ny_block_groups <- block_groups("NY")
ny_county_subdivisions <- county_subdivisions("NY")
ny_counties <- counties("NY")

## ----warning=FALSE------------------------------------------------------------
ny_division_assessment <- ww_multi_scale(
  ny_trees,
  agb,
  predicted,
  grids = list(
    ny_block_groups,
    ny_county_subdivisions,
    ny_counties
  )
)

ny_division_assessment %>%
  mutate(
    division = rep(c("Block group", "County subdivision", "County"), each = 2)
  ) %>%
  ggplot(aes(x = division, y = .estimate, fill = .metric)) +
  geom_col(position = position_dodge())

