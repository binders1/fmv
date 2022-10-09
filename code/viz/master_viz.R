# Load packages ####
library(tidyverse)
library(arrow)
library(RColorBrewer)
library(data.table)
library(ggplot2)
library(magrittr)
library(sf)
library(ggtext)

# Set up ####
root <- "~/fmv"

ddir <- file.path(root, "data")
cdir <- file.path(root, "code")
odir <- file.path(root, "output")

v.dir <- file.path(cdir, "viz")
f.dir <- file.path(cdir, "functions")
e.dir <- file.path(odir, "exhibits")

# Source Prep ####
source(file.path(f.dir, "sourceFuncs.R"))
sourceFuncs()
source(file.path(cdir, "misc/ag_regions.R"))
source(file.path(v.dir, "model/frr/viz_frr_prep.R"))




figs <- 
  dplyr::tribble(
    "", compare_ffb_fcb_mse
  )

png(
  filename = file.path(e.dir, "compare_ffb_fcb_mse.png"),
  res = 600,
  width = 7,
  height = 4,
  units = "in"
)

compare_ffb_fcb_mse()

dev.off()




