# =====================================================
# 01). Set up
# =====================================================

# Load packages
library(tidyverse)
library(arrow)
library(RColorBrewer)
library(data.table)
library(ggplot2)
library(magrittr)
library(sf)
library(ggtext)
library(lubridate)

# Set directory paths
root <- here::here()

ddir <- file.path(root, "data")
cdir <- file.path(root, "code")

z.dir <- file.path(ddir, "zipped")

f.dir <- file.path(cdir, "functions")

e.dir <- file.path(odir, "exhibits")

m.dir <- file.path(ddir, "model")
clean.dir <- file.path(ddir, "cleaned")



