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
library(googlesheets4)
library(lubridate)

# Set directory paths
root <- "~/fmv"

ddir <- file.path(root, "data")
cdir <- file.path(root, "code")

nolte.dir <- file.path(ddir, "Nolte")

f.dir <- file.path(cdir, "functions")

m.dir <- file.path(ddir, "model")
clean.dir <- file.path(ddir, "cleaned")a
arc.dir <- file.path(ddir, "ArcResults")
pqt_dir <- file.path(arc.dir, "parquet") 

# Source custom functions 
walk(
  list.files(f.dir, full.names = TRUE),
  source)
