# Nutriyogur

Data and R code for the Nutriyogur randomized three-period crossover trial in adults with early-onset type 2 diabetes.

## Requirements

R. No additional packages are required; `readxl` is used when installed.

## Contents

    data/cgm/          continuous glucose monitoring exports, <participant>_<product>.xlsx
    data/sensory.csv   satiety and liking, one row per participant-day
    data/baseline.csv  baseline characteristics, one row per participant
    scripts/           analysis scripts, in the order they run
    run_all.R          runs every script in order

Products are coded A = Nutriyogur, B = commercial Greek yogurt, C = non-Greek yogurt. In `data/sensory.csv`, `saciedad` is satiety and `agrado` is liking.

Dates in the CGM exports are shifted by a whole number of days per participant so that each participant's first observation falls on 2000-01-01. Time of day and sampling intervals are unchanged.

## Reproducing the analysis

    Rscript run_all.R

This writes the tables to `results/` and Figures 2–4 to `figures/`. Record `18009_C` carries no usable glucose data and is excluded after the per-record step.

## Licence

Code: MIT (`LICENSE`). Data, tables and figures: CC BY 4.0 (`LICENSE-DATA`).
