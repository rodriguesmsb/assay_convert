# Lab Results Processor

A Shiny app for processing ELISA plate data from Excel workbooks.

## Overview

Point the app at a folder of `.xlsx` files and it will:

1. Locate the `*_value` and `*_map` sheets in each workbook
2. Convert plate-format data into tidy long format, joining sample IDs,
   well positions, dilutions, and OD values
3. Collapse replicate wells by averaging
4. Compute the area under the dilution curve (AUC) for each sample using
   the trapezoidal rule on log-transformed dilutions
5. Render an interactive Plotly curve plot, faceted by file
6. Export a zip containing the combined long-format data, per-sample AUC
   values, and a high-resolution plot image

## Input format

Each Excel workbook should contain two sheets:

- `<assay>_value` — plate-shaped OD readings
- `<assay>_map`   — plate-shaped sample IDs matching the value sheet layout


which is stripped automatically.

## Tech stack

R · Shiny · bslib · shinyFiles · tidyverse · plotly · writexl
