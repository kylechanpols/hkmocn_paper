# README

This is a Github Repository for the Replication materials for the paper "Can Economic Integration reduce social unrest? Evidence from China, Hong Kong and Macau" Published on the Journal of East Asian Studies.

## Main Text

`main-text.R` - Code to replicate the figures and tables found in the main text.

## Appendix

`appendix.Rmd` - Code to replicate the Online Appendix.

## GIS Analysis (Figure 3)

`mapping/plotting_map.R` - Code to replicate figure 3 (the GIS rendition of protest counts in different parts of China). Note that you will need shape files from [GADM](https://gadm.org/).

`mapping/reverse-geocoding.R` - Code to reverse geocode GDELT events data and sort them to provinces.

## Data

`cn_dataset_augmeneted_MERGED.csv` - China Protest Counts dataset. See `codebook_china.pdf` for a description of the variables in the dataset.

`hkmo-dataset.csv` - Hong Kong/Macau Confidence in One Country, Two Systems dataset.  See `codebook_hkmo.pdf` for a description of the variables in the dataset.

`Protest Count/cnhkmo_protest_data_REG_YEAR.csv` - Hong Kong/Macau Protest Counts dataset. See `codebook_hk_mo_protest.pdf` for a description of the variables in the dataset.

## R workspace files

These are created to speed up the rendering process of the manuscript. You do not need them, you can replicate the contents in these workspace files by running all code chunks in the `main-text.R`.

`hk_mod2.Rdata` - Workspace containing all fitted Hong Kong and Macau regression models.

`protest_map.Rdata` - Workspace containing Figure 3

## Misc

`min-template.tex` - A LaTeX template that I used in this manuscript.

`zotero2019.bib` - BibTex library for citations used in this manuscript.