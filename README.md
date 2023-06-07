# Foliage and stem density analysis, Cape York Peninsula, Australia
R code for 

Crowley and Murphy (in press) Carbon-dioxide-driven increase in projective foliage cover is not the same as elevated carbon sequestration: Lessons from an Australian tropical savanna. _The Rangelands Journal_  

All files have been prepared using R version 4.1.3, RStudio version 2022.02.0 Build 443 and mgcv version 1.8-40. Slightly different results may be obtained if the code is run using a different mgcv version.

The files are in two sets.

(1) The "Fire and PG analysis.R" file is for running with "CYP.RData"
These files use generalized additive modelling (GAM) to characterise changes in the occurrence of Early Dry Season and Late Dry Season fires, and changes in Autumn Persistent Green (APG) values across Cape York Peninsula, Australia, between 2001 and 2018, and identify potential drivers of change in APG.

(2) The "Stem density change.R" file is for running with files stored in "Veg.RData" and "WP.RData".
These files analyse changes in stem density (2001 and 2021) and APG (2001-2018) in Artemis Antbed Nature Refuge on east-central Cape York Peninsula. They produce variograms for characterising changes in APG between 2001 and 2018, and use two-sample sign tests to compare stem density in 2001 and 2021.

Autumn Persistent Green is a measure of foliage projective cover produced by the Joint Remote Sensing Project and is available at https://portal.tern.org.au/metadata/TERN%2Fdd359b61-3ce2-4cd5-bc63-d54d2d0e2509.

Fire scar mapping used in these analyses is available from the North Australian Fire Information website (https://firenorth.org.au/). Vegetation mapping is available from the Queensland Spatial Catalogue https://qldspatial.information.qld.gov.au/catalogue/. Rainfall mapping is available from the Australian Bureau of Meteorology (http://www.bom.gov.au/).

The code in these files will automatically access all data required from the relevant RData file or directly from the Seasonal Persistent Green respository. This assumes that the URL of the repository remains active.
