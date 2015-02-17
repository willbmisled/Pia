---
title: "PiaData"
author: "Bryan Milstead"
date: "Tuesday, February 17, 2015"
output: html_document
---
<!---
use these command instead of the knit icon if you want the data and work loaded into the R workspace
  library(knitr)
  setwd("~/PortableApps/R/scripts/Pia/Data")
  knit('PiaData.rmd')
-->
To Do
-------------------------
* Complete Bird List
* Complete Plant List

Work Notes
-------------------------
20150216
* Seno Pia split into Seno Pia Oeste and Seno Pia Este.  PBO is the only site in the west branch, the rest are in in Seno Pia Este.

20150217
* Harmonized lists between the mist nets and the counts.  Species captured in mistnet that weren't counted were added as zero values and vice versa for the mist net data.  This allows them to be plotted in the same order.

Introduction
-------------------------
* Bird Counts were conducted at sites on the Beagle Channel from Jan.21-31 2015.
* Mist netting was conducted at 3 sites during this period.

Data Processing
-------------------------
* read data from "DatosAvesPia.xls"
  - Counts: the raw count data
  - Obs: Observations on relative abundance by SiteCode, this table also contains the bird taxonmy and codes to be used throughout. Relative Abundance: 0=absent; 1=1-5 individuals; 2=6-20 individuals; 3=>20 individuals.
  - Sites: details on locations-this is joines to sites
  - PlantNames: plant taxonomy used


  
* add the Site information to Sites



* Aggregate Bird and Plant Counts by Area



* standardize the mistnet data



* Save the data to PiaData.rda




