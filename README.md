

# :beetle: Assessing tick attachments to humans with citizen science data: spatio-temporal mapping in Switzerland from 2015 to 2021 using spatialMaxent🗺️

## Overview
Tick-borne diseases represent a significant public health concern in Switzerland, with increasing incidence rates reported in recent years. Diseases such as Lyme borreliosis and tick-borne encephalitis, transmitted by ticks, pose risks to individuals and place a strain on healthcare systems. 

This project aims to map the tick attachment to humans in Switzerland from 2015 to 2021 monthly on a spatial resolution of 100m.

## 📊 Data Source
Data collected by the users of the Tick Prevention App ([available for example at the google play store](https://play.google.com/store/apps/details?id=com.garzotto.zecke&hl=de)) for Switzerland were used to map the tick attachment to humans in Switzerland.

## 🛠️ Methodology
Using data collected from 2015 to 2021, we performed monthly mapping of tick attachment to humans in Switzerland. This was achieved by employing the software extension [spatialMaxent](https://doi.org/10.1002/ece3.10635) modeling approach at a spatial resolution of 100 meters. 

## 📈 Results
The resulting maps offer valuable insights into the spatial distribution of tick attachment to humans in Switzerland. These insights can guide public health interventions aimed at reducing human exposure to ticks and inform resource planning for healthcare facilities.



## 📂 Repository Contents
- **R/data_preparation**: Includes scripts used for data processing.
- **R/modeling**: Includes all R scripts for modeling, testing and mapping.
