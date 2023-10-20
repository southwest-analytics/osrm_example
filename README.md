# OSRM Example

### Example of usage of osrm package for travel time analysis

#### Author: Richard Blackwell
#### Email: richard.blackwell@healthinnovationsouthwest.com
#### Date: 2023-10-20

----

## Setup

The following data files are required for the running of this script

### Census Output Areas 2021 ESRI Shapefile

This can be found on the [Open Geography Portal](https://geoportal.statistics.gov.uk/) under the menu item **Boundaries > Census Boundaries > Output Areas > 2021 Boundaries**

The contents of the zipfile should be extracted to the root directory of this project in a folder titled OA21 and the files in that folder should all have the base filename of **OA21.xxx** where xxx is the relevant file extension of each file

### Census Output Areas 2021 Population Weighted Centroids CSV file

This can be found on the [Open Geography Portal](https://geoportal.statistics.gov.uk/) under the menu item **Boundaries > Centroids > Population Weighted Centroids > Output Areas**

This file should be saved to the root directory of this project and should have the filename **OA21_PWC.csv**

### Census Output Areas 2021 to Integrated Care Board 2023 Lookup CSV file

This can be found on the [Open Geography Portal](https://geoportal.statistics.gov.uk/) under the menu item **Lookups > Health Lookups > All Health Lookups**

This file should be saved to the root directory of this project and should have the filename **OA21_ICB23_LU.csv**
