# Sanctions-Drought-Korea

## System Requirements
- Operating System: Windows 10 (64-bit) with x64-based processor.
- Software and Platforms: Google Earth Engine (GEE), R v4.2.1, and Matlab 2018a
- Dependencies:
  - Google Earth Engine: Requires a Google account for activation and access.
  - R v4.2.1: Requires the installation of the following R packages: `"tidyverse", "rio", "ggplot2", "ggplotify", "ggimage", "reshape2", "cowplot", "readxl", "ggpubr", "ggsci", "RColorBrewer", "ggsignif"`

## Installation Guide
- Google Earth Engine: Requires registration with a Google account to enable access. No additional installation is needed for the cloud-based platform. 
- R v4.2.1: Install the required R packages using the `install.packages()` function. The full installation process typically takes less than one minute on a standard desktop computer. 

## Instructions for use
- Google Earth Engine: Directly accesses data from Google Assets, eliminating the need for local storage. Then run the code: *Drought assessment.txt* and *GPP series.txt*
- R v4.2.1: The dataset (*Data_Statistics.xlsx*) is available in the repository, with with unit details provided in the excel file (*Data_Statistics_UnitInfo.xlsx*) and the sheet '*unit info*' within the *Data_Statistics.xlsx*. To generate SEM results, users need to load the dataset and then run the provided scripts (*Analysis_SEM_DPRK.R* and *Analysis_SEM_DROK.R*) in R studio or other compatible R environment. 
- Expected Run Time: Running the demo script on a standard desktop computer should take tens of seconds.

## License
This project is licensed under the MIT License, an Open Source Initiative (OSI)-approved license. 