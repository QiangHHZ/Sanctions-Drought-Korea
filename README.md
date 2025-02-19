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
- Google Earth Engine: Directly accesses data from Google Assets, eliminating the need for local storage. Than run the code: *Drought assessment.txt* and *GPP series.txt*
- R v4.2.1: The dataset (*Data_Statistics.xlsx*) is provided within the repository. To generate SEM (Structural Equation Modeling) results, users need to load the dataset and run the provided script (*Attribution_analysis.txt*) in R studio.
- Expected Run Time: Running the demo script on a standard desktop computer should take only a few seconds.

## License
This project is licensed under the MIT License, an Open Source Initiative (OSI)-approved license. 