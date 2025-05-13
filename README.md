# Energy shortages undermine agricultural drought resistance in the Democratic People’s Republic of Korea

## Environment Requirements
- **Operating System:** Windows 10 (64-bit) with x64-based processor.
- **Software & Platforms:** Google Earth Engine (GEE), R v4.2.1, Matlab 2021, and ArcGIS 10.7
- **Dependencies:**
  - **Google Earth Engine:** Requires a Google account for activation and access.
  - **R v4.2.1:** Requires the installation of the following R packages: `"tidyverse", "rio", "ggplot2", "ggplotify", "ggimage", "reshape2", "cowplot", "readxl", "ggpubr", "ggsci", "RColorBrewer", "ggsignif", "tidyr", "dplyr", "raster", "rgdal"`
  - **Matlab 2021:** No additional packages are required. 
  - **ArcGIS 10.7:** No additional packages are required. 

--- 

## Installation Guide
- **Google Earth Engine:** Requires registration with a Google account for access. No additional installation is needed for the cloud-based platform. 
- **R v4.2.1:** Install the required R packages using the `install.packages()` function. The full installation process may take anywhere from a few minutes to several tens of minutes on a standard desktop computer, depending on the internet connection speed. 

--- 

## Instructions for Users

### Drought Characterization
- Google Earth Engine: Accesses data directly from Google Assets, eliminating the need for local storage. Then run the code: *`./Codes/DroughtCharacterization_VIs.txt`*. Running these script on a standard desktop computer should take dozens of minutes. 
- R v4.2.1: Processes CSIF datasets to analyze spatial patterns of CSIF relative change. Run the script: *`./Codes/DroughtCharacterization_CSIF.R`*. On a standard desktop computer, processing takes about 2–3 days.
- SPI Calculation: Processes precipitation datasets (TerraClimate, CHIRPS, and IMERG) for SPI calculation and outputs regional drought maps. Run the script: *`./Codes/DroughtCharacterization_SPI.txt`*. On a standard desktop computer, processing takes about 2–3 days. 

--- 

### Drought Propagation
- Matlab 2021: The input dataset (*`./Data/Data_DroughtPropagation_RiceArea_CSIF/EVI/NIRv/LSWI.xlsx`*) is available in the repository. To generate results of propagation time and risk probability based on different VIs and different drought levels, users need to load the input dataset and run the provided script (*`./Codes/Drought propagation`*) in Matlab 2021. Running the demo script on a standard desktop computer should take about 3 days. 

--- 

### Statistics Analysis
- R v4.2.1: The dataset (*`./Data/Data_Statistics.xlsx`*) is available in the repository, with with unit details provided in the excel file (*`./Data/Data_Statistics_UnitInfo.xlsx`*) and the sheet *`unit info`* within the *`./Data/Data_Statistics.xlsx`*. To generate SEM results, users need to load the dataset and then run the provided scripts (*`./Codes/StatisticalAnalysis_SEM_DPRK.R`* and *`./Codes/StatisticalAnalysis_SEM_ROK.R`*) in R studio or other compatible R environment. Running these script on a standard desktop computer should take tens of seconds. 

--- 

### Figure generation 
- ArcGIS 10.7: To generation the Fig. 1a–d, the Users should load the corresponding `.tif` files:
  - *`./Figures/Fig_1a_PropagationTime.tif`*
  - *`./Figures/Fig_1b_RiskProbability.tif`*
  - *`./Figures/Fig_1c_SPI.tif`*
  - *`./Figures/Fig_1d_CSIF_RelativeChange.tif`*  
  These figures can also be visualized using other GIS platforms like **QGIS**, **R**, or **Python**.  

- R v4.2.1: The input dataset (*`./Data/Data_DroughtPropagation_RiceArea_CSIF/EVI/NIRv/LSWI.xlsx`*) is available in the repository. Users can run the following scripts to generate related figures:
  - *`./Figures/Fig. 1e.R`*
  - *`./Figures/Fig. 1f.R`*
  - *`./Figures/Fig. 2b-j.R`*

---  

## License
MIT License 

Copyright (c) 2025 Qiang Zhang, Chinese Academy of Sciences

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.