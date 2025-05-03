# Sanctions-Drought-Korea

## System Requirements
- Operating System: Windows 10 (64-bit) with x64-based processor.
- Software and Platforms: Google Earth Engine (GEE), R v4.2.1, and Matlab 2021
- Dependencies:
  - Google Earth Engine: Requires a Google account for activation and access.
  - R v4.2.1: Requires the installation of the following R packages: `"tidyverse", "rio", "ggplot2", "ggplotify", "ggimage", "reshape2", "cowplot", "readxl", "ggpubr", "ggsci", "RColorBrewer", "ggsignif"`

## Installation Guide
- Google Earth Engine: Requires registration with a Google account to enable access. No additional installation is needed for the cloud-based platform. 
- R v4.2.1: Install the required R packages using the `install.packages()` function. The full installation process may take anywhere from a few minutes to several tens of minutes on a standard desktop computer, depending on the internet connection speed. 

## Instructions for use
- Google Earth Engine: Directly accesses data from Google Assets, eliminating the need for local storage. Then run the code: *Drought assessment.txt* and *GPP series.txt*. Running these script on a standard desktop computer should take dozens of minutes. 
- R v4.2.1: The dataset (*Data_Statistics.xlsx*) is available in the repository, with with unit details provided in the excel file (*Data_Statistics_UnitInfo.xlsx*) and the sheet '*unit info*' within the *Data_Statistics.xlsx*. To generate SEM results, users need to load the dataset and then run the provided scripts (*Codes/Analysis_SEM_DPRK.R* and *Codes/Analysis_SEM_DROK.R*) in R studio or other compatible R environment. Running these script on a standard desktop computer should take tens of seconds. 
- MATLAB 2021: To generate results of probability of vegetation in different drought levels, users need to load the dataset () and run the provided script (*Codes/Drought propagation*) in MATLAB 2021. Running the demo script on a standard desktop computer should take about 20 to 30 minutes. 

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