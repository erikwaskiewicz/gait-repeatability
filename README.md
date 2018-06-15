# Gait reproducibility report generator
R script for processing, analysing and reporting data from a gait analysis reproducibility study.

---

## Installation
The program consists of three scripts and is designed to be run within the project folder. Before using the program for the first time, you will need to download the three scripts into a master directory and change some initial settings. From this master directory, two of the scripts need to be copy and pasted into each project folder and run to produce the report. The third script can remain in the master directory.  

### Files
```repeatability_script.Rmd``` - An R markdown file to perform the main analysis.   
This file loads in functions and settings from the repeatability_functions.R and repeatability_settings.R files, sorts the raw data, loops through each test parameter and outputs a PDF report. This file should be copy and pasted into the project folder, along with the repeatability_settings.R file, and run to produce the output PDF report.  

```repeatability_settings.R``` - An R file where all of the study variables can be entered before running the main script.  
The filepath to the repeatability_function.R file should be editted on initial installation. This file should be copy and pasted into the project folder, along with the repeatability_script.Rmd file. The settings can then be changed depending on the requirements of each project, which will be loaded by the repeatability_script.Rmd file when it produces the report. The different settings are described below.

```repeatability_function.R``` - An R file containing all of the functions for repeatability_script.Rmd to work.   
This file should be saved in the master directory and not copy and pasted into the project folder. The filepath within the repeatability_settings.R file pointing to this file should be edited during the initial setup.  


### Installation steps  
1. Save all files from this repository into a new master directory
2. Change the filepath at the end of the repeatability_settings.R file so that it directs to the repeatability_function.R file in your master directory and save  
These files can now be kept as they are as a master copy, follow the instuctions below for each project

### Dependencies
* RStudio (or pandoc installed, for knitting the Rmd file)
* R version ??????
* R packages:
  - ggplot2 (v3.4.0)
  - reshape2 (v1.4.3)
  - cowplot (v3.4.3)
  - xtable (v1.8.2)
* latex

---

## Creating a report
1. Create a new directory for the study
2. Copy repeatability script and settings script into the new project directory
3. Copy raw data into new directory named 'raw_data' within the study directory
4. Open settings file and follow instructions
5. Edit title, author and date in script file
6. Once setting have been checked, save both files and click knit from within the script file

## Report settings
```working_directory``` -  
```study_sides``` -  
```study_participants``` -  
```study_assessors``` -  
```study_sessions``` -   
```study_parameters``` -  
