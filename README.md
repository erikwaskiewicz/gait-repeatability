# Gait reproducibility report generator
R script for processing, analysing and reporting data from a gait analysis reproducibility study.

## Files:
- repeatability_function.R  
An R file containing all of the function for repeatability_script.Rmd to work.
- repeatability_settings.R  
An R file where all of the study variables can be entered before running the main script.
- repeatability_script.Rmd  
An R markdown file to perform the main analysis. This file loads in functions and settings from the repeatability_functions.R and repeatability_settings.R files, sorts the raw data, loops through each test parameter and outputs a PDF report.

## Initial setup
The script is designed to be run within the project folder. Before using the script for the first time, download the scripts into a master directory from which they can be copy and pasted into each project folder.  
Steps:  
- Save all files from this repository into a directory
- The functions file will remain in this directory, the script and settings file will be copied into each project
- Change the filepath at the end of the settings file so that it directs to the functions file in your new directory
- These files can now be kept as they are as a master copy, follow the instuctions below for each project

## Creating a report
- Create directory for study
- Copy repeatability script and settings script into new study directory
- Copy raw data into new directory named 'raw_data' within the study directory
- Open settings file and follow instructions
- Edit title, author and date in script file
- Once setting have been checked, save both files and click knit from within the script file
