# Gait reproducibility report generator
R script for processing, analysing and reporting data from a gait analysis reproducibility study.

---

## Installation
The program consists of three scripts and is designed to be run within the project folder. Before using the program for the first time, you will need to download the three scripts into a master directory and change some initial settings. From this master directory, two of the scripts need to be copy and pasted into each project folder and run to produce the report. The third script can remain in the master directory.  

### Files
```repeatability_script_<pdf/html>.Rmd``` - An R markdown file to perform the main analysis. There is a PDF and a HTML version of the file, select the one you want depending on what type of output you would like.  
This file allows you to change the settings for the project, then loads in functions from the ```repeatability_functions.R``` file, sorts the raw data, loops through each test parameter and outputs a PDF/ HTML report. This file should be copy and pasted into the project folder and run to produce the output report.  

```repeatability_function.R``` - An R file containing all of the functions for repeatability_script.Rmd to work.   
This file should be saved in the master directory and not copy and pasted into the project folder. The filepath within the repeatability_settings.R file pointing to this file should be edited during the initial setup.  


### Installation steps  
1. Save all files from this repository into a new master directory
2. Change the ```load functions``` filepath at the end of the setting section in the ```repeatability_script_pdf.Rmd``` and ```repeatability_script_html.Rmd``` files so that it directs to the ```repeatability_function.R``` file in your master directory and save.  
These files can now be kept as they are as a master copy, follow the instuctions below for each project

### Dependencies
* RStudio (or pandoc installed, for rendering the .Rmd file)
* R
* R packages:
  - ggplot2 (v3.4.0)
  - reshape2 (v1.4.3)
  - cowplot (v3.4.3)
  - xtable (v1.8.2)
* Latex (for PDF output)

---

## Creating a report
1. Create a new directory for the study
2. Copy repeatability script into the new project directory
3. Copy raw data into new directory named 'raw_data' within the study directory
4. Open repeatability script file and follow instructions
5. Once setting have been checked, save both files and click 'Knit' in RStudio from within the script file

## Report settings
```working_directory``` - Full filepath to the current project.   
```study_sides``` - Must include only 2 records - one left and one right. The first field must be either left or right, the second must be the filepath to the data for that side, relative to the working directory.  
```study_participants``` - Can be as many participants as you want, provided they are within the dataset. You don't have to include all participants in the study. First field is the participant name as it will be in the report, the second is how the participant will be identified within the dataset.    
```study_assessors``` - Can be as many assessors as you want, provided they are within the dataset. You don't have to include all assessors in the study. First field is the assessor name as it will be in the report, the second is how the assessor will be identified within the dataset.    
```study_sessions``` - Can be as many sessions as you want, provided they are within the dataset. You don't have to include all sessions in the study.First field is the session name as it will be in the report, the second is how the session will be identified within the dataset.     
```study_parameters``` - The parameters to be reported in the study. All possible parameters are included, it is recommended that, if you want to remove a parameter, you comment out a line by adding a # to the start of the line, rather than delete the whole line.  
- Field 1: How the parameter will be named in the report  
- Fields 2 & 3: Joint and plane. How the parameter is labelled in the data. Note that 'Left' and 'Right' will be added before the joint by the script, so there is no need to add this.  
- Field 4 & 5: Y axis minimum and maximum values, respectively. Change if some of the graphs don't fit within the axes.
- Field 6: Y axis increments, the gap between each Y axis label.
