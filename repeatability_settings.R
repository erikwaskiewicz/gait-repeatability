## Repeatability script settings file

## Set working directory - report pdf is saved to this directory and output files are saved in a folder within 
# this directory called results
working_directory <- '/Users/erik/Projects/gait'
setwd(working_directory)
dir.create('results')

## Sides - must be left and right
# 1- how the side will be labelled in any dataframes/graphs
# 2- file path to the input for that side - ensure these filepaths match where your data is
study_sides = list(
  c('Left', 'raw_data/Export Left 20-03-18.txt'),
  c('Right', 'raw_data/Export Right 20-03-18.txt')
)
## List of participants included in the study
# 1- Participant name - how the participant will be labelled in any dataframes/graphs
# 2- participant identifier - the name of this participant within the raw input file
study_participants = list(
  c('Participant 1', 'Dube_Philiani_1982-09-24_Repeatability')
)

## List of assessors included in the study
# 1- Assessor name - how the assessor will be labelled in any dataframes/graphs
# 2- Assessor identifier - how the assessor will be identified from the raw input
study_assessors = list(
  c('Jav', 'JAV'),
  c('Jon', 'JON'),
  c('Lewis', 'LEWIS')
)

## The number of sessions
# 1- Session name - how the session will be labelled in any dataframes/graphs
# 2- Session identifier - how the session will be identified from the raw input
study_sessions = list(
  c('Session 1', '01'),
  c('Session 2', '02'),
  c('Session 3', '03'),
  c('Session 4', '04')
)

## The joint parameters to be tested
#-----------------------------------
# All potential parameters are listed below, comment out any that aren't to be included
# NOTE - be careful not to leave a trailing comma in the last element of the list
# 1 -  Parameter name - how the parameter will be labelled in the report and any dataframes/graphs
# 2 -  Parameter identifier - how the parameter will be identified from the raw input 
#       (minus Left/Right - this field is combined with study_sides[1] to create the identifier)
# 3 -  Plane - the plane that the parameter is in
# 4:6- Parameters for plotting graphs: 4:min y value, 5:max y value, 6:increments

study_parameters = list(
  c('Foot pitch', 'Foot Pitch Angles', 'X', -80, 30, 20),
  c('Foot progression', 'Foot Pitch Angles', 'Z', -25, 25, 10),
  c('Ankle dorsiflexion', 'Ankle Angles', 'X', -25, 15, 5),
  c('Ankle inversion', 'Ankle Angles', 'Y', 0, 30, 5),
  c('Knee flexion', 'Knee Angles', 'X', -15, 70, 10),
  c('Knee adduction', 'Knee Angles', 'Y', -15, 10, 5),
  c('Knee rotation', 'Knee Angles', 'Z', -22, 12, 10),
  c('Hip flexion', 'Hip Angles', 'X', -20, 40, 10),
  c('Hip adduction', 'Hip Angles', 'Y', -15, 10, 5),
  c('Hip rotation', 'Hip Angles', 'Z', -30, 10, 10),
  c('Pelvic tilt', 'Pelvic Angles', 'X', 0, 20, 5),
  c('Pelvic obliquity', 'Pelvic Angles', 'Y', -6, 6, 2),
  c('Pelvic rotation', 'Pelvic Angles', 'Z', -10, 12, 5)
)

## Load functions - path to the repeatability functions file, you should only have to change this once
source('/Users/erik/Projects/gait/repeatability_functions.R')

