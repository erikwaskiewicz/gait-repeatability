## Repeatability functions
# Define functions called within the repeatabiity script

## Load libraries ---------------------------------------------------------------------------------
library('ggplot2')
library('reshape2')
library('xtable')
library('cowplot')


## Loading data -----------------------------------------------------------------------------------

# Load headers from raw input
load_headers <- function(x) {
  df <- read.delim(file = x, header = FALSE, nrows = 5)
  df <- data.frame(t(df[, seq(2, ncol(df))]))
}

# Load data from raw input
load_data <- function(x) {
  df <- read.delim(file = x, header = FALSE, skip = 5)
  df <- data.frame(t(df[, seq(2, ncol(df))]))
}


## Settings summary functions - for summary on first page of report -------------------------------

# Count number of settings
settings_summary_count <- function(x) {
  num <- length(x)
  return(num)
}

# List settings
settings_summary_list <- function(x) {
  for (parameter in x) {
    cat('  \n*', parameter[1], '  \n')
  }
}


## Sorting data -----------------------------------------------------------------------------------

# Parse participant from raw input
parse_participant <- function(x) {
  field = x[6]
  return(field)
}

# Parse date from raw input
parse_date <- function(x) {
  field = x[8]
  date = unlist(strsplit(field, '_'))[1]
  return(date)
}

# Parse assessor from raw input
parse_assessor <- function(x) {
  field = x[8]
  field_split = unlist(strsplit(field, '_'))[2]
  assessor = gsub('[[:digit:]]+', '', field_split)
  return(assessor)
}

# Parse session number from raw input
parse_number <- function(x) {
  field = x[8]
  field_split = unlist(strsplit(field, '_'))[2]
  assessor = gsub('[[:digit:]]+', '', field_split)
  no = gsub(assessor, '', field_split)
  return(no)
}

# Parse trial number from raw input
parse_trial <- function(x) {
  field = x[9]
  field_split = unlist(strsplit(field, ' '))
  field_split = field_split[length(field_split)]
  field_split = gsub('.c3d', '', field_split)
  return(field_split)
}

# Use parsing functions above to sort raw data into nice format
sort_data <- function(dat, head){
  # get filepaths from header data to extract data from
  paths <- strsplit(toString(head$X1), ',')
  paths_split <- strsplit(unlist(paths), '\\\\')
  # extract data for each record and save in dataframe
  temp_df <- data.frame(
    'participant' = sapply(paths_split, parse_participant),
    'assessor' = sapply(paths_split, parse_assessor),
    'session' = sapply(paths_split, parse_number),
    'date' = sapply(paths_split, parse_date),
    'trial' = sapply(paths_split, parse_trial),
    'joint' = head$X2,
    'plane' = head$X5
    )
  temp_df$unique_id <- make.names(paste(temp_df$assessor, temp_df$session, temp_df$joint, temp_df$plane, temp_df$trial, sep='_'), unique = TRUE)
  # replace names in columns with nicely formatted names
  for (participant in study_participants) {
    temp_df$participant <- sapply(temp_df$participant, function(x) gsub(participant[2], participant[1], x))
  }
  for (assessor in study_assessors) {
    temp_df$assessor <- sapply(temp_df$assessor, function(x) gsub(assessor[2], assessor[1], x))
  }
  for (session in study_sessions) {
    temp_df$session <- sapply(temp_df$session, function(x) gsub(session[2], session[1], x))
  }
  # merge headers and data and remove data that isnt needed
  sorted_data <- cbind(temp_df, dat)
  remove_df <- subset(sorted_data, (trial=='Static' | joint=='Right Ankle Angles' & plane=='Z') | (joint=='Right Foot Pitch Angles' & plane=='Y') | joint=='Right Ankle Angles_CGM' | joint=='Right Foot Progression' | joint=='Left Ankle Angles' & plane=='Z' | (joint=='Left Foot Pitch Angles' & plane=='Y') | joint=='Left Ankle Angles_CGM' | joint=='Left Foot Progression')
  sorted_data <- sorted_data[setdiff(rownames(sorted_data), rownames(remove_df)),]
  
  return(sorted_data)
}


## Subset data by parameter -----------------------------------------------------------------------
split_by_joint <- function(dat, x,y) {
  df <- subset(dat, joint == x & plane == y)
  df <- df[setdiff(colnames(df), c('participant', 'date', 'trial', 'joint', 'plane'))]
  df <- melt(df, id.vars = c('assessor', 'session', 'unique_id'))
}

## Plot graphs ------------------------------------------------------------------------------------

# All sessions - inputs - data split by day, label
plot_graph_all <- function(x, label, study, side, parameter) {
  # subset
  temp_subset <- subset(x, session == study[1] & variable != 'mean')
  # plot
  plot <- ggplot(temp_subset, aes(x=as.numeric(variable)-1, y=value, group=unique_id, colour=assessor)) +
  geom_line() +
  labs(title=paste(study[1], side[1], sep = ' - '), x='Gait cycle (%)', y=paste(label, ' (\u00B0)')) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(expand = c(0, 0), limits = c(as.numeric(parameter[4]), as.numeric(parameter[5])), breaks = c(seq(-360, 360, as.numeric(parameter[6])))) + 
  scale_x_continuous(expand = c(0, 0), limits = c(0, 101), breaks = c(seq(0, 100, 20))) + 
  theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(),
      legend.position = 'bottom'
      )
}

# Means - inputs - mean data for all sessions and assessors, [y label, (title + y axid limits?)] - hard coded
plot_graph_means <- function(x, side, parameter) {
  # subset
  temp_subset <- subset(x, variable != 'mean' & measurement == 'trial mean')
  # plot
  plot <- ggplot(temp_subset, aes(x=as.numeric(variable)-1, y=value, group=unique_id, colour=assessor, linetype=session)) +
    geom_line() +
    labs(title=paste('All sessions', side[1], sep = ' - '), x='Gait cycle (%)', y=paste(parameter[1], ' (\u00B0)')) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(expand = c(0, 0), limits = c(as.numeric(parameter[4]), as.numeric(parameter[5])), breaks = c(seq(-360, 360, as.numeric(parameter[6])))) + 
    scale_x_continuous(expand = c(0, 0), limits = c(0, 101), breaks = c(seq(0, 100, 20))) + 
    theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = 'bottom'
        )
}

# SEM - inputs - mean data for all sessions and assessors
plot_graph_sem <- function(x, side) {
  temp_subset <- subset(x, variable != 'mean')
  plot <- ggplot(subset(temp_subset, measurement == 'SEM'), aes(x=as.numeric(variable)-1, y=value, group=unique_id, colour=assessor, linetype=session)) +
    geom_line() +
    labs(title=paste('SEM', side[1], sep = ' - '), x='Gait cycle (%)', y=paste('SEM (\u00B0)')) +
    geom_line(data = subset(temp_subset, measurement == 'WSSD'), colour = 'black') +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 101), breaks = c(seq(0, 100, 20))) + 
    theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = 'bottom'
        )
}

# Kinematics
plot_kinematics <- function(x, side) {
  df <- subset(x, variable == 'mean')
  trial_mean <- mean(df$value)
  df$dev <- lapply(df$value, function(x){x - trial_mean})
  temp_plot <- ggplot(df, aes(x=unique_id, y=value, group=assessor, colour=session)) + 
    geom_point() +
    geom_hline(yintercept = trial_mean, colour = '#999999') +
    labs(title=paste('Kinematics', side[1], sep = ' - '), x='trial', y='Average kinematics (\u00B0)') +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = 'bottom') +
    facet_grid(. ~ assessor, scales = 'free')
}

# Combine plots for all session plots
combine_plots <- function(x, leg) {
  plot_left <- plot_grid(plotlist = x[[1]], ncol = 1)
  plot_right <- plot_grid(plotlist = x[[2]], ncol = 1)
  plot_combined <- plot_grid(plot_left, plot_right, ncol = 2)
  plot <- plot_grid(plot_combined, leg, ncol = 1, rel_heights = c(20, 1))
}

# Combine plots and legends for aggregate plots
combine_plots2 <- function(x, leg) {
  plot_combined <- plot_grid(plotlist = x, ncol = 2)
  plot <- plot_grid(plot_combined, leg, ncol = 1, rel_heights = c(20, 1))
}

# Combine plots for aggregated plots
combine_plots3 <- function(temp_mean_list, leg_means, temp_sem_list, leg_sem, temp_kin_list, leg_kin) {
  temp_plot_means <- combine_plots2(temp_mean_list, leg_means)
  temp_plot_sem <- combine_plots2(temp_sem_list, leg_sem)
  temp_plot_kinematics <- combine_plots2(temp_kin_list, leg_kin)
  temp_plot <- plot_grid(temp_plot_means, temp_plot_sem, temp_plot_kinematics, ncol = 1)
  return(temp_plot)
}

## Aggregated data --------------------------------------------------------------------------------

# calculate means per session+assessor - mean at each percentile of all the trials each assessor has done in a session
agg_means <- function(temp_df) {
  temp_means <- dcast(temp_df, assessor+session~variable, mean)
  temp_means$measurement <- rep('trial mean', nrow(temp_means))
  temp_means <- melt(temp_means, id.vars = c('session', 'assessor', 'measurement'))
  return(temp_means)
}

# aggregate sd
agg_sd <- function(temp_df) {
  temp_sd <- dcast(temp_df, assessor+session~variable, sd)
  temp_sd$measurement <- rep('trial SD', nrow(temp_sd))
  temp_sd <- melt(temp_sd, id.vars = c('session', 'assessor', 'measurement'))
  return(temp_sd)
}

# means per participant - 
agg_assessor_means <- function(x) {
  temp_assessor_means <- data.frame(
    'session' = rep('All', length(study_assessors)),
    'measurement' = rep('trial mean', length(study_assessors))
    )
  temp_assessor_means <- cbind(temp_assessor_means, dcast(x, assessor ~ variable, mean))
  temp_assessor_means <- melt(temp_assessor_means, id.vars = c('session', 'assessor', 'measurement'))
  return(temp_assessor_means)
}

# sd per participant - within assessor SD
agg_assessor_sd <- function(x) {
  temp_assessor_sd <- data.frame(
    'session' = rep('All', length(study_assessors)),
    'measurement' = rep('Within assessor SD', length(study_assessors))
    )
  temp_assessor_sd <- cbind(temp_assessor_sd, dcast(x, assessor ~ variable, sd))
  temp_assessor_sd <- melt(temp_assessor_sd, id.vars = c('session', 'assessor', 'measurement'))
  return(temp_assessor_sd)
}

# mean within assessor SD
agg_mean_assessor_sd <- function(x) {
  temp_subset <- subset(x, variable == 'mean')
  temp_df <- data.frame(
    'session' = 'All',
    'assessor' = 'All',
    'measurement' = 'Mean within assessor SD'
    )
  temp_df <- cbind(temp_df, dcast(temp_subset, session ~ variable, function(x) {sqrt(sum(x^2) / length(x))}))
  temp_df <- melt(temp_df, id.vars = c('session', 'assessor', 'measurement'))
  return(temp_df)
}

# Between assessor SD
agg_basd <- function(x, y) {
  temp_basd <- data.frame(
    'session' = rep('All', length(unique(x$variable))),
    'assessor' = rep('All', length(unique(x$variable))),
    'measurement' = rep('Between assessor SD', length(unique(x$variable)))
    )
  temp_basd <- cbind(temp_basd, aggregate(value~variable, data = y, FUN = sd))
  return(temp_basd)
}

# inter-trial rms - per assessor
agg_intertrial_RMS <- function(x) {
  temp_df <- data.frame(
    'session' = rep('All', length(study_assessors)),
    'measurement' = rep('Inter-trial RMS', length(study_assessors))
    )
  temp_df <- cbind(temp_df, dcast(x, assessor ~ variable, function(x) {sqrt(sum(x^2) / length(x))}))
  temp_df <- melt(temp_df, id.vars = c('session', 'assessor', 'measurement'))
  return(temp_df)
}

# intertrial rms - mean
agg_mean_intertrial_RMS <- function(x) {
  temp_subset <- subset(x, variable == 'mean')
  temp_df <- data.frame(
    'session' = 'All',
    'assessor' = 'All',
    'measurement' = 'Inter-trial RMS'
    )
  temp_df <- cbind(temp_df, aggregate(value~variable, data = temp_subset, function(x) {sqrt(sum(x^2) / length(x))}))
  return(temp_df)
}

# individual sem - for each participant - 
agg_ind_sem <- function(x) {
  temp_ind_sem <- data.frame(
    'session' = rep('All', length(study_assessors)),
    'measurement' = rep('Individual_SEM', length(study_assessors))
    )
  temp_ind_sem <- cbind(temp_ind_sem, dcast(x, assessor ~ variable, sd))
  temp_ind_sem <- melt(temp_ind_sem, id.vars = c('session', 'assessor', 'measurement'))
  return(temp_ind_sem)
}

# sem
agg_sem <- function(x) {
  temp_sem <- data.frame(
    'session' = x$session,
    'assessor' = x$assessor,
    'measurement' = rep('SEM', nrow(x)),
    'variable' = x$variable,
    'value' = sapply(x$value, function(y) {sqrt(sum(y^2) / length(study_participants))})
  )
  return(temp_sem)
}

# wssd
agg_wssd <- function(x) {
  temp_wssd <- data.frame(
    'session' = rep('All', length(unique(x$variable))),
    'assessor' = rep('All', length(unique(x$variable))),
    'measurement' = rep('WSSD', length(unique(x$variable)))
    )
  temp_wssd <- cbind(temp_wssd, aggregate(value~variable, data = x, FUN = sd))
  return(temp_wssd)
}

# wass
agg_wass <- function(x, y) {
  temp_wass <- data.frame(
    'session' = rep('All', length(unique(x$variable))),
    'assessor' = rep('All', length(unique(x$variable))),
    'measurement' = rep('WASS', length(unique(x$variable)))
    )
  temp_wass <- cbind(temp_wass, aggregate(value~variable, data = y, FUN = function(x) {sqrt(sum(x^2) / length(study_participants))}))
  return(temp_wass)
}

# Combine all aggregated data
agg_combine <- function(temp_df) {
  temp_means <- agg_means(temp_df)
  temp_sd <- agg_sd(temp_df)
  temp_assessor_means <- agg_assessor_means(temp_means)
  temp_assessor_sd <- agg_assessor_sd(temp_means)
  temp_mean_assessor_sd <- agg_mean_assessor_sd(temp_assessor_sd)
  temp_intertrial_RMS <- agg_intertrial_RMS(temp_sd)
  temp_mean_intertrial_RMS <- agg_mean_intertrial_RMS(temp_sd)
  temp_basd <- agg_basd(temp_means, temp_assessor_means)
  temp_ind_sem <- agg_ind_sem(temp_means)
  temp_sem <- agg_sem(temp_ind_sem)
  temp_wssd <- agg_wssd(temp_means)
  temp_wass <- agg_wass(temp_means, temp_ind_sem)
  
  names(temp_means) <- c('session', 'assessor', 'measurement', 'variable', 'value')
  names(temp_sd) <- c('session', 'assessor', 'measurement', 'variable', 'value')
  names(temp_assessor_means) <- c('session', 'assessor', 'measurement', 'variable', 'value')
  names(temp_assessor_sd) <- c('session', 'assessor', 'measurement', 'variable', 'value')
  names(temp_mean_assessor_sd) <- c('session', 'assessor', 'measurement', 'variable', 'value')
  names(temp_intertrial_RMS) <- c('session', 'assessor', 'measurement', 'variable', 'value')
  names(temp_mean_intertrial_RMS) <- c('session', 'assessor', 'measurement', 'variable', 'value')
  names(temp_basd) <- c('session', 'assessor', 'measurement', 'variable', 'value')
  names(temp_ind_sem) <- c('session', 'assessor', 'measurement', 'variable', 'value')
  names(temp_sem) <- c('session', 'assessor', 'measurement', 'variable', 'value')
  names(temp_wssd) <- c('session', 'assessor', 'measurement', 'variable', 'value')
  names(temp_wass) <- c('session', 'assessor', 'measurement', 'variable', 'value')
  df <- rbind(temp_means, temp_sd, temp_assessor_means, temp_assessor_sd, temp_mean_assessor_sd, temp_intertrial_RMS, temp_mean_intertrial_RMS, temp_basd, temp_ind_sem, temp_sem, temp_wssd, temp_wass)
  df$unique_id <- make.names(paste(df$assessor, df$session, df$measurement, sep='_'))
  
  return(df)
}


## Gait reliability profile -----------------------------------------------------------------------

# create gait reliability profile df
create_grp_df <- function(x) {
  df <- data.frame('Parameter' = character(),
                    'Inter-trial' = character(), 
                    'WASS' = character(),
                    'BASS' = character(),
                    'Procedural' = character(),
                    'Total' = character(),
                    'Total-procedural' = character()
  )
  return(df)
}

# collect grp data from each loop of the parameters
grp_df_loop <- function(x, side, parameter) {
  temp_grp <- data.frame(
    'Parameter' = paste(side[1], parameter[1], sep = ' '),
    'Intertrial' = subset(x, assessor == 'All' & session == 'All' & variable == 'mean' & measurement == 'Inter-trial RMS')$value,
    'WASS' = subset(x, assessor == 'All' & session == 'All' & variable == 'mean' & measurement == 'Mean within assessor SD')$value,
    'BASS' = subset(x, assessor == 'All' & session == 'All' & variable == 'mean' & measurement == 'Between assessor SD')$value
  )
}

# Aggregate data for assessor grp
combine_grp <- function(x, temp_wass_list, temp_wass_names, temp_intertrial_list, temp_intertrial_names) {
  temp_wass_df <- data.frame(temp_wass_list)
  names(temp_wass_df) <- temp_wass_names
  temp_intertrial_df <- data.frame(temp_intertrial_list)
  names(temp_intertrial_df) <- temp_intertrial_names
  temp_grp <- cbind(x, temp_wass_df, temp_intertrial_df)
  return(temp_grp)
}

combine_grp2 <- function(x, y) {
  x$Procedural = sqrt(sum(x$WASS^2)+(x$BASS^2))
  x$Total = sqrt(sum(x$WASS^2) + (x$BASS^2) + (x$Intertrial^2))
  x$`Total-procedural` = x$Total - x$Procedural
  # add to list
  grp_df <- rbind(y, x)
}


plot_all_grp <- function(x) {
  df_long <- melt(grp_df, id.vars = 'Parameter')
  
  g <- ggplot(subset(df_long, variable == 'Total'), aes(x=Parameter, y=value)) + 
    geom_bar(stat='identity', position = 'identity', fill='gold') + 
    geom_bar(data = subset(df_long, variable == 'Procedural'), aes(x=Parameter, y=value), stat='identity', position = 'identity', fill='royalblue') + 
    geom_point(data = subset(df_long, variable == 'BASS'), aes(x=Parameter, y=value), size=12, shape=95) + 
    geom_point(data = subset(df_long, variable == 'WASS'), aes(x=Parameter, y=value), size=12, shape=95, colour='red') +
    labs(y='SD (\u00B0)') +
    theme(axis.text.x = element_text(angle = 90, vjust=0.4, size = 12),
          axis.title.x = element_blank())
  return(g)
}

# create df for gait reliability profile per assessor
# input - assessor name
create_grp_assessor_df <- function(x, grp_long) {
  temp_intertrial <- paste('Intertrial', x[1], sep = '-')
  temp_wass <- paste('WASS', x[1], sep = '-')
  temp_df <- data.frame(
    'Parameter' = unique(grp_long$Parameter),
    'Intertrial' = subset(grp_long, variable == temp_intertrial)$value,
    'WASS' = subset(grp_long, variable == temp_wass)$value
  )
  temp_df$Total = sqrt((as.numeric(temp_df$WASS)^2) + (as.numeric(temp_df$Intertrial)^2))
  temp_df$`Total-procedural` = as.numeric(temp_df$Total) - as.numeric(temp_df$WASS)
  
  return(temp_df)
}

# plot gait reliability profile per assessor
plot_assessor_grp <- function(x) {
  df_long <- melt(x, id.vars = 'Parameter')
  plot <- ggplot(subset(df_long, variable == 'Total'), aes(x=Parameter, y=value)) + 
          geom_bar(stat='identity', position = 'identity', fill='forestgreen') + 
          geom_bar(data = subset(df_long, variable == 'WASS'), aes(x=Parameter, y=value), stat='identity', position = 'identity', fill='firebrick2') + 
          labs(y='SD (\u00B0)') +
          theme(axis.text.x = element_text(angle = 90, vjust=0.4, size = 12),
               axis.title.x = element_blank())
  return(plot)
}

## Call functions ------------------------------------------------------
main_summary <- function() {
  cat('  \n#Summary {.tabset .tabset-fade .tabset-pills}', '  \n')
  cat('  \n## Parameters - ', settings_summary_count(study_parameters), '  \n')
  settings_summary_list(study_parameters)
  
  cat('  \n## Assessors - ', settings_summary_count(study_assessors), '  \n')
  settings_summary_list(study_assessors)
  
  cat('  \n## Number of sessions - ', settings_summary_count(study_sessions), '  \n')
}


main_sorting <- function(study_sides) {
  input_list <- vector('list', length(study_sides))
  i=1
  for (data in study_sides) {
    # load headers and data seperately
    temp_headers <- load_headers(data[2])
    temp_data <- load_data(data[2])
    temp_data$mean <- rowMeans(temp_data)
    # combine and sort data
    temp_sorted <- sort_data(temp_data, temp_headers)
    input_list[[i]] <- temp_sorted
    i=i+1
  }
  # concatenate dataframes in list
  sorted_data <- do.call('rbind', input_list)
  write.csv(sorted_data, 'results/all_data_sorted.csv')
  
  return(sorted_data)
}


main_analysis_latex <- function(sorted_data, study_parameters, study_sides, study_sessions, study_assessors) {
  grp_df <- create_grp_df()
  for (parameter in study_parameters){
    ### Data from all trials
    # format pdf
    cat('\n\\pagebreak\n')
    cat('  \n##', parameter[1], '  \n')
    cat('  \n###All trials', '  \n')
    # subset by side
    temp_side_list <- vector('list', length(study_sides))
    i=1
    for (side in study_sides){
      # subset total data by joint and axis
      temp_parameter <- paste(side[1], parameter[2], sep = ' ')
      temp_df <- split_by_joint(sorted_data, temp_parameter, parameter[3])
      temp_output <- dcast(temp_df, assessor+session+unique_id~variable, value.var = 'value')
      write.csv(temp_output, paste('results/', side[1], '_', parameter[1], '_df_all.csv', sep = ''))
      # Loop through sessions
      temp_session_list <- vector('list', length(study_sessions))
      j=1
      for (study in study_sessions) {
        # plot graph
        temp_plot <- plot_graph_all(temp_df, parameter[1], study, side, parameter) + theme(legend.position = 'none')
        temp_leg <- get_legend(plot_graph_all(temp_df, parameter[1], study, side, parameter))
        temp_session_list[[j]] <- temp_plot
        j=j+1
      }
      temp_side_list[[i]] <- temp_session_list
      i=i+1
    }
    # combine left and right plots, display plot and save
    temp_plot <- combine_plots(temp_side_list, temp_leg)
    plot(temp_plot)
    temp_name <- paste('results/', parameter[1], '_plot_all.png', sep = '')
    save_plot(temp_name, temp_plot, base_height = 15, base_width = 12)
    
    ### Aggregated data for plots
    # format pdf
    cat('\n\\pagebreak\n')
    cat('  \n##', parameter[1], '  \n')
    cat('  \n###Aggregated', '  \n')
    # create empty lists
    temp_mean_list <- vector('list', length(study_sides))
    temp_sem_list <- vector('list', length(study_sides))
    temp_kin_list <- vector('list', length(study_sides))
    # subset by side
    i=1
    for (side in study_sides){
      # subset total data by joint and axis and calculate aggregate data
      temp_parameter <- paste(side[1], parameter[2], sep = ' ')
      temp_df <- split_by_joint(sorted_data, temp_parameter, parameter[3])
      temp_output <- agg_combine(temp_df)
      # plot means, sem and kinematics and add to list to be combined later
      temp_mean_list[[i]] <- plot_graph_means(temp_output, side, parameter) + theme(legend.position = 'none')
      leg_means <- get_legend(plot_graph_means(temp_output, side, parameter))
      temp_sem_list[[i]] <- plot_graph_sem(temp_output, side) + theme(legend.position = 'none')
      leg_sem <- get_legend(plot_graph_sem(temp_output, side))
      temp_kin_list[[i]] <- plot_kinematics(temp_df, side) + theme(legend.position = 'none')
      leg_kin <- get_legend(plot_kinematics(temp_df, side))
      
      ### Aggregated data for gait reproducibility profile
      temp_grp <- grp_df_loop(temp_output, side, parameter)
      # loop through assessors for assessor WASS
      temp_wass_list <- vector('list', length(study_assessors))
      temp_wass_names <- vector('list', length(study_assessors))
      temp_intertrial_list <- vector('list', length(study_assessors))
      temp_intertrial_names <- vector('list', length(study_assessors))
      j=1
      for (a in study_assessors){
        temp_wass_list[[j]] <- subset(temp_output, assessor == a[1] & session == 'All' & variable == 'mean' & measurement == 'Within assessor SD')$value
        temp_intertrial_list[[j]] <- subset(temp_output, assessor == a[1] & session == 'All' & variable == 'mean' & measurement == 'Inter-trial RMS')$value
        temp_wass_names[[j]] <- paste('WASS', a[1], sep = '-')
        temp_intertrial_names[[j]] <- paste('Intertrial', a[1], sep = '-')
        j=j+1
      }
      temp_grp <- combine_grp(temp_grp, temp_wass_list, temp_wass_names, temp_intertrial_list, temp_intertrial_names)
      # calcualtions and add to list
      grp_df <- combine_grp2(temp_grp, grp_df)
      
      ### Save dataframe
      # unmelt and save df
      temp_output <- dcast(temp_output, assessor+session+measurement~variable, value.var = 'value')
      write.csv(temp_output, paste('results/', side[1], '_', parameter[1], '_df_processed.csv', sep = ''))
      i=i+1
    }
    ### Combine left and right aggregate plots, display and save
    temp_plot <- combine_plots3(temp_mean_list, leg_means, temp_sem_list, leg_sem, temp_kin_list, leg_kin)
    plot(temp_plot)
    temp_name <- paste('results/', parameter[1], '_plot_aggregated.png', sep = '')
    save_plot(temp_name, temp_plot, base_height = 15, base_width = 12)
  }
  
  return(grp_df)
}

main_analysis_html <- function(sorted_data, study_parameters, study_sides, study_sessions, study_assessors) {
  grp_df <- create_grp_df()
  cat('\n\\pagebreak\n')
  cat('  \n#Results {.tabset .tabset-fade .tabset-pills}', '  \n')
  for (parameter in study_parameters){
    ### Data from all trials
    # format pdf
    cat('\n\\pagebreak\n')
    cat('  \n##', parameter[1], '  \n')
    cat('  \n###All trials', '  \n')
    # subset by side
    temp_side_list <- vector('list', length(study_sides))
    i=1
    for (side in study_sides){
      # subset total data by joint and axis
      temp_parameter <- paste(side[1], parameter[2], sep = ' ')
      temp_df <- split_by_joint(sorted_data, temp_parameter, parameter[3])
      temp_output <- dcast(temp_df, assessor+session+unique_id~variable, value.var = 'value')
      write.csv(temp_output, paste('results/', side[1], '_', parameter[1], '_df_all.csv', sep = ''))
      # Loop through sessions
      temp_session_list <- vector('list', length(study_sessions))
      j=1
      for (study in study_sessions) {
        # plot graph
        temp_plot <- plot_graph_all(temp_df, parameter[1], study, side, parameter) + theme(legend.position = 'none')
        temp_leg <- get_legend(plot_graph_all(temp_df, parameter[1], study, side, parameter))
        temp_session_list[[j]] <- temp_plot
        j=j+1
      }
      temp_side_list[[i]] <- temp_session_list
      i=i+1
    }
    # combine left and right plots, display plot and save
    temp_plot <- combine_plots(temp_side_list, temp_leg)
    plot(temp_plot)
    temp_name <- paste('results/', parameter[1], '_plot_all.png', sep = '')
    save_plot(temp_name, temp_plot, base_height = 15, base_width = 12)
    
    ### Aggregated data for plots
    # format pdf
    cat('\n\\pagebreak\n')
    cat('  \n###Aggregated', '  \n')
    # create empty lists
    temp_mean_list <- vector('list', length(study_sides))
    temp_sem_list <- vector('list', length(study_sides))
    temp_kin_list <- vector('list', length(study_sides))
    # subset by side
    i=1
    for (side in study_sides){
      # subset total data by joint and axis and calculate aggregate data
      temp_parameter <- paste(side[1], parameter[2], sep = ' ')
      temp_df <- split_by_joint(sorted_data, temp_parameter, parameter[3])
      temp_output <- agg_combine(temp_df)
      # plot means, sem and kinematics and add to list to be combined later
      temp_mean_list[[i]] <- plot_graph_means(temp_output, side, parameter) + theme(legend.position = 'none')
      leg_means <- get_legend(plot_graph_means(temp_output, side, parameter))
      temp_sem_list[[i]] <- plot_graph_sem(temp_output, side) + theme(legend.position = 'none')
      leg_sem <- get_legend(plot_graph_sem(temp_output, side))
      temp_kin_list[[i]] <- plot_kinematics(temp_df, side) + theme(legend.position = 'none')
      leg_kin <- get_legend(plot_kinematics(temp_df, side))
      
      ### Aggregated data for gait reproducibility profile
      temp_grp <- grp_df_loop(temp_output, side, parameter)
      # loop through assessors for assessor WASS
      temp_wass_list <- vector('list', length(study_assessors))
      temp_wass_names <- vector('list', length(study_assessors))
      temp_intertrial_list <- vector('list', length(study_assessors))
      temp_intertrial_names <- vector('list', length(study_assessors))
      j=1
      for (a in study_assessors){
        temp_wass_list[[j]] <- subset(temp_output, assessor == a[1] & session == 'All' & variable == 'mean' & measurement == 'Within assessor SD')$value
        temp_intertrial_list[[j]] <- subset(temp_output, assessor == a[1] & session == 'All' & variable == 'mean' & measurement == 'Inter-trial RMS')$value
        temp_wass_names[[j]] <- paste('WASS', a[1], sep = '-')
        temp_intertrial_names[[j]] <- paste('Intertrial', a[1], sep = '-')
        j=j+1
      }
      temp_grp <- combine_grp(temp_grp, temp_wass_list, temp_wass_names, temp_intertrial_list, temp_intertrial_names)
      # calcualtions and add to list
      grp_df <- combine_grp2(temp_grp, grp_df)
      
      ### Save dataframe
      # unmelt and save df
      temp_output <- dcast(temp_output, assessor+session+measurement~variable, value.var = 'value')
      write.csv(temp_output, paste('results/', side[1], '_', parameter[1], '_df_processed.csv', sep = ''))
      i=i+1
    }
    ### Combine left and right aggregate plots, display and save
    temp_plot <- combine_plots3(temp_mean_list, leg_means, temp_sem_list, leg_sem, temp_kin_list, leg_kin)
    plot(temp_plot)
    temp_name <- paste('results/', parameter[1], '_plot_aggregated.png', sep = '')
    save_plot(temp_name, temp_plot, base_height = 15, base_width = 12)
  }
  
  return(grp_df)
}



main_grp_latex <- function(grp_df, study_assessors) {
  cat('\n\\pagebreak\n')
  cat('  \n## Gait reliability profile -- all', '  \n')
  # Plot graph and table for gait reliability profile of all assessors
  grp_long <- melt(grp_df, id.vars = 'Parameter')
  plot(plot_all_grp(grp_df))
  colnames(grp_df)[3] <- '\\textbf{\\textcolor{red}{WASS}}'
  colnames(grp_df)[4] <- '\\textbf{BASS}'
  colnames(grp_df)[11] <- '\\textbf{\\textcolor{RoyalBlue}{Procedural}}'
  colnames(grp_df)[12] <- '\\textbf{\\textcolor{Goldenrod}{Total}}'
  print(xtable(grp_df[c(1,2,3,4,11,12,13)]), type = 'latex', include.rownames = FALSE, comment = FALSE, sanitize.colnames.function = function(x) {x})
  # Plot graph and table for gait reliability profile of each assessor
  for (assessor in study_assessors) {
    # format pdf
    cat('\n\\pagebreak\n')
    cat('  \n## Gait reliability profile -- ', assessor[1], '  \n')
    # make per assessor dataframe, plot graph and print table
    assessor_df <- create_grp_assessor_df(assessor, grp_long)
    plot(plot_assessor_grp(assessor_df))
    colnames(assessor_df)[3] <- '\\textbf{\\textcolor{red}{WASS}}'
    colnames(assessor_df)[4] <- '\\textbf{\\textcolor{ForestGreen}{Total}}'
    print(xtable(assessor_df), type = 'latex', include.rownames = FALSE, comment = FALSE, sanitize.colnames.function = function(x) {x})
  }
}


main_grp_html <- function(grp_df, study_assessors) {
  cat('\n\\pagebreak\n')
  cat('  \n## Gait reliability profile -- all', '  \n')
  # Plot graph and table for gait reliability profile of all assessors
  grp_long <- melt(grp_df, id.vars = 'Parameter')
  plot(plot_all_grp(grp_df))
  colnames(grp_df)[3] <- '\\textbf{\\textcolor{red}{WASS}}'
  colnames(grp_df)[4] <- '\\textbf{BASS}'
  colnames(grp_df)[11] <- '\\textbf{\\textcolor{RoyalBlue}{Procedural}}'
  colnames(grp_df)[12] <- '\\textbf{\\textcolor{Goldenrod}{Total}}'
  grp_df[c(1,2,3,4,11,12,13)]
  # Plot graph and table for gait reliability profile of each assessor
  for (assessor in study_assessors) {
    # format pdf
    cat('\n\\pagebreak\n')
    cat('  \n## Gait reliability profile -- ', assessor[1], '  \n')
    # make per assessor dataframe, plot graph and print table
    assessor_df <- create_grp_assessor_df(assessor, grp_long)
    plot(plot_assessor_grp(assessor_df))
    colnames(assessor_df)[3] <- '\\textbf{\\textcolor{red}{WASS}}'
    colnames(assessor_df)[4] <- '\\textbf{\\textcolor{ForestGreen}{Total}}'
    assessor_df
  }
}
