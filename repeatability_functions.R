## Repeatability functions
# Define functions called within the repeatabiity script

## Load libraries ---------------------------------------------------------------------------------
library('ggplot2')
library('reshape2')
library('xtable')
library('cowplot')


## Loading data -----------------------------------------------------------------------------------

# Load headers from raw input
load_headers <- function(infile) {
  df <- read.delim(file = infile, header = FALSE, nrows = 5)
  df <- data.frame(t(df[, seq(2, ncol(df))]))
}

# Load data from raw input
load_data <- function(infile) {
  df <- read.delim(file = infile, header = FALSE, skip = 5)
  df <- data.frame(t(df[, seq(2, ncol(df))]))
}


## Settings summary functions - for summary on first page of report -------------------------------

# Count number of settings
settings_summary_count <- function(setting) {
  num <- length(setting)
  return(num)
}

# List settings
settings_summary_list <- function(setting) {
  for (parameter in setting) {
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
  df <- data.frame(
    'participant' = sapply(paths_split, parse_participant),
    'assessor' = sapply(paths_split, parse_assessor),
    'session' = sapply(paths_split, parse_number),
    'date' = sapply(paths_split, parse_date),
    'trial' = sapply(paths_split, parse_trial),
    'joint' = head$X2,
    'plane' = head$X5
    )
  df$unique_id <- make.names(paste(df$assessor, df$session, df$joint, df$plane, df$trial, sep='_'), unique = TRUE)
  # replace names in columns with nicely formatted names
  for (participant in study_participants) {
    df$participant <- sapply(df$participant, function(x) gsub(participant[2], participant[1], x))
  }
  for (assessor in study_assessors) {
    df$assessor <- sapply(df$assessor, function(x) gsub(assessor[2], assessor[1], x))
  }
  for (session in study_sessions) {
    df$session <- sapply(df$session, function(x) gsub(session[2], session[1], x))
  }
  # merge headers and data and remove data that isnt needed
  sorted_data <- cbind(df, dat)
  remove_df <- subset(sorted_data, (trial=='Static' | joint=='Right Ankle Angles' & plane=='Z') | (joint=='Right Foot Pitch Angles' & plane=='Y') | joint=='Right Ankle Angles_CGM' | joint=='Right Foot Progression' | joint=='Left Ankle Angles' & plane=='Z' | (joint=='Left Foot Pitch Angles' & plane=='Y') | joint=='Left Ankle Angles_CGM' | joint=='Left Foot Progression')
  sorted_data <- sorted_data[setdiff(rownames(sorted_data), rownames(remove_df)),]
  
  return(sorted_data)
}


## Subset data by parameter -----------------------------------------------------------------------
split_by_joint <- function(dat, subset_joint, subset_plane) {
  df <- subset(dat, joint == subset_joint & plane == subset_plane)
  df <- df[setdiff(colnames(df), c('participant', 'date', 'trial', 'joint', 'plane'))]
  df <- melt(df, id.vars = c('assessor', 'session', 'unique_id'))
}


## Plot graphs ------------------------------------------------------------------------------------

# Plot graph with all sessions overlayed
plot_graph_all <- function(df, study, side, parameter) {
  # subset
  df_subset <- subset(df, session == study[1] & variable != 'mean')
  # plot
  plot <- ggplot(df_subset, aes(x=as.numeric(variable)-1, y=value, group=unique_id, colour=assessor)) +
  geom_line() +
  labs(title=paste(study[1], side[1], sep = ' - '), x='Gait cycle (%)', y=paste(parameter[1], ' (\u00B0)')) +
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

# Plot graph of assessor-session means
plot_graph_means <- function(df, side, parameter) {
  # subset
  df_subset <- subset(df, variable != 'mean' & measurement == 'trial mean')
  # plot
  plot <- ggplot(df_subset, aes(x=as.numeric(variable)-1, y=value, group=unique_id, colour=assessor, linetype=session)) +
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

# Plot SEM graph for average and per assessor
plot_graph_sem <- function(df, side) {
  df_subset <- subset(df, variable != 'mean')
  plot <- ggplot(subset(df_subset, measurement == 'SEM'), aes(x=as.numeric(variable)-1, y=value, group=unique_id, colour=assessor, linetype=session)) +
    geom_line() +
    labs(title=paste('SEM', side[1], sep = ' - '), x='Gait cycle (%)', y=paste('SEM (\u00B0)')) +
    geom_line(data = subset(df_subset, measurement == 'WSSD'), colour = 'black') +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 101), breaks = c(seq(0, 100, 20))) + 
    theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = 'bottom'
        )
}

# Plot kinematics graph
plot_kinematics <- function(df, side) {
  df_subset <- subset(df, variable == 'mean')
  trial_mean <- mean(df_subset$value)
  df_subset$dev <- lapply(df_subset$value, function(x){x - trial_mean})
  plot <- ggplot(df_subset, aes(x=unique_id, y=value, group=assessor, colour=session)) + 
    geom_point() +
    geom_hline(yintercept = trial_mean, colour = '#999999') +
    labs(title=paste('Kinematics', side[1], sep = ' - '), x='Trial', y='Average kinematics (\u00B0)') +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = 'bottom') +
    facet_grid(. ~ assessor, scales = 'free')
}

# Combine plots for all session plots
combine_plots_all <- function(plots, leg) {
  plot_left <- plot_grid(plotlist = plots[[1]], ncol = 1)
  plot_right <- plot_grid(plotlist = plots[[2]], ncol = 1)
  plot_combined <- plot_grid(plot_left, plot_right, ncol = 2)
  plot <- plot_grid(plot_combined, leg, ncol = 1, rel_heights = c(20, 1))
}

# Combine plots and legends for each set of aggregate plots
combine_plots_agg2 <- function(plots, leg) {
  plot_combined <- plot_grid(plotlist = plots, ncol = 2)
  plot <- plot_grid(plot_combined, leg, ncol = 1, rel_heights = c(20, 1))
}

# Combine plots for all aggregated plots
combine_plots_agg <- function(mean_list, leg_means, sem_list, leg_sem, kin_list, leg_kin) {
  plot_means <- combine_plots_agg2(mean_list, leg_means)
  plot_sem <- combine_plots_agg2(sem_list, leg_sem)
  plot_kinematics <- combine_plots_agg2(kin_list, leg_kin)
  plot <- plot_grid(plot_means, plot_sem, plot_kinematics, ncol = 1)
  return(plot)
}

## Aggregated data --------------------------------------------------------------------------------

# calculate means per session+assessor - mean at each percentile of all the trials each assessor has done in a session
agg_means <- function(df) {
  means <- dcast(df, assessor+session~variable, mean)
  means$measurement <- rep('trial mean', nrow(means))
  means <- melt(means, id.vars = c('session', 'assessor', 'measurement'))
  return(means)
}

# aggregate sd
agg_sd <- function(df) {
  sd_df <- dcast(df, assessor+session~variable, sd)
  sd_df$measurement <- rep('trial SD', nrow(sd_df))
  sd_df <- melt(sd_df, id.vars = c('session', 'assessor', 'measurement'))
  return(sd_df)
}

# means per participant - 
agg_assessor_means <- function(df) {
  assessor_means <- data.frame(
    'session' = rep('All', length(study_assessors)),
    'measurement' = rep('trial mean', length(study_assessors))
    )
  assessor_means <- cbind(assessor_means, dcast(df, assessor ~ variable, mean))
  assessor_means <- melt(assessor_means, id.vars = c('session', 'assessor', 'measurement'))
  return(assessor_means)
}

# sd per participant - within assessor SD
agg_assessor_sd <- function(df) {
  assessor_sd <- data.frame(
    'session' = rep('All', length(study_assessors)),
    'measurement' = rep('Within assessor SD', length(study_assessors))
    )
  assessor_sd <- cbind(assessor_sd, dcast(df, assessor ~ variable, sd))
  assessor_sd <- melt(assessor_sd, id.vars = c('session', 'assessor', 'measurement'))
  return(assessor_sd)
}

# mean within assessor SD
agg_mean_assessor_sd <- function(df) {
  df_subset <- subset(df, variable == 'mean')
  out_df <- data.frame(
    'session' = 'All',
    'assessor' = 'All',
    'measurement' = 'Mean within assessor SD'
    )
  out_df <- cbind(out_df, dcast(df_subset, session ~ variable, function(x) {sqrt(sum(x^2) / length(x))}))
  out_df <- melt(out_df, id.vars = c('session', 'assessor', 'measurement'))
  return(out_df)
}

# Between assessor SD
agg_basd <- function(means, assessor_means) {
  basd_df <- data.frame(
    'session' = rep('All', length(unique(means$variable))),
    'assessor' = rep('All', length(unique(means$variable))),
    'measurement' = rep('Between assessor SD', length(unique(means$variable)))
    )
  basd_df <- cbind(basd_df, aggregate(value~variable, data = assessor_means, FUN = sd))
  return(basd_df)
}

# inter-trial rms - per assessor
agg_intertrial_RMS <- function(inp) {
  df <- data.frame(
    'session' = rep('All', length(study_assessors)),
    'measurement' = rep('Inter-trial RMS', length(study_assessors))
    )
  df <- cbind(df, dcast(inp, assessor ~ variable, function(x) {sqrt(sum(x^2) / length(x))}))
  df <- melt(df, id.vars = c('session', 'assessor', 'measurement'))
  return(df)
}

# intertrial rms - mean
agg_mean_intertrial_RMS <- function(inp) {
  df_subset <- subset(inp, variable == 'mean')
  df <- data.frame(
    'session' = 'All',
    'assessor' = 'All',
    'measurement' = 'Inter-trial RMS'
    )
  df <- cbind(df, aggregate(value~variable, data = df_subset, function(x) {sqrt(sum(x^2) / length(x))}))
  return(df)
}

# individual sem - for each participant - 
agg_ind_sem <- function(inp) {
  ind_sem <- data.frame(
    'session' = rep('All', length(study_assessors)),
    'measurement' = rep('Individual_SEM', length(study_assessors))
    )
  ind_sem <- cbind(ind_sem, dcast(inp, assessor ~ variable, sd))
  ind_sem <- melt(ind_sem, id.vars = c('session', 'assessor', 'measurement'))
  return(ind_sem)
}

# sem
agg_sem <- function(inp) {
  sem_df <- data.frame(
    'session' = inp$session,
    'assessor' = inp$assessor,
    'measurement' = rep('SEM', nrow(inp)),
    'variable' = inp$variable,
    'value' = sapply(inp$value, function(x) {sqrt(sum(x^2) / length(study_participants))})
  )
  return(sem_df)
}

# wssd
agg_wssd <- function(inp) {
  wssd <- data.frame(
    'session' = rep('All', length(unique(inp$variable))),
    'assessor' = rep('All', length(unique(inp$variable))),
    'measurement' = rep('WSSD', length(unique(inp$variable)))
    )
  wssd <- cbind(wssd, aggregate(value~variable, data = inp, FUN = sd))
  return(wssd)
}

# wass
agg_wass <- function(means, ind_sem) {
  wass <- data.frame(
    'session' = rep('All', length(unique(means$variable))),
    'assessor' = rep('All', length(unique(means$variable))),
    'measurement' = rep('WASS', length(unique(means$variable)))
    )
  wass <- cbind(wass, aggregate(value~variable, data = ind_sem, FUN = function(x) {sqrt(sum(x^2) / length(study_participants))}))
  return(wass)
}

# Combine all aggregated data
agg_combine <- function(df) {
  means <- agg_means(df)
  stddev <- agg_sd(df)
  assessor_means <- agg_assessor_means(means)
  assessor_sd <- agg_assessor_sd(means)
  mean_assessor_sd <- agg_mean_assessor_sd(assessor_sd)
  intertrial_RMS <- agg_intertrial_RMS(stddev)
  mean_intertrial_RMS <- agg_mean_intertrial_RMS(stddev)
  basd <- agg_basd(means, assessor_means)
  ind_sem <- agg_ind_sem(means)
  sem <- agg_sem(ind_sem)
  wssd <- agg_wssd(means)
  wass <- agg_wass(means, ind_sem)
  
  names(means) <- c('session', 'assessor', 'measurement', 'variable', 'value')
  names(stddev) <- c('session', 'assessor', 'measurement', 'variable', 'value')
  names(assessor_means) <- c('session', 'assessor', 'measurement', 'variable', 'value')
  names(assessor_sd) <- c('session', 'assessor', 'measurement', 'variable', 'value')
  names(mean_assessor_sd) <- c('session', 'assessor', 'measurement', 'variable', 'value')
  names(intertrial_RMS) <- c('session', 'assessor', 'measurement', 'variable', 'value')
  names(mean_intertrial_RMS) <- c('session', 'assessor', 'measurement', 'variable', 'value')
  names(basd) <- c('session', 'assessor', 'measurement', 'variable', 'value')
  names(ind_sem) <- c('session', 'assessor', 'measurement', 'variable', 'value')
  names(sem) <- c('session', 'assessor', 'measurement', 'variable', 'value')
  names(wssd) <- c('session', 'assessor', 'measurement', 'variable', 'value')
  names(wass) <- c('session', 'assessor', 'measurement', 'variable', 'value')
  df <- rbind(means, stddev, assessor_means, assessor_sd, mean_assessor_sd, intertrial_RMS, mean_intertrial_RMS, basd, ind_sem, sem, wssd, wass)
  df$unique_id <- make.names(paste(df$assessor, df$session, df$measurement, sep='_'))
  
  return(df)
}


## Gait reliability profile -----------------------------------------------------------------------

# create empty gait reliability profile df
create_grp_df <- function() {
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
grp_df_loop <- function(df, side, parameter) {
  grp <- data.frame(
    'Parameter' = paste(side[1], parameter[1], sep = ' '),
    'Intertrial' = subset(df, assessor == 'All' & session == 'All' & variable == 'mean' & measurement == 'Inter-trial RMS')$value,
    'WASS' = subset(df, assessor == 'All' & session == 'All' & variable == 'mean' & measurement == 'Mean within assessor SD')$value,
    'BASS' = subset(df, assessor == 'All' & session == 'All' & variable == 'mean' & measurement == 'Between assessor SD')$value
  )
  return(grp)
}

# Aggregate data for assessor grp
combine_subset_grp <- function(df, wass_list, wass_names, intertrial_list, intertrial_names) {
  wass_df <- data.frame(wass_list)
  names(wass_df) <- wass_names
  intertrial_df <- data.frame(intertrial_list)
  names(intertrial_df) <- intertrial_names
  grp <- cbind(df, wass_df, intertrial_df)
  return(grp)
}

# add data to main df
combine_all_grp <- function(df, grp_df) {
  df$Procedural = sqrt(sum(df$WASS^2)+(df$BASS^2))
  df$Total = sqrt(sum(df$WASS^2) + (df$BASS^2) + (df$Intertrial^2))
  df$`Total-procedural` = df$Total - df$Procedural
  # add to list
  grp_df <- rbind(grp_df, df)
}

# plot overall gait reliability profile
plot_all_grp <- function(df) {
  df_long <- melt(df, id.vars = 'Parameter')
  
  p <- ggplot(subset(df_long, variable == 'Total'), aes(x=Parameter, y=value)) + 
    geom_bar(stat='identity', position = 'identity', fill='gold') + 
    geom_bar(data = subset(df_long, variable == 'Procedural'), aes(x=Parameter, y=value), stat='identity', position = 'identity', fill='royalblue') + 
    geom_point(data = subset(df_long, variable == 'BASS'), aes(x=Parameter, y=value), size=12, shape=95) + 
    geom_point(data = subset(df_long, variable == 'WASS'), aes(x=Parameter, y=value), size=12, shape=95, colour='red') +
    labs(y='SD (\u00B0)') +
    theme(axis.text.x = element_text(angle = 90, vjust=0.4, size = 12),
          axis.title.x = element_blank())
  return(p)
}

# create df for gait reliability profile per assessor
create_grp_assessor_df <- function(assessor, df) {
  intertrial <- paste('Intertrial', assessor[1], sep = '-')
  wass <- paste('WASS', assessor[1], sep = '-')
  out_df <- data.frame(
    'Parameter' = unique(df$Parameter),
    'Intertrial' = subset(df, variable == intertrial)$value,
    'WASS' = subset(df, variable == wass)$value
  )
  out_df$Total = sqrt((as.numeric(out_df$WASS)^2) + (as.numeric(out_df$Intertrial)^2))
  out_df$`Total-procedural` = as.numeric(out_df$Total) - as.numeric(out_df$WASS)
  
  return(out_df)
}

# plot gait reliability profile per assessor
plot_assessor_grp <- function(df) {
  df_long <- melt(df, id.vars = 'Parameter')
  p <- ggplot(subset(df_long, variable == 'Total'), aes(x=Parameter, y=value)) + 
          geom_bar(stat='identity', position = 'identity', fill='forestgreen') + 
          geom_bar(data = subset(df_long, variable == 'WASS'), aes(x=Parameter, y=value), stat='identity', position = 'identity', fill='firebrick2') + 
          labs(y='SD (\u00B0)') +
          theme(axis.text.x = element_text(angle = 90, vjust=0.4, size = 12),
               axis.title.x = element_blank())
  return(p)
}



## ------------------------------ Call functions --------------------------------------------------------------------

## ------------------------------------------------------------------------------------------------------------------
# Make summary page
main_summary <- function() {
  cat('  \n#Summary {.tabset .tabset-fade .tabset-pills}', '  \n')
  cat('  \n## Parameters - ', settings_summary_count(study_parameters), '  \n')
  settings_summary_list(study_parameters)
  
  cat('  \n## Assessors - ', settings_summary_count(study_assessors), '  \n')
  settings_summary_list(study_assessors)
  
  cat('  \n## Number of sessions - ', settings_summary_count(study_sessions), '  \n')
}


## ------------------------------------------------------------------------------------------------------------------
# Sort input data

main_sorting <- function(sides) {
  input_list <- vector('list', length(sides))
  i=1
  for (data in sides) {
    # load headers and data seperately
    headers <- load_headers(data[2])
    rawdata <- load_data(data[2])
    rawdata$mean <- rowMeans(rawdata)
    # combine and sort data
    sorted <- sort_data(rawdata, headers)
    input_list[[i]] <- sorted
    i=i+1
  }
  # concatenate dataframes in list
  sorted_data <- do.call('rbind', input_list)
  write.csv(sorted_data, 'results/all_data_sorted.csv')
  return(sorted_data)
}


## ------------------------------------------------------------------------------------------------------------------
# Main analysis script - formatted for latex output
main_analysis_latex <- function(sorted_data, parameters, sides, sessions, assessors) {
  grp_all_df <- create_grp_df()
  for (parameter in parameters){
    ### Data from all trials
    # format pdf
    cat('\n\\pagebreak\n')
    cat('  \n##', parameter[1], '  \n')
    cat('  \n###All trials', '  \n')
    # subset by side
    side_list <- vector('list', length(sides))
    i=1
    for (side in sides){
      # subset total data by joint and axis
      query_parameter <- paste(side[1], parameter[2], sep = ' ')
      split_df <- split_by_joint(sorted_data, query_parameter, parameter[3])
      output <- dcast(split_df, assessor+session+unique_id~variable, value.var = 'value')
      write.csv(output, paste('results/', side[1], '_', parameter[1], '_df_all.csv', sep = ''))
      # Loop through sessions
      session_list <- vector('list', length(sessions))
      j=1
      for (study in sessions) {
        # plot graph
        p <- plot_graph_all(split_df, study, side, parameter) + theme(legend.position = 'none')
        leg <- get_legend(plot_graph_all(split_df, study, side, parameter))
        session_list[[j]] <- p
        j=j+1
      }
      side_list[[i]] <- session_list
      i=i+1
    }
    # combine left and right plots, display plot and save
    p <- combine_plots_all(side_list, leg)
    plot(p)
    plot_name <- paste('results/', parameter[1], '_plot_all.png', sep = '')
    save_plot(plot_name, p, base_height = 15, base_width = 12)
    
    ### Aggregated data for plots
    # format pdf
    cat('\n\\pagebreak\n')
    cat('  \n##', parameter[1], '  \n')
    cat('  \n###Aggregated', '  \n')
    # create empty lists
    mean_list <- vector('list', length(sides))
    sem_list <- vector('list', length(sides))
    kin_list <- vector('list', length(sides))
    # subset by side
    i=1
    for (side in sides){
      # subset total data by joint and axis and calculate aggregate data
      query_parameter <- paste(side[1], parameter[2], sep = ' ')
      split_df <- split_by_joint(sorted_data, query_parameter, parameter[3])
      output <- agg_combine(split_df)
      # plot means, sem and kinematics and add to list to be combined later
      mean_list[[i]] <- plot_graph_means(output, side, parameter) + theme(legend.position = 'none')
      leg_means <- get_legend(plot_graph_means(output, side, parameter))
      sem_list[[i]] <- plot_graph_sem(output, side) + theme(legend.position = 'none')
      leg_sem <- get_legend(plot_graph_sem(output, side))
      kin_list[[i]] <- plot_kinematics(split_df, side) + theme(legend.position = 'none')
      leg_kin <- get_legend(plot_kinematics(split_df, side))
      
      ### Aggregated data for gait reproducibility profile
      grp_subset <- grp_df_loop(output, side, parameter)
      # loop through assessors for assessor WASS
      wass_list <- vector('list', length(assessors))
      wass_names <- vector('list', length(assessors))
      intertrial_list <- vector('list', length(assessors))
      intertrial_names <- vector('list', length(assessors))
      j=1
      for (a in assessors){
        wass_list[[j]] <- subset(output, assessor == a[1] & session == 'All' & variable == 'mean' & measurement == 'Within assessor SD')$value
        intertrial_list[[j]] <- subset(output, assessor == a[1] & session == 'All' & variable == 'mean' & measurement == 'Inter-trial RMS')$value
        wass_names[[j]] <- paste('WASS', a[1], sep = '-')
        intertrial_names[[j]] <- paste('Intertrial', a[1], sep = '-')
        j=j+1
      }
      grp_subset <- combine_subset_grp(grp_subset, wass_list, wass_names, intertrial_list, intertrial_names)
      # calcualtions and add to list
      grp_all_df <- combine_all_grp(grp_subset, grp_all_df)
      
      ### Save dataframe
      # unmelt and save df
      output <- dcast(output, assessor+session+measurement~variable, value.var = 'value')
      write.csv(output, paste('results/', side[1], '_', parameter[1], '_df_processed.csv', sep = ''))
      i=i+1
    }
    ### Combine left and right aggregate plots, display and save
    p <- combine_plots_agg(mean_list, leg_means, sem_list, leg_sem, kin_list, leg_kin)
    plot(p)
    plot_name <- paste('results/', parameter[1], '_plot_aggregated.png', sep = '')
    save_plot(plot_name, p, base_height = 15, base_width = 12)
  }
  
  return(grp_all_df)
}


## ------------------------------------------------------------------------------------------------------------------
# Main analysis script - formatted for html output
main_analysis_html <- function(sorted_data, parameters, sides, sessions, assessors) {
  grp_all_df <- create_grp_df()
  cat('\n\\pagebreak\n')
  cat('  \n#Results {.tabset .tabset-fade .tabset-pills}', '  \n')
  for (parameter in parameters){
    ### Data from all trials
    # format pdf
    cat('\n\\pagebreak\n')
    cat('  \n##', parameter[1], '  \n')
    cat('  \n###All trials', '  \n')
    # subset by side
    side_list <- vector('list', length(sides))
    i=1
    for (side in sides){
      # subset total data by joint and axis
      query_parameter <- paste(side[1], parameter[2], sep = ' ')
      split_df <- split_by_joint(sorted_data, query_parameter, parameter[3])
      output <- dcast(split_df, assessor+session+unique_id~variable, value.var = 'value')
      write.csv(output, paste('results/', side[1], '_', parameter[1], '_df_all.csv', sep = ''))
      # Loop through sessions
      session_list <- vector('list', length(sessions))
      j=1
      for (study in sessions) {
        # plot graph
        p <- plot_graph_all(split_df, study, side, parameter) + theme(legend.position = 'none')
        leg <- get_legend(plot_graph_all(split_df, study, side, parameter))
        session_list[[j]] <- p
        j=j+1
      }
      side_list[[i]] <- session_list
      i=i+1
    }
    # combine left and right plots, display plot and save
    p <- combine_plots_all(side_list, leg)
    plot(p)
    plot_name <- paste('results/', parameter[1], '_plot_all.png', sep = '')
    save_plot(plot_name, p, base_height = 15, base_width = 12)
    
    ### Aggregated data for plots
    # format pdf
    cat('\n\\pagebreak\n')
    cat('  \n###Aggregated', '  \n')
    # create empty lists
    mean_list <- vector('list', length(sides))
    sem_list <- vector('list', length(sides))
    kin_list <- vector('list', length(sides))
    # subset by side
    i=1
    for (side in sides){
      # subset total data by joint and axis and calculate aggregate data
      query_parameter <- paste(side[1], parameter[2], sep = ' ')
      split_df <- split_by_joint(sorted_data, query_parameter, parameter[3])
      output <- agg_combine(split_df)
      # plot means, sem and kinematics and add to list to be combined later
      mean_list[[i]] <- plot_graph_means(output, side, parameter) + theme(legend.position = 'none')
      leg_means <- get_legend(plot_graph_means(output, side, parameter))
      sem_list[[i]] <- plot_graph_sem(output, side) + theme(legend.position = 'none')
      leg_sem <- get_legend(plot_graph_sem(output, side))
      kin_list[[i]] <- plot_kinematics(split_df, side) + theme(legend.position = 'none')
      leg_kin <- get_legend(plot_kinematics(split_df, side))
      
      ### Aggregated data for gait reproducibility profile
      grp_subset <- grp_df_loop(output, side, parameter)
      # loop through assessors for assessor WASS
      wass_list <- vector('list', length(assessors))
      wass_names <- vector('list', length(assessors))
      intertrial_list <- vector('list', length(assessors))
      intertrial_names <- vector('list', length(assessors))
      j=1
      for (a in assessors){
        wass_list[[j]] <- subset(output, assessor == a[1] & session == 'All' & variable == 'mean' & measurement == 'Within assessor SD')$value
        intertrial_list[[j]] <- subset(output, assessor == a[1] & session == 'All' & variable == 'mean' & measurement == 'Inter-trial RMS')$value
        wass_names[[j]] <- paste('WASS', a[1], sep = '-')
        intertrial_names[[j]] <- paste('Intertrial', a[1], sep = '-')
        j=j+1
      }
      grp_subset <- combine_subset_grp(grp_subset, wass_list, wass_names, intertrial_list, intertrial_names)
      # calcualtions and add to list
      grp_all_df <- combine_all_grp(grp_subset, grp_all_df)
      
      ### Save dataframe
      # unmelt and save df
      output <- dcast(output, assessor+session+measurement~variable, value.var = 'value')
      write.csv(output, paste('results/', side[1], '_', parameter[1], '_df_processed.csv', sep = ''))
      i=i+1
    }
    ### Combine left and right aggregate plots, display and save
    p <- combine_plots_agg(mean_list, leg_means, sem_list, leg_sem, kin_list, leg_kin)
    plot(p)
    plot_name <- paste('results/', parameter[1], '_plot_aggregated.png', sep = '')
    save_plot(plot_name, p, base_height = 15, base_width = 12)
  }
  
  return(grp_all_df)
}


## ------------------------------------------------------------------------------------------------------------------
# Gait reliability profile analysis script, formatted for latex output
main_grp_latex <- function(grp_all_df, assessors) {
  cat('\n\\pagebreak\n')
  cat('  \n## Gait reliability profile -- all', '  \n')
  # Plot graph and table for gait reliability profile of all assessors
  grp_long <- melt(grp_all_df, id.vars = 'Parameter')
  plot(plot_all_grp(grp_all_df))
  colnames(grp_all_df)[3] <- '\\textbf{\\textcolor{red}{WASS}}'
  colnames(grp_all_df)[4] <- '\\textbf{BASS}'
  colnames(grp_all_df)[11] <- '\\textbf{\\textcolor{RoyalBlue}{Procedural}}'
  colnames(grp_all_df)[12] <- '\\textbf{\\textcolor{Goldenrod}{Total}}'
  print(xtable(grp_all_df[c(1,2,3,4,11,12,13)]), type = 'latex', include.rownames = FALSE, comment = FALSE, sanitize.colnames.function = function(x) {x})
  # Plot graph and table for gait reliability profile of each assessor
  for (assessor in assessors) {
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


## ------------------------------------------------------------------------------------------------------------------
# Gait reliability profile analysis script, formatted for html output
main_grp_html <- function(grp_all_df, assessors) {
  cat('\n\\pagebreak\n')
  cat('  \n## Gait reliability profile -- all', '  \n')
  # Plot graph and table for gait reliability profile of all assessors
  grp_long <- melt(grp_all_df, id.vars = 'Parameter')
  plot(plot_all_grp(grp_all_df))
  colnames(grp_all_df)[3] <- '\\textbf{\\textcolor{red}{WASS}}'
  colnames(grp_all_df)[4] <- '\\textbf{BASS}'
  colnames(grp_all_df)[11] <- '\\textbf{\\textcolor{RoyalBlue}{Procedural}}'
  colnames(grp_all_df)[12] <- '\\textbf{\\textcolor{Goldenrod}{Total}}'
  grp_all_df[c(1,2,3,4,11,12,13)]
  # Plot graph and table for gait reliability profile of each assessor
  for (assessor in assessors) {
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
