## Repeatability functions

## Load libraries
library('ggplot2')
library('reshape2')
library('xtable')
library('cowplot')


## Loading data
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


## Settings summary functions - for summary on first page of report
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


## Sorting data
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
  no = gsub(assessor, '', field_split)
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
  
  paths <- strsplit(toString(head$X1), ',')
  paths_split <- strsplit(unlist(paths), '\\\\')
  
  temp_df <- data.frame(
    'Participant' = sapply(paths_split, parse_participant),
    'Session assessor' = sapply(paths_split, parse_assessor),
    'Session number' = sapply(paths_split, parse_number),
    'Session date' = sapply(paths_split, parse_date),
    'Trial' = sapply(paths_split, parse_trial),
    'Joint' = head$X2,
    'Plane' = head$X5
    )
  temp_df$unique_id <- make.names(paste(temp_df$Session.assessor, temp_df$Session.number, temp_df$Joint, temp_df$Plane, temp_df$Trial, sep='_'), unique = TRUE)
  
  #replace names in columns with nicely formatted names
  for (participant in study_participants) {
    temp_df$Participant <- sapply(temp_df$Participant, function(x) gsub(participant[2], participant[1], x))
  }
  for (assessor in study_assessors) {
    temp_df$Session.assessor <- sapply(temp_df$Session.assessor, function(x) gsub(assessor[2], assessor[1], x))
  }
  for (session in study_sessions) {
    temp_df$Session.number <- sapply(temp_df$Session.number, function(x) gsub(session[2], session[1], x))
  }
  
  sorted_data <- cbind(temp_df, dat)
  remove_df <- subset(sorted_data, (Trial=='Static' | Joint=='Right Ankle Angles' & Plane=='Z') | (Joint=='Right Foot Pitch Angles' & Plane=='Y') | Joint=='Right Ankle Angles_CGM' | Joint=='Right Foot Progression' | Joint=='Left Ankle Angles' & Plane=='Z' | (Joint=='Left Foot Pitch Angles' & Plane=='Y') | Joint=='Left Ankle Angles_CGM' | Joint=='Left Foot Progression')
  sorted_data <- sorted_data[setdiff(rownames(sorted_data), rownames(remove_df)),]
  
  return(sorted_data)
}


## Subset data by parameter
split_by_joint <- function(dat, x,y) {
  df <- subset(dat, Joint == x & Plane == y)
  df <- df[setdiff(colnames(df), c('Participant', 'Session.date', 'Trial', 'Joint', 'Plane'))]
  df <- melt(df, id.vars = c('Session.assessor', 'Session.number', 'unique_id'))
}

## Plot graphs
# All sessions - inputs - data split by day, y label, day label, (y axis limits?)
plot_graph_all <- function(x, label) {
  temp_subset <- subset(x, Session.number == study[1] & variable != 'mean')
  
  plot <- ggplot(temp_subset, aes(x=as.numeric(variable)-1, y=value, group=unique_id, colour=Session.assessor)) +
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
# Combine plots for each parameter
combine_plots <- function(x, leg) {
  plot_left <- plot_grid(plotlist = x[[1]], ncol = 1)
  plot_right <- plot_grid(plotlist = x[[2]], ncol = 1)
  plot_combined <- plot_grid(plot_left, plot_right, ncol = 2)
  plot <- plot_grid(plot_combined, leg, ncol = 1, rel_heights = c(20, 1))
  # save plot
  #save_plot(name, plot, base_height = 15, base_width = 12)
}


# inputs - mean data for all sessions and assessors, [y label, (title + y axid limits?)] - hard coded
plot_graph_means <- function(x,l) {
  data_subset <- subset(x, variable != 'mean' & measurement == 'Trial mean')
  plot <- ggplot(data_subset, aes(x=as.numeric(variable)-1, y=value, group=unique_id, colour=assessor, linetype=no)) +
    geom_line() +
    labs(title=paste('All sessions', side[1], sep = ' - '), x='Gait cycle (%)', y=paste(l, ' (\u00B0)')) +
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

# inputs - mean data for all sessions and assessors, y label, (title + y axid limits?)
plot_graph_sem <- function(x) {
  data_subset <- subset(x, variable != 'mean')
  plot <- ggplot(subset(data_subset, measurement == 'SEM'), aes(x=as.numeric(variable)-1, y=value, group=unique_id, colour=assessor, linetype=no)) +
    geom_line() +
    labs(title=paste('SEM', side[1], sep = ' - '), x='Gait cycle (%)', y=paste('SEM (\u00B0)')) +
    geom_line(data = subset(data_subset, measurement == 'WSSD'), colour = 'black') +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, 101), breaks = c(seq(0, 100, 20))) + 
    theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(),
        legend.position = 'bottom'
        )
}

plot_kinematics <- function(x) {
  df <- subset(x, variable == 'mean')
  trial_mean <- mean(df$value)
  df$dev <- lapply(df$value, function(x){x - trial_mean})
  temp_plot <- ggplot(df, aes(x=unique_id, y=value, group=Session.assessor, colour=Session.number)) + 
    geom_point() +
    geom_hline(yintercept = trial_mean, colour = '#999999') +
    labs(title=paste('Kinematics', side[1], sep = ' - '), x='Trial', y='Average kinematics (\u00B0)') +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = 'bottom') +
    facet_grid(. ~ Session.assessor, scales = 'free')
}

# Combine plots for each parameter
combine_plots2 <- function(x, leg) {
  plot_combined <- plot_grid(plotlist = x, ncol = 2)
  plot <- plot_grid(plot_combined, leg, ncol = 1, rel_heights = c(20, 1))
  # save plot
  #save_plot(name, plot, base_height = 15, base_width = 12)
}

# Combine plots for all parameters
combine_plots3 <- function() {
  temp_plot_means <- combine_plots2(temp_mean_list, leg_means)
  temp_plot_sem <- combine_plots2(temp_sem_list, leg_sem)
  temp_plot_kinematics <- combine_plots2(temp_kin_list, leg_kin)
  temp_plot <- plot_grid(temp_plot_means, temp_plot_sem, temp_plot_kinematics, ncol = 1)
  return(temp_plot)
}

## Aggregated data
# calculate means per session+assessor - mean at each percentile of all the trials each assessor has done in a session
agg_means <- function() {
  temp_means <- dcast(temp_df, Session.assessor+Session.number~variable, mean)
  temp_means$measurement <- rep('Trial mean', nrow(temp_means))
  temp_means <- melt(temp_means, id.vars = c('Session.number', 'Session.assessor', 'measurement'))
  return(temp_means)
}
# aggregate sd
agg_sd <- function() {
  temp_sd <- dcast(temp_df, Session.assessor+Session.number~variable, sd)
  temp_sd$measurement <- rep('Trial SD', nrow(temp_sd))
  temp_sd <- melt(temp_sd, id.vars = c('Session.number', 'Session.assessor', 'measurement'))
  return(temp_sd)
}
# means per participant - 
agg_assessor_means <- function(x) {
  temp_assessor_means <- data.frame(
    'Session.number' = rep('All', length(study_assessors)),
    'measurement' = rep('Trial mean', length(study_assessors))
    )
  temp_assessor_means <- cbind(temp_assessor_means, dcast(x, Session.assessor ~ variable, mean))
  temp_assessor_means <- melt(temp_assessor_means, id.vars = c('Session.number', 'Session.assessor', 'measurement'))
  return(temp_assessor_means)
}
# sd per participant - within assessor SD
agg_assessor_sd <- function(x) {
  temp_assessor_sd <- data.frame(
    'Session.number' = rep('All', length(study_assessors)),
    'measurement' = rep('Within assessor SD', length(study_assessors))
    )
  temp_assessor_sd <- cbind(temp_assessor_sd, dcast(x, Session.assessor ~ variable, sd))
  temp_assessor_sd <- melt(temp_assessor_sd, id.vars = c('Session.number', 'Session.assessor', 'measurement'))
  return(temp_assessor_sd)
}
# mean within assessor SD
agg_mean_assessor_sd <- function(x) {
  temp_subset <- subset(x, variable == 'mean')
  temp_df <- data.frame(
    'Session.number' = 'All',
    'Session.assessor' = 'All',
    'measurement' = 'Mean within assessor SD'
    )
  temp_df <- cbind(temp_df, dcast(temp_subset, Session.number ~ variable, function(x) {sqrt(sum(x^2) / length(x))}))
  temp_df <- melt(temp_df, id.vars = c('Session.number', 'Session.assessor', 'measurement'))
  return(temp_df)
}
# Between assessor SD
agg_basd <- function(x, y) {
  temp_basd <- data.frame(
    'Session.number' = rep('All', length(unique(x$variable))),
    'Session.assessor' = rep('All', length(unique(x$variable))),
    'measurement' = rep('Between assessor SD', length(unique(x$variable)))
    )
  temp_basd <- cbind(temp_basd, aggregate(value~variable, data = y, FUN = sd))
  return(temp_basd)
}
# inter-trial rms - per assessor
agg_intertrial_RMS <- function(x) {
  temp_df <- data.frame(
    'Session.number' = rep('All', length(study_assessors)),
    'measurement' = rep('Inter-trial RMS', length(study_assessors))
    )
  temp_df <- cbind(temp_df, dcast(x, Session.assessor ~ variable, function(x) {sqrt(sum(x^2) / length(x))}))
  temp_df <- melt(temp_df, id.vars = c('Session.number', 'Session.assessor', 'measurement'))
  return(temp_df)
}
# intertrial rms - mean
agg_mean_intertrial_RMS <- function(x) {
  temp_subset <- subset(x, variable == 'mean')
  temp_df <- data.frame(
    'Session.number' = 'All',
    'Session.assessor' = 'All',
    'measurement' = 'Inter-trial RMS'
    )
  temp_df <- cbind(temp_df, aggregate(value~variable, data = temp_subset, function(x) {sqrt(sum(x^2) / length(x))}))
  return(temp_df)
}
# individual sem - for each participant - 
agg_ind_sem <- function(x) {
  temp_ind_sem <- data.frame(
    'Session.number' = rep('All', length(study_assessors)),
    'measurement' = rep('Individual_SEM', length(study_assessors))
    )
  temp_ind_sem <- cbind(temp_ind_sem, dcast(x, Session.assessor ~ variable, sd))
  temp_ind_sem <- melt(temp_ind_sem, id.vars = c('Session.number', 'Session.assessor', 'measurement'))
  return(temp_ind_sem)
}
# sem
agg_sem <- function(x) {
  temp_sem <- data.frame(
    'Session.number' = x$Session.number,
    'Session.assessor' = x$Session.assessor,
    'measurement' = rep('SEM', nrow(x)),
    'variable' = x$variable,
    'value' = sapply(x$value, function(y) {sqrt(sum(y^2) / length(study_participants))})
  )
  return(temp_sem)
}
# wssd
agg_wssd <- function(x) {
  temp_wssd <- data.frame(
    'Session.number' = rep('All', length(unique(x$variable))),
    'Session.assessor' = rep('All', length(unique(x$variable))),
    'measurement' = rep('WSSD', length(unique(x$variable)))
    )
  temp_wssd <- cbind(temp_wssd, aggregate(value~variable, data = x, FUN = sd))
  return(temp_wssd)
}
# wass
agg_wass <- function(x, y) {
  temp_wass <- data.frame(
    'Session.number' = rep('All', length(unique(x$variable))),
    'Session.assessor' = rep('All', length(unique(x$variable))),
    'measurement' = rep('WASS', length(unique(x$variable)))
    )
  temp_wass <- cbind(temp_wass, aggregate(value~variable, data = y, FUN = function(x) {sqrt(sum(x^2) / length(study_participants))}))
  return(temp_wass)
}

# Combine all aggregated data
agg_combine <- function() {
  temp_means <- agg_means()
  temp_sd <- agg_sd()
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
  
  names(temp_means) <- c('no', 'assessor', 'measurement', 'variable', 'value')
  names(temp_sd) <- c('no', 'assessor', 'measurement', 'variable', 'value')
  names(temp_assessor_means) <- c('no', 'assessor', 'measurement', 'variable', 'value')
  names(temp_assessor_sd) <- c('no', 'assessor', 'measurement', 'variable', 'value')
  names(temp_mean_assessor_sd) <- c('no', 'assessor', 'measurement', 'variable', 'value')
  names(temp_intertrial_RMS) <- c('no', 'assessor', 'measurement', 'variable', 'value')
  names(temp_mean_intertrial_RMS) <- c('no', 'assessor', 'measurement', 'variable', 'value')
  names(temp_basd) <- c('no', 'assessor', 'measurement', 'variable', 'value')
  names(temp_ind_sem) <- c('no', 'assessor', 'measurement', 'variable', 'value')
  names(temp_sem) <- c('no', 'assessor', 'measurement', 'variable', 'value')
  names(temp_wssd) <- c('no', 'assessor', 'measurement', 'variable', 'value')
  names(temp_wass) <- c('no', 'assessor', 'measurement', 'variable', 'value')
  df <- rbind(temp_means, temp_sd, temp_assessor_means, temp_assessor_sd, temp_mean_assessor_sd, temp_intertrial_RMS, temp_mean_intertrial_RMS, temp_basd, temp_ind_sem, temp_sem, temp_wssd, temp_wass)
  df$unique_id <- make.names(paste(df$assessor, df$no, df$measurement, sep='_'))
  
  return(df)
}


## Gait reliability profile
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
grp_df_loop <- function(x) {
  temp_grp <- data.frame(
    'Parameter' = paste(side[1], parameter[1], sep = ' '),
    'Intertrial' = subset(x, assessor == 'All' & no == 'All' & variable == 'mean' & measurement == 'Inter-trial RMS')$value,
    'WASS' = subset(x, assessor == 'All' & no == 'All' & variable == 'mean' & measurement == 'Mean within assessor SD')$value,
    'BASS' = subset(x, assessor == 'All' & no == 'All' & variable == 'mean' & measurement == 'Between assessor SD')$value
  )
}

# Aggregate data for assessor grp
combine_grp <- function(x) {
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
    theme(axis.text.x = element_text(angle = 45, vjust=0.4, size = 12),
          axis.title.x = element_blank())
  return(g)
}

# create df for gait reliability profile per assessor
# input - assessor name
create_grp_assessor_df <- function(x) {
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
          theme(axis.text.x = element_text(angle = 45, vjust=0.4, size = 12),
               axis.title.x = element_blank())
  return(plot)
}
