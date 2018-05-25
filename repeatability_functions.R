## Repeatability functions


library('ggplot2')
library('reshape2')
library('xtable')
library('cowplot')





# write functions
load_headers <- function(x) {
  df <- read.delim(file = x, header = FALSE, nrows = 5)
  df <- data.frame(t(df[, seq(2, ncol(df))]))
}

load_data <- function(x) {
  df <- read.delim(file = x, header = FALSE, skip = 5)
  df <- data.frame(t(df[, seq(2, ncol(df))]))
}


# sort data
parse_participant <- function(x) {
  field = x[6]
  return(field)
}

parse_date <- function(x) {
  field = x[8]
  date = unlist(strsplit(field, '_'))[1]
  return(date)
}

parse_assessor <- function(x) {
  field = x[8]
  field_split = unlist(strsplit(field, '_'))[2]
  assessor = gsub('[[:digit:]]+', '', field_split)
  no = gsub(assessor, '', field_split)
  return(assessor)
}

parse_number <- function(x) {
  field = x[8]
  field_split = unlist(strsplit(field, '_'))[2]
  assessor = gsub('[[:digit:]]+', '', field_split)
  no = gsub(assessor, '', field_split)
  return(no)
}

parse_trial <- function(x) {
  field = x[9]
  field_split = unlist(strsplit(field, ' '))
  field_split = field_split[length(field_split)]
  field_split = gsub('.c3d', '', field_split)
  return(field_split)
}



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

# make summary data
settings_summary_count <- function(x) {
  num <- length(x)
  return(num)
}

settings_summary_list <- function(x) {
  for (parameter in x) {
    cat('  \n*', parameter[1], '  \n')
  }
}


# subset
split_by_joint <- function(dat, x,y) {
  df <- subset(dat, Joint == x & Plane == y)
  df <- df[setdiff(colnames(df), c('Participant', 'Session.date', 'Trial', 'Joint', 'Plane'))]
  df <- melt(df, id.vars = c('Session.assessor', 'Session.number', 'unique_id'))
}



# inputs - data split by day, y label, day label, (y axis limits?)
plot_graph_all <- function(x, t, l, ylower, yupper, increments) {
  plot <- ggplot(x, aes(x=as.numeric(variable)-1, y=value, group=unique_id, colour=Session.assessor)) +
  geom_line() +
  labs(title=t, x='Gait cycle (%)', y=paste(l, ' (\u00B0)')) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(expand = c(0, 0), limits = c(as.numeric(ylower), as.numeric(yupper)), breaks = c(seq(-360, 360, as.numeric(increments)))) + 
  scale_x_continuous(expand = c(0, 0), limits = c(0, 101), breaks = c(seq(0, 100, 20))) + 
  theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(),
      legend.position = 'bottom'
      )
}



combine_plots <- function(x, leg) {
  plot_left <- plot_grid(plotlist = x[[1]], ncol = 1)
  plot_right <- plot_grid(plotlist = x[[2]], ncol = 1)
  plot_combined <- plot_grid(plot_left, plot_right, ncol = 2)
  plot <- plot_grid(plot_combined, leg, ncol = 1, rel_heights = c(20, 1))
  # save plot
  #save_plot(name, plot, base_height = 15, base_width = 12)
}

  

combine_plots2 <- function(x, leg) {
  plot_combined <- plot_grid(plotlist = x, ncol = 2)
  plot <- plot_grid(plot_combined, leg, ncol = 1, rel_heights = c(20, 1))
  # save plot
  #save_plot(name, plot, base_height = 15, base_width = 12)
}

  

# inputs - mean data for all sessions and assessors, y label, (title + y axid limits?)
plot_graph_means <- function(x,l, ylower, yupper, increments) {
  plot <- ggplot(x, aes(x=as.numeric(variable)-1, y=value, group=unique_id, colour=assessor, linetype=no)) +
    geom_line() +
    labs(title=paste('All sessions', side[1], sep = ' ') , x='Gait cycle (%)', y=paste(l, ' (\u00B0)')) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(expand = c(0, 0), limits = c(as.numeric(ylower), as.numeric(yupper)), breaks = c(seq(-360, 360, as.numeric(increments)))) + 
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
  plot <- ggplot(subset(x, measurement == 'SEM'), aes(x=as.numeric(variable)-1, y=value, group=unique_id, colour=assessor, linetype=no)) +
    geom_line() +
    labs(title='SEM', x='Gait cycle (%)', y=paste('SEM (\u00B0)')) +
    geom_line(data = subset(x, measurement == 'WSSD'), colour = 'black') +
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
    labs(title='Kinematics', x='Trial', y='Average kinematics (\u00B0)') +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = 'bottom') +
    facet_grid(. ~ Session.assessor, scales = 'free')
}



# calculate means per session+assessor - mean at each percentile of all the trials each assessor has done in a session
agg_means <- function() {
  temp_means <- dcast(temp_df, Session.assessor+Session.number~variable, mean)
  temp_means$measurement <- rep('Trial mean', nrow(temp_means))
  temp_means <- melt(temp_means, id.vars = c('Session.number', 'Session.assessor', 'measurement'))
}
# aggregate sd
agg_sd <- function() {
  temp_sd <- dcast(temp_df, Session.assessor+Session.number~variable, sd)
  temp_sd$measurement <- rep('Trial SD', nrow(temp_sd))
  temp_sd <- melt(temp_sd, id.vars = c('Session.number', 'Session.assessor', 'measurement'))
}
  
# means per participant - 
agg_assessor_means <- function() {
  temp_assessor_means <- data.frame(
    'Session.number' = rep('All', length(study_assessors)),
    'measurement' = rep('Trial mean', length(study_assessors))
    )
  temp_assessor_means <- cbind(temp_assessor_means, dcast(temp_means, Session.assessor ~ variable, mean))
  temp_assessor_means <- melt(temp_assessor_means, id.vars = c('Session.number', 'Session.assessor', 'measurement'))
}

# sd per participant - within assessor SD
agg_assessor_sd <- function() {
  temp_assessor_sd <- data.frame(
    'Session.number' = rep('All', length(study_assessors)),
    'measurement' = rep('Within assessor SD', length(study_assessors))
    )
  temp_assessor_sd <- cbind(temp_assessor_sd, dcast(temp_means, Session.assessor ~ variable, sd))
  temp_assessor_sd <- melt(temp_assessor_sd, id.vars = c('Session.number', 'Session.assessor', 'measurement'))
}

# mean within assessor SD
agg_mean_assessor_sd <- function() {
  temp_subset <- subset(temp_assessor_sd, variable == 'mean')
  temp_df <- data.frame(
    'Session.number' = 'All',
    'Session.assessor' = 'All',
    'measurement' = 'Mean within assessor SD'
    )
  temp_df <- cbind(temp_df, dcast(temp_subset, Session.number ~ variable, function(x) {sqrt(sum(x^2) / length(x))}))
  temp_df <- melt(temp_df, id.vars = c('Session.number', 'Session.assessor', 'measurement'))
}

# Between assessor SD
agg_basd <- function() {
  temp_basd <- data.frame(
    'Session.number' = rep('All', length(unique(temp_means$variable))),
    'Session.assessor' = rep('All', length(unique(temp_means$variable))),
    'measurement' = rep('Between assessor SD', length(unique(temp_means$variable)))
    )
  temp_basd <- cbind(temp_basd, aggregate(value~variable, data = temp_assessor_means, FUN = sd))
}

# inter-trial rms - per assessor
agg_intertrial_RMS <- function() {
  temp_df <- data.frame(
    'Session.number' = rep('All', length(study_assessors)),
    'measurement' = rep('Inter-trial RMS', length(study_assessors))
    )
  temp_df <- cbind(temp_df, dcast(temp_sd, Session.assessor ~ variable, function(x) {sqrt(sum(x^2) / length(x))}))
  temp_df <- melt(temp_df, id.vars = c('Session.number', 'Session.assessor', 'measurement'))
}

# intertrial rms - mean
agg_mean_intertrial_RMS <- function() {
  temp_subset <- subset(temp_sd, variable == 'mean')
  temp_df <- data.frame(
    'Session.number' = 'All',
    'Session.assessor' = 'All',
    'measurement' = 'Inter-trial RMS'
    )
  temp_df <- cbind(temp_df, aggregate(value~variable, data = temp_subset, function(x) {sqrt(sum(x^2) / length(x))}))
}

# individual sem - for each participant - 
agg_ind_sem <- function() {
  temp_ind_sem <- data.frame(
    'Session.number' = rep('All', length(study_assessors)),
    'measurement' = rep('Individual_SEM', length(study_assessors))
    )
  temp_ind_sem <- cbind(temp_ind_sem, dcast(temp_means, Session.assessor ~ variable, sd))
  temp_ind_sem <- melt(temp_ind_sem, id.vars = c('Session.number', 'Session.assessor', 'measurement'))
}
  
# sem
agg_sem <- function() {
  temp_sem <- data.frame(
    'Session.number' = temp_ind_sem$Session.number,
    'Session.assessor' = temp_ind_sem$Session.assessor,
    'measurement' = rep('SEM', nrow(temp_ind_sem)),
    'variable' = temp_ind_sem$variable,
    'value' = sapply(temp_ind_sem$value, function(x) {sqrt(sum(x^2) / length(study_participants))})
  )
}
  
# wssd
agg_wssd <- function() {
  temp_wssd <- data.frame(
    'Session.number' = rep('All', length(unique(temp_means$variable))),
    'Session.assessor' = rep('All', length(unique(temp_means$variable))),
    'measurement' = rep('WSSD', length(unique(temp_means$variable)))
    )
  temp_wssd <- cbind(temp_wssd, aggregate(value~variable, data = temp_means, FUN = sd))
}
  
# wass
agg_wass <- function() {
  temp_wass <- data.frame(
    'Session.number' = rep('All', length(unique(temp_means$variable))),
    'Session.assessor' = rep('All', length(unique(temp_means$variable))),
    'measurement' = rep('WASS', length(unique(temp_means$variable)))
    )
  temp_wass <- cbind(temp_wass, aggregate(value~variable, data = temp_ind_sem, FUN = function(x) {sqrt(sum(x^2) / length(study_participants))}))
}

agg_combine <- function() {
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
  t$Total = sqrt((as.numeric(t$WASS)^2) + (as.numeric(t$Intertrial)^2))
  t$`Total-procedural` = as.numeric(t$Total) - as.numeric(t$WASS)

}
# plot gait reliability profile per assessor
plot_assessor_grp <- function(x) {
  plot <- ggplot(subset(x, variable == 'Total'), aes(x=Parameter, y=value)) + 
          geom_bar(stat='identity', position = 'identity', fill='forestgreen') + 
          geom_bar(data = subset(x, variable == 'WASS'), aes(x=Parameter, y=value), stat='identity', position = 'identity', fill='firebrick2') + 
          labs(y='SD (\u00B0)') +
          theme(axis.text.x = element_text(angle = 45, vjust=0.4, size = 12),
               axis.title.x = element_blank())
  return(plot)
}







