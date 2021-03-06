# Name: Yanli Xu
# Date: Jan 6th, 2017
# Note: This script is used for compare the solute vs time of the same concentrations among different groups, the output is one plot for every concentration. 
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("compare_time_vs_solute_profile_1A.R"), the script will start working 
# Input files: To compare n groups of experiments, every group of experiments has m "mean_profile.csv" files in its directory: for example, if you want to 
# compare two groups of experiments, then you should put the two groups of experiments(".csv" files) in two different directories. 
# This script cretes 8 pages of plots, every page has all the conecntration of one solute

# An example to run this script
# > source("compare_time_vs_solute_Profile_1A.R")
# Please input the number of groups of experiments you would like to compare, for example 2: 2
# Please input the directory of experiments of group 1: /Users/yanlixu/Desktop/experiment_data/bolus_in_2016_12/161215_161219/exp_data_0p1_to_0p9 
# Please input a name to distinguish this group, for example, infusion_liver: bolus_liver
# Please input the directory of experiments of group 2: /Users/yanlixu/Desktop/experiment_data/bolus_in_2016_12/161220_161225/bolus_exp_data 
# Please input a name to distinguish this group, for example, infusion_liver: bolus_culture
# Please input 2 different colors to represent different groups, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: black, red
# Please input the number of experiments in every group, for example 9: 9
# You have  9  experiments in every group, please input how many rows and columns you would like to display the plots, for example (3, 3): 3, 3
# Enter 9 concentration values in every group and separated by comma: 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9
# Please input the cycleLimit of these experiments: 21600
library(svglite)
svg("plots.svg")

mav <- function(x,n=301){filter(x,rep(1/n,n), sides=2)}
compare_time_vs_solute <- function()
{
  # Ask user to input the number of groups of experiments
  num_of_groups <- as.numeric(unlist(strsplit(readline(prompt="Please input the number of groups of experiments you would like to compare, for example 2: "), "[, ]+")))
  # Ask user to input the directories for all the groups of experiments
  exp_dirs <- c()
  exp_group_names <- c()
  for(d_num in 1:num_of_groups)
  {
    exp_dirs[d_num] <- unlist(strsplit(readline(paste("Please input the directory of experiments of group ", d_num, ": ", sep = "")), "[, ]+"))
    exp_group_names[d_num] <- unlist(strsplit(readline(paste("Please input a name to distinguish this group, for example, infusion_liver: ", sep = "")), "[,.]+"))
  }
  # Ask user to input one color for every group
  col_str <- readline(paste("Please input ", num_of_groups, " different colors to represent different groups, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: ", sep = "")) 
  color <- unlist(strsplit(col_str, "[, ]+"))
  
  # Ask the user to input the number of experiments in every group
  num_of_exp_perGroup <- as.numeric(unlist(strsplit(readline(prompt="Please input the number of experiments in every group, for example 9: "), "[, ]+")))
  # Ask the user to decide the layout of output plots
  mf_input <- readline(paste("You have ", num_of_exp_perGroup, " experiments in every group, please input how many rows and columns you would like to display the plots, for example (3, 3): "))
  mf <- as.numeric(unlist(strsplit(mf_input, "[, ]+")))
  par(mfrow=mf)
  # Ask user to input the concentrations
  conc_str <- readline(paste("Enter ", num_of_exp_perGroup,  " concentration values in every group and separated by comma: ", sep = ""))
  conc <- unlist(strsplit(conc_str, "[, ]+"))
  
  #   # Ask the user which solute to plot
  #   solute_names <- names(exp)    # "Time"   "S"      "nMD"    "MitoDD" "oAPAP"  "G"      "Marker" "Repair" "N" 
  #   solute_str <- readline("Please choose a number for the solute you would like to plot, 1 for S, 2 for nMD, 3 for MitoDD, 4 for oAPAP, 5 for G, 6 for Marker, 7 for Repair, 8 for N: ")
  #   solute_num <- as.numeric(unlist(strsplit(solute_str, "[, ]+")))
  
  # x-axis and y-axis values
  run_time_str <- readline("Please input the cycleLimit of these experiments: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  #   y_lim_str <- readline(paste("Please input the bottom and upper value of y-axis to represent the min and max solute value, for example 0, 10: ", sep = ""))
  #   y_limits <- as.numeric(unlist(strsplit(y_lim_str, "[, ]+")))
  
  ##############
  solute_names <- c("S", "nMD", "MitoDD", "oAPAP", "G", "Marker", "Repair", "N")
  
  for(solute_num in 1:length(solute_names)) # This for loop is for every solute
  {
    
    ##### Begining of "Decide the y_min and y_max"
    max_of_every_group <- c()
    for (grp_num in 1:num_of_groups) # for every group group 
    {
      # Start of "Read all the files in one group"
      exp_names <- c()
      for(i in 1:length(conc))
      {
        exp_names[i] <- paste("profile_", i, sep="")
        files <- list.files(path = exp_dirs[grp_num], pattern = "[0-9]+_profile_mean_per_node.csv", recursive = T)
        assign(exp_names[i], read.csv(paste(exp_dirs[grp_num], files[i], sep= "/"), header = TRUE, sep = ","))
      }
      # End of "Read all the files in one group"
      max_for_every_exp <- c()
      for(i in 1:length(conc))
      {
        exp <- eval(parse(text = exp_names[i]))
        max_for_every_exp[i] = max(mav(exp[, (solute_num + 1)]), na.rm = TRUE)
      }
      max_of_every_group[grp_num] = max(max_for_every_exp)
    }
    y_max = max(max_of_every_group)
    y_min = 0
    # This is for the cases that all the values are "0"
    if(y_max == 0)
    {
      y_min = -1
      y_max = 1
    }
    ##### End of "Decide the y_min and y_max"
    # Start plotting plot
    for(exp_num in 1:num_of_exp_perGroup) # This for loop is for every concentration in every group
    {
      # read the files in every group
      exp_names <- c()
      for (grp_num in 1:num_of_groups)
      {
        
        exp_names[grp_num] <- paste("profile_", grp_num, sep="")
        files <- list.files(path = exp_dirs[grp_num], pattern = "[0-9]+_profile_mean_per_node.csv", recursive = T)
        assign(exp_names[grp_num], read.csv(paste(exp_dirs[grp_num], files[exp_num], sep= "/"), header = TRUE, sep = ","))
      }
      
      plot(c(), c(), type = "l", main = paste("Profile_Time_vs", solute_names[solute_num], conc[exp_num], sep = "_"), xlab = "time", ylab = solute_names[solute_num], xlim = c(0, run_time), ylim = c(y_min, y_max))
      
      for(i in 1:length(exp_names))
      {
        exp <- eval(parse(text = exp_names[i]))
        lines(mav(exp[, (solute_num + 1)]) ~ exp$Time, col = color[i])
      }
      if(exp_num == 1)
      {
        legend(0, y_max, exp_group_names, col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
      }
      Sys.sleep(10)
    }# end of for loop for every concentration
    
    
  } # end of for loop for every solute
  
}
compare_time_vs_solute()

dev.off()
