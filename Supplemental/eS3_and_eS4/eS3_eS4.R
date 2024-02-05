## Corporate Essence Analyses ## 


# Set working directory to current file location 
# remove(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Import packages 
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load('openxlsx', #open Excel spreadsheets 
               'ggplot2', #plot stuff 
               'ggimage', #add images to ggplot legend 
               'pbkrtest', 
               'lme4', #linear regression
               'jtools', 
               'psych', 
               'plotrix', #used for std.error() to get standard error 
               'cowplot', #reads image files into R; add images as x-axis labels
               'grid', #used for rasterGrob()
               'png', #read PNG files
               'sentimentr', #sentiment analysis
               'filesstrings' #move files around 
)


# ======================================== DEFINE FUNCTIONS ======================================== #


GetBarPlot <- function(vignette_num, my_table, y_lab, condition_1, condition_2, break_labs, plot_labs) { 
  
    "Creates bar plots from study results 
    Input(s): number of vignettes, results table, y-axis label, condition 2, x-axis labels 
    Output(s): bar plots 
    "
    
    # Define conditions, vignettes, and values; insert into data frame 
    my_conditions <- factor(rep(c(-1, 1), each = vignette_num), levels = unique(c(-1, 1)))
    my_vignettes <- factor(c(1:vignette_num), levels = unique(c(1:vignette_num)))
    my_means <- as.data.frame(my_table)$x[, "mean"] #get means 
    my_sds <- as.data.frame(my_table)$x[, "sd"] #get standard deviations  
    my_ses <- as.data.frame(my_table)$x[, "se"] #get standard errors 
    my_df <- data.frame(my_conditions, my_vignettes, my_means, my_sds, my_ses) 
    
    # Use ggplot() to build a bar plot 
    bar_plot <- ggplot(my_df, aes(x=my_vignettes, y=my_means, fill=my_conditions)) + 
      geom_hline(yintercept = 50, linetype = "dashed", color = "gray65") + 
      geom_bar(position="dodge", stat="identity") + 
      geom_errorbar(aes(ymin=my_means-my_ses, ymax=my_means+my_ses), #add error bars 
                    width=.2, position=position_dodge(.9)) + 
      theme_classic() + 
      theme(text = element_text(size = 25),
            axis.title.y = element_text(size = 25), 
            axis.title.x = element_text(size = 25, margin = margin(t = 20, r = 0, b = 0, l = 0)), 
            axis.text.x = element_text(size = 20),
            # axis.ticks.x = element_blank(), 
            # axis.line.x = element_blank(), 
            legend.position="top") + 
      labs(x = "Firm Type", y = y_lab, fill = "") + 
      ylim(0, 100) + 
      scale_fill_manual(labels = c(condition_1, condition_2), 
                        values = c("gray40", "gray85")) + 
      scale_x_discrete(breaks = break_labs, labels = plot_labs) 
    
    return(bar_plot)
}


GetBarPlotMeans <- function(vignette_num, my_table, y_lab, break_labs, plot_labs) { 
  
    "Creates bar plots from study results 
    Input(s): number of vignettes, results table, y-axis label, condition 2, x-tick labels  
    Output(s): bar plots 
    "
    
    # Define vignettes and values; insert into data frame 
    my_vignettes <- factor(c(1:vignette_num), levels = unique(c(1:vignette_num)))
    my_means <- as.data.frame(my_table)$x[, "mean"] #get means 
    my_sds <- as.data.frame(my_table)$x[, "sd"] #get standard deviations  
    my_ses <- as.data.frame(my_table)$x[, "se"] #get standard errors 
    my_df <- data.frame(my_vignettes, my_means, my_sds, my_ses) 
    
    
    # Use ggplot() to build a bar plot 
    bar_plot <- ggplot(my_df, aes(x=my_vignettes, y=my_means)) +
      # geom_hline(yintercept = 50, linetype = "dashed", color = "gray65") +
      geom_bar(position="dodge", stat="identity") +
      geom_errorbar(aes(ymin=my_means-my_ses, ymax=my_means+my_ses), #add error bars
                    width=.2, position=position_dodge(.9)) +
      theme_classic() +
      theme(text = element_text(size = 25),
            axis.title.y = element_text(size = 25),
            axis.title.x = element_text(size = 25, margin = margin(t = 20, r = 0, b = 0, l = 0)),
            axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
            legend.position="top") +
      labs(x = "Agents", y = y_lab, fill = "") +
      ylim(0, 100) +
      scale_x_discrete(breaks = break_labs, labels = plot_labs)
    
    
    return(bar_plot)
}


GetAgentPlotMeans <- function(vignette_num, my_table, image_path, y_lab, list_agents) { 
  
    "Creates bar plots from study results (with agent images as plot labels)
    Input(s): number of vignettes, results table, icon_path, y-axis label, agent_order  
    Output(s): bar plots with agent images
    "
    
    # vignette_num <- agent_num
    # my_table <- tab19 
    # image_path <- icon_path
    # y_lab <- "Psychopathy score"
    # list_agents <- agent_order 
    
    # Define vignettes and values; insert into data frame 
    my_vignettes <- list_agents
    my_means <- as.data.frame(my_table)$x[, "mean"] #get means 
    my_sds <- as.data.frame(my_table)$x[, "sd"] #get standard deviations  
    my_ses <- as.data.frame(my_table)$x[, "se"] #get standard errors 
    my_df <- data.frame(my_vignettes, my_means, my_sds, my_ses) 
    
    
    # Plot images as plot labels 
    alph_agents <- list_agents[order(list_agents)] #alphabetize the agents 
    
    
    # Import images 
    big_comp_gen <- readPNG(paste0(image_path, alph_agents[[1]], ".png"))  
      big_comp_gen <- matrix(rgb(big_comp_gen[,,1],big_comp_gen[,,2],big_comp_gen[,,3], big_comp_gen[,,4] * 0.5), nrow=dim(big_comp_gen)[1]) #0.5 is the alpha
    
    big_comp_tegna <- readPNG(paste0(image_path, alph_agents[[2]], ".png"))   
      big_comp_tegna <- matrix(rgb(big_comp_tegna[,,1],big_comp_tegna[,,2],big_comp_tegna[,,3], big_comp_tegna[,,4] * 0.5), nrow=dim(big_comp_tegna)[1]) #0.5 is the alpha
    
    high_emp_dalai <- readPNG(paste0(image_path, alph_agents[[3]], ".png"))   
      high_emp_dalai <- matrix(rgb(high_emp_dalai[,,1],high_emp_dalai[,,2],high_emp_dalai[,,3], high_emp_dalai[,,4] * 0.5), nrow=dim(high_emp_dalai)[1]) #0.5 is the alpha
    
    high_emp_gandhi <- readPNG(paste0(image_path, alph_agents[[4]], ".png"))   
      high_emp_gandhi <- matrix(rgb(high_emp_gandhi[,,1],high_emp_gandhi[,,2],high_emp_gandhi[,,3], high_emp_gandhi[,,4] * 0.5), nrow=dim(high_emp_gandhi)[1]) #0.5 is the alpha
    
    high_emp_mother <- readPNG(paste0(image_path, alph_agents[[5]], ".png")) 
      high_emp_mother <- matrix(rgb(high_emp_mother[,,1],high_emp_mother[,,2],high_emp_mother[,,3], high_emp_mother[,,4] * 0.5), nrow=dim(high_emp_mother)[1]) #0.5 is the alpha
    
    low_emp_bundy <- readPNG(paste0(image_path, alph_agents[[6]], ".png"))   
      low_emp_bundy <- matrix(rgb(low_emp_bundy[,,1],low_emp_bundy[,,2],low_emp_bundy[,,3], low_emp_bundy[,,4] * 0.5), nrow=dim(low_emp_bundy)[1]) #0.5 is the alpha
    
    low_emp_hannibal <- readPNG(paste0(image_path, alph_agents[[7]], ".png"))   
      low_emp_hannibal <- matrix(rgb(low_emp_hannibal[,,1],low_emp_hannibal[,,2],low_emp_hannibal[,,3], low_emp_hannibal[,,4] * 0.5), nrow=dim(low_emp_hannibal)[1]) #0.5 is the alpha
    
    low_emp_joker <- readPNG(paste0(image_path, alph_agents[[8]], ".png"))    
      low_emp_joker <- matrix(rgb(low_emp_joker[,,1],low_emp_joker[,,2],low_emp_joker[,,3], low_emp_joker[,,4] * 0.5), nrow=dim(low_emp_joker)[1]) #0.5 is the alpha
    
    neutral_brit_spears <- readPNG(paste0(image_path, alph_agents[[9]], ".png"))  
      neutral_brit_spears <- matrix(rgb(neutral_brit_spears[,,1],neutral_brit_spears[,,2],neutral_brit_spears[,,3], neutral_brit_spears[,,4] * 0.5), nrow=dim(neutral_brit_spears)[1]) #0.5 is the alpha
    
    neutral_hs_teacher <- readPNG(paste0(image_path, alph_agents[[10]], ".png"))   
      neutral_hs_teacher <- matrix(rgb(neutral_hs_teacher[,,1],neutral_hs_teacher[,,2],neutral_hs_teacher[,,3], neutral_hs_teacher[,,4] * 0.5), nrow=dim(neutral_hs_teacher)[1]) #0.5 is the alpha
    
    neutral_radio_tech <- readPNG(paste0(image_path, alph_agents[[11]], ".png"))    
      neutral_radio_tech <- matrix(rgb(neutral_radio_tech[,,1],neutral_radio_tech[,,2],neutral_radio_tech[,,3], neutral_radio_tech[,,4] * 0.5), nrow=dim(neutral_radio_tech)[1]) #0.5 is the alpha
    
    small_comp_ikonics <- readPNG(paste0(image_path, alph_agents[[12]], ".png"))  
      small_comp_ikonics <- matrix(rgb(small_comp_ikonics[,,1],small_comp_ikonics[,,2],small_comp_ikonics[,,3], small_comp_ikonics[,,4] * 0.5), nrow=dim(small_comp_ikonics)[1]) #0.5 is the alpha
    
    
    # Create a grid of plot labels
    df <- data.frame() #empty dataframe 
    
    y_value <- my_df$my_means
    y_height <- 7.5 # + or - the height of each image 
    x_width <- 0.3 
    my_midpoints <- c(-0.25, 0.95, 1.95, 0.3, 1.4, 0, -0.05, 0.65, 0, -0.15, 0.5, 1) #the midpoints of plot images below 
    
    image_plot <- ggplot(df) + 

      # Plot the agent images 
      annotation_custom(rasterGrob(high_emp_mother), xmin = my_midpoints[[1]]-x_width, xmax = my_midpoints[[1]]+x_width, ymin = y_value[[1]] - y_height, ymax = y_value[[1]] + y_height) + 
      annotation_custom(rasterGrob(high_emp_gandhi), xmin = my_midpoints[[2]]-x_width, xmax = my_midpoints[[2]]+x_width, ymin = y_value[[2]] - y_height, ymax = y_value[[2]] + y_height) + 
      annotation_custom(rasterGrob(high_emp_dalai), xmin = my_midpoints[[3]]-x_width, xmax = my_midpoints[[3]]+x_width, ymin = y_value[[3]] - y_height, ymax = y_value[[3]] + y_height) + 
      annotation_custom(rasterGrob(neutral_radio_tech), xmin = my_midpoints[[4]]-x_width, xmax = my_midpoints[[4]]+x_width, ymin = y_value[[4]] - y_height, ymax = y_value[[4]] + y_height) +
      annotation_custom(rasterGrob(neutral_hs_teacher), xmin = my_midpoints[[5]]-x_width, xmax = my_midpoints[[5]]+x_width, ymin = y_value[[5]] - y_height, ymax = y_value[[5]] + y_height) +
      annotation_custom(rasterGrob(small_comp_ikonics), xmin = my_midpoints[[6]]-x_width, xmax = my_midpoints[[6]]+x_width, ymin = y_value[[6]] - y_height, ymax = y_value[[6]] + y_height) +
      annotation_custom(rasterGrob(big_comp_tegna), xmin = my_midpoints[[7]]-x_width, xmax = my_midpoints[[7]]+x_width, ymin = y_value[[7]] - y_height, ymax = y_value[[7]] + y_height) +
      annotation_custom(rasterGrob(neutral_brit_spears), xmin = my_midpoints[[8]]-x_width, xmax = my_midpoints[[8]]+x_width, ymin = y_value[[8]] - y_height, ymax = y_value[[8]] + y_height) +
      annotation_custom(rasterGrob(big_comp_gen), xmin = my_midpoints[[9]]-x_width, xmax = my_midpoints[[9]]+x_width, ymin = y_value[[9]] - y_height, ymax = y_value[[9]] + y_height) +
      annotation_custom(rasterGrob(low_emp_joker), xmin = my_midpoints[[10]]-x_width, xmax = my_midpoints[[10]]+x_width, ymin = y_value[[10]] - y_height, ymax = y_value[[10]] + y_height) +
      annotation_custom(rasterGrob(low_emp_hannibal), xmin = my_midpoints[[11]]-x_width, xmax = my_midpoints[[11]]+x_width, ymin = y_value[[11]] - y_height, ymax = y_value[[11]] + y_height) +
      annotation_custom(rasterGrob(low_emp_bundy), xmin = my_midpoints[[12]]-x_width, xmax = my_midpoints[[12]]+x_width, ymin = y_value[[12]] - y_height, ymax = y_value[[12]] + y_height) +
      
      # Plot error bars and customize plot 
      geom_errorbar(my_df, 
                    mapping = aes(x = my_midpoints, y = my_means, 
                                  ymin = my_means-my_ses, ymax = my_means+my_ses), #add error bars 
                    width = 0, size = 1, position = position_dodge(0.9), color = "red") +
      geom_jitter(my_df, 
                  mapping = aes(x = my_midpoints, y = my_means), 
                  size = 2, position = position_dodge(0.9), color = "black") + 
      xlim(-0.25, 12.75) + ylim(0, 100) + labs(x = "", y = y_lab) + 
      theme_classic() + 
      theme(text = element_text(size = 25),
            axis.title.y = element_text(size = 25),
            axis.title.x = element_text(size = 25, margin = margin(t = 20, r = 0, b = 0, l = 0)),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
      ) 
    
    return(image_plot)
}


AddCustomLegend <- function(vignette_num, my_table, image_path, y_lab, list_agents, agent_labs) { 
  
    "
    Calls GetAgentPlotMeans() function 
    Adds custom legend to bar plot (with agent images as plot labels)
    Input(s): number of vignettes, results table, icon_path, y-axis label, agent_order, agent_labels  
    Output(s): bar plots with agent images as legend 
    "
    
    # vignette_num <- agent_num
    # my_table <- tab19 
    # image_path <- icon_path
    # y_lab <- "Psychopathy score"
    # list_agents <- agent_order 
    # agent_labs <- agent_labels 
    
    # Plot images as plot labels 
    alph_agents <- list_agents[order(list_agents)] #alphabetize the agents 
    
    # Import images 
    big_comp_gen <- readPNG(paste0(image_path, alph_agents[[1]], ".png"))  
    big_comp_gen <- matrix(rgb(big_comp_gen[,,1],big_comp_gen[,,2],big_comp_gen[,,3], big_comp_gen[,,4]), nrow=dim(big_comp_gen)[1]) #0.5 is the alpha
    
    big_comp_tegna <- readPNG(paste0(image_path, alph_agents[[2]], ".png"))   
    big_comp_tegna <- matrix(rgb(big_comp_tegna[,,1],big_comp_tegna[,,2],big_comp_tegna[,,3], big_comp_tegna[,,4]), nrow=dim(big_comp_tegna)[1]) #0.5 is the alpha
    
    high_emp_dalai <- readPNG(paste0(image_path, alph_agents[[3]], ".png"))   
    high_emp_dalai <- matrix(rgb(high_emp_dalai[,,1],high_emp_dalai[,,2],high_emp_dalai[,,3], high_emp_dalai[,,4]), nrow=dim(high_emp_dalai)[1]) #0.5 is the alpha
    
    high_emp_gandhi <- readPNG(paste0(image_path, alph_agents[[4]], ".png"))   
    high_emp_gandhi <- matrix(rgb(high_emp_gandhi[,,1],high_emp_gandhi[,,2],high_emp_gandhi[,,3], high_emp_gandhi[,,4]), nrow=dim(high_emp_gandhi)[1]) #0.5 is the alpha
    
    high_emp_mother <- readPNG(paste0(image_path, alph_agents[[5]], ".png")) 
    high_emp_mother <- matrix(rgb(high_emp_mother[,,1],high_emp_mother[,,2],high_emp_mother[,,3], high_emp_mother[,,4]), nrow=dim(high_emp_mother)[1]) #0.5 is the alpha
    
    low_emp_bundy <- readPNG(paste0(image_path, alph_agents[[6]], ".png"))   
    low_emp_bundy <- matrix(rgb(low_emp_bundy[,,1],low_emp_bundy[,,2],low_emp_bundy[,,3], low_emp_bundy[,,4]), nrow=dim(low_emp_bundy)[1]) #0.5 is the alpha
    
    low_emp_hannibal <- readPNG(paste0(image_path, alph_agents[[7]], ".png"))   
    low_emp_hannibal <- matrix(rgb(low_emp_hannibal[,,1],low_emp_hannibal[,,2],low_emp_hannibal[,,3], low_emp_hannibal[,,4]), nrow=dim(low_emp_hannibal)[1]) #0.5 is the alpha
    
    low_emp_joker <- readPNG(paste0(image_path, alph_agents[[8]], ".png"))    
    low_emp_joker <- matrix(rgb(low_emp_joker[,,1],low_emp_joker[,,2],low_emp_joker[,,3], low_emp_joker[,,4]), nrow=dim(low_emp_joker)[1]) #0.5 is the alpha
    
    neutral_brit_spears <- readPNG(paste0(image_path, alph_agents[[9]], ".png"))  
    neutral_brit_spears <- matrix(rgb(neutral_brit_spears[,,1],neutral_brit_spears[,,2],neutral_brit_spears[,,3], neutral_brit_spears[,,4]), nrow=dim(neutral_brit_spears)[1]) #0.5 is the alpha
    
    neutral_hs_teacher <- readPNG(paste0(image_path, alph_agents[[10]], ".png"))   
    neutral_hs_teacher <- matrix(rgb(neutral_hs_teacher[,,1],neutral_hs_teacher[,,2],neutral_hs_teacher[,,3], neutral_hs_teacher[,,4]), nrow=dim(neutral_hs_teacher)[1]) #0.5 is the alpha
    
    neutral_radio_tech <- readPNG(paste0(image_path, alph_agents[[11]], ".png"))    
    neutral_radio_tech <- matrix(rgb(neutral_radio_tech[,,1],neutral_radio_tech[,,2],neutral_radio_tech[,,3], neutral_radio_tech[,,4]), nrow=dim(neutral_radio_tech)[1]) #0.5 is the alpha
    
    small_comp_ikonics <- readPNG(paste0(image_path, alph_agents[[12]], ".png"))  
    small_comp_ikonics <- matrix(rgb(small_comp_ikonics[,,1],small_comp_ikonics[,,2],small_comp_ikonics[,,3], small_comp_ikonics[,,4]), nrow=dim(small_comp_ikonics)[1]) #0.5 is the alpha 
    
    # Call agent function 
    image_plot <- GetAgentPlotMeans(vignette_num, my_table, image_path, y_lab, list_agents) #agent_labels
    
    # Add custom legend 
    x_min <- 3.5 #define x and y intervals
    x_max <- 4.5 
    y_min <- seq(7, 84, by = 7)
    y_max <- seq(14, 91, by = 7)
    box_min <- x_min 
    box_max <- x_max+2.95 
    
    legend_plot <- image_plot + 
    
      # Plot the agent images as legend 
      annotation_custom(rasterGrob(high_emp_mother), xmin = x_min, xmax = x_max, ymin = y_min[1], ymax = y_max[1]) + 
      annotation_custom(rasterGrob(high_emp_gandhi), xmin = x_min, xmax = x_max, ymin = y_min[2], ymax = y_max[2]) + 
      annotation_custom(rasterGrob(high_emp_dalai), xmin = x_min, xmax = x_max, ymin = y_min[3], ymax = y_max[3]) + 
      annotation_custom(rasterGrob(neutral_radio_tech), xmin = x_min, xmax = x_max, ymin = y_min[4], ymax = y_max[4]) +
      annotation_custom(rasterGrob(neutral_hs_teacher), xmin = x_min, xmax = x_max, ymin = y_min[5], ymax = y_max[5]) +
      annotation_custom(rasterGrob(small_comp_ikonics), xmin = x_min, xmax = x_max, ymin = y_min[6], ymax = y_max[6]) +
      annotation_custom(rasterGrob(big_comp_tegna), xmin = x_min, xmax = x_max, ymin = y_min[7], ymax = y_max[7]) +
      annotation_custom(rasterGrob(neutral_brit_spears), xmin = x_min, xmax = x_max, ymin = y_min[8], ymax = y_max[8]) +
      annotation_custom(rasterGrob(big_comp_gen), xmin = x_min, xmax = x_max, ymin = y_min[9], ymax = y_max[9]) +
      annotation_custom(rasterGrob(low_emp_joker), xmin = x_min, xmax = x_max, ymin = y_min[10], ymax = y_max[10]) +
      annotation_custom(rasterGrob(low_emp_hannibal), xmin = x_min, xmax = x_max, ymin = y_min[11], ymax = y_max[11]) +
      annotation_custom(rasterGrob(low_emp_bundy), xmin = x_min, xmax = x_max, ymin = y_min[12], ymax = y_max[12]) + 
      
      # Add agent names as labels 
      annotate(geom="text", size = 5, x=x_max-0.15, y=((y_max[1]+y_min[1])/2), label=agent_labs[1], hjust = 0) +
      annotate(geom="text", size = 5, x=x_max-0.15, y=((y_max[2]+y_min[2])/2), label=agent_labs[2], hjust = 0) +
      annotate(geom="text", size = 5, x=x_max-0.15, y=((y_max[3]+y_min[3])/2), label=agent_labs[3], hjust = 0) +
      annotate(geom="text", size = 5, x=x_max-0.15, y=((y_max[4]+y_min[4])/2), label=agent_labs[4], hjust = 0) +
      annotate(geom="text", size = 5, x=x_max-0.15, y=((y_max[5]+y_min[5])/2), label=agent_labs[5], hjust = 0) +
      annotate(geom="text", size = 5, x=x_max-0.15, y=((y_max[6]+y_min[6])/2), label=agent_labs[6], hjust = 0) +
      annotate(geom="text", size = 5, x=x_max-0.15, y=((y_max[7]+y_min[7])/2), label=agent_labs[7], hjust = 0) +
      annotate(geom="text", size = 5, x=x_max-0.15, y=((y_max[8]+y_min[8])/2), label=agent_labs[8], hjust = 0) +
      annotate(geom="text", size = 5, x=x_max-0.15, y=((y_max[9]+y_min[9])/2), label=agent_labs[9], hjust = 0) +
      annotate(geom="text", size = 5, x=x_max-0.15, y=((y_max[10]+y_min[10])/2), label=agent_labs[10], hjust = 0) +
      annotate(geom="text", size = 5, x=x_max-0.15, y=((y_max[11]+y_min[11])/2), label=agent_labs[11], hjust = 0) +
      annotate(geom="text", size = 5, x=x_max-0.15, y=((y_max[12]+y_min[12])/2), label=agent_labs[12], hjust = 0) + 
      
      # Add "Key" label 
      annotate(geom="text", size = 7, x=(box_max+box_min)/2, y=y_max[12]+3, label="Key", fontface=2) + 
      
      # Add box around legend 
      geom_rect(aes(xmin = box_min, xmax = box_max, ymin = y_min[1]-3, ymax = y_max[12] + 9), 
                alpha = 0, color = "black") 
    
    return(legend_plot)
}


GetImageLabels <- function(bar_plot, image_folder, image_list) {

    "Creates plot images to be attached to bar plots
    Input(s): bar plot, image folder, list of image names
    Output(s): plot images
    "

    # Use axis_canvas() to plot vignette images on the x-axis
    x_start <- 0.35
    x_interval <- 0.325
    x_scale <- 1.5

    plot_x_axis <- axis_canvas(bar_plot, axis = 'x') +
      draw_image(paste0(image_folder, image_list[1], ".png"), x = x_start, scale = x_scale) +
      draw_image(paste0(image_folder, image_list[2], ".png"), x = x_start + x_interval, scale = x_scale) +
      draw_image(paste0(image_folder, image_list[3], ".png"), x = x_start + 1, scale = x_scale) +
      draw_image(paste0(image_folder, image_list[4], ".png"), x = (x_start + x_interval) + 1, scale = x_scale) +
      draw_image(paste0(image_folder, image_list[5], ".png"), x = x_start + 2, scale = x_scale) +
      draw_image(paste0(image_folder, image_list[6], ".png"), x = (x_start + x_interval) + 2, scale = x_scale) +
      draw_image(paste0(image_folder, image_list[7], ".png"), x = x_start + 3, scale = x_scale) +
      draw_image(paste0(image_folder, image_list[8], ".png"), x = (x_start + x_interval) + 3, scale = x_scale) +
      draw_image(paste0(image_folder, image_list[9], ".png"), x = x_start + 4, scale = x_scale) +
      draw_image(paste0(image_folder, image_list[10], ".png"), x = (x_start + x_interval) + 4, scale = x_scale) +
      draw_image(paste0(image_folder, image_list[11], ".png"), x = x_start + 5, scale = x_scale) +
      draw_image(paste0(image_folder, image_list[12], ".png"), x = (x_start + x_interval) + 5, scale = x_scale) +
      draw_image(paste0(image_folder, image_list[13], ".png"), x = x_start + 6, scale = x_scale) +
      draw_image(paste0(image_folder, image_list[14], ".png"), x = (x_start + x_interval) + 6, scale = x_scale) +
      draw_image(paste0(image_folder, image_list[15], ".png"), x = x_start + 7, scale = x_scale) +
      draw_image(paste0(image_folder, image_list[16], ".png"), x = (x_start + x_interval) + 7, scale = x_scale)

    return(plot_x_axis)
}


GetTweetSentiment <- function(comp_data, n_cmps, n_twts, comp_names) {
    "
    Create funtion to get sentiment score means and standard errors for Tweets    
    Input: company_tweets, n_comps, n_tweets, company_names  
    Output: average mean and standard error sentiment scores for Tweets by company
    "
    
    # comp_data <- company_tweets 
    # n_cmps <- n_comps 
    # n_twts <- n_tweets 
    # comp_names <- company_names
    
    
    # Clean text 
    tweet_gen <- comp_data$Text 
    unclean_tweet <- tolower(tweet_gen) #make all words in each sentence lowercase 
    clean_tweet <- unclean_tweet %>% 
      
      # Remove URLs
      str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>%
      # Remove mentions e.g. "@my_account"
      str_remove_all("@[[:alnum:]_]{4,}") %>%
      # Remove hashtags
      str_remove_all("#[[:alnum:]_]+") %>%
      # Replace "&" character reference with "and"
      str_replace_all("&amp;", "and") %>%
      # Remove puntucation, using a standard character class
      str_remove_all("[[:punct:]]") %>%
      # Remove "RT: " from beginning of retweets
      str_remove_all("^RT:? ") %>%
      # Replace any newline characters with a space
      str_replace_all("\\\n", " ") %>%
      # Make everything lowercase
      str_to_lower() %>%
      # Remove any trailing whitespace around the text
      str_trim("both") 
    
    
    # Sort words by company 
    sentiment_sorted <- c()  
    for(i in 1:n_cmps) {
      sentiment_sorted[[i]] <- clean_tweet[ (((i-1)*n_twts) + 1) : (i*n_twts) ] 
    }
    names(sentiment_sorted) <- comp_names 
    
    
    # Get means and standard errors of words for every plot 
    sentiment_summary <- c() 
    for(i in 1:n_cmps) {
      sentiment_summary[[i]] <- c(mean(sentiment_by(sentiment_sorted[[i]])$ave_sentiment), 
                                  std.error(sentiment_by(sentiment_sorted[[i]])$ave_sentiment)) 
    } 
    sentiment_data <- data.frame(comp_names = comp_names, 
                                    mean = unlist(sentiment_summary)[c(TRUE, FALSE)], 
                                    se = unlist(sentiment_summary)[c(FALSE, TRUE)]) 
    
    # Get company type: big, small, or general 
    sentiment_data$comp_label <- NA #create empty column 
    for(i in 1:nrow(sentiment_data)) {
      
      if(sentiment_data$comp_names[[i]] %in% comp_names[1:20]) {
        sentiment_data$comp_label[[i]] <- "Big Company" 
      } else if(sentiment_data$comp_names[[i]] %in% comp_names[21:40]) {
        sentiment_data$comp_label[[i]] <- "Small Company" 
      } else if(sentiment_data$comp_names[[i]] == comp_names[41]) {
        sentiment_data$comp_label[[i]] <- "Big Companies in General"
      }
    }
    
    
    # Get sentiment scores for ALL of individual subjects and plots  
    sentiment_all <- c() 
    for(i in 1:n_cmps) {
      sentiment_all[[i]] <- sentiment_by(sentiment_sorted[[i]])$ave_sentiment 
    }
    names(sentiment_all) <- comp_names 
    
    
    sentiment_list <- list(sentiment_data, sentiment_all, sentiment_sorted)
    return(sentiment_list) 
}


GetSentimentBarPlot <- function(sentiment_data) {
    "
    Plot sentiment bar graph 
    Input: sentiment_df 
    Output: the sentiment bar graph by ascending scores
    " 
    
    # sentiment_data <- sentiment_df 
    
    
    # Reorder the sentiment scores 
    comp_order <- order(sentiment_data$mean) #get the order of this subset of data by ascending mean 
    names_order <- sentiment_data[comp_order, "comp_names"] #get the order of plot names 
    sentiment_data$comp_names <- factor(sentiment_data$comp_names, levels = names_order) #set the reordered data using comp_names (for plotting)
    
    
    # Plot bar graph 
    comp_type <- unique(sentiment_data$comp_label) #get the company type  
    
    sentiment_bar <- ggplot(sentiment_data, aes(x = comp_names, y = mean, fill = comp_label)) + 
      geom_bar(position="dodge", stat="identity") +
      geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width=.2,
                    position=position_dodge(.9)) +
      ggtitle("Mean Sentiment Scores") + 
      xlab("Companies") + ylab("Mean Sentiment Score") +
      theme_classic() + 
      theme(
        plot.title = element_text(size=20, face="bold", hjust = 0.5),
        legend.title = element_text(size=15, face="bold"),
        legend.position = "top",
        legend.title.align = 0.5, 
        text = element_text(size = 15),          
        axis.title.y = element_text(size=15, face="bold"), 
        axis.title.x = element_text(size=15, face="bold", margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size=15, angle = 45, hjust = 1), 
        axis.ticks.x = element_blank() 
      ) + scale_fill_manual(
        name = "Company Type", 
        breaks = comp_type, 
        labels = comp_type, 
        values = c("gray40", "gray85", "darkorange"), #c("darkorange4", "darkorange", "turquoise4", "turquoise2"), #c("#56B4E9", "#009E73", "firebrick3", "darkorange"),
        guide = guide_legend(title.position = "top")
      )
    
    
    # Return items 
    bar_plot_list <- list(sentiment_bar, names_order)
    return(bar_plot_list)
}


GetPsychWordFrequency <- function(psychopath_data, twts_list, comp_names, n_cmps) {
    "
    Create funtion to get the frequency of psychopathy words for each company     
    Input: psychopath_df, tweets_list, company_names, n_comps 
    Output: frequency of psychopathy words for each company 
    "
    
    # psychopath_data <- psychopath_df
    # twts_list <- tweets_list 
    # comp_names <- company_names 
    # n_cmps <- n_comps 
    
    
    # Define list of psychopathic words (from the Hare Scale)
    psychopath_words <- psychopath_data$psychopathic
    
    
    # Create empty data frame to store frequency of psychopathy words 
    psych_data <- array(0, dim = c(length(comp_names), 2)) 
    psych_data <- as.data.frame(psych_data, stringsAsFactors=FALSE) 
    colnames(psych_data) <- c("comp_names", "psych_freq") 
    psych_data$comp_names <- comp_names
    
    
    # Get frequency of psychopathy words 
    for(i in 1:n_cmps) { 
      
      # Get all tweets for each company 
      tweets_by_comp <- unlist(twts_list[[i]]) 
      tweets_by_comp <- paste(tweets_by_comp, collapse = " ") #unlist the words to get frequency 
      
      # Get frequency of words in each string 
      tweets_by_comp <- tweets_by_comp %>% 
        strsplit(split = " ") %>% # or strsplit(split = "\\W") 
        unlist() %>%
        table() %>% 
        sort(decreasing = TRUE) %>% 
        as.data.frame()
      
      # Rename data frame columns 
      colnames(tweets_by_comp) <- c("word", "freq")
      
      # How many instances of psychopath words are there for each company? 
      psych_freq <- tweets_by_comp[tweets_by_comp$word %in% psychopath_words, ] # %in% c("walmart", "the"), ] 
      psych_data$psych_freq[[i]] <- nrow(psych_freq) #store in data frame 
    }
    
    
    # Get company type: big, small, or general 
    psych_data$comp_label <- NA #create empty column 
    for(i in 1:nrow(psych_data)) {
      
      if(psych_data$comp_names[[i]] %in% comp_names[1:20]) {
        psych_data$comp_label[[i]] <- "Big Company" 
      } else if(psych_data$comp_names[[i]] %in% comp_names[21:40]) {
        psych_data$comp_label[[i]] <- "Small Company" 
      } else if(psych_data$comp_names[[i]] == comp_names[41]) {
        psych_data$comp_label[[i]] <- "Big Companies in General"
      }
    }
    
    
    return(psych_data)
}


GetPsychFreqBarPlot <- function(psych_data) {
  "
    Plot bar graph of frequency of psychopathy words 
    Input: psych_df 
    Output: the frequency bar graph by ascending scores
    " 
  
    # psych_data <- psych_df 
    
    
    # Reorder the sentiment scores 
    comp_order <- order(psych_data$psych_freq) #get the order of this subset of data by ascending psych_freq 
    names_order <- psych_data[comp_order, "comp_names"] #get the order of plot names 
    psych_data$comp_names <- factor(psych_data$comp_names, levels = names_order) #set the reordered data using comp_names (for plotting)
    
    
    # Plot bar graph 
    comp_type <- unique(psych_data$comp_label) #get the company type  
    
    psych_bar <- ggplot(psych_data, aes(x = comp_names, y = psych_freq, fill = comp_label)) + 
      geom_bar(position="dodge", stat="identity") +
      ggtitle("Frequency of Psychopathy Words") + 
      xlab("Companies") + ylab("Frequency") + 
      ylim(0, 5) +
      theme_classic() + 
      theme(
        plot.title = element_text(size=20, face="bold", hjust = 0.5),
        legend.title = element_text(size=15, face="bold"),
        legend.position = "top",
        legend.title.align = 0.5, 
        text = element_text(size = 15),          
        axis.title.y = element_text(size=15, face="bold"), 
        axis.title.x = element_text(size=15, face="bold", margin = margin(t = 20, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(size=15, angle = 45, hjust = 1), 
        axis.ticks.x = element_blank() 
      ) + scale_fill_manual(
        name = "Company Type", 
        breaks = comp_type, 
        labels = comp_type, 
        values = c("gray40", "gray85", "darkorange"), #c("darkorange4", "darkorange", "turquoise4", "turquoise2"), #c("#56B4E9", "#009E73", "firebrick3", "darkorange"),
        guide = guide_legend(title.position = "top")
      )
    
    
    # Return items 
    bar_plot_list <- list(psych_bar, names_order)
    return(bar_plot_list)
}


GetCompanyMeans <- function(vignette_num, big_comp, small_comp, x_lab, y_lab, break_labs, plot_labs) { 
  
    "Creates bar plots from study results 
    Input(s): number of vignettes, big_tweets_means_all, small_tweets_means_all, x-axis label, y-axis label, condition 2, x-tick labels  
    Output(s): bar plots 
    "
    
    # vignette_num <- 2
    # big_comp <- big_tweets_means_all
    # small_comp <- small_tweets_means_all
    # x_lab <- "Firm size" 
    # y_lab <- "Tweet sentiment"
    # break_labs <- as.character(c(1, vignette_num))
    # plot_labs <- c("Big", "Small")
    
    # Get means and combine into data frame 
    my_vignettes <- 1:vignette_num 
    my_comps <- as.character(plot_labs) 
    my_means <- c(mean(big_comp$sentiment), mean(small_comp$sentiment)) #get means 
    my_sds <- c(sd(big_comp$sentiment), sd(small_comp$sentiment)) #get standard deviations  
    my_ses <- c(std.error(big_comp$sentiment), std.error(small_comp$sentiment)) #get standard errors 
    my_df <- data.frame(my_vignettes, my_comps, my_means, my_sds, my_ses) 
    
    
    # Use ggplot() to build a bar plot 
    bar_plot <- ggplot(my_df, aes(x=my_vignettes, y=my_means)) +
      geom_bar(position="dodge", stat="identity") +
      geom_errorbar(aes(ymin=my_means-my_ses, ymax=my_means+my_ses), #add error bars
                    width=.2, position=position_dodge(.9)) +
      theme_classic() +
      theme(text = element_text(size = 30),
            axis.title.y = element_text(size = 30),
            axis.title.x = element_text(size = 30, margin = margin(t = 20, r = 0, b = 0, l = 0)),
            axis.text.x = element_text(size = 25),
            legend.position="top") +
      labs(x = x_lab, y = y_lab, fill = "") +
      scale_x_continuous(breaks = break_labs, labels = plot_labs)
    
    
  return(bar_plot)
}


GetIndividualCompanies <- function(big_comp, small_comp, x_lab, y_lab, plot_labs) { 
  
    "Creates bar plots from study results 
    Input(s): big_tweets_means_all, small_tweets_means_all, x-axis label, y-axis label, company labels  
    Output(s): bar plot 
    "
    
    # big_comp <- big_tweets_means_all
    # small_comp <- small_tweets_means_all
    # x_lab <- "Company"
    # y_lab <- "Tweet sentiment"
    # plot_labs <- c("Big", "Small")
    
    # Get means and combine into data frame 
    my_comps <- as.character(plot_labs) 
    my_sent <- rbind(big_comp, small_comp) 
    
    my_sent$comp_type <- NA #create new column  
    my_sent$comp_type[1:nrow(big_comp)] <- my_comps[1] #set labels for big companies 
    my_sent$comp_type[ (nrow(big_comp) + 1) : nrow(my_sent) ] <- my_comps[2] #set labels for small companies 
    
    my_sent <- my_sent[order(my_sent$sentiment), ] 
    my_sent$company <- factor(my_sent$company, levels = my_sent$company) #set the reordered data using company names
    
    # Use ggplot() to build a bar plot 
    bar_plot <- ggplot(my_sent, aes(x=company, y=sentiment, fill=comp_type)) +
      geom_bar(position="dodge", stat="identity") +
      theme_classic() +
      theme(text = element_text(size = 30),
            axis.title.y = element_text(size = 30),
            axis.title.x = element_text(size = 30,  margin = margin(t = 20, r = 0, b = 0, l = 0)),
            axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust=1,),
            legend.position="top") + 
      labs(x = x_lab, y = y_lab, fill = "") + 
      scale_fill_manual(labels = my_comps, 
                        values = c("gray40", "gray85"))
    
    
    return(bar_plot)
}


# ======================================== MAIN ======================================== #


# Define variables 
vig_num <- 8
vig_breaks <- c("1", "2", "3", "4", "5", "6", "7", "8")
industry_labels <- c("Clothing", "Cosmetics", "Apparel", "Mining", 
                 "Technology", "Petroleum", "Tobacco", "Makeup") #set category/industry names


# Pathway to the files
exp6F <- read.xlsx("data/corporate_essence_6.xlsx")

# For Psychopathy Study: 
company_tweets <- read.csv("data/comp_tweets.csv")
#psychopath_df <- read.xlsx("data/psychopath_dictionary.xlsx")

big_tweets_means_all <- read.csv("data/big_tweets_means_all.csv")
small_tweets_means_all <- read.csv("data/small_tweets_means_all.csv")

# Exclusions
exp6 <- subset(exp6F,exp6F$ac.errors==0 & exp6F$familiarity<=1) 

#### Study 6 ####

# Define variables 
group_num <- 4 
group_breaks <- as.character(1:group_num)  
group_labels <- c("High-empathy", "Neutral", "Companies", "Low-empathy")  #in the order of tab18$Condition

agent_list <- unique(exp6$condition) 
agent_num <- length(agent_list) 
agent_breaks <- as.character(1:agent_num) 
# agent_labels <- c("Mother Teresa", "Mahatma Gandhi", "The Dalai Lama",          #in the order of agent_order (see tab19$Condition below)
#                   "Olivia Johnson,\na radiology\ntechnician", "Keith Williamson,\na high school\nteacher", "IKONICS,\na small company", 
#                   "Tegna,\na big Fortune 500\ncompany", "Britney Spears", "Big companies\nin general", 
#                   "The Joker", "Hannibal Lecter", "Ted Bundy") 

agent_labels <- c("Mother Teresa", "Mahatma Gandhi", "The Dalai Lama",          #in the order of agent_order (see tab19$Condition below)
                  "Radiology technician", "High school teacher", "IKONICS, small company", 
                  "Tegna, big Fortune 500 company", "Britney Spears", "Big companies in general", 
                  "The Joker", "Hannibal Lecter", "Ted Bundy") 

comp_num <- 2 
comp_breaks <- 1:comp_num 
comp_labels <- c("Big", "Small") 

icon_path <- "logos/e6_psychopathy/" 
n_tweets <- 100 
n_comps <- 41 #top 20 and bottom 20 companies on Fortune 1000 list, plus the term "Big Companies" in general 
company_names <- unique(company_tweets$Company) 


# Prepare a data frame for grouping agents together 
exp6_grouped <- exp6 

# Group agents together 
high_emp <- agent_list[grep("high_emp", agent_list)] #high-empathy agents 
low_emp <- agent_list[grep("low_emp", agent_list)] #low-empathy agents 
neutral <- agent_list[grep("neutral", agent_list)] #neutral-empathy agents 
companies <- agent_list[grep("comp", agent_list)] #companies

for(i in 1:nrow(exp6_grouped)) {
  if(exp6_grouped$condition[[i]] %in% high_emp) { 
    exp6_grouped$condition[[i]] <- "high_emp" 
  } else if(exp6_grouped$condition[[i]] %in% low_emp) { 
    exp6_grouped$condition[[i]] <- "low_emp" 
  } else if(exp6_grouped$condition[[i]] %in% neutral) { 
    exp6_grouped$condition[[i]] <- "neutral" 
  } else if(exp6_grouped$condition[[i]] %in% companies) { 
    exp6_grouped$condition[[i]] <- "companies" 
  } 
}


# Means
tab18 <- with(exp6_grouped, aggregate(psychopathy, list(Condition=condition), function(x) {c(mean=mean(x, na.rm=TRUE), sd=sd(x, na.rm=TRUE), se=std.error(x, na.rm=TRUE))}))
tab18 <- tab18[order(tab18$x[, "mean"]), ] 
tab18$Condition <- factor(tab18$Condition, levels = tab18$Condition) #set the order of group names for plotting 

tab19 <- with(exp6, aggregate(psychopathy, list(Condition=condition), function(x) {c(mean=mean(x, na.rm=TRUE), sd=sd(x, na.rm=TRUE), se=std.error(x, na.rm=TRUE))}))
tab19 <- tab19[order(tab19$x[, "mean"]), ] 
tab19$Condition <- factor(tab19$Condition, levels = tab19$Condition) #set the order of agent names for plotting 
agent_order <- as.character(tab19$Condition) 


# Plots 
pdf(file = "e7_group_psychopathy.pdf", width = 10, height = 7)
  GetBarPlotMeans(group_num, tab18, "Psychopathy score", 
                  group_breaks, group_labels) 
dev.off()

pdf(file = "e7_agent_psychopathy.pdf", width = 15, height = 7)
  AddCustomLegend(agent_num, tab19, icon_path, "Psychopathy score", agent_order, agent_labels)
dev.off()

# Frequency of Psychopathy Words 
tweets_list <- GetTweetSentiment(company_tweets, n_comps, n_tweets, company_names)[[3]]

# Plotting means for big and small companies 
pdf(file = "e7_comp_means_bar_plot.pdf", width = 10, height = 7)
  GetCompanyMeans(comp_num, big_tweets_means_all, small_tweets_means_all, 
                  "Firm size", "Tweet sentiment", 
                  comp_breaks, comp_labels)
dev.off()


pdf(file = "e7_comp_all_bar_plot.pdf", width = 15, height = 7)
  GetIndividualCompanies(big_tweets_means_all, small_tweets_means_all, 
                         "Company", "Tweet sentiment", comp_labels)
dev.off()


# T-tests
t.test(big_tweets_means_all$sentiment,small_tweets_means_all$sentiment,paired=F,var.equal=T) 

target_comp_num <- 100 
big_comp_excluded <- target_comp_num - nrow(big_tweets_means_all) 
small_comp_excluded <- target_comp_num - nrow(small_tweets_means_all) 

# =========================================== Move Files ==================================================== #

dir.create("analysis_plots")
plot_files <- list.files(pattern = ".pdf")
file.move(plot_files, "analysis_plots", overwrite = TRUE)


