# load libraries
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(cowplot)
library(ggpmisc)

## IMPORTANT MESSAGE: THE PIPELINE ALWAYS DETECT GROUNDTRUTH BUT,
## IT WRONGLY CLASSIFY THE OBJECT

# upload data
data <- read_csv("data/Giessen_detect.csv")

# filter truth only
truth = data %>% filter(!truth.class == "NA")  ## The only truth data
# assign the truth length
length_ground_truth_count= length(truth$pic_name)

sum(is.na(truth)) ## AGAIN: ALL TRUTH OBJECTS CAN BE DETECTED BUT WRONGLY CLASSIFIED

#####################################   #####################################   #####################################

# check the right detected objects in the truth data
truth_only = truth %>% mutate(detection_status = ifelse(detect.class == truth.class, "TRUE", "FALSE"))
true_detected_truth_only = truth_only %>% filter(detection_status == "TRUE") 
length_true_matched_only = length(true_detected_truth_only$pic_name)
length_true_matched_only    ## The correct detected stomata trough truth only

#####################################   #####################################   #####################################

# filter false detected
false_detected = data %>% filter(is.na(truth.class)) ## The false detected stomata
# assign the false_detected length
length_false_detected_count= length(false_detected$pic_name) ## Imp to calculate FDI "last parameter"

#####################################   #####################################   #####################################

# rewrite false over NA values in detection_status
data_NA_false = data
data_NA_false$detection_status <- ifelse(data_NA_false$truth.class == data_NA_false$detect.class, TRUE, FALSE)
data_NA_false1 = na.omit(data_NA_false) ## Truly detected with detection classes
length(data_NA_false1$pic_name)

# Assuming data_NA_false is your data frame and length_ground_truth_count is the total number of true instances
unique_classes <- unique(truth$truth.class)

class_lengths <- sapply(unique_classes, function(class) sum(nchar(truth$truth.class == class)))

for (i in seq_along(unique_classes)) {
  class_filter <- data_NA_false1$truth.class == unique_classes[i]
  class_lengths[i] <- sum(data_NA_false1$detection_status[class_filter] == "TRUE") / sum(class_filter)
}

# Now, class_lengths contains recall values for each class
print(class_lengths)

# Creating a data.frame from the output
recall_per_class <- data.frame(Class = names(class_lengths), Recall = as.numeric(class_lengths))

# Printing the data.frame
print(recall_per_class)

#####################################  #####################################   ##################################### 
#####################################  #####################################   #####################################  

# Calculate false detection index
false_detection_index <- length(false_detected$pic_name) / length(data$pic_name)

# Get unique false classes
false_classes <- unique(false_detected$detect.class)

# Initialize a vector to store class lengths
false_class_lengths <- sapply(false_classes, function(class) sum(false_detected$detect.class == class))

for (i in seq_along(false_classes)) {
  false_class_lengths[i] <- sum(false_detected$detect.class == false_classes[i]) / nrow(false_detected)
  
}

print(false_class_lengths)

false_index_per_class <- data.frame(Class = names(false_class_lengths), False_detection_index = as.numeric(false_class_lengths))

half_farmer_updated = left_join(recall_per_class,false_index_per_class, by= "Class")

# Saving the data.frame 
write.csv(half_farmer_updated, file = "Giessen.csv", row.names = FALSE)

# ==========================================================================================================================
colnames(truth)

traits_columns = c("detect.width",
                   "truth.width", 
                   "detect.length",
                   "truth.length",
                   "detect.area",
                   "truth.area")

truth_long = truth %>% pivot_longer(cols =  c("detect.width",
                                              "truth.width", 
                                              "detect.length",
                                              "truth.length",
                                              "detect.area",
                                              "truth.area"), names_to = "trait", values_to = "value")

custom_theme <- function() {
  theme_minimal(base_size = 14) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "#581845", fill = NA, size = 0.6),
      plot.title = element_text(hjust= 0.5, colour = "#581845", face = "bold.italic", size = 25),
      axis.text = element_text(color = "#581845", size = 30, face = "bold"),
      axis.title = element_text(color = "#581845", size = 18, face = "bold"),
      #axis.title.y = element_text(size = 17.5, margin = margin(t = 0, r = 30, b = 0, l = 0), color = "#581845"),  # Change the color here
      #axis.title.x = element_text(size = 17.5, margin = margin(t = 0, r = 30, b = 0, l = 0), color = "#581845"),  # Change the color here
      axis.text.x =  element_text(size = 16, color = "#581845"),
      axis.text.y = element_text(size = 16, color = "#581845"), 
      legend.text = element_text(size = 18), 
      strip.text = element_text(size = 20))
}

truth$leaf_side <- ifelse(grepl("_[1-4]$", truth$pic_name), "abaxial", "adaxial")

# Create the ggplot with specified limits
p1 = ggplot(truth, aes(x=detect.length, y=truth.length, color = leaf_side))+
  geom_point(shape=1, size = 4, alpha = 1)+# color = "#036100")+
  geom_abline(linetype ='dashed', color ="#581845", size = 0.8)+
  #coord_fixed(ratio = 1)+
  xlim(0, 100) +  # Set x-axis limits
  ylim(0, 100) +  # Set y-axis limits
  ggtitle("Stomata length (µm)") +
  stat_poly_eq(use_label(c("eq", "R2")), size= 6) +
  custom_theme()+
  guides(color = FALSE)

p1

p2 = ggplot(truth, aes(x=detect.width, y=truth.width, color = leaf_side))+
  geom_point(shape=1, size = 4, alpha = 1)+# color = "#036100")+
  geom_abline(linetype ='dashed', color ="#581845", size = 0.8)+
  #coord_fixed(ratio = 1)+
  xlim(0, 100) +  # Set x-axis limits
  ylim(0, 100) +  # Set y-axis limits
  ggtitle("Stomata width (µm)") +
  stat_poly_eq(use_label(c("eq", "R2")), size= 6) +
  custom_theme()+
  guides(color = FALSE)

p2

p3  = ggplot(truth, aes(x=detect.area, y=truth.area, color = leaf_side))+
  geom_point(shape=1, size = 4, alpha = 1)+# color = "#036100")+
  geom_abline(linetype ='dashed', color ="#581845", size = 0.8)+
  #coord_fixed(ratio = 1)+
  xlim(0, 4000) +  # Set x-axis limits
  ylim(0, 4000) +  # Set y-axis limits
  ggtitle("Stomata area (µm^2)") +
  stat_poly_eq(use_label(c("eq", "R2")), size= 6) +
  custom_theme()

p3

  combined_plot <- plot_grid(
    p1, p2, p3, ncol = 3) 
  
  combined_plot
# ggsave(combined_plot, file= "output/traits_check_V2.pdf", height=8, width = 25, dpi = 600)

  #  =================================================================================================================
  truth_leaf = truth

  truth_leaf$leaf <- gsub("_\\d+$", "", truth_leaf$pic_name)

  truth_leaf = truth_leaf %>%
    group_by(leaf) %>%
    summarise(
      detect_width = mean(detect.width, na.rm = TRUE),
      truth_width = mean(truth.width, na.rm = TRUE),
      detect_length = mean(detect.length, na.rm = TRUE),
      truth_length = mean(truth.length, na.rm = TRUE),
      detect_area = mean(detect.area, na.rm = TRUE),
      truth_area = mean(truth.area, na.rm = TRUE)
    )

  # ===================================================================================================================
  
  # Create the ggplot with specified limits
  p4 = ggplot(truth_leaf, aes(x=detect_length, y=truth_length))+#, color = leaf_side))+
    geom_point(shape=1, size = 7, alpha = 1, color = "#581845")+
    geom_abline(linetype ='dashed', color ="#581845", size = 0.8)+
    #coord_fixed(ratio = 1)+
    xlim(0, 100) +  # Set x-axis limits
    ylim(0, 100) +  # Set y-axis limits
    ggtitle("Stomata length (µm) - leaf") +
    stat_poly_eq(use_label(c("eq", "R2")), size= 6) +
    custom_theme()+
    guides(color = FALSE)
  
  p4
  
  p5 = ggplot(truth_leaf, aes(x=detect_width, y=truth_width))+#, color = leaf_side))+
    geom_point(shape=1, size = 7, alpha = 1, color = "#581845")+
    geom_abline(linetype ='dashed', color ="#581845", size = 0.8)+
    #coord_fixed(ratio = 1)+
    xlim(0, 100) +  # Set x-axis limits
    ylim(0, 100) +  # Set y-axis limits
    ggtitle("Stomata width (µm) - leaf") +
    stat_poly_eq(use_label(c("eq", "R2")), size= 6) +
    custom_theme()+
    guides(color = FALSE)
  
  p5
  
  p6  = ggplot(truth_leaf, aes(x=detect_area, y=truth_area))+#, color = leaf_side))+
    geom_point(shape=1, size = 7, alpha = 1, color = "#581845")+
    geom_abline(linetype ='dashed', color ="#581845", size = 0.8)+
    #coord_fixed(ratio = 1)+
    xlim(0, 4000) +  # Set x-axis limits
    ylim(0, 4000) +  # Set y-axis limits
    ggtitle("Stomata area (µm^2) - leaf") +
    stat_poly_eq(use_label(c("eq", "R2")), size= 6) +
    custom_theme()
  
  p6
  
  leaf_plot <- plot_grid(p1 ,p2 ,p3,
    p4, p5, p6, ncol = 3) 
  
  leaf_plot
  
  ggsave(leaf_plot, file= "output/plot_all.jpg", height=16, width = 25, dpi = 600)
 # =================================================================================================================================
  
  check = truth
  check$detect_truth = paste(check$detect.class, check$truth.class, sep = "_")
  frequency = as.data.frame(table(check$detect_truth))
  colnames(frequency) = c("detect.class _ truth.class","Freq")
  