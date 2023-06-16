setwd("S:/MA335_Modelling and observational data in R/Final project")

library(dplyr)
library(ggplot2)
library(superml)
library(tidyr)
library(plotly)
library(ggExtra)

# read the csv file as dataframe
raw_data <- read.csv("project data.csv", header = TRUE)

#---------------------------DATA CLEANING---------------------------------------

# find the row numbers with Group = "Converted"
converted_group_rows <- which(raw_data$Group=="Converted")

# remove the rows with Group value "Converted"
raw_data <- raw_data[-converted_group_rows, ]

# Convert text data to numerical values 

# 1. Group (Demented - 1, Non-Demented - 0)

# create encoder object
group_encoder = LabelEncoder$new()

# fit and transform the Group column using the created object
raw_data$Group <- group_encoder$fit_transform(raw_data$Group)

# 2. M.F (M - 0, F - 1)

raw_data$M.F <- factor(raw_data$M.F,
                       levels = c("M", "F"),
                       labels = c(0, 1))

# rename column name from "M.F" to "Gender"
raw_data <- rename(raw_data,
                   Gender = M.F
                   )

# replace missing values

# 1. Replace NA values with MODE of column - SES

# function to calculate the mode of a column
calculate_mode <- function(col_to_calculate) {
  unique_values <- unique(col_to_calculate)
  mode_of_col <- unique_values[which.max(tabulate(match(col_to_calculate, unique_values)))]
  return(mode_of_col)
}

# get the mode of SES
mode_SES <- calculate_mode(raw_data$SES)

# get the median of MMSE
median_MMSE <- median(raw_data$MMSE, na.rm = TRUE)

# replace the NA values with mode of SES (categorical column) and median of MMSE (continuous column)
raw_data <- raw_data %>% replace_na(list(SES = mode_SES, MMSE = median_MMSE))

# Standardize the volume columns to have same scale

calculate_mean_sd <-  function(column_name) {
  column_name <- as.numeric(column_name)
  mean_sd_list <- list("mean_col" = mean(column_name), "sd_col" = sd(column_name))
  return(mean_sd_list)
}

eTIV_mean_sd <- calculate_mean_sd(raw_data$eTIV)
nWBV_mean_sd <- calculate_mean_sd(raw_data$nWBV)
asf_mean_sd <- calculate_mean_sd(raw_data$ASF)

raw_data$eTIV <- (raw_data$eTIV - eTIV_mean_sd$mean_col)/ eTIV_mean_sd$sd_col
raw_data$nWBV <- (raw_data$nWBV - nWBV_mean_sd$mean_col)/ nWBV_mean_sd$sd_col
raw_data$ASF <- (raw_data$ASF - asf_mean_sd$mean_col)/ asf_mean_sd$sd_col

raw_data$CDR <- as.factor(raw_data$CDR)

clean_data <- raw_data

#------------------------DESCRIPTIVE ANALYSIS-----------------------------------

# get the summary of the data fields
summary(clean_data)

# Number of patients with different CDR for each gender

ggplotly(clean_data %>%
  select(c("Gender", "CDR")) %>%
  group_by(Gender, CDR) %>%
  summarise(count_n = n()) %>%
  ggplot(aes(x = Gender, y = count_n, fill = CDR)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_manual(values = colorspace::diverge_hcl(4)) +
  scale_x_discrete(labels = c("Male", "Female")) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  labs(title = "Count of patients with CDR rating for each gender",
       x = "Clinical Dementia Rating", y = "Count of patients"))


# distribution of total intracranial volume for each gender

ggplotly(clean_data %>%
    group_by(Gender) %>%
    ggplot(aes(x=Gender, y=eTIV, fill=Gender)) +
    geom_boxplot() +
    scale_x_discrete(labels = c("Male", "Female")) +
    coord_flip() +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    ) +
    labs(title = "Distribution of total Intracranial volume for each gender",
         x = "Gender", y = "Estimated Total Intracranial Volume")
  )

# distribution of eTIV for all age groups

ggplotly(clean_data %>%
  group_by(Age) %>%
  summarise(avg_eTIV = mean(eTIV)) %>%
  ggplot(aes(x=Age, y=avg_eTIV, group=1)) +
  geom_line() + geom_point(color="blue") +
  scale_x_continuous(breaks = unique(clean_data$Age)) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  labs(title = "Average total intracranial volume for all age groups",
       x = "Age", y = "Average Total Intracranial Volume")
  )

# correlation matrix

corr_plot <- clean_data %>%
  ggplot(aes(x=Age, y=nWBV, color=Gender)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  scale_color_manual(values = c("deepskyblue3", "hotpink3"), 
                     labels = c("Male", "Female")) +
  theme(
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "none"
  ) +
  labs(title = "Correlation - Age vs whole brain volume",
       x = "Age", y = "Whole Brain Volume")

ggMarginal(corr_plot, type = "histogram", 
           fill = "slategrey")
