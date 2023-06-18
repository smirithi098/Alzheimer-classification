setwd("S:/MA335_Modelling and observational data in R/Final project/Alzheimer-classification")

library(dplyr)
library(ggplot2)
library(superml)
library(tidyr)
library(plotly)
library(corrplot)
library(RColorBrewer)
library(fclust)
library(tidymodels)
library(glmnet)

# read the csv file as data frame
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

corrplot(
  cor(clean_data[, -c(1,2,7)]),
  type="upper",
  order = "hclust",
  col = brewer.pal(n=6, "PuOr"),
  bg = "mintcream",
  tl.col = "black",
  diag = FALSE,
  addCoef.col = "black"
)

# scatter plot between Age and nWBV for each gender group

ggplotly(clean_data %>%
           ggplot(aes(x=Age, y=nWBV, color=Gender)) +
           geom_point() +
           geom_smooth(method = lm, se = FALSE) +
           scale_color_manual(values = c("deepskyblue3", "hotpink3"), 
                              labels = c("Male", "Female")) +
           scale_x_continuous(breaks = unique(clean_data$Age)) +
           theme(
             panel.background = element_blank(),
             axis.line = element_line(colour = "black"),
             legend.position = "none"
           ) +
           labs(title = "Correlation - Age vs whole brain volume",
                x = "Age", y = "Whole Brain Volume")
)

#----------------------------CLUSTERING ALGORITHM-------------------------------

# maximum clusters we want to form
max_number_of_clusters <- 5

# vector to store the values of index returned by the method for each k value
index_values <- numeric(max_number_of_clusters - 1)

# function to calculate the index for each k value and choose the optimal k
get_optimal_cluster <- function(data, max_clust) {
  
  # iterate over k values from 2 to max value
  for (k in 2:max_clust) {
    print(k)
    # perform fuzzy clustering
    cluster_result <- Fclust(data.matrix(data), k)
    # get the index value for the used k value
    index_values[k-1] <- Fclust.index(cluster_result)
  }
  
  # create a dataframe containing clusters and index values
  index_data <- data.frame(clusters = 2:max_clust, indices = index_values)
  
  # select the k value with maximum index
  optimal_index <- which.max(index_values) + 1
  
  # return the data frame and chosen optimal k
  return(list(optimal_value = optimal_index, index_df = index_data))
  
}


# get the output returned by function
result <- get_optimal_cluster(clean_data, max_number_of_clusters)

# optimal k value returned by function
optimal_k <- result$optimal_value

# data frame returned by function with k and index values
cluster_index_df <- result$index_df

# plot the clusters and corresponding indices
ggplotly(ggplot(cluster_index_df, aes(x = clusters, y = indices)) +
  geom_line(linewidth = 0.5) +
  geom_point(color = "red") + 
  labs(title = "Index values for the correspoding K value",
       x = "Number of clusters (k)",
       y = "Index values") +
  theme_classic()
)

cat("Optimal value of k: ", optimal_k)

# Perform fuzzy k-means clustering

clustering_FKM <-  function(data, k_clusters){
  fkm_output <- FKM(data.matrix(data),
                    k =k_clusters,
                    stand = 1
                    )
  
  return(fkm_output)
}

fuzzy_k_means_result <- clustering_FKM(clean_data, optimal_k)

# print the summary of the clustering algorithm
summary.fclust(fuzzy_k_means_result)

# plot the clusters
plot(fuzzy_k_means_result,
     colclus = c("dodgerblue3", "palevioletred3"),
     pca = TRUE)

#----------------------------LOGISTIC REGRESSION--------------------------------

clean_data$Group <- as.factor(clean_data$Group)

#_________________________SPLIT INTO TRAIN/TEST DATA____________________________
# set random state for splitting data
set.seed(345)

# split data into training and test set
data_split <- initial_split(data = clean_data,
                            prop = 0.8,
                            strata = Group)

# create the train and tests data set
train_data <- data_split %>% training()

test_data <- data_split %>% testing()

#____________________________HYPER PARAMTER TUNING______________________________

# hyper-parameter tuning for `mixture` and `penalty`

# define the logistic regression model
logis_model <- logistic_reg(mixture = tune(), 
                            penalty = tune(),
                            engine = "glmnet")

# define a grid search for hyper-parameters

grid_Search <- grid_regular(mixture(),
                            penalty(),
                            levels = c(mixture = 4, penalty = 4))

# define the workflow for parameter tuning and building model

log_reg_workflow <- workflow() %>%
  add_model(logis_model) %>% # add the above created model
  add_formula(Group ~ .) # set the formula: response variable ~ predictors

# define cross validation method for grid search

cv_folds <- vfold_cv(train_data,
                     v = 7)

# tune the parameters using the above defined methods

tune_logis_reg <- tune_grid(
  object = log_reg_workflow,
  resamples = cv_folds,
  grid = grid_Search,
  control = control_grid(save_pred = TRUE)
)

# choose the best values for the logistic regression model parameters
chosen_params <- select_best(tune_logis_reg, metric = "accuracy")

#_____________________________LOGISTIC REGRESSION MODEL_________________________

# fit the model with the chosen parameter values
final_model <- logistic_reg(
  mixture = chosen_params$mixture,
  penalty = chosen_params$mixture) %>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  fit(Group ~ ., data = train_data)


# Predict the results on test data
pred_results <- predict(final_model,
                        new_data = test_data,
                        type = "class")

# combine the actual and predicted class variables in one data frame
predicted_class <- test_data %>%
  select(Group) %>%
  bind_cols(pred_results)

# print the confusion matrix
conf_mat(predicted_class,
         truth = Group,
         estimate = .pred_class)

# get the significance of all the predictors used in descending order
predictors_significance <- tidy(final_model) %>%
  arrange(desc(abs(estimate)))

# visualize the predictors significance in classification results
predictors_significance %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_col(fill = "rosybrown1", color = "lightcoral") +
  geom_text(aes(label = round(estimate, 3)), 
            parse = TRUE, 
            colour = "black",
            size = 2.75,
            vjust = 0, hjust = 1) +
  labs(title = "Siginificance of predictor variables",
       x = "Predictor variables",
       y = "Siginificance level") +
  theme_classic() +
  coord_flip()
