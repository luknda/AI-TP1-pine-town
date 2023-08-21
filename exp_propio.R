library(ggplot2)  # For creating plots
library(dplyr)    # For data manipulation
library(rpart)



# Constants and global variables
PARALLELIZE <- FALSE # Set the option for parallelization of computations,sacar en el caso de debuggear
N_THREADS <- 30     # Define the number of threads for parallel processing
N_BINS <- 10        # Define the number of bins for discretization
RERUN_EXP <- TRUE   # Set the option to rerun the experiment

# Load provided functions
source("./provided_functions.R")
#' Run an experiment to evaluate the performance of a predictive model under different conditions.
#'
#' @param datasets_to_pred A list of data frames, each containing a dataset to be predicted.
#' @param filepath The path to the file where the experiment results will be saved.
#' @return None (the experiment results are saved to a file).
#'
#' @details
#' This function iterates through the given datasets, imputation methods, and proportions
#' of missing data. For each combination, it configures the preprocessing options, performs
#' the experiment, and stores the results in a list. The list of results is then combined into
#' a single data frame, which is saved to the specified file.
#'
#'
#'
#'



#run_experiment <- function(datasets_to_pred, filepath) {
  
  exp_results <- list()  # Store experiment results
  i <- 1  # Initialize counter for experiment results
  
  for (dtp in datasets_to_pred) { # recorre todos los datasets
    
    # 1. Train the tree and get the variable importance
    tree <- rpart(as.formula(paste(dtp$var_to_predict, " ~ .")),
                  data=dtp$data_df)
    var_importance <- tree$variable.importance
    important_vars <- names(sort(var_importance, decreasing=TRUE))
    
    # We'll add noise to the top 3 most important variables (this number can be adjusted)
    for (j in 1:2) {  # Consider the top 1 to 5 important variables
      top_vars <- important_vars[1:j]  # Select the top j variables
      considered_vars_string <- paste(top_vars, collapse=", ")
      for (noise_level in seq(0, 0.5, 0.025)) {
        for (var in top_vars) {
          noisy_data <- dtp$data_df
          noisy_data[, var] <- noisy_data[, var] + rnorm(nrow(noisy_data), mean=0, sd=noise_level)
          dtp$data_df <- noisy_data
          print(paste("Adding noise to", var, "with level", noise_level))
        
        # Your preprocessing setup remains unchanged
          preprocess_control <- list(
          impute_NAs=FALSE,
          treat_NAs_as_new_levels=FALSE,
          do_ohe=FALSE,
          discretize=FALSE,
          n_bins=N_BINS,
          ord_to_numeric=FALSE,
          prop_switch_y=0,
          prop_NAs = 0)
        
        # Perform the experiment for the current settings
        if (PARALLELIZE == TRUE) {
          res_tmp <- est_auc_across_depths(dtp, preprocess_control,
                                           max_maxdepth=30, prop_val=0.25,
                                           val_reps=30)
        } else {
          res_tmp <- est_auc_across_depths_no_par(dtp, preprocess_control,
                                                  max_maxdepth=30, prop_val=0.25,
                                                  val_reps=30)
        }
        
        res_tmp$cant_var <- j
        res_tmp$considered_vars <- considered_vars_string
        res_tmp$noise_level <- noise_level
        exp_results[[i]] <- res_tmp
        
        rm(res_tmp)  # Clean up temporary result
        i <- i + 1  # Increment result counter
      }
    }
  }}
  
  # Combine experiment results into a single data frame
  exp_results <- do.call(rbind, exp_results)
  
  # Save experiment results to a file
  write.table(exp_results, filepath, row.names=FALSE, sep="\t")
}

run_experiment <- function(datasets_to_pred, filepath) {
  
  add_noise_to_categorical <- function(data, var, noise_level) {
    total_changes <- ceiling(nrow(data) * noise_level)
    index_changes <- sample(1:nrow(data), total_changes)
    unique_levels <- unique(data[, var])
    data[index_changes, var] <- sample(unique_levels, total_changes, replace=TRUE)
    return(data)
  }
  
  exp_results <- list()  # Store experiment results
  i <- 1  # Initialize counter for experiment results
  
  for (dtp in datasets_to_pred) {
    tree <- rpart(as.formula(paste(dtp$var_to_predict, " ~ .")), data=dtp$data_df)
    var_importance <- tree$variable.importance
    important_vars <- names(sort(var_importance, decreasing=TRUE))
    
    for (j in 1:5) {
      top_vars <- important_vars[1:j]
      considered_vars_string <- paste(top_vars, collapse=", ")
      
      for (noise_level in seq(0, 0.5, 0.025)) {
        for (var in top_vars) {
          noisy_data <- dtp$data_df
          if (is.numeric(noisy_data[, var])) {
            noisy_data[, var] <- noisy_data[, var] + rnorm(nrow(noisy_data), mean=0, sd=noise_level)
          } else {
            noisy_data <- add_noise_to_categorical(noisy_data, var, noise_level)
          }
          dtp$data_df <- noisy_data
          print(paste("Adding noise to", var, "with level", noise_level))
          
          preprocess_control <- list(
            impute_NAs=FALSE,
            treat_NAs_as_new_levels=FALSE,
            do_ohe=FALSE,
            discretize=FALSE,
            n_bins=N_BINS,
            ord_to_numeric=FALSE,
            prop_switch_y=0,
            prop_NAs = 0
          )
          
          if (PARALLELIZE == TRUE) {
            res_tmp <- est_auc_across_depths(dtp, preprocess_control, max_maxdepth=30, prop_val=0.25, val_reps=30)
          } else {
            res_tmp <- est_auc_across_depths_no_par(dtp, preprocess_control, max_maxdepth=30, prop_val=0.25, val_reps=30)
          }
          
          res_tmp$cant_var <- j
          res_tmp$considered_vars <- considered_vars_string
          res_tmp$noise_level <- noise_level
          exp_results[[i]] <- res_tmp
          
          rm(res_tmp)
          i <- i + 1
        }
      }
    }
  }
  
  exp_results <- do.call(rbind, exp_results)
  write.table(exp_results, filepath, row.names=FALSE, sep="\t")
}


#' Plot the results of the sample experiment using ggplot2.
#'
#' @param filename_exp_results The filename of the experiment results file.
#' @param filename_plot The filename to save the plot (e.g., "my_plot.png").
#' @param width The width of the plot in inches.
#' @param height The height of the plot in inches.
#' @return None (the plot is saved as an image file).
#'
#' @details
#' This function reads the experiment results, calculates the mean AUC values for different
#' experimental conditions, and generates a line plot using ggplot2. The plot displays the mean AUC
#' values against maximum tree depths, with different lines for different imputation methods and facets
#' for different datasets and proportions of missing data. The resulting plot is saved as the specified file.



#plot_exp_results <- function(filename_exp_results, filename_plot, width, height) {
  # Load experiment results
  exp_results <- read.table(filename_exp_results, header=TRUE, sep="\t")
  
  # Calculate mean AUC values for different groups of experimental results
  #data_for_plot <- exp_results %>%
  #  group_by(dataset_name, maxdepth, prop_switch_y) %>%
  #  summarize(mean_auc=mean(auc), .groups='drop')
  data_for_plot <- exp_results %>%
    group_by(dataset_name,noise_level) %>%
    summarize(max_auc = max(auc), .groups = 'drop')
  
  #Comentarios ayuda de este:
  # Esta linea de codigo basicamente te agrupa por porp_switch_Y no le importa max_depth ya que queres el max auc score por 
  # nivel de prop_switch_Y  sin importar la depth del arbol y en el max_auc se te guarda el maximo para cada nivel de 
  # prop_switch_y
  #Si estás utilizando summarize después de group_by, 
  #entonces summarize va a calcular una métrica (como mean(), max(), etc.) para cada grupo único definido por group_by.
  
  
  # Create a ggplot object for the line plot
  g <- ggplot(data_for_plot, aes(x= noise_level, y=max_auc)) +
    geom_line() +
    theme_bw() +
    xlab("prop_switch_y") +
    ylab("AUC (estimated through repeated validation)") +
    facet_grid(dataset_name ~ cant_var , scales="free_y") +
    facet_grid(cols = vars(dataset_name)) + # separa en base al nombre del dataset
    theme(legend.position="bottom",
          panel.grid.major=element_blank(),
          strip.background=element_blank(),
          panel.border=element_rect(colour="black", fill=NA))
  
  # Save the plot to a file
  ggsave(filename_plot, g, width=width, height=height)
}


plot_exp_results <- function(filename_exp_results, filename_plot, width, height) {
  # Load experiment results
  exp_results <- read.table(filename_exp_results, header=TRUE, sep="\t")
  
  # Calculate mean AUC values for different groups of experimental results
  data_for_plot <- exp_results %>%
    group_by(dataset_name, noise_level, cant_var) %>%
    summarize(max_auc = max(auc), .groups = 'drop')
  
  # Create a ggplot object for the line plot
  g <- ggplot(data_for_plot, aes(x= noise_level, y=max_auc)) +
    geom_line() +
    theme_bw() +
    xlab("Noise Level") +
    ylab("Max AUC") +
    facet_grid(dataset_name ~ cant_var, scales="free_y") +
    theme(legend.position="bottom",
          panel.grid.major=element_blank(),
          strip.background=element_blank(),
          panel.border=element_rect(colour="black", fill=NA))
  
  # Save the plot to a file
  ggsave(filename_plot, g, width=width, height=height)
}


# Load the datasets
datasets_to_pred <- list(
  #load_df("./data/heart.csv", "Heart", "HeartDisease"),
  #load_df("./data/customer_churn.csv", "Churn", "churn") # Source: https://archive.ics.uci.edu/dataset/563/iranian+churn+dataset
  load_df("./data/diabetes-vid.csv", "diabetes-vid", "Outcome")
  #load_df("./data/heart.csv", "Heart", "HeartDisease"),   # Source: https://www.kaggle.com/datasets/arnabchaki/data-science-salaries-2023
  #load_df("./data/loan_approval_dataset.csv", "Loan", "loan_aproval")
)





set.seed(873770356)
# Run the experiment
if (RERUN_EXP ==  TRUE) {
  run_experiment(datasets_to_pred, "./outputs/tables/exp_propio.txt")
}

# Plot the experiment results
plot_exp_results( "./outputs/tables/exp_propio.txt", "./outputs/plots/plot_exp_propio.jpg", width=5, height=4)
