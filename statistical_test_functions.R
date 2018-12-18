
conversion_test <- function(data,experiment_name,variant_id,country,device){
  control_data <- data[data$variant_id == 0,]
  v_data <- data[data$variant_id == variant_id,]
  
  control_n <- sum(control_data$num_users)
  v_n <- sum(v_data$num_users)
  
  control_c <- sum(control_data$num_customers)
  v_c <- sum(v_data$num_customers)
  
  control_proportion <- control_c / control_n
  v_proportion <- v_c / v_n
  
  delta_cr <- (v_proportion - control_proportion) / control_proportion
  #delta_cr <- percent(delta_cr)
  
  v_sample_proportion <- (control_c + v_c) / (control_n + v_n)
  
  v_z_value <- ((control_proportion - v_proportion) - 0) / sqrt( (v_sample_proportion*(1-v_sample_proportion)) * ((1/control_n)+(1/v_n)  ))
  v_p_value_cr <- 2*pnorm(-abs(v_z_value))
  v_p_value_cr <- format(v_p_value_cr, scientific=FALSE)
  
  v_p_value_cr <- as.numeric(v_p_value_cr)
  v_conf_cr <- (1-as.numeric(v_p_value_cr))
  #v_p_value_cr <-  percent(v_p_value_cr)
  
  
  
  results <- list()
  
  results$experiment_name <- experiment_name
  results$variant_id <- variant_id
  results$country <- country
  results$device <- device
  #results$days_run <- days_run
  results$v_conf_cr <- v_conf_cr
  results$delta_cr <- delta_cr
  results$control_n <- control_n
  results$v_n <- v_n
  results$control_proportion <- control_proportion
  results$v_proportion <- v_proportion
  
  return(results)
  
  
  #print(v_p_value_cr)
  #print(delta_cr)
}

#conversion_test(data)
#################################################
margin_test <- function(data,experiment_name,variant_id,country,device){
  
  #split into new dataset
  control_data <- data[data$variant_id == 0,]
  v_data <- data[data$variant_id == variant_id,]
  
  #calculate count users (n)
  control_n <- sum(control_data$num_users)
  v_n <- sum(v_data$num_users)
  
  #calculate total metric
  control_sum_metric <- sum(control_data$sum_margin_product_gbp)
  v_sum_metric <- sum(v_data$sum_margin_product_gbp)
  
  #calculate sum of squared metric
  control_sum_metric_sq <- sum(control_data$sum_margin_product_gbp_sq)
  v_sum_metric_sq <- sum(v_data$sum_margin_product_gbp_sq)
  
  #calculate mean
  control_x_bar_margin <- control_sum_metric / control_n
  v_x_bar_margin <- v_sum_metric / v_n
  
  delta_margin <- (v_x_bar_margin - control_x_bar_margin) / control_x_bar_margin
  #delta_margin <- percent(delta_margin)
  
  #calculate variance
  control_var <- (control_sum_metric_sq - ((control_sum_metric)^2) / control_n) /  (control_n - 1)
  v_var <- (v_sum_metric_sq - ((v_sum_metric)^2) / v_n) /  (v_n - 1)
  
  #calculate standard deviation
  control_sd <- sqrt(control_var)
  v_sd <- sqrt(v_var)
  
  #calculate pooled values
  v_pooled_sum_metric <- control_sum_metric + v_sum_metric
  v_pooled_sum_metric_sq <- control_sum_metric_sq + v_sum_metric_sq
  v_pooled_n <-  control_n + v_n
  v_pooled_var <- (v_pooled_sum_metric_sq - ((v_pooled_sum_metric)^2) / v_pooled_n) /  (v_pooled_n - 1)
  v_pooled_sd <- sqrt(v_pooled_var)
  
  
  #f-test for equality for variances
  v_f_statistic <- ifelse(control_var > v_var, control_var / v_var,  v_var / control_var)
  v_deg_freedom_numerator <- ifelse(control_var > v_var, control_n - 1,  v_n -1)
  v_deg_freedom_denominator <- ifelse(control_var > v_var, v_n - 1, control_var -1)
  v_f_p_value <- pf(q=v_f_statistic, df1=v_deg_freedom_numerator, df2=v_deg_freedom_denominator, lower.tail=FALSE)
  
  #calculate standard error - if f test is not significant (variances are equal) we use the pooled variances.
  v_standard_error <- ifelse(v_f_p_value > 0.05, sqrt(v_pooled_var) * sqrt((1/control_n)+(1/v_n)) ,sqrt((control_var / control_n) + (v_var / v_n)))
  
  #calculate degrees of freedom
  v_deg_freedom <- (control_n + v_n) - 2
  
  #run the t-test
  v_t_statistic <- (control_x_bar_margin - v_x_bar_margin) / v_standard_error
  
  
  #show p-value
  v_p_value_margin <- format(2*pt(-abs(v_t_statistic), v_deg_freedom),scientific = FALSE)
  
  v_p_value_margin <- as.numeric(v_p_value_margin)
  v_conf_margin <- (1-as.numeric(v_p_value_margin))
  #v_p_value_margin <-  percent(v_p_value_margin)
  
  results <- list()
  
  results$control_x_bar_margin <- control_x_bar_margin
  results$v_x_bar_margin <- v_x_bar_margin
  results$v_conf_margin <- v_conf_margin
  results$delta_margin <- delta_margin
  
  return(results)
  
}

#margin_test(data)


#################################################
sales_test <- function(data,experiment_name,variant_id,country,device){
  
  #split into new dataset
  control_data <- data[data$variant_id == 0,]
  v_data <- data[data$variant_id == variant_id,]
  
  #calculate count users (n)
  control_n <- sum(control_data$num_users)
  v_n <- sum(v_data$num_users)
  
  #calculate total metric
  control_sum_metric <- sum(control_data$sum_sales_gbp_isev)
  v_sum_metric <- sum(v_data$sum_sales_gbp_isev)
  
  #calculate sum of squared metric
  control_sum_metric_sq <- sum(control_data$sum_sales_gbp_isev_sq)
  v_sum_metric_sq <- sum(v_data$sum_sales_gbp_isev_sq)
  
  #calculate mean
  control_x_bar_sales <- control_sum_metric / control_n
  v_x_bar_sales <- v_sum_metric / v_n
  
  delta_sales <- (v_x_bar_sales - control_x_bar_sales) / control_x_bar_sales
  #delta_sales <- percent(delta_sales)
  
  #calculate variance
  control_var <- (control_sum_metric_sq - ((control_sum_metric)^2) / control_n) /  (control_n - 1)
  v_var <- (v_sum_metric_sq - ((v_sum_metric)^2) / v_n) /  (v_n - 1)
  
  #calculate standard deviation
  control_sd <- sqrt(control_var)
  v_sd <- sqrt(v_var)
  
  #calculate pooled values
  v_pooled_sum_metric <- control_sum_metric + v_sum_metric
  v_pooled_sum_metric_sq <- control_sum_metric_sq + v_sum_metric_sq
  v_pooled_n <-  control_n + v_n
  v_pooled_var <- (v_pooled_sum_metric_sq - ((v_pooled_sum_metric)^2) / v_pooled_n) /  (v_pooled_n - 1)
  v_pooled_sd <- sqrt(v_pooled_var)
  
  
  #f-test for equality for variances
  v_f_statistic <- ifelse(control_var > v_var, control_var / v_var,  v_var / control_var)
  v_deg_freedom_numerator <- ifelse(control_var > v_var, control_n - 1,  v_n -1)
  v_deg_freedom_denominator <- ifelse(control_var > v_var, v_n - 1, control_var -1)
  v_f_p_value <- pf(q=v_f_statistic, df1=v_deg_freedom_numerator, df2=v_deg_freedom_denominator, lower.tail=FALSE)
  
  #calculate standard error - if f test is not significant (variances are equal) we use the pooled variances.
  v_standard_error <- ifelse(v_f_p_value > 0.05, sqrt(v_pooled_var) * sqrt((1/control_n)+(1/v_n)) ,sqrt((control_var / control_n) + (v_var / v_n)))
  
  #calculate degrees of freedom
  v_deg_freedom <- (control_n + v_n) - 2
  
  #run the t-test
  v_t_statistic <- (control_x_bar_sales - v_x_bar_sales) / v_standard_error
  
  
  #show p-value
  v_p_value_sales <- format(2*pt(-abs(v_t_statistic), v_deg_freedom),scientific = FALSE)
  
  v_p_value_sales <- as.numeric(v_p_value_sales)
  v_conf_sales <- (1-as.numeric(v_p_value_sales))
  #v_p_value_sales <-  percent(v_p_value_sales)
  
  results <- list()
  
  results$control_x_bar_sales <- control_x_bar_sales
  results$v_x_bar_sales <- v_x_bar_sales
  results$v_conf_sales <- v_conf_sales
  results$delta_sales <- delta_sales
  
  return(results)
  
  #print(v_p_value_sales)
  #print(delta_sales)
  
}

#sales_test(data)


conversion_test_daily <- function(data,experiment_name,variant_id,country,device){
  
  control_data <- data[data$variant_id == 0,]
  v_data <- data[data$variant_id == variant_id,]
  
  days_run <- v_data$days_run
  allocation_date <- as.Date(v_data$allocation_date) 
  
  control_n <- sum(control_data$num_users)
  v_n <- sum(v_data$num_users)
  
  control_c <- sum(control_data$num_customers)
  v_c <- sum(v_data$num_customers)
  
  control_proportion <- control_c / control_n
  v_proportion <- v_c / v_n
  
  delta_cr <- (v_proportion - control_proportion) / control_proportion
  delta_cr <- percent(delta_cr)
  
  v_sample_proportion <- (control_c + v_c) / (control_n + v_n)
  
  v_z_value <- ((control_proportion - v_proportion) - 0) / sqrt( (v_sample_proportion*(1-v_sample_proportion)) * ((1/control_n)+(1/v_n)  ))
  v_p_value_cr <- 2*pnorm(-abs(v_z_value))
  v_p_value_cr <- format(v_p_value_cr, scientific=FALSE)
  
  v_p_value_cr <- as.numeric(v_p_value_cr)
  v_conf_cr <- (1-as.numeric(v_p_value_cr))
  #v_p_value_cr <-  percent(v_p_value_cr)
  
  
  results <- list()
  
  results$experiment_name <- experiment_name
  results$variant_id <- variant_id
  results$country <- country
  results$device <- device
  results$allocation_date <- allocation_date
  results$days_run <- days_run
  results$v_conf_cr <- v_conf_cr
  results$delta_cr <- delta_cr
  results$control_n <- control_n
  results$v_n <- v_n
  results$control_proportion <- control_proportion
  results$v_proportion <- v_proportion
  
  return(results)
  
  
  #print(v_p_value_cr)
  #print(delta_cr)
}



