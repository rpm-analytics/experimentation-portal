############################################################################################################################
library(formattable)
library(scales)
library(ggplot2)
library(plyr)
library(dplyr)
library(zoo)
library(DBI)
library(RPostgreSQL)
library(googlesheets)
library(sqldf)
source("/home/remy/pbx_ab_testing_portal/results_pipeline/statistical_test_functions.R")

#setwd('G:/My Drive/Projects/Product_Analytics_Dashboarding/Apps/AB_Testing_Portal/Analysis/AB Results Restating/Temp_Pipeline/')
setwd("/home/remy/pbx_ab_testing_portal/results_pipeline/")


drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host="cloud-dwh-analysis.cede9zhbujyu.eu-west-1.redshift.amazonaws.com", 
                 port="5439",
                 dbname="phxred", 
                 user="xxx", 
                 password="xxx"
                 
                 #,
                 #ssl="true",
                 #sslfactory="com.amazon.redshift.ssl.NonValidatingFactory"
)

#pull results input data from exp_analysis_d
data_raw <- dbGetQuery(con, "  SELECT * FROM photobox_tmp.test_exp_analysis_d;")

#pull data for experiment completion flags. some experiments have multiple / periods of exclusion, so we only want to take start and end dates from the first and last valid runs
exp_runs <- dbGetQuery(con, "SELECT a.experiment_name
 , a.start_ts
 , b.end_ts
 , date(a.start_ts) as start_date
 , CASE WHEN b.is_hard_stop = 1 THEN date(b.end_ts) + 2 ELSE date(b.end_ts) + 9  END as end_date
FROM 
(
  SELECT *
    FROM 
    (SELECT *
            , ROW_NUMBER() OVER (PARTITION BY experiment_name ORDER BY experiment_name, start_ts ASC) as start_counter
     FROM photobox_tmp.test_exp_experiment_runs_d
     WHERE include_in_analysis = 1
) a1
WHERE start_counter = 1
) a
LEFT JOIN
 ( SELECT *
    FROM 
    (SELECT *
            , ROW_NUMBER() OVER (PARTITION BY experiment_name ORDER BY experiment_name, start_ts DESC) as end_counter
     FROM photobox_tmp.test_exp_experiment_runs_d
     WHERE include_in_analysis = 1
) b1
WHERE end_counter = 1
) b
ON a.experiment_name = b.experiment_name
;")


###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
#CUMULATIVE RESULTS


#data_raw <- read.csv('results_data_input_20180802.csv')

#data_raw <- read.csv('custom_results_input_21092018.csv')
data <- data_raw
data$has_multiple_variants <- data$multiple_variant_user

data[,4] = toupper(data[,4])

data_country_device <-data
data_all_all <- data
data_all_all$country <- 'All'
data_all_all$device <- 'All'

data_all_device <- data
data_all_device$country <- 'All'
data_all_country <- data
data_all_country$device <- 'All'

data <- rbind(data_country_device,data_all_all,data_all_device,data_all_country)
data <- data[data$include_in_analysis == 1,]
data <- data[data$has_multiple_variants == 0,]
data <- data[data$country == 'GB' |
               data$country == 'FR' | 
               data$country == 'All'  ,]

data$include_in_analysis <- NULL
data$has_multiple_variants <- NULL
data$run_seq <- NULL


data_clean <- data

data <- ddply(data, .(experiment_name,variant_id,country,device), numcolwise(sum))



experiments <- unique(data$experiment_name)

experiment_name_vector <- NULL
days_run_vector <- NULL
variant_id_vector <- NULL
country_vector <- NULL
device_vector <- NULL
control_n_vector <- NULL
v_n_vector <- NULL

control_proportion_vector <- NULL
v_proportion_vector <- NULL
delta_cr_vector <- NULL
v_conf_cr_vector <- NULL

control_x_bar_margin_vector <- NULL
v_x_bar_margin_vector <- NULL
delta_margin_vector <- NULL
v_conf_margin_vector <- NULL

control_x_bar_sales_vector <- NULL
v_x_bar_sales_vector <- NULL
delta_sales_vector <- NULL
v_conf_sales_vector <- NULL


for (experiment_name in experiments) {
  data_1 <- data
  data_1 <- data_1[data_1$experiment_name == experiment_name,]
  variants <- unique(data_1$variant_id)
  variants <- variants[!is.na(variants)]
  variants <- variants[2:4]
  variants <- variants[!is.na(variants)]
  countries <- unique(data_1$country)
  devices <- unique(data_1$device)
  days_run <- data_clean[data_clean$experiment_name == experiment_name,]
  days_run <- length(unique(days_run$allocation_date))
  
  for (country in countries) {
    data_2 <- data[data$experiment_name == experiment_name,]
    data_2 <- data_2[data_2$country == country,]
    
    for (device in devices) {  
      data_3 <- data[data$experiment_name == experiment_name,]
      data_3 <- data_3[data_3$country == country,]
      data_3 <- data_3[data_3$device == device,]
      
      for (variant_id in variants){  
        data_4 <- data[data$experiment_name == experiment_name,]
        data_4 <- data_4[data_4$country == country,]
        data_4 <- data_4[data_4$device == device,]
        
        
        cr_vals <- conversion_test(data_4,experiment_name,variant_id,country,device)
        margin_vals <- margin_test(data_4,experiment_name,variant_id,country,device)
        sales_vals <- sales_test(data_4,experiment_name,variant_id,country,device)
        
        experiment_name_vector <- c(experiment_name_vector,cr_vals$experiment_name)
        variant_id_vector <- c(variant_id_vector,cr_vals$variant_id)
        days_run_vector <- c(days_run_vector,days_run)
        country_vector <- c(country_vector,cr_vals$country)
        device_vector <- c(device_vector,cr_vals$device)
        control_n_vector <- c(control_n_vector,cr_vals$control_n)
        v_n_vector <- c(v_n_vector,cr_vals$v_n)
        
        control_proportion_vector <- c(control_proportion_vector,cr_vals$control_proportion)
        v_proportion_vector <- c(v_proportion_vector,cr_vals$v_proportion)
        delta_cr_vector <- c(delta_cr_vector,cr_vals$delta_cr)
        v_conf_cr_vector <- c(v_conf_cr_vector,cr_vals$v_conf_cr)
        
        control_x_bar_margin_vector <- c(control_x_bar_margin_vector,margin_vals$control_x_bar_margin)
        v_x_bar_margin_vector <- c(v_x_bar_margin_vector,margin_vals$v_x_bar_margin)
        delta_margin_vector <- c(delta_margin_vector,margin_vals$delta_margin)
        v_conf_margin_vector <- c(v_conf_margin_vector,margin_vals$v_conf_margin)
        
        control_x_bar_sales_vector <- c(control_x_bar_sales_vector,sales_vals$control_x_bar_sales)
        v_x_bar_sales_vector <- c(v_x_bar_sales_vector,sales_vals$v_x_bar_sales)
        delta_sales_vector <- c(delta_sales_vector,sales_vals$delta_sales)
        v_conf_sales_vector <- c(v_conf_sales_vector,sales_vals$v_conf_sales)
        
      }
    }
  }
}




results_data_raw <- data.frame(experiment_name_vector,
                               variant_id_vector,
                               days_run_vector,
                               country_vector,
                               device_vector,
                               control_n_vector ,
                               v_n_vector ,
                               
                               control_proportion_vector ,
                               v_proportion_vector ,
                               delta_cr_vector ,
                               v_conf_cr_vector 
                               ,
                               
                               control_x_bar_margin_vector ,
                               v_x_bar_margin_vector ,
                               delta_margin_vector ,
                               v_conf_margin_vector
                               ,
                               
                               control_x_bar_sales_vector ,
                               v_x_bar_sales_vector ,
                               delta_sales_vector ,
                               v_conf_sales_vector
) 

capFirst <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

names(results_data_raw) <- c("experiment_name", "variant_id","days_run","country","device","control_n", "v_n","control_cr","v_cr","delta_cr",
                             "v_conf_cr","control_mpu","v_mpu","delta_mpu","v_conf_mpu","control_spu","v_spu","delta_spu", "v_conf_spu")

#capitalise first letter of device
results_data_raw$device <- capFirst(results_data_raw$device)


#add flags that will allow us to set the colour style in the shiny app
results_data_raw$cr_conf_flag <- ifelse(results_data_raw$v_conf_cr >= 0.9 & results_data_raw$delta_cr > 0,1, 
                                        ifelse(results_data_raw$v_conf_cr >= 0.9 & results_data_raw$delta_cr < 0, -1,
                                               0))

results_data_raw$mpu_conf_flag <- ifelse(results_data_raw$v_conf_mpu >= 0.9 & results_data_raw$delta_mpu > 0,1, 
                             ifelse(results_data_raw$v_conf_mpu >= 0.9 & results_data_raw$delta_mpu < 0, -1,
                                    0))

results_data_raw$spu_conf_flag <- ifelse(results_data_raw$v_conf_spu >= 0.9 & results_data_raw$delta_spu > 0,1, 
                             ifelse(results_data_raw$v_conf_spu >= 0.9 & results_data_raw$delta_spu < 0, -1,
                                    0))



#add completed / running field and flag - 2 days behind because order data is always 2 days behind

exp_runs$completion_flag <- ifelse(Sys.Date() > exp_runs$end_date, 1,0)

exp_runs$completion_flag[is.na(exp_runs$completion_flag)] <- 0
exp_runs$completion_text <- ifelse(exp_runs$completion_flag == 1, "Completed", "Running")

results_data_raw <- sqldf("SELECT results_data_raw.*
                  , exp_runs.completion_text
                  , exp_runs.completion_flag
                  , exp_runs.start_date
                  , exp_runs.end_date
                 FROM results_data_raw
                 LEFT JOIN exp_runs
                 ON results_data_raw.experiment_name = exp_runs.experiment_name",drv="SQLite")



names(results_data_raw) <- c("experiment_name", "variant_id","days_run","country","device","control_n", "v_n","control_cr","v_cr","delta_cr",
                             "v_conf_cr","control_mpu","v_mpu","delta_mpu","v_conf_mpu","control_spu","v_spu","delta_spu", "v_conf_spu","cr_conf_flag",
                             "mpu_conf_flag","spu_conf_flag","completion_text","completion_flag","start_date" ,"end_date")



filename <- paste0("Historical_Results/overall_results_",as.character(Sys.Date()-2),".csv")

write.csv(results_data_raw,filename)



###########################################################################################################################
###########################################################################################################################
###########################################################################################################################
#DAILY CUMULATIVE RESULTS


#data_raw <- read.csv('custom_results_input_21092018.csv')
data_raw$allocation_date <- as.Date(data_raw$allocation_date)

data <- data_raw
data$has_multiple_variants <- data$multiple_variant_user
data[,4] = toupper(data[,4])


data_country_device <-data
data_all_all <- data
data_all_all$country <- 'All'
data_all_all$device <- 'All'

data_all_device <- data
data_all_device$country <- 'All'
data_all_country <- data
data_all_country$device <- 'All'

data <- rbind(data_country_device,data_all_all,data_all_device,data_all_country)
data <- data[data$include_in_analysis == 1,]
data <- data[data$has_multiple_variants == 0,]
data <- data[data$country == 'GB' |
               data$country == 'FR' | 
               data$country == 'All'  ,]

#data <- data[data$num_customers != 0,]

data$include_in_analysis <- NULL
data$has_multiple_variants <- NULL
data$run_seq <- NULL
data$experiment_variant <- NULL


data_clean <- data

library(plyr)
library(dplyr)
data <- ddply(data, .(experiment_name, allocation_date,variant_id,country,device), numcolwise(sum))

data <- data %>%group_by(experiment_name, variant_id,country,device) %>% mutate(num_users= cumsum(num_users ))
data <- data %>%group_by(experiment_name, variant_id,country,device) %>% mutate(num_customers= cumsum(num_customers ))
data <- data %>%group_by(experiment_name, variant_id,country,device) %>% mutate(sum_sales_gbp_esev= cumsum(sum_sales_gbp_esev ))
data <- data %>%group_by(experiment_name, variant_id,country,device) %>% mutate(sum_sales_gbp_esev_sq= cumsum(sum_sales_gbp_esev_sq ))
data <- data %>%group_by(experiment_name, variant_id,country,device) %>% mutate(sum_sales_gbp_isev= cumsum(sum_sales_gbp_isev ))
data <- data %>%group_by(experiment_name, variant_id,country,device) %>% mutate(sum_sales_gbp_isev_sq= cumsum(sum_sales_gbp_isev_sq ))
data <- data %>%group_by(experiment_name, variant_id,country,device) %>% mutate(sum_revenue_gbp_esev= cumsum(sum_revenue_gbp_esev ))
data <- data %>%group_by(experiment_name, variant_id,country,device) %>% mutate(sum_revenue_gbp_esev_sq= cumsum(sum_revenue_gbp_esev_sq ))
data <- data %>%group_by(experiment_name, variant_id,country,device) %>% mutate(sum_revenue_gbp_isev= cumsum(sum_revenue_gbp_isev ))
data <- data %>%group_by(experiment_name, variant_id,country,device) %>% mutate(sum_revenue_gbp_isev_sq= cumsum(sum_revenue_gbp_isev_sq ))
data <- data %>%group_by(experiment_name, variant_id,country,device) %>% mutate(sum_product_cogs_gbp= cumsum(sum_product_cogs_gbp ))
data <- data %>%group_by(experiment_name, variant_id,country,device) %>% mutate(sum_product_cogs_gbp_sq= cumsum(sum_product_cogs_gbp_sq ))
data <- data %>%group_by(experiment_name, variant_id,country,device) %>% mutate(sum_shipping_cogs_gbp= cumsum(sum_shipping_cogs_gbp ))
data <- data %>%group_by(experiment_name, variant_id,country,device) %>% mutate(sum_shipping_cogs_gbp_sq= cumsum(sum_shipping_cogs_gbp_sq ))
data <- data %>%group_by(experiment_name, variant_id,country,device) %>% mutate(sum_margin_product_gbp= cumsum(sum_margin_product_gbp ))
data <- data %>%group_by(experiment_name, variant_id,country,device) %>% mutate(sum_margin_product_gbp_sq= cumsum(sum_margin_product_gbp_sq ))
data <- data %>%group_by(experiment_name, variant_id,country,device) %>% mutate(sum_margin_shipping_gpb= cumsum(sum_margin_shipping_gpb ))
data <- data %>%group_by(experiment_name, variant_id,country,device) %>% mutate(sum_margin_shipping_gpb_sq= cumsum(sum_margin_shipping_gpb_sq ))

#detach(package:plyr)
data <- data %>% group_by(experiment_name, variant_id,country,device) %>% mutate(days_run = row_number())



experiment_name_vector <- NULL
days_run_vector <- NULL
allocation_date_vector <- NULL
variant_id_vector <- NULL
country_vector <- NULL
device_vector <- NULL
control_n_vector <- NULL
v_n_vector <- NULL

control_proportion_vector <- NULL
v_proportion_vector <- NULL
delta_cr_vector <- NULL
v_conf_cr_vector <- NULL

control_x_bar_margin_vector <- NULL
v_x_bar_margin_vector <- NULL
delta_margin_vector <- NULL
v_conf_margin_vector <- NULL

control_x_bar_sales_vector <- NULL
v_x_bar_sales_vector <- NULL
delta_sales_vector <- NULL
v_conf_sales_vector <- NULL

experiments <- unique(data$experiment_name)
#experiments <- c("shop-all-menu-iconchange-mobile-only-may18")

for (experiment_name in experiments) {
  data_1 <- data
  data_1 <- data_1[data_1$experiment_name == experiment_name,]
  variants <- unique(data_1$variant_id)
  #variants <- variants[1:4]
  variants <- variants[!is.na(variants)]
  variants <- variants[2:4]
  variants <- variants[!is.na(variants)]
  countries <- unique(data_1$country)
  devices <- unique(data_1$device)
  #days_run <- data_clean[data_clean$experiment_name == experiment_name,]
  #days_run <- length(unique(days_run$allocation_date))
  
  
  for (country in countries) {
    #data_2 <- data[data$experiment_name == experiment_name,]
    #data_2 <- data_2[data_2$country == country,]
    
    for (device in devices) {  
      #data_3 <- data[data$experiment_name == experiment_name,]
      #data_3 <- data_3[data_3$country == country,]
      #data_3 <- data_3[data_3$device == device,]
      
      for (variant_id in variants){  
        data_4 <- data[data$experiment_name == experiment_name,]
        data_4 <- data_4[data_4$country == country,]
        data_4 <- data_4[data_4$device == device,]
        data_4 <- data_4[data_4$variant_id == variant_id,]
        
        date_range <- unique(data_4$allocation_date)
        
        for (date in date_range){  
          data_5 <- data[data$experiment_name == experiment_name,]
          data_5 <- data_5[data_5$country == country,]
          data_5 <- data_5[data_5$device == device,]
          data_5 <- data_5[data_5$allocation_date == date,]
          
          
          
          cr_vals <- conversion_test_daily(data_5,experiment_name,variant_id,country,device)
          margin_vals <- margin_test(data_5,experiment_name,variant_id,country,device)
          sales_vals <- sales_test(data_5,experiment_name,variant_id,country,device)
          
          experiment_name_vector <- c(experiment_name_vector,cr_vals$experiment_name)
          variant_id_vector <- c(variant_id_vector,cr_vals$variant_id)
          days_run_vector <- c(days_run_vector,cr_vals$days_run)
          allocation_date_vector <- as.Date(c(allocation_date_vector,cr_vals$allocation_date))
          country_vector <- c(country_vector,cr_vals$country)
          device_vector <- c(device_vector,cr_vals$device)
          control_n_vector <- c(control_n_vector,cr_vals$control_n)
          v_n_vector <- c(v_n_vector,cr_vals$v_n)
          
          control_proportion_vector <- c(control_proportion_vector,cr_vals$control_proportion)
          v_proportion_vector <- c(v_proportion_vector,cr_vals$v_proportion)
          delta_cr_vector <- c(delta_cr_vector,cr_vals$delta_cr)
          v_conf_cr_vector <- c(v_conf_cr_vector,cr_vals$v_conf_cr)
          
          control_x_bar_margin_vector <- c(control_x_bar_margin_vector,margin_vals$control_x_bar_margin)
          v_x_bar_margin_vector <- c(v_x_bar_margin_vector,margin_vals$v_x_bar_margin)
          delta_margin_vector <- c(delta_margin_vector,margin_vals$delta_margin)
          v_conf_margin_vector <- c(v_conf_margin_vector,margin_vals$v_conf_margin)
          
          control_x_bar_sales_vector <- c(control_x_bar_sales_vector,sales_vals$control_x_bar_sales)
          v_x_bar_sales_vector <- c(v_x_bar_sales_vector,sales_vals$v_x_bar_sales)
          delta_sales_vector <- c(delta_sales_vector,sales_vals$delta_sales)
          v_conf_sales_vector <- c(v_conf_sales_vector,sales_vals$v_conf_sales)
        }
      }
    }
  }
}

daily_results_data_raw <- data.frame(experiment_name_vector,
                                     variant_id_vector,
                                     days_run_vector,
                                     allocation_date_vector,
                                     country_vector,
                                     device_vector,
                                     control_n_vector ,
                                     v_n_vector ,
                                     
                                     control_proportion_vector ,
                                     v_proportion_vector ,
                                     delta_cr_vector ,
                                     v_conf_cr_vector 
                                     ,
                                     
                                     control_x_bar_margin_vector ,
                                     v_x_bar_margin_vector ,
                                     delta_margin_vector ,
                                     v_conf_margin_vector
                                     ,
                                     
                                     control_x_bar_sales_vector ,
                                     v_x_bar_sales_vector ,
                                     delta_sales_vector ,
                                     v_conf_sales_vector
) 

names(daily_results_data_raw) <- c("experiment_name", "variant_id","days_run","allocation_date","country","device","control_n", "v_n","control_cr","v_cr","delta_cr",
                                   "v_conf_cr","control_mpu","v_mpu","delta_mpu","v_conf_mpu","control_spu","v_spu","delta_spu", "v_conf_spu")

daily_results_data_raw <- sqldf("SELECT daily_results_data_raw.*
                  , exp_runs.completion_text
                  , exp_runs.completion_flag
                  , exp_runs.start_date
                  , exp_runs.end_date
                 FROM daily_results_data_raw
                 LEFT JOIN exp_runs
                 ON daily_results_data_raw.experiment_name = exp_runs.experiment_name",drv="SQLite")

#capitalise first letter of device
daily_results_data_raw$device <- capFirst(daily_results_data_raw$device)

names(daily_results_data_raw) <- c("experiment_name", "variant_id","days_run","allocation_date","country","device","control_n", "v_n","control_cr","v_cr","delta_cr",
                                   "v_conf_cr","control_mpu","v_mpu","delta_mpu","v_conf_mpu","control_spu","v_spu","delta_spu", "v_conf_spu",
                                   "completion_text","completion_flag","start_date" ,"end_date")


filename <- paste0("Historical_Results/daily_results_",as.character(Sys.Date()-2),".csv")

write.csv(daily_results_data_raw,filename)


################################
# Save to S3
library(aws.s3)
library(aws.signature)
 #library(aws.ec2metadata)
library(data.table)
# graphics.off()
# pdf(NULL)

#setwd("/home/remy/pbx_ab_testing_portal/Temporary_Results_Pipeline/")
#data <- read.csv("custom_results_data_20180921.csv")


bucket_name<-"bi-photobox-analytics-eu"
aws_access_key = 'xxx'
aws_secret_key='xxx'
Sys.setenv('AWS_SECRET_ACCESS_KEY' =aws_secret_key,
           'AWS_ACCESS_KEY_ID' = aws_access_key
           ,'AWS_DEFAULT_REGION' ='eu-west-1'
)


data <- as.data.frame(results_data_raw)
#x = aws.signature::locate_credentials()

#Sys.setenv('AWS_SESSION_TOKEN' = x['session_token'],
#           'AWS_SECRET_ACCESS_KEY' = x['secret'],
#           'AWS_ACCESS_KEY_ID' = x['key'])

conn <- rawConnection(raw(0), "r+")

write.csv(data, conn, row.names=FALSE)
put_object(file = rawConnectionValue(conn),
           bucket = "s3://bi-photobox-analytics-eu/remy/pbx - ab testing portal/", object = "s3://bi-photobox-analytics-eu/remy/pbx - ab testing portal/results_data.csv",check_region = FALSE)
close(conn)


bucket_name<-"bi-photobox-analytics-eu"
aws_access_key = 'xxx'
aws_secret_key='xxx'
Sys.setenv('AWS_SECRET_ACCESS_KEY' =aws_secret_key,
           'AWS_ACCESS_KEY_ID' = aws_access_key
           ,'AWS_DEFAULT_REGION' ='eu-west-1'
)

data <- as.data.frame(daily_results_data_raw)
#x = aws.signature::locate_credentials()

#Sys.setenv('AWS_SESSION_TOKEN' = x['session_token'],
#           'AWS_SECRET_ACCESS_KEY' = x['secret'],
#           'AWS_ACCESS_KEY_ID' = x['key'])

conn <- rawConnection(raw(0), "r+")

write.csv(data, conn, row.names=FALSE)
put_object(file = rawConnectionValue(conn),
           bucket = "s3://bi-photobox-analytics-eu/remy/pbx - ab testing portal/", object = "s3://bi-photobox-analytics-eu/remy/pbx - ab testing portal/daily_results_data.csv",check_region = FALSE)
close(conn)

