library(DBI)
library(RPostgreSQL)
library(readr)
setwd("/home/remy/pbx_ab_testing_portal/results_pipeline/")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host="cloud-dwh-analysis.cede9zhbujyu.eu-west-1.redshift.amazonaws.com", 
                 port="5439",
                 dbname="phxred", 
                 user="ENTER USERNAME HERE", 
                 password="ENTER PASSWORD HERE"
                 
                 #,
                 #ssl="true",
                 #sslfactory="com.amazon.redshift.ssl.NonValidatingFactory"
)

data_model_query <- read_file("exp_analysis_tables_query.txt")

create_exp_data_model_in_redshift <- dbGetQuery(con, data_model_query)
