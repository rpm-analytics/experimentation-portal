library(DBI)
library(RPostgreSQL)
library(readr)
setwd("/home/remy/pbx_ab_testing_portal/results_pipeline/")

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host="cloud-dwh-analysis.cede9zhbujyu.eu-west-1.redshift.amazonaws.com", 
                 port="5439",
                 dbname="phxred", 
                 user="ENTER YOUR USERNAME", 
                 password="ENTER YOUR PASSWORD"
                 
                 #,
                 #ssl="true",
                 #sslfactory="com.amazon.redshift.ssl.NonValidatingFactory"
)

variant_d_query <- read_file("exp_variant_d.txt")
experiment_runs_d_query <- read_file("exp_experiment_runs_d.txt")
#sop_d_query <- read_file("exp_sop_d.txt")

push_experiment_runs_d_to_redshift <- dbGetQuery(con, variant_d_query)
push_variant_d_to_redshift <- dbGetQuery(con, experiment_runs_d_query)
#push_sop_d_to_redshift <- dbGetQuery(con, sop_d_query)
