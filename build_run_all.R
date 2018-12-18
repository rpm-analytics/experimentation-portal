print("***JOB START***")
print("building manual tables: START")
source("/home/remy/pbx_ab_testing_portal/results_pipeline/build_exp_manual_tables.R") 
print("building manual tables: COMPLETE")

print("building data model - allocation tables: START")
source("/home/remy/pbx_ab_testing_portal/results_pipeline/build_exp_allocation_tables.R") 
print("building data model - allocation tables: COMPLETE")

print("building data model - analysis tables: START")
source("/home/remy/pbx_ab_testing_portal/results_pipeline/build_exp_analysis_tables.R") 
print("building data model - analysis tables: COMPLETE")

print("building results data: START")
source("/home/remy/pbx_ab_testing_portal/results_pipeline/build_exp_results.R")
print("building results data: COMPLETE")
print("***JOB COMPLETE***")

print("building size of prize data: START")
source("/home/remy/pbx_ab_testing_portal/results_pipeline/build_sop_results.R")
print("building size of prize data: COMPLETE")
print("***JOB COMPLETE***")

