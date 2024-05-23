# Preliminary information -----

# This R script was developed for the publication:
# Centralized or onsite testing? Examining the costs of water quality monitoring in rural Africa

# It compares the costs of four approaches to microbial water quality testing, 
# each representing different a different level of centralization (please see the publication for details).

# Using the accompanying Excel spreadsheet, input parameter assumptions can be changed to reflect different scenarios and settings.
# The original assumptions in the spreadsheet correspond to the generalized analysis from the publication,
# simulating a range of possible conditions in sub-Saharan Africa.

# Please refer to the "Instructions" tab in the input spreadsheet for step-by-step instructions to run this script.



# Load required packages and input assumptions -----

# Load required libraries (install packages as needed)
library(lhs)
library(readxl)
library(tibble)
library(openxlsx)
library(ggplot2)
library(reshape)

# Update working directory as needed
setwd("C:\\Users\\jttrimme\\Dropbox\\Aquaya\\WQ testing costs\\Cost model")

# Load cost input spreadsheet
inputs <- read_xlsx("WQtesting_cost_inputs.xlsx", sheet = "Model inputs")


# LHS functions -----

# Define functions to perform Latin Hypercube Sampling (LHS) on uncertain input parameters.
# Both functions employ uniform distributions. 
# The integer function is designed for uncertain parameters that can only include integer values.

# uniform distribution
LHS_uniform <- function(minimum, maximum, N) {
  qunif(randomLHS(N,1), min = minimum, max = maximum)
}

# integers
LHS_integer <- function(minimum, maximum, N) {
  floor(randomLHS(N,1)*(maximum - minimum + 1)) + minimum
}

# Monte Carlo inputs -----

# number of Monte Carlo simulations (can update as desired)
N <- 10000

# create data frame to hold all input values pulled from probability distributions
inputs_MC <- data.frame(matrix(nrow = N, ncol = nrow(inputs)))
colnames(inputs_MC) <- inputs$Variable_name

# Cycle through each parameter in the input spreadsheet, and create & store a distribution.
# Constant parameters are assigned the same value for all simulations
# The warning options prevent display of irrelevant warning messages during this procedure.
oldWarningLevel <- getOption("warn")
options(warn = -1)
for (i in 1:nrow(inputs)) {
  if (inputs$Distribution_type[i] == "constant") {
    if (is.na(as.numeric(inputs$Assumed_value[i]))) {
      inputs_MC[,i] <- inputs$Assumed_value[i]
    } else {
      inputs_MC[,i] <- as.numeric(inputs$Assumed_value[i])
    }
  } else if (inputs$Distribution_type[i] == "uniform") {
    inputs_MC[,i] <- LHS_uniform(inputs$Minimum_value[i], inputs$Maximum_value[i], N)
  } else if (inputs$Distribution_type[i] == "integer") {
    inputs_MC[,i] <- LHS_integer(inputs$Minimum_value[i], inputs$Maximum_value[i], N)
  }
}
options(warn = oldWarningLevel)



# Monte Carlo calculations - centralized -----

# Create data frame to hold intermediate and final results from calculations
centralized_result_names <- c("points_per_month", "travel_time_central", "points_sampled_per_day", "distance_traveled_per_day",
                              "sampling_staff_days_per_month", "analysis_staff_days_per_month", "maximum_cars_needed",
                              "maximum_samples_per_day", "incubators_needed",
                              "annual_equipment_cost_total", "annual_consumable_cost_total",
                              "annual_labor_cost_total", "annual_transport_cost_total", "annual_overall_cost_total",
                              "annual_equipment_cost_per_system", "annual_consumable_cost_per_system",
                              "annual_labor_cost_per_system", "annual_transport_cost_per_system", "annual_overall_cost_per_system",
                              "equipment_cost_per_test", "consumable_cost_per_test", 
                              "labor_cost_per_test", "transport_cost_per_test", "overall_cost_per_test")
centralized_results_MC <- data.frame(matrix(nrow = N, ncol = length(centralized_result_names)))
colnames(centralized_results_MC) <- centralized_result_names

# intermediate calculations
# Required total number of samples per month
centralized_results_MC$points_per_month <- inputs_MC$N_points * (inputs_MC$N_annual_samples/12)

# Average travel time from central lab to water system
centralized_results_MC$travel_time_central <- inputs_MC$distance_central / inputs_MC$speed_average
# Maximum number of water systems that can be sampled in a day
centralized_results_MC$points_sampled_per_day <- floor((inputs_MC$hours_sampling - centralized_results_MC$travel_time_central * (2 - inputs_MC$cluster_distance_fraction)) /
                                                         (inputs_MC$time_sample/60 + centralized_results_MC$travel_time_central * inputs_MC$cluster_distance_fraction))
# correct for very long travel times larger than working time
centralized_results_MC$points_sampled_per_day <- ifelse(centralized_results_MC$points_sampled_per_day<0, 0, centralized_results_MC$points_sampled_per_day) 

# Distance traveled by sampling car per day
centralized_results_MC$distance_traveled_per_day <- (2*inputs_MC$distance_central + 
                                                       ((pmin(centralized_results_MC$points_sampled_per_day, ceiling(centralized_results_MC$points_per_month)) - 1) * (inputs_MC$cluster_distance_fraction * inputs_MC$distance_central)))
# Required total number of working days for sampling staff per month
centralized_results_MC$sampling_staff_days_per_month <- centralized_results_MC$points_per_month / centralized_results_MC$points_sampled_per_day
# Required total number of working days for lab analysis staff per month
centralized_results_MC$analysis_staff_days_per_month <- centralized_results_MC$points_per_month / inputs_MC$samples_analyzed_day
# Number of cars required to complete all required sampling
centralized_results_MC$maximum_cars_needed <- ceiling(centralized_results_MC$sampling_staff_days_per_month / inputs_MC$workdays_month)
# Actual maximum number of water systems sampled per day (if total number of samples per month < maximum possible per day)
centralized_results_MC$maximum_samples_per_day <- (pmin(centralized_results_MC$points_sampled_per_day, ceiling(centralized_results_MC$points_per_month) / centralized_results_MC$maximum_cars_needed) * 
                                                     centralized_results_MC$maximum_cars_needed)
# Number of incubators needed to store maximum number of samples collected in a day
centralized_results_MC$incubators_needed <- ceiling(centralized_results_MC$maximum_samples_per_day / inputs_MC$incubator_capacity)
# correct for very long travel times larger than working time
centralized_results_MC$incubators_needed <- ifelse(centralized_results_MC$incubators_needed <= 0, 1, centralized_results_MC$incubators_needed) 

# annual totals (inclusive of all systems)
centralized_results_MC$annual_equipment_cost_total <- ifelse(inputs_MC$discount_rate != 0, 
                                                             # incubator(s)
                                                             (((centralized_results_MC$incubators_needed * inputs_MC$incubator_cost) *
                                                                 ((inputs_MC$discount_rate * (1 + inputs_MC$discount_rate)^inputs_MC$incubator_life) /
                                                                    ((1 + inputs_MC$discount_rate)^inputs_MC$incubator_life - 1))) +
                                                                # membrane filtration equipment
                                                                ((inputs_MC$equipment_cost) *
                                                                   ((inputs_MC$discount_rate * (1 + inputs_MC$discount_rate)^inputs_MC$equipment_life) /
                                                                      ((1 + inputs_MC$discount_rate)^inputs_MC$equipment_life - 1))) +
                                                                # autoclave
                                                                ((inputs_MC$autoclave_cost) *
                                                                   ((inputs_MC$discount_rate * (1 + inputs_MC$discount_rate)^inputs_MC$autoclave_life) /
                                                                      ((1 + inputs_MC$discount_rate)^inputs_MC$autoclave_life - 1))) +
                                                                # car(s)
                                                                (centralized_results_MC$maximum_cars_needed * inputs_MC$car_cost) *
                                                                ((inputs_MC$discount_rate * (1 + inputs_MC$discount_rate)^inputs_MC$car_life) /
                                                                   ((1 + inputs_MC$discount_rate)^inputs_MC$car_life - 1)) +
                                                                # cooler(s)
                                                                (centralized_results_MC$maximum_cars_needed * inputs_MC$cooler_cost) *
                                                                ((inputs_MC$discount_rate * (1 + inputs_MC$discount_rate)^inputs_MC$cooler_life) /
                                                                   ((1 + inputs_MC$discount_rate)^inputs_MC$cooler_life - 1)) +
                                                                # lab space rental
                                                                ((inputs_MC$lab_monthly_rent*ifelse(centralized_results_MC$analysis_staff_days_per_month/inputs_MC$workdays_month > 1,
                                                                                                    1, centralized_results_MC$analysis_staff_days_per_month/inputs_MC$workdays_month))*12)),
                                                             # alternative formula to accommodate 0% discount rate
                                                             (centralized_results_MC$incubators_needed * inputs_MC$incubator_cost / inputs_MC$incubator_life + 
                                                                inputs_MC$equipment_cost / inputs_MC$equipment_life +
                                                                inputs_MC$autoclave_cost / inputs_MC$autoclave_life +
                                                                centralized_results_MC$maximum_cars_needed * inputs_MC$car_cost / inputs_MC$car_life +
                                                                centralized_results_MC$maximum_cars_needed * inputs_MC$cooler_cost / inputs_MC$cooler_life +
                                                                ((inputs_MC$lab_monthly_rent*ifelse(centralized_results_MC$analysis_staff_days_per_month/inputs_MC$workdays_month > 1,
                                                                                                    1, centralized_results_MC$analysis_staff_days_per_month/inputs_MC$workdays_month))*12)))
centralized_results_MC$annual_consumable_cost_total <- (inputs_MC$N_points * inputs_MC$N_annual_samples) * inputs_MC$consumable_cost * (1+inputs_MC$QC_markup)
centralized_results_MC$annual_labor_cost_total <- (((centralized_results_MC$sampling_staff_days_per_month / inputs_MC$workdays_month) *
                                                      (inputs_MC$wage_sampling + inputs_MC$wage_driver) +
                                                      ((centralized_results_MC$analysis_staff_days_per_month / inputs_MC$workdays_month) *
                                                         inputs_MC$wage_analyst)) * 12)
centralized_results_MC$annual_transport_cost_total <- (centralized_results_MC$distance_traveled_per_day * centralized_results_MC$sampling_staff_days_per_month *
                                                         12 / inputs_MC$fuel_economy) * inputs_MC$fuel_price
centralized_results_MC$annual_overall_cost_total <- (centralized_results_MC$annual_equipment_cost_total + centralized_results_MC$annual_consumable_cost_total +
                                                       centralized_results_MC$annual_labor_cost_total + centralized_results_MC$annual_transport_cost_total)

# total cost per test
centralized_results_MC$equipment_cost_per_test <- centralized_results_MC$annual_equipment_cost_total / (inputs_MC$N_points * inputs_MC$N_annual_samples)
centralized_results_MC$consumable_cost_per_test <- centralized_results_MC$annual_consumable_cost_total / (inputs_MC$N_points * inputs_MC$N_annual_samples)
centralized_results_MC$labor_cost_per_test <- centralized_results_MC$annual_labor_cost_total / (inputs_MC$N_points * inputs_MC$N_annual_samples)
centralized_results_MC$transport_cost_per_test <- centralized_results_MC$annual_transport_cost_total / (inputs_MC$N_points * inputs_MC$N_annual_samples)
centralized_results_MC$overall_cost_per_test <- (centralized_results_MC$equipment_cost_per_test + centralized_results_MC$consumable_cost_per_test +
                                                   centralized_results_MC$labor_cost_per_test + centralized_results_MC$transport_cost_per_test)



# Monte Carlo calculations - mobile lab -----

# Create data frame to hold intermediate and final results from calculations
mobile_result_names <- c("points_per_month", "travel_time_cluster", "points_sampled_per_day", "distance_traveled_per_day",
                         "staff_days_per_month", "maximum_mobile_labs_needed",
                         "maximum_samples_per_lab_per_day", "incubators_needed_per_lab",
                         "annual_equipment_cost_total", "annual_consumable_cost_total",
                         "annual_labor_cost_total", "annual_transport_cost_total", "annual_overall_cost_total",
                         "annual_equipment_cost_per_system", "annual_consumable_cost_per_system",
                         "annual_labor_cost_per_system", "annual_transport_cost_per_system", "annual_overall_cost_per_system",
                         "equipment_cost_per_test", "consumable_cost_per_test", 
                         "labor_cost_per_test", "transport_cost_per_test", "overall_cost_per_test")
mobile_results_MC <- data.frame(matrix(nrow = N, ncol = length(mobile_result_names)))
colnames(mobile_results_MC) <- mobile_result_names

# intermediate calculations
# Required total number of samples per month
mobile_results_MC$points_per_month <- inputs_MC$N_points * (inputs_MC$N_annual_samples/12)

# Average travel time between water systems
mobile_results_MC$travel_time_cluster <- (inputs_MC$distance_central * inputs_MC$cluster_distance_fraction) / (inputs_MC$lab_speed*inputs_MC$speed_average)
# Maximum number of water systems that can be sampled in a day
mobile_results_MC$points_sampled_per_day <- floor((inputs_MC$hours_sampling) /
                                                    (inputs_MC$time_sample/60 + ((8/inputs_MC$samples_analyzed_day)) + mobile_results_MC$travel_time_cluster))
# correct for very long travel times larger than working time
mobile_results_MC$points_sampled_per_day <- ifelse(mobile_results_MC$points_sampled_per_day<0, 0, mobile_results_MC$points_sampled_per_day) 

# Distance traveled by mobile lab per day
mobile_results_MC$distance_traveled_per_day <- (inputs_MC$distance_central * inputs_MC$cluster_distance_fraction) * pmin(mobile_results_MC$points_sampled_per_day, ceiling(mobile_results_MC$points_per_month))
# Required total number of working days for mobile lab staff per month
mobile_results_MC$staff_days_per_month <- mobile_results_MC$points_per_month / mobile_results_MC$points_sampled_per_day
# Number of mobile labs required to complete all required sampling
mobile_results_MC$maximum_mobile_labs_needed <- ceiling(mobile_results_MC$staff_days_per_month / inputs_MC$workdays_month)
# Actual maximum number of water systems sampled by 1 mobile lab per day (if total number of samples per month < maximum possible per day)
mobile_results_MC$maximum_samples_per_lab_per_day <- (pmin(mobile_results_MC$points_sampled_per_day, ceiling(mobile_results_MC$points_per_month)))
# Number of incubators needed to store maximum number of samples collected by 1 mobile lab in a day
mobile_results_MC$incubators_needed_per_lab <- ceiling(mobile_results_MC$maximum_samples_per_lab_per_day / inputs_MC$incubator_capacity)
# correct for very long travel times larger than working time
mobile_results_MC$incubators_needed_per_lab <- ifelse(mobile_results_MC$incubators_needed_per_lab <= 0, 1, mobile_results_MC$incubators_needed_per_lab) 

# annual totals (all systems)
mobile_results_MC$annual_equipment_cost_total <- ifelse(inputs_MC$discount_rate != 0, 
                                                        # incubator(s)
                                                        (((mobile_results_MC$incubators_needed_per_lab * inputs_MC$incubator_cost) *
                                                            ((inputs_MC$discount_rate * (1 + inputs_MC$discount_rate)^inputs_MC$incubator_life) /
                                                               ((1 + inputs_MC$discount_rate)^inputs_MC$incubator_life - 1))) +
                                                           # membrane filtration equipment
                                                           ((inputs_MC$equipment_cost) *
                                                              ((inputs_MC$discount_rate * (1 + inputs_MC$discount_rate)^inputs_MC$equipment_life) /
                                                                 ((1 + inputs_MC$discount_rate)^inputs_MC$equipment_life - 1))) +
                                                           # autoclave
                                                           ((inputs_MC$autoclave_cost) *
                                                              ((inputs_MC$discount_rate * (1 + inputs_MC$discount_rate)^inputs_MC$autoclave_life) /
                                                                 ((1 + inputs_MC$discount_rate)^inputs_MC$autoclave_life - 1))) +
                                                           # lab vehicle
                                                           (inputs_MC$lab_vehicle_cost) *
                                                           ((inputs_MC$discount_rate * (1 + inputs_MC$discount_rate)^inputs_MC$lab_vehicle_life) /
                                                              ((1 + inputs_MC$discount_rate)^inputs_MC$lab_vehicle_life - 1))),
                                                        # discount rate = 0%
                                                        (mobile_results_MC$incubators_needed_per_lab * inputs_MC$incubator_cost / inputs_MC$incubator_life + 
                                                           inputs_MC$equipment_cost / inputs_MC$equipment_life +
                                                           inputs_MC$autoclave_cost / inputs_MC$autoclave_life +
                                                           inputs_MC$lab_vehicle_cost / inputs_MC$lab_vehicle_life)) * mobile_results_MC$maximum_mobile_labs_needed
mobile_results_MC$annual_consumable_cost_total <- (inputs_MC$N_points * inputs_MC$N_annual_samples) * inputs_MC$consumable_cost * (1+inputs_MC$QC_markup)
mobile_results_MC$annual_labor_cost_total <- (((mobile_results_MC$staff_days_per_month / inputs_MC$workdays_month) *
                                                 (inputs_MC$wage_analyst + inputs_MC$wage_driver)) * 12)
mobile_results_MC$annual_transport_cost_total <- (mobile_results_MC$distance_traveled_per_day * mobile_results_MC$staff_days_per_month *
                                                    12 / (inputs_MC$lab_fuel_economy * inputs_MC$fuel_economy)) * inputs_MC$fuel_price
mobile_results_MC$annual_overall_cost_total <- (mobile_results_MC$annual_equipment_cost_total + mobile_results_MC$annual_consumable_cost_total +
                                                  mobile_results_MC$annual_labor_cost_total + mobile_results_MC$annual_transport_cost_total)

# totals per test
mobile_results_MC$equipment_cost_per_test <- mobile_results_MC$annual_equipment_cost_total / (inputs_MC$N_points * inputs_MC$N_annual_samples)
mobile_results_MC$consumable_cost_per_test <- mobile_results_MC$annual_consumable_cost_total / (inputs_MC$N_points * inputs_MC$N_annual_samples)
mobile_results_MC$labor_cost_per_test <- mobile_results_MC$annual_labor_cost_total / (inputs_MC$N_points * inputs_MC$N_annual_samples)
mobile_results_MC$transport_cost_per_test <- mobile_results_MC$annual_transport_cost_total / (inputs_MC$N_points * inputs_MC$N_annual_samples)
mobile_results_MC$overall_cost_per_test <- (mobile_results_MC$equipment_cost_per_test + mobile_results_MC$consumable_cost_per_test +
                                              mobile_results_MC$labor_cost_per_test + mobile_results_MC$transport_cost_per_test)



# Monte Carlo calculations - semi-centralized -----

# Create data frame to hold intermediate and final results from calculations
semicentralized_result_names <- c("points_per_month", "travel_time_cluster", "points_sampled_per_day", "distance_traveled_per_day",
                                  "sampling_staff_days_per_month", "analysis_staff_days_per_month", "maximum_cars_needed",
                                  "maximum_samples_per_day", "incubators_needed",
                                  "annual_equipment_cost_total", "annual_consumable_cost_total",
                                  "annual_labor_cost_total", "annual_transport_cost_total", "annual_overall_cost_total",
                                  "annual_equipment_cost_per_system", "annual_consumable_cost_per_system",
                                  "annual_labor_cost_per_system", "annual_transport_cost_per_system", "annual_overall_cost_per_system",
                                  "equipment_cost_per_test", "consumable_cost_per_test", 
                                  "labor_cost_per_test", "transport_cost_per_test", "overall_cost_per_test")
semicentralized_results_MC <- data.frame(matrix(nrow = N, ncol = length(semicentralized_result_names)))
colnames(semicentralized_results_MC) <- semicentralized_result_names

# intermediates
# Required total number of samples per month
semicentralized_results_MC$points_per_month <- inputs_MC$N_points * (inputs_MC$N_annual_samples/12)

# Average travel time between water systems
semicentralized_results_MC$travel_time_cluster <- (inputs_MC$distance_central * inputs_MC$cluster_distance_fraction) / inputs_MC$speed_average
# Maximum number of water systems that can be sampled in a day
semicentralized_results_MC$points_sampled_per_day <- floor((inputs_MC$hours_sampling - semicentralized_results_MC$travel_time_cluster) /
                                                             (inputs_MC$time_sample/60 + semicentralized_results_MC$travel_time_cluster))
# correct for very long travel times larger than working time
semicentralized_results_MC$points_sampled_per_day <- ifelse(semicentralized_results_MC$points_sampled_per_day<0, 0, semicentralized_results_MC$points_sampled_per_day) 

# Distance traveled by sampling car per day
semicentralized_results_MC$distance_traveled_per_day <- ((inputs_MC$distance_central * inputs_MC$cluster_distance_fraction) * 
                                                           (pmin(semicentralized_results_MC$points_sampled_per_day, ceiling(semicentralized_results_MC$points_per_month/inputs_MC$N_clusters)) + 1))
# Required total number of working days for sampling staff per cluster per month
semicentralized_results_MC$sampling_staff_days_per_month <- (semicentralized_results_MC$points_per_month / inputs_MC$N_clusters) / semicentralized_results_MC$points_sampled_per_day
# Required total number of working days for lab analysis staff per cluster per month
semicentralized_results_MC$analysis_staff_days_per_month <- ((semicentralized_results_MC$points_per_month / inputs_MC$N_clusters)) / inputs_MC$samples_analyzed_day
# Number of cars required per cluster to complete all required sampling
semicentralized_results_MC$maximum_cars_needed <- ceiling(semicentralized_results_MC$sampling_staff_days_per_month / inputs_MC$workdays_month)
# Actual maximum number of water systems sampled per cluster per day (if total number of samples per month < maximum possible per day)
semicentralized_results_MC$maximum_samples_per_day <- (pmin(semicentralized_results_MC$points_sampled_per_day, ceiling(semicentralized_results_MC$points_per_month / inputs_MC$N_clusters) / semicentralized_results_MC$maximum_cars_needed) * 
                                                         semicentralized_results_MC$maximum_cars_needed)
# Number of incubators needed per cluster to store maximum number of samples collected in a day
semicentralized_results_MC$incubators_needed <- ceiling(semicentralized_results_MC$maximum_samples_per_day / inputs_MC$incubator_capacity)
# correct for very long travel times larger than working time
semicentralized_results_MC$incubators_needed <- ifelse(semicentralized_results_MC$incubators_needed <= 0, 1, semicentralized_results_MC$incubators_needed)

# annual totals (all systems)
semicentralized_results_MC$annual_equipment_cost_total <- ifelse(inputs_MC$discount_rate != 0, 
                                                                 # incubator(s)
                                                                 (((semicentralized_results_MC$incubators_needed * inputs_MC$incubator_cost) *
                                                                     ((inputs_MC$discount_rate * (1 + inputs_MC$discount_rate)^inputs_MC$incubator_life) /
                                                                        ((1 + inputs_MC$discount_rate)^inputs_MC$incubator_life - 1))) +
                                                                    # membrane filtration equipment
                                                                    ((inputs_MC$equipment_cost) *
                                                                       ((inputs_MC$discount_rate * (1 + inputs_MC$discount_rate)^inputs_MC$equipment_life) /
                                                                          ((1 + inputs_MC$discount_rate)^inputs_MC$equipment_life - 1))) +
                                                                    # autoclave
                                                                    ((inputs_MC$autoclave_cost) *
                                                                       ((inputs_MC$discount_rate * (1 + inputs_MC$discount_rate)^inputs_MC$autoclave_life) /
                                                                          ((1 + inputs_MC$discount_rate)^inputs_MC$autoclave_life - 1))) +
                                                                    # car(s)
                                                                    (semicentralized_results_MC$maximum_cars_needed * inputs_MC$car_cost) *
                                                                    ((inputs_MC$discount_rate * (1 + inputs_MC$discount_rate)^inputs_MC$car_life) /
                                                                       ((1 + inputs_MC$discount_rate)^inputs_MC$car_life - 1)) +
                                                                    # cooler(s)
                                                                    (semicentralized_results_MC$maximum_cars_needed * inputs_MC$cooler_cost) *
                                                                    ((inputs_MC$discount_rate * (1 + inputs_MC$discount_rate)^inputs_MC$cooler_life) /
                                                                       ((1 + inputs_MC$discount_rate)^inputs_MC$cooler_life - 1)) +
                                                                    # lab rental
                                                                    ((inputs_MC$lab_monthly_rent*ifelse(semicentralized_results_MC$analysis_staff_days_per_month/inputs_MC$workdays_month > 1,
                                                                                                        1, semicentralized_results_MC$analysis_staff_days_per_month/inputs_MC$workdays_month))*12)) * inputs_MC$N_clusters,
                                                                 # 0% discount rate
                                                                 (semicentralized_results_MC$incubators_needed * inputs_MC$incubator_cost / inputs_MC$incubator_life + 
                                                                    inputs_MC$equipment_cost / inputs_MC$equipment_life +
                                                                    inputs_MC$autoclave_cost / inputs_MC$autoclave_life +
                                                                    semicentralized_results_MC$maximum_cars_needed * inputs_MC$car_cost / inputs_MC$car_life +
                                                                    semicentralized_results_MC$maximum_cars_needed * inputs_MC$cooler_cost / inputs_MC$cooler_life +
                                                                    inputs_MC$lab_monthly_rent*12) * inputs_MC$N_clusters)

semicentralized_results_MC$annual_consumable_cost_total <- ((inputs_MC$N_annual_samples * inputs_MC$N_points) * 
                                                              inputs_MC$consumable_cost * (1+inputs_MC$consumable_markup) * (1+inputs_MC$QC_markup))
semicentralized_results_MC$annual_labor_cost_total <- (((semicentralized_results_MC$sampling_staff_days_per_month / inputs_MC$workdays_month) *
                                                          (inputs_MC$wage_sampling + inputs_MC$wage_driver)) +
                                                         ((semicentralized_results_MC$analysis_staff_days_per_month / inputs_MC$workdays_month) *
                                                            inputs_MC$wage_analyst)) * 12 * (inputs_MC$N_clusters)
semicentralized_results_MC$annual_transport_cost_total <- (semicentralized_results_MC$distance_traveled_per_day * semicentralized_results_MC$sampling_staff_days_per_month *
                                                             12 / inputs_MC$fuel_economy) * inputs_MC$fuel_price * (inputs_MC$N_clusters)
semicentralized_results_MC$annual_overall_cost_total <- (semicentralized_results_MC$annual_equipment_cost_total + semicentralized_results_MC$annual_consumable_cost_total +
                                                           semicentralized_results_MC$annual_labor_cost_total + semicentralized_results_MC$annual_transport_cost_total)

# totals per test
semicentralized_results_MC$equipment_cost_per_test <- semicentralized_results_MC$annual_equipment_cost_total / (inputs_MC$N_annual_samples * inputs_MC$N_points)
semicentralized_results_MC$consumable_cost_per_test <- semicentralized_results_MC$annual_consumable_cost_total / (inputs_MC$N_annual_samples * inputs_MC$N_points)
semicentralized_results_MC$labor_cost_per_test <- semicentralized_results_MC$annual_labor_cost_total / (inputs_MC$N_annual_samples * inputs_MC$N_points)
semicentralized_results_MC$transport_cost_per_test <- semicentralized_results_MC$annual_transport_cost_total / (inputs_MC$N_annual_samples * inputs_MC$N_points)
semicentralized_results_MC$overall_cost_per_test <- (semicentralized_results_MC$equipment_cost_per_test + semicentralized_results_MC$consumable_cost_per_test +
                                                       semicentralized_results_MC$labor_cost_per_test + semicentralized_results_MC$transport_cost_per_test)




# Monte Carlo calculations - decentralized -----
# *calculations assume no transport needed*

# Create data frame to hold intermediate and final results from calculations
decentralized_result_names <- c("points_per_month", "sampling_staff_days_per_month", "analysis_staff_days_per_month", 
                                "maximum_samples_per_day", "incubators_needed",
                                "annual_equipment_cost_total", "annual_consumable_cost_total",
                                "annual_labor_cost_total", "annual_transport_cost_total", "annual_overall_cost_total",
                                "annual_equipment_cost_per_system", "annual_consumable_cost_per_system",
                                "annual_labor_cost_per_system", "annual_transport_cost_per_system", "annual_overall_cost_per_system",
                                "equipment_cost_per_test", "consumable_cost_per_test", 
                                "labor_cost_per_test", "transport_cost_per_test", "overall_cost_per_test")
decentralized_results_MC <- data.frame(matrix(nrow = N, ncol = length(decentralized_result_names)))
colnames(decentralized_results_MC) <- decentralized_result_names

# intermediate calculations
# Required total number of samples per month
decentralized_results_MC$points_per_month <- inputs_MC$N_points * (inputs_MC$N_annual_samples/12)

# Required total number of working days for sampling per system per month
decentralized_results_MC$sampling_staff_days_per_month <- (decentralized_results_MC$points_per_month / inputs_MC$N_points) * (inputs_MC$time_sample/60) / inputs_MC$hours_sampling
# Required total number of working days for lab analysis per month
decentralized_results_MC$analysis_staff_days_per_month <- (decentralized_results_MC$points_per_month / inputs_MC$N_points) / inputs_MC$samples_analyzed_day
# Actual maximum number of water systems sampled per day (if total number of samples per month < maximum possible per day)
decentralized_results_MC$maximum_samples_per_day <- decentralized_results_MC$points_per_month / inputs_MC$N_points
# Number of incubators needed to store maximum number of samples collected in a day
decentralized_results_MC$incubators_needed <- ceiling(decentralized_results_MC$maximum_samples_per_day / inputs_MC$incubator_capacity)

# annual totals (all systems)
decentralized_results_MC$annual_equipment_cost_total <- ifelse(inputs_MC$discount_rate != 0, 
                                                               # incubator(s)
                                                               (((decentralized_results_MC$incubators_needed * inputs_MC$incubator_cost) *
                                                                   ((inputs_MC$discount_rate * (1 + inputs_MC$discount_rate)^inputs_MC$incubator_life) /
                                                                      ((1 + inputs_MC$discount_rate)^inputs_MC$incubator_life - 1))) +
                                                                  # membrane filtration equipment
                                                                  ((inputs_MC$equipment_cost) * 
                                                                     ((inputs_MC$discount_rate * (1 + inputs_MC$discount_rate)^inputs_MC$equipment_life) /
                                                                        ((1 + inputs_MC$discount_rate)^inputs_MC$equipment_life - 1))) +
                                                                  # autoclave
                                                                  ((inputs_MC$autoclave_cost) *
                                                                     ((inputs_MC$discount_rate * (1 + inputs_MC$discount_rate)^inputs_MC$autoclave_life) /
                                                                        ((1 + inputs_MC$discount_rate)^inputs_MC$autoclave_life - 1))) +
                                                                  # lab rental
                                                                  ((inputs_MC$lab_monthly_rent*ifelse(decentralized_results_MC$analysis_staff_days_per_month/inputs_MC$workdays_month > 1,
                                                                                                      1, decentralized_results_MC$analysis_staff_days_per_month/inputs_MC$workdays_month))*12)) * inputs_MC$N_points,
                                                               (decentralized_results_MC$incubators_needed * inputs_MC$incubator_cost / inputs_MC$incubator_life +
                                                                  inputs_MC$equipment_cost / inputs_MC$equipment_life +
                                                                  inputs_MC$autoclave_cost / inputs_MC$autoclave_life) +
                                                                 ((inputs_MC$lab_monthly_rent*ifelse(decentralized_results_MC$analysis_staff_days_per_month/inputs_MC$workdays_month > 1,
                                                                                                       1, decentralized_results_MC$analysis_staff_days_per_month/inputs_MC$workdays_month))*12) * inputs_MC$N_points)
  
decentralized_results_MC$annual_consumable_cost_total <- ((inputs_MC$N_annual_samples * inputs_MC$N_points) * 
                                                              inputs_MC$consumable_cost * (1+inputs_MC$consumable_markup) * (1+inputs_MC$QC_markup))

decentralized_results_MC$annual_labor_cost_total <- (((decentralized_results_MC$sampling_staff_days_per_month / inputs_MC$workdays_month) *
                                                        inputs_MC$wage_decentralized) +
                                                       ((decentralized_results_MC$analysis_staff_days_per_month / inputs_MC$workdays_month) *
                                                          inputs_MC$wage_decentralized)) * 12 * decentralized_results_MC$points_per_month
decentralized_results_MC$annual_transport_cost_total <- 0
decentralized_results_MC$annual_overall_cost_total <- (decentralized_results_MC$annual_equipment_cost_total + decentralized_results_MC$annual_consumable_cost_total +
                                                         decentralized_results_MC$annual_labor_cost_total + decentralized_results_MC$annual_transport_cost_total)

# totals per test
decentralized_results_MC$equipment_cost_per_test <- decentralized_results_MC$annual_equipment_cost_total / (inputs_MC$N_annual_samples * inputs_MC$N_points)
decentralized_results_MC$consumable_cost_per_test <- decentralized_results_MC$annual_consumable_cost_total / (inputs_MC$N_annual_samples * inputs_MC$N_points)
decentralized_results_MC$labor_cost_per_test <- decentralized_results_MC$annual_labor_cost_total / (inputs_MC$N_annual_samples * inputs_MC$N_points)
decentralized_results_MC$transport_cost_per_test <- decentralized_results_MC$annual_transport_cost_total / (inputs_MC$N_annual_samples * inputs_MC$N_points)
decentralized_results_MC$overall_cost_per_test <- (decentralized_results_MC$equipment_cost_per_test + decentralized_results_MC$consumable_cost_per_test +
                                                     decentralized_results_MC$labor_cost_per_test + decentralized_results_MC$transport_cost_per_test)



# Results summary -----

# create data frame to store summarized results from all approaches
results_summary_MC_test <- data.frame(matrix(nrow = 5, ncol = 20))
colnames(results_summary_MC_test) <- c("Category", 
                                       "Centralized_average", "Mobile_average", "Semicentralized_average", "Decentralized_average", ".",
                                       "Centralized_25thPerc", "Mobile_25thPerc", "Semicentralized_25thPerc", "Decentralized_25thPerc", "..",
                                       "Centralized_50thPerc", "Mobile_50thPerc", "Semicentralized_50thPerc", "Decentralized_50thPerc", "...",
                                       "Centralized_75thPerc", "Mobile_75thPerc", "Semicentralized_75thPerc", "Decentralized_75thPerc")

results_summary_MC_test$Category <- c("Total cost per test (USD)", 
                                      "Equipment cost per test (USD)",
                                      "Consumable cost per test (USD)", 
                                      "Labor cost per test (USD)",
                                      "Transport cost per test (USD)")

# calculate mean cost per test
results_summary_MC_test$Centralized_average[1] <- mean(centralized_results_MC$overall_cost_per_test[is.finite(centralized_results_MC$overall_cost_per_test)], na.rm = TRUE)
results_summary_MC_test$Centralized_average[2] <- mean(centralized_results_MC$equipment_cost_per_test[is.finite(centralized_results_MC$equipment_cost_per_test)], na.rm = TRUE)
results_summary_MC_test$Centralized_average[3] <- mean(centralized_results_MC$consumable_cost_per_test[is.finite(centralized_results_MC$consumable_cost_per_test)], na.rm = TRUE)
results_summary_MC_test$Centralized_average[4] <- mean(centralized_results_MC$labor_cost_per_test[is.finite(centralized_results_MC$labor_cost_per_test)], na.rm = TRUE)
results_summary_MC_test$Centralized_average[5] <- mean(centralized_results_MC$transport_cost_per_test[is.finite(centralized_results_MC$transport_cost_per_test)], na.rm = TRUE)

results_summary_MC_test$Mobile_average[1] <- mean(mobile_results_MC$overall_cost_per_test[is.finite(mobile_results_MC$overall_cost_per_test)], na.rm = TRUE)
results_summary_MC_test$Mobile_average[2] <- mean(mobile_results_MC$equipment_cost_per_test[is.finite(mobile_results_MC$equipment_cost_per_test)], na.rm = TRUE)
results_summary_MC_test$Mobile_average[3] <- mean(mobile_results_MC$consumable_cost_per_test[is.finite(mobile_results_MC$consumable_cost_per_test)], na.rm = TRUE)
results_summary_MC_test$Mobile_average[4] <- mean(mobile_results_MC$labor_cost_per_test[is.finite(mobile_results_MC$labor_cost_per_test)], na.rm = TRUE)
results_summary_MC_test$Mobile_average[5] <- mean(mobile_results_MC$transport_cost_per_test[is.finite(mobile_results_MC$transport_cost_per_test)], na.rm = TRUE)

results_summary_MC_test$Semicentralized_average[1] <- mean(semicentralized_results_MC$overall_cost_per_test[is.finite(semicentralized_results_MC$overall_cost_per_test)], na.rm = TRUE)
results_summary_MC_test$Semicentralized_average[2] <- mean(semicentralized_results_MC$equipment_cost_per_test[is.finite(semicentralized_results_MC$equipment_cost_per_test)], na.rm = TRUE)
results_summary_MC_test$Semicentralized_average[3] <- mean(semicentralized_results_MC$consumable_cost_per_test[is.finite(semicentralized_results_MC$consumable_cost_per_test)], na.rm = TRUE)
results_summary_MC_test$Semicentralized_average[4] <- mean(semicentralized_results_MC$labor_cost_per_test[is.finite(semicentralized_results_MC$labor_cost_per_test)], na.rm = TRUE)
results_summary_MC_test$Semicentralized_average[5] <- mean(semicentralized_results_MC$transport_cost_per_test[is.finite(semicentralized_results_MC$transport_cost_per_test)], na.rm = TRUE)

results_summary_MC_test$Decentralized_average[1] <- mean(decentralized_results_MC$overall_cost_per_test[is.finite(decentralized_results_MC$overall_cost_per_test)], na.rm = TRUE)
results_summary_MC_test$Decentralized_average[2] <- mean(decentralized_results_MC$equipment_cost_per_test[is.finite(decentralized_results_MC$equipment_cost_per_test)], na.rm = TRUE)
results_summary_MC_test$Decentralized_average[3] <- mean(decentralized_results_MC$consumable_cost_per_test[is.finite(decentralized_results_MC$consumable_cost_per_test)], na.rm = TRUE)
results_summary_MC_test$Decentralized_average[4] <- mean(decentralized_results_MC$labor_cost_per_test[is.finite(decentralized_results_MC$labor_cost_per_test)], na.rm = TRUE)
results_summary_MC_test$Decentralized_average[5] <- mean(decentralized_results_MC$transport_cost_per_test[is.finite(decentralized_results_MC$transport_cost_per_test)], na.rm = TRUE)

# order simulations from low to high cost per test to calculate percentile values
centralized_results_MC_stack_all <- centralized_results_MC[order(centralized_results_MC$overall_cost_per_test),]
mobile_results_MC_stack_all <- mobile_results_MC[order(mobile_results_MC$overall_cost_per_test),]
semicentralized_results_MC_stack_all <- semicentralized_results_MC[order(semicentralized_results_MC$overall_cost_per_test),]
decentralized_results_MC_stack_all <- decentralized_results_MC[order(decentralized_results_MC$overall_cost_per_test),]

# calculate percentile values, accounting for infeasible scenarios (these essentially have infinite cost)
results_summary_MC_test$Centralized_25thPerc[1] <- mean(centralized_results_MC_stack_all$overall_cost_per_test[2500:2501])
results_summary_MC_test$Centralized_50thPerc[1] <- mean(centralized_results_MC_stack_all$overall_cost_per_test[5000:5001])
results_summary_MC_test$Centralized_75thPerc[1] <- mean(centralized_results_MC_stack_all$overall_cost_per_test[7500:7501])

results_summary_MC_test$Mobile_25thPerc[1] <- mean(mobile_results_MC_stack_all$overall_cost_per_test[2500:2501])
results_summary_MC_test$Mobile_50thPerc[1] <- mean(mobile_results_MC_stack_all$overall_cost_per_test[5000:5001])
results_summary_MC_test$Mobile_75thPerc[1] <- mean(mobile_results_MC_stack_all$overall_cost_per_test[7500:7501])

results_summary_MC_test$Semicentralized_25thPerc[1] <- mean(semicentralized_results_MC_stack_all$overall_cost_per_test[2500:2501])
results_summary_MC_test$Semicentralized_50thPerc[1] <- mean(semicentralized_results_MC_stack_all$overall_cost_per_test[5000:5001])
results_summary_MC_test$Semicentralized_75thPerc[1] <- mean(semicentralized_results_MC_stack_all$overall_cost_per_test[7500:7501])

results_summary_MC_test$Decentralized_25thPerc[1] <- mean(decentralized_results_MC_stack_all$overall_cost_per_test[2500:2501])
results_summary_MC_test$Decentralized_50thPerc[1] <- mean(decentralized_results_MC_stack_all$overall_cost_per_test[5000:5001])
results_summary_MC_test$Decentralized_75thPerc[1] <- mean(decentralized_results_MC_stack_all$overall_cost_per_test[7500:7501])


results_summary_MC_test$Centralized_25thPerc[2] <- mean(centralized_results_MC_stack_all$equipment_cost_per_test[2500:2501])
results_summary_MC_test$Centralized_50thPerc[2] <- mean(centralized_results_MC_stack_all$equipment_cost_per_test[5000:5001])
results_summary_MC_test$Centralized_75thPerc[2] <- mean(centralized_results_MC_stack_all$equipment_cost_per_test[7500:7501])

results_summary_MC_test$Mobile_25thPerc[2] <- mean(mobile_results_MC_stack_all$equipment_cost_per_test[2500:2501])
results_summary_MC_test$Mobile_50thPerc[2] <- mean(mobile_results_MC_stack_all$equipment_cost_per_test[5000:5001])
results_summary_MC_test$Mobile_75thPerc[2] <- mean(mobile_results_MC_stack_all$equipment_cost_per_test[7500:7501])

results_summary_MC_test$Semicentralized_25thPerc[2] <- mean(semicentralized_results_MC_stack_all$equipment_cost_per_test[2500:2501])
results_summary_MC_test$Semicentralized_50thPerc[2] <- mean(semicentralized_results_MC_stack_all$equipment_cost_per_test[5000:5001])
results_summary_MC_test$Semicentralized_75thPerc[2] <- mean(semicentralized_results_MC_stack_all$equipment_cost_per_test[7500:7501])

results_summary_MC_test$Decentralized_25thPerc[2] <- mean(decentralized_results_MC_stack_all$equipment_cost_per_test[2500:2501])
results_summary_MC_test$Decentralized_50thPerc[2] <- mean(decentralized_results_MC_stack_all$equipment_cost_per_test[5000:5001])
results_summary_MC_test$Decentralized_75thPerc[2] <- mean(decentralized_results_MC_stack_all$equipment_cost_per_test[7500:7501])


results_summary_MC_test$Centralized_25thPerc[3] <- mean(centralized_results_MC_stack_all$consumable_cost_per_test[2500:2501])
results_summary_MC_test$Centralized_50thPerc[3] <- mean(centralized_results_MC_stack_all$consumable_cost_per_test[5000:5001])
results_summary_MC_test$Centralized_75thPerc[3] <- mean(centralized_results_MC_stack_all$consumable_cost_per_test[7500:7501])

results_summary_MC_test$Mobile_25thPerc[3] <- mean(mobile_results_MC_stack_all$consumable_cost_per_test[2500:2501])
results_summary_MC_test$Mobile_50thPerc[3] <- mean(mobile_results_MC_stack_all$consumable_cost_per_test[5000:5001])
results_summary_MC_test$Mobile_75thPerc[3] <- mean(mobile_results_MC_stack_all$consumable_cost_per_test[7500:7501])

results_summary_MC_test$Semicentralized_25thPerc[3] <- mean(semicentralized_results_MC_stack_all$consumable_cost_per_test[2500:2501])
results_summary_MC_test$Semicentralized_50thPerc[3] <- mean(semicentralized_results_MC_stack_all$consumable_cost_per_test[5000:5001])
results_summary_MC_test$Semicentralized_75thPerc[3] <- mean(semicentralized_results_MC_stack_all$consumable_cost_per_test[7500:7501])

results_summary_MC_test$Decentralized_25thPerc[3] <- mean(decentralized_results_MC_stack_all$consumable_cost_per_test[2500:2501])
results_summary_MC_test$Decentralized_50thPerc[3] <- mean(decentralized_results_MC_stack_all$consumable_cost_per_test[5000:5001])
results_summary_MC_test$Decentralized_75thPerc[3] <- mean(decentralized_results_MC_stack_all$consumable_cost_per_test[7500:7501])


results_summary_MC_test$Centralized_25thPerc[4] <- mean(centralized_results_MC_stack_all$labor_cost_per_test[2500:2501])
results_summary_MC_test$Centralized_50thPerc[4] <- mean(centralized_results_MC_stack_all$labor_cost_per_test[5000:5001])
results_summary_MC_test$Centralized_75thPerc[4] <- mean(centralized_results_MC_stack_all$labor_cost_per_test[7500:7501])

results_summary_MC_test$Mobile_25thPerc[4] <- mean(mobile_results_MC_stack_all$labor_cost_per_test[2500:2501])
results_summary_MC_test$Mobile_50thPerc[4] <- mean(mobile_results_MC_stack_all$labor_cost_per_test[5000:5001])
results_summary_MC_test$Mobile_75thPerc[4] <- mean(mobile_results_MC_stack_all$labor_cost_per_test[7500:7501])

results_summary_MC_test$Semicentralized_25thPerc[4] <- mean(semicentralized_results_MC_stack_all$labor_cost_per_test[2500:2501])
results_summary_MC_test$Semicentralized_50thPerc[4] <- mean(semicentralized_results_MC_stack_all$labor_cost_per_test[5000:5001])
results_summary_MC_test$Semicentralized_75thPerc[4] <- mean(semicentralized_results_MC_stack_all$labor_cost_per_test[7500:7501])

results_summary_MC_test$Decentralized_25thPerc[4] <- mean(decentralized_results_MC_stack_all$labor_cost_per_test[2500:2501])
results_summary_MC_test$Decentralized_50thPerc[4] <- mean(decentralized_results_MC_stack_all$labor_cost_per_test[5000:5001])
results_summary_MC_test$Decentralized_75thPerc[4] <- mean(decentralized_results_MC_stack_all$labor_cost_per_test[7500:7501])


results_summary_MC_test$Centralized_25thPerc[5] <- mean(centralized_results_MC_stack_all$transport_cost_per_test[2500:2501])
results_summary_MC_test$Centralized_50thPerc[5] <- mean(centralized_results_MC_stack_all$transport_cost_per_test[5000:5001])
results_summary_MC_test$Centralized_75thPerc[5] <- mean(centralized_results_MC_stack_all$transport_cost_per_test[7500:7501])

results_summary_MC_test$Mobile_25thPerc[5] <- mean(mobile_results_MC_stack_all$transport_cost_per_test[2500:2501])
results_summary_MC_test$Mobile_50thPerc[5] <- mean(mobile_results_MC_stack_all$transport_cost_per_test[5000:5001])
results_summary_MC_test$Mobile_75thPerc[5] <- mean(mobile_results_MC_stack_all$transport_cost_per_test[7500:7501])

results_summary_MC_test$Semicentralized_25thPerc[5] <- mean(semicentralized_results_MC_stack_all$transport_cost_per_test[2500:2501])
results_summary_MC_test$Semicentralized_50thPerc[5] <- mean(semicentralized_results_MC_stack_all$transport_cost_per_test[5000:5001])
results_summary_MC_test$Semicentralized_75thPerc[5] <- mean(semicentralized_results_MC_stack_all$transport_cost_per_test[7500:7501])

results_summary_MC_test$Decentralized_25thPerc[5] <- mean(decentralized_results_MC_stack_all$transport_cost_per_test[2500:2501])
results_summary_MC_test$Decentralized_50thPerc[5] <- mean(decentralized_results_MC_stack_all$transport_cost_per_test[5000:5001])
results_summary_MC_test$Decentralized_75thPerc[5] <- mean(decentralized_results_MC_stack_all$transport_cost_per_test[7500:7501])




# Best scenario - Monte Carlo -----

# initialize a vector to record which testing approach is lowest cost in each simulation
best_scenario <- rep(NA, times = N)

# determine which testing approach is lowest cost in each simulation
for (i in 1:length(best_scenario)) {
  if (min(centralized_results_MC$overall_cost_per_test[i],
          mobile_results_MC$overall_cost_per_test[i],
          semicentralized_results_MC$overall_cost_per_test[i],
          decentralized_results_MC$overall_cost_per_test[i], na.rm = TRUE) == centralized_results_MC$overall_cost_per_test[i] &
      is.finite(centralized_results_MC$overall_cost_per_test[i])) {
    best_scenario[i] <- "Centralized"
  } else if (min(centralized_results_MC$overall_cost_per_test[i],
                 mobile_results_MC$overall_cost_per_test[i],
                 semicentralized_results_MC$overall_cost_per_test[i],
                 decentralized_results_MC$overall_cost_per_test[i], na.rm = TRUE) == mobile_results_MC$overall_cost_per_test[i] &
             is.finite(mobile_results_MC$overall_cost_per_test[i])) {
    best_scenario[i] <- "Mobile lab"
  } else if (min(centralized_results_MC$overall_cost_per_test[i],
                 mobile_results_MC$overall_cost_per_test[i],
                 semicentralized_results_MC$overall_cost_per_test[i],
                 decentralized_results_MC$overall_cost_per_test[i], na.rm = TRUE) == semicentralized_results_MC$overall_cost_per_test[i] &
             is.finite(semicentralized_results_MC$overall_cost_per_test[i])) {
    best_scenario[i] <- "Semi-Centralized"
  } else if (min(centralized_results_MC$overall_cost_per_test[i],
                 mobile_results_MC$overall_cost_per_test[i],
                 semicentralized_results_MC$overall_cost_per_test[i],
                 decentralized_results_MC$overall_cost_per_test[i], na.rm = TRUE) == decentralized_results_MC$overall_cost_per_test[i] &
             is.finite(decentralized_results_MC$overall_cost_per_test[i])) {
    best_scenario[i] <- "Decentralized"
  }
}

# convert to a factor variable for easier plotting
best_scenario <- factor(best_scenario)
  
# create a data frame to summarize the results where each approach is lowest cost
best_scenario_summary <- data.frame(matrix(nrow = ncol(inputs_MC)+1, ncol = 7))
colnames(best_scenario_summary) <- c("Input_variable", "Average_statistic", "Centralized", "Mobile", "Semicentralized", "Decentralized", "p_anova")

# record % of simulations where each approach is lowest cost
best_scenario_summary$Input_variable[2:nrow(best_scenario_summary)] <- inputs$Input_variable
best_scenario_summary$Average_statistic <- c("Fraction best", colnames(inputs_MC))
best_scenario_summary$Centralized[1] <- sum(best_scenario == "Centralized", na.rm = TRUE) / sum(!is.na(best_scenario))
best_scenario_summary$Mobile[1] <- sum(best_scenario == "Mobile lab", na.rm = TRUE) / sum(!is.na(best_scenario))
best_scenario_summary$Semicentralized[1] <- sum(best_scenario == "Semi-Centralized", na.rm = TRUE) / sum(!is.na(best_scenario))
best_scenario_summary$Decentralized[1] <- sum(best_scenario == "Decentralized", na.rm = TRUE) / sum(!is.na(best_scenario))

# record average input variables where each approach is lowest cost
for (i in 1:ncol(inputs_MC)) {
  best_scenario_summary$Centralized[i+1] <- mean(inputs_MC[best_scenario == "Centralized",i], na.rm = TRUE)
  best_scenario_summary$Mobile[i+1] <- mean(inputs_MC[best_scenario == "Mobile lab",i], na.rm = TRUE)
  best_scenario_summary$Semicentralized[i+1] <- mean(inputs_MC[best_scenario == "Semi-Centralized",i], na.rm = TRUE)
  best_scenario_summary$Decentralized[i+1] <- mean(inputs_MC[best_scenario == "Decentralized",i], na.rm = TRUE)
  best_scenario_summary$p_anova[i+1] <- summary(aov(inputs_MC[,i] ~ best_scenario))[[1]][1,5]
}

# remove any variables that don't change
best_scenario_summary <- best_scenario_summary[!is.nan(best_scenario_summary$p_anova),]

# order the input parameters from the greatest to least relative difference between average values
best_scenario_summary$stdev_norm <- NA

best_scenario_summary$range_min[2:nrow(best_scenario_summary)] <- inputs$Minimum_value
best_scenario_summary$range_max[2:nrow(best_scenario_summary)] <- inputs$Maximum_value

best_scenario_summary$Centralized_norm[2:nrow(best_scenario_summary)] <- ((best_scenario_summary$Centralized[2:nrow(best_scenario_summary)] - best_scenario_summary$range_min[2:nrow(best_scenario_summary)]) / 
                                                                            (best_scenario_summary$range_max[2:nrow(best_scenario_summary)] - best_scenario_summary$range_min[2:nrow(best_scenario_summary)]))
best_scenario_summary$Mobile_norm[2:nrow(best_scenario_summary)] <- ((best_scenario_summary$Mobile[2:nrow(best_scenario_summary)] - best_scenario_summary$range_min[2:nrow(best_scenario_summary)]) / 
                                                                            (best_scenario_summary$range_max[2:nrow(best_scenario_summary)] - best_scenario_summary$range_min[2:nrow(best_scenario_summary)]))
best_scenario_summary$Semicentralized_norm[2:nrow(best_scenario_summary)] <- ((best_scenario_summary$Semicentralized[2:nrow(best_scenario_summary)] - best_scenario_summary$range_min[2:nrow(best_scenario_summary)]) / 
                                                                            (best_scenario_summary$range_max[2:nrow(best_scenario_summary)] - best_scenario_summary$range_min[2:nrow(best_scenario_summary)]))
best_scenario_summary$Decentralized_norm[2:nrow(best_scenario_summary)] <- ((best_scenario_summary$Decentralized[2:nrow(best_scenario_summary)] - best_scenario_summary$range_min[2:nrow(best_scenario_summary)]) / 
                                                                            (best_scenario_summary$range_max[2:nrow(best_scenario_summary)] - best_scenario_summary$range_min[2:nrow(best_scenario_summary)]))

for (i in 2:nrow(best_scenario_summary)) {
  best_scenario_summary$stdev_norm[i] <- sd(best_scenario_summary[i,c("Centralized_norm", "Mobile_norm",
                                                                      "Semicentralized_norm", "Decentralized_norm")])
}

best_scenario_summary <- best_scenario_summary[order(-best_scenario_summary$stdev_norm),]
best_scenario_summary <- best_scenario_summary[c(nrow(best_scenario_summary), 1:(nrow(best_scenario_summary)-1)),
                                               c("Input_variable", "Average_statistic", "Centralized", "Mobile", "Semicentralized", "Decentralized")]

# Spearman correlations -----

# create data frame to calculate Spearman correlation coefficients for each testing approach
spearman_correlations <- data.frame(cor(inputs_MC, 
                            cbind(centralized_results_MC$overall_cost_per_test,
                                  mobile_results_MC$overall_cost_per_test,
                                  semicentralized_results_MC$overall_cost_per_test,
                                  decentralized_results_MC$overall_cost_per_test), 
                            method = "spearman", use = "complete.obs"))
colnames(spearman_correlations) <- c("Centralized", "Mobile", "Semicentralized", "Decentralized")

# remove constant inputs 
spearman_correlations <- spearman_correlations[!is.na(spearman_correlations$Centralized),]

# order input parameters from highest to lowest maximum coefficient (abs value)
spearman_correlations$max <- NA
for (i in 1:nrow(spearman_correlations)) {
  spearman_correlations$max[i] <- max(abs(spearman_correlations[i,1:4]))
}
spearman_correlations <- spearman_correlations[order(-spearman_correlations$max),]
spearman_correlations <- subset(spearman_correlations, select = -c(max))

# Store variable names and descriptions for ease of reference
spearman_correlations$Variable_name <- row.names(spearman_correlations)
for (i in 1:nrow(spearman_correlations)) {
  spearman_correlations$Input_variable[i] <- inputs$Input_variable[inputs$Variable_name == spearman_correlations$Variable_name[i]]
}
spearman_correlations <- spearman_correlations[, c("Input_variable", "Variable_name",
                                                   "Centralized", "Mobile", "Semicentralized", "Decentralized")]

# Export results to Excel -----

# create list of data frames that contain results to export
results_list <- list("Simulation results" = results_summary_MC_test,
                     "Best scenario results" = best_scenario_summary,
                     "Spearman coefficients" = spearman_correlations)

# export as Excel spreadsheet
wb <- write.xlsx(results_list, "WQtesting_cost_results.xlsx", overwrite = TRUE,
                 rowNames = FALSE)
saveWorkbook(wb, "WQtesting_cost_results.xlsx", overwrite = TRUE)


# Cost summary graph -----

# This section creates and exports a stacked bar graph, showing cost per test for each testing approach
#  - total height of bar: median cost per test
#  - lower and upper error bars: 25th and 75th percentile costs per test
#  - stacked sections of bar: contributions from equipment, consumables, labor, and transport

# Cost breakdowns
cost_breakdown <- melt(results_summary_MC_test[2:5,c("Category", "Centralized_50thPerc", "Mobile_50thPerc",
                                                     "Semicentralized_50thPerc", "Decentralized_50thPerc")], id.vars = c("Category"))
levels(cost_breakdown$variable) <- c("Centralized", "Mobile lab", "Semi-centralized", "Decentralized")

# range: 25th and 75th percentile values
cost_breakdown_range <- data.frame("typology" = c("Centralized", "Mobile lab", "Semi-centralized", "Decentralized"), 
                                   "range_low" = unlist(results_summary_MC_test[1,c("Centralized_25thPerc", "Mobile_25thPerc",
                                                                                    "Semicentralized_25thPerc", "Decentralized_25thPerc")]),
                                   "range_high" = unlist(results_summary_MC_test[1,c("Centralized_75thPerc", "Mobile_75thPerc",
                                                                                     "Semicentralized_75thPerc", "Decentralized_75thPerc")]))

cost_breakdown_plot <- (ggplot() +
                          geom_bar(data = cost_breakdown, aes(x=variable, y=value, fill=Category), 
                                   position = "stack", stat = "identity", color = "black", width = 0.75) +
                          geom_errorbar(data = cost_breakdown_range, aes(x=typology, ymin=range_low, ymax=range_high), 
                                        color = "black", width = 0.5) +
                          scale_x_discrete(name = NULL) +
                          scale_y_continuous(name = "Total cost per water test (USD)",
                                             limits = c(0,max(cost_breakdown_range$range_high)),
                                             expand = expansion(mult = c(0, 0.05))) +
                          scale_fill_discrete(name = NULL) +
                          theme(plot.background = element_rect(fill = "white"),
                                panel.background = element_rect(fill = "white"),
                                panel.border = element_rect(fill = "NA", color = "black"),
                                axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
                                plot.title = element_text(hjust = 0.5, size = 11),
                                plot.subtitle = element_text(hjust = 0.5, size = 10),
                                legend.position = "right",
                                text = element_text(size=11)))
ggsave("WQtesting_cost_plot.png", plot = cost_breakdown_plot, dpi = 300, height = 4, width = 5, units = "in")


