# Rscript      : chop_gmm_model.R
# Author       : David Grethlein
# Author Email : david@diagnosticdriving.com
# Description  : An ad hoc script that receives named arguments from the
#                command line, parses the arguments as appropriate and uses
#                them as inputs for route-specific clustering using the GMM
#                saved as a serialized model, provided by the CHOP team.
#                The model is that which is described in the published
#                Journal of Transportation Research: Part F
#                https://doi.org/10.1016/j.trf.2022.04.009


######################################################################
# Install dependencies for parsing command line arguments if needed
######################################################################
# For `checkpoint()` function
if (!suppressMessages(require("checkpoint"))) {
  suppressMessages(install.packages("checkpoint",
                                    repos="http://cran.us.r-project.org"))
  suppressMessages(library("checkpoint"))
  suppressMessages(checkpoint("2023-15-02",
                              r_version="4.2.2"))
}

# For `commandArgs()` function
if (!suppressMessages(require("R.utils"))) {
  suppressMessages(install.packages("R.utils"))
  suppressMessages(library("R.utils"))
}

# For `str_extract()` function
if (!suppressMessages(require("stringr"))) {
  suppressMessages(install.packages("stringr"))
  suppressMessages(library("stringr"))
}

# For `select()` function
if (!suppressMessages(require("dplyr"))) {
  suppressMessages(install.packages("dplyr"))
  suppressMessages(library("dplyr"))
}

# For `predict()` function
if (!suppressMessages(require("mclust"))) {
  suppressMessages(install.packages("mclust"))
  suppressMessages(library("mclust"))
}


######################################################################
# Function for type-casting command line arguments into R variables
######################################################################
get_as_int <- function(str_value) {
  # Attempts to extract an integer sub-string (sequence of consecutive
  # digits) from the provided string argument.
  int_str <- str_extract(str_value, "-*\\d+\\.*\\d*")

  # If such an integer sub-string exists, attempt a type-cast and return.
  if (!is.na(int_str)) {
    as.integer(int_str)
  }
}


get_as_float <- function(str_value) {
  # Attempts to extract a floating point value sub-string (sequence of
  # consecutive digits potentially with a decimal point) from the
  # provided string argument.
  float_str <- str_extract(str_value, "-*\\d+\\.*\\d*")

  # If such a floating point value sub-string exists, attempt a
  # type-cast and return.
  if (!is.na(float_str)) {
    as.numeric(float_str)
  }
}


get_route_int <- function(str_value) {
  # Attempts to extract the route integer sub-string from the provided
  # string argument as a sequence of consecutive digits.
  route_int_str <- str_extract(str_value, "[:digit:]+")

  # If such an integer sub-string exists, attempt a type-cast
  if (!is.na(route_int_str)) {
    route_int <- as.integer(route_int_str)

    # Only return if the extracted integer is in the range 1-10
    if (route_int < 11 && route_int > 0) {
      route_int
    }
  }
}


######################################################################
# Function for parsing command line arguments into R variables
######################################################################
parse_args <- function(cmd_args) {

  # Indicator of which VDT route was driven (`City_1`, `City_2`, ..., `City_10`)
  route_int <- get_route_int(cmd_args$"Route"[1])

  # Number of non-collision-related critical driving errors made (in tallies)
  REPLAY_Major_Red_Light <- get_as_int(cmd_args$"REPLAY_Major_Red_Light"[1])
  REPLAY_Major_Stop_Sign <- get_as_int(cmd_args$"REPLAY_Major_Stop_Sign"[1])
  REPLAY_Teleport <- get_as_int(cmd_args$"REPLAY_Teleport"[1])

  # Number of collision-related critical driving errors made (in tallies)
  GLOBAL_num_ped_collisions <- get_as_int(cmd_args$"GLOBAL_num_ped_collisions"[1])
  GLOBAL_num_veh_collisions <- get_as_int(cmd_args$"GLOBAL_num_veh_collisions"[1])
  GLOBAL_num_misc_obj_collisions <- get_as_int(cmd_args$"GLOBAL_num_misc_obj_collisions"[1])
  GLOBAL_num_static_collisions <- get_as_int(cmd_args$"GLOBAL_num_static_collisions"[1])

  # Amount of time/distance driven along VDT route in a `near-collision` scenario
  GLOBAL_time_spent_ttc_lt_5_seconds <- get_as_float(cmd_args$"GLOBAL_time_spent_ttc_lt_5_seconds"[1])
  GLOBAL_time_spent_ttc_lt_3_seconds <- get_as_float(cmd_args$"GLOBAL_time_spent_ttc_lt_3_seconds"[1])
  GLOBAL_miles_driven_ttc_lt_5_seconds <- get_as_float(cmd_args$"GLOBAL_miles_driven_ttc_lt_5_seconds"[1])
  GLOBAL_miles_driven_ttc_lt_3_seconds <- get_as_float(cmd_args$"GLOBAL_miles_driven_ttc_lt_3_seconds"[1])

  # Length of the assessment drive (in seconds)
  GLOBAL_ts_assessment_duration <- get_as_float(cmd_args$"GLOBAL_ts_assessment_duration"[1])

  # Speed variables (in mph)
  GLOBAL_max_mph_speed <- get_as_float(cmd_args$"GLOBAL_max_mph_speed"[1])
  GLOBAL_mean_mph_speed <- get_as_float(cmd_args$"GLOBAL_mean_mph_speed"[1])
  GLOBAL_stdev_mph_speed <- get_as_float(cmd_args$"GLOBAL_stdev_mph_speed"[1])

  # Acceleration (first derivative of speed) variables (in mph/second)
  GLOBAL_max_mph_acceleration <- get_as_float(cmd_args$"GLOBAL_max_mph_acceleration"[1])
  GLOBAL_mean_mph_acceleration <- get_as_float(cmd_args$"GLOBAL_mean_mph_acceleration"[1])
  GLOBAL_stdev_mph_acceleration <- get_as_float(cmd_args$"GLOBAL_stdev_mph_acceleration"[1])

  # Jerk (first derivative of acceleration/second derivative of speed) variables (in mph/second^2)
  GLOBAL_max_mph_jerk <- get_as_float(cmd_args$"GLOBAL_max_mph_jerk"[1])
  GLOBAL_mean_mph_jerk <- get_as_float(cmd_args$"GLOBAL_mean_mph_jerk"[1])
  GLOBAL_stdev_mph_jerk <- get_as_float(cmd_args$"GLOBAL_stdev_mph_jerk"[1])

  # Throttle/Accelerator pedal usage variables (in percent pedal depression [0.0,1.0])
  GLOBAL_max_throttle <- get_as_float(cmd_args$"GLOBAL_max_throttle"[1])
  GLOBAL_mean_throttle <- get_as_float(cmd_args$"GLOBAL_mean_throttle"[1])
  GLOBAL_stdev_throttle <- get_as_float(cmd_args$"GLOBAL_stdev_throttle"[1])

  # Brake pedal usage variables (in percent pedal depression [0.0,1.0])
  GLOBAL_max_brake <- get_as_float(cmd_args$"GLOBAL_max_brake"[1])
  GLOBAL_mean_brake <- get_as_float(cmd_args$"GLOBAL_mean_brake"[1])
  GLOBAL_stdev_brake <- get_as_float(cmd_args$"GLOBAL_stdev_brake"[1])

  # Heading error variables
  # (in degrees difference between heading angle of vehicle and road-following direction [0.0, 180.0])
  GLOBAL_max_heading_error <- get_as_float(cmd_args$"GLOBAL_max_heading_error"[1])
  GLOBAL_mean_heading_error <- get_as_float(cmd_args$"GLOBAL_mean_heading_error"[1])
  GLOBAL_stdev_heading_error <- get_as_float(cmd_args$"GLOBAL_stdev_heading_error"[1])

  # Lateral offset from center of lane variables (in meters [0.0,inf))
  GLOBAL_max_lane_deviation <- get_as_float(cmd_args$"GLOBAL_max_lane_deviation"[1])
  GLOBAL_mean_lane_deviation <- get_as_float(cmd_args$"GLOBAL_mean_lane_deviation"[1])
  GLOBAL_stdev_lane_deviation <- get_as_float(cmd_args$"GLOBAL_stdev_lane_deviation"[1])

  # Lateral offset from center of road (in meters [0.0,inf))
  GLOBAL_max_distance_from_road_center <- get_as_float(cmd_args$"GLOBAL_max_distance_from_road_center"[1])
  GLOBAL_mean_distance_from_road_center <- get_as_float(cmd_args$"GLOBAL_mean_distance_from_road_center"[1])
  GLOBAL_stdev_distance_from_road_center <- get_as_float(cmd_args$"GLOBAL_stdev_distance_from_road_center"[1])

  ######################################################################
  # EDGE CASE: Anticipate mutated inputs for validating CHOP data
  ######################################################################
  Times_TTC_3_5 <- get_as_float(cmd_args$"Times_TTC_3_5"[1])
  Times_TTC_3 <- get_as_float(cmd_args$"Times_TTC_3"[1])
  Miles_TTC_3_5 <- get_as_float(cmd_args$"Miles_TTC_3_5"[1])
  total_number_of_failures_to_stop <- get_as_int(cmd_args$"total_number_of_failures_to_stop"[1])
  total_number_of_collisions <- get_as_int(cmd_args$"total_number_of_collisions"[1])
  ######################################################################

  # Organizes the parsed args into a dataframe comprised of a single row
  args_df <- data.frame(route_int = c(route_int),
                        REPLAY_Major_Red_Light = c(REPLAY_Major_Red_Light),
                        REPLAY_Major_Stop_Sign = c(REPLAY_Major_Stop_Sign),
                        REPLAY_Teleport = c(REPLAY_Teleport),
                        GLOBAL_num_ped_collisions = c(GLOBAL_num_ped_collisions),
                        GLOBAL_num_veh_collisions = c(GLOBAL_num_veh_collisions),
                        GLOBAL_num_misc_obj_collisions = c(GLOBAL_num_misc_obj_collisions),
                        GLOBAL_num_static_collisions = c(GLOBAL_num_static_collisions),
                        GLOBAL_time_spent_ttc_lt_5_seconds = c(GLOBAL_time_spent_ttc_lt_5_seconds),
                        GLOBAL_time_spent_ttc_lt_3_seconds = c(GLOBAL_time_spent_ttc_lt_3_seconds),
                        GLOBAL_miles_driven_ttc_lt_5_seconds = c(GLOBAL_miles_driven_ttc_lt_5_seconds),
                        GLOBAL_miles_driven_ttc_lt_3_seconds = c(GLOBAL_miles_driven_ttc_lt_3_seconds),
                        GLOBAL_ts_assessment_duration = c(GLOBAL_ts_assessment_duration),
                        GLOBAL_max_mph_speed = c(GLOBAL_max_mph_speed),
                        GLOBAL_mean_mph_speed = c(GLOBAL_mean_mph_speed),
                        GLOBAL_stdev_mph_speed = c(GLOBAL_stdev_mph_speed),
                        GLOBAL_max_mph_acceleration = c(GLOBAL_max_mph_acceleration),
                        GLOBAL_mean_mph_acceleration = c(GLOBAL_mean_mph_acceleration),
                        GLOBAL_stdev_mph_acceleration = c(GLOBAL_stdev_mph_acceleration),
                        GLOBAL_max_mph_jerk = c(GLOBAL_max_mph_jerk),
                        GLOBAL_mean_mph_jerk = c(GLOBAL_mean_mph_jerk),
                        GLOBAL_stdev_mph_jerk = c(GLOBAL_stdev_mph_jerk),
                        GLOBAL_max_throttle = c(GLOBAL_max_throttle),
                        GLOBAL_mean_throttle = c(GLOBAL_mean_throttle),
                        GLOBAL_stdev_throttle = c(GLOBAL_stdev_throttle),
                        GLOBAL_max_brake = c(GLOBAL_max_brake),
                        GLOBAL_mean_brake = c(GLOBAL_mean_brake),
                        GLOBAL_stdev_brake = c(GLOBAL_stdev_brake),
                        GLOBAL_max_heading_error = c(GLOBAL_max_heading_error),
                        GLOBAL_mean_heading_error = c(GLOBAL_mean_heading_error),
                        GLOBAL_stdev_heading_error = c(GLOBAL_stdev_heading_error),
                        GLOBAL_max_lane_deviation = c(GLOBAL_max_lane_deviation),
                        GLOBAL_mean_lane_deviation = c(GLOBAL_mean_lane_deviation),
                        GLOBAL_stdev_lane_deviation = c(GLOBAL_stdev_lane_deviation),
                        GLOBAL_max_distance_from_road_center = c(GLOBAL_max_distance_from_road_center),
                        GLOBAL_mean_distance_from_road_center = c(GLOBAL_mean_distance_from_road_center),
                        GLOBAL_stdev_distance_from_road_center = c(GLOBAL_stdev_distance_from_road_center),
                        Times_TTC_3_5 = c(Times_TTC_3_5),
                        Times_TTC_3 = c(Times_TTC_3),
                        Miles_TTC_3_5 = c(Miles_TTC_3_5),
                        total_number_of_failures_to_stop = c(total_number_of_failures_to_stop),
                        total_number_of_collisions = c(total_number_of_collisions))

  args_df
}

######################################################################
# Function for taking VDT variables and converting to GMM inputs
######################################################################
prep_gmm_inputs <- function(df, clust_means, clust_stdevs) {

  # Computes new non-VDT performance variables mutated GMM inputs
  df_transformed <- df %>%
    mutate(total_number_of_failures_to_stop = (REPLAY_Major_Red_Light +
                                               REPLAY_Major_Stop_Sign),
           total_number_of_collisions = (GLOBAL_num_veh_collisions +
                                         GLOBAL_num_ped_collisions +
                                         GLOBAL_num_misc_obj_collisions +
                                         GLOBAL_num_static_collisions),
           Times_TTC_3_5 = ((GLOBAL_time_spent_ttc_lt_5_seconds - GLOBAL_time_spent_ttc_lt_3_seconds)
                            / GLOBAL_ts_assessment_duration),
           Times_TTC_3 = GLOBAL_time_spent_ttc_lt_3_seconds / GLOBAL_ts_assessment_duration,
           Miles_TTC_3_5 = GLOBAL_miles_driven_ttc_lt_5_seconds - GLOBAL_miles_driven_ttc_lt_3_seconds) %>%

    # Select the desired subset of columns of data
    select(GLOBAL_max_mph_speed,
           GLOBAL_mean_mph_speed,
           GLOBAL_stdev_mph_speed,
           GLOBAL_max_mph_acceleration,
           GLOBAL_mean_mph_acceleration,
           GLOBAL_stdev_mph_acceleration,
           GLOBAL_max_mph_jerk,
           GLOBAL_mean_mph_jerk,
           GLOBAL_stdev_mph_jerk,
           GLOBAL_max_throttle,
           GLOBAL_mean_throttle,
           GLOBAL_stdev_throttle,
           GLOBAL_max_brake,
           GLOBAL_mean_brake,
           GLOBAL_stdev_brake,
           GLOBAL_max_heading_error,
           GLOBAL_mean_heading_error,
           GLOBAL_stdev_heading_error,
           GLOBAL_max_lane_deviation,
           GLOBAL_mean_lane_deviation,
           GLOBAL_stdev_lane_deviation,
           GLOBAL_max_distance_from_road_center,
           GLOBAL_mean_distance_from_road_center,
           GLOBAL_stdev_distance_from_road_center,
           Times_TTC_3_5,
           Times_TTC_3,
           Miles_TTC_3_5,
           GLOBAL_miles_driven_ttc_lt_3_seconds,
           total_number_of_failures_to_stop,
           REPLAY_Teleport,
           total_number_of_collisions,
           GLOBAL_ts_assessment_duration)

    # Applies the (normalization) route-specific scaling for the supplied data
    sweep(df_transformed, 2, as.vector(clust_means), FUN = "-") %>%
    sweep( 2, as.vector(clust_stdevs), FUN = "/")
}


######################################################################
# Parse command-line arguments
######################################################################
main_func <- function() {

  # Accepts command line arguments
  # cat("\nCommand line arguments:\n")
  cargs <- commandArgs(trailingOnly = TRUE, asValue = TRUE)

  # Parses and type-casts the command line arguments
  parsed_args_df <- parse_args(cargs)

  # Loads the pre-processing data provided by CHOP
  # (route-specific feature means and )
  load("preprocess.Rdata")

  # Loads the model and data from Ohio project:
  load("mix.Rdata")

  data_df <- parsed_args_df

  # Optionally scale the parsed VDT data by the route driven
  if (length(cargs$"scale_by_route"[1]) > 0) {
    # cat("\nScaling VDT inputs by route!\n")

    # Applies the mutation and route-specific scaling
    data_df <- prep_gmm_inputs(data_df,
                               cmean[data_df$route_int[1]],
                               csd[data_df$route_int[1]])

  }

  # Hard-select only the fields required
  data_df <- data_df %>%
             select(GLOBAL_max_mph_speed,
                    GLOBAL_mean_mph_speed,
                    GLOBAL_stdev_mph_speed,
                    GLOBAL_max_mph_acceleration,
                    GLOBAL_mean_mph_acceleration,
                    GLOBAL_stdev_mph_acceleration,
                    GLOBAL_max_mph_jerk,
                    GLOBAL_mean_mph_jerk,
                    GLOBAL_stdev_mph_jerk,
                    GLOBAL_max_throttle,
                    GLOBAL_mean_throttle,
                    GLOBAL_stdev_throttle,
                    GLOBAL_max_brake,
                    GLOBAL_mean_brake,
                    GLOBAL_stdev_brake,
                    GLOBAL_max_heading_error,
                    GLOBAL_mean_heading_error,
                    GLOBAL_stdev_heading_error,
                    GLOBAL_max_lane_deviation,
                    GLOBAL_mean_lane_deviation,
                    GLOBAL_stdev_lane_deviation,
                    GLOBAL_max_distance_from_road_center,
                    GLOBAL_mean_distance_from_road_center,
                    GLOBAL_stdev_distance_from_road_center,
                    Times_TTC_3_5,
                    Times_TTC_3,
                    Miles_TTC_3_5,
                    GLOBAL_miles_driven_ttc_lt_3_seconds,
                    total_number_of_failures_to_stop,
                    REPLAY_Teleport,
                    total_number_of_collisions,
                    GLOBAL_ts_assessment_duration)

  # Clusters the provided data
  predictions <- predict(mix,
                         data.matrix(data_df) %*% data.matrix(t(loadings)))

  # Formats the results for output
  output <- data.matrix(c(predictions$classification,
                          predictions$z))

  output
}

# Main function call upon script invokation
main_func()
