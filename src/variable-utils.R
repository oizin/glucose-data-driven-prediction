
#' Convert data.table time variables to datetime
#' Note: this function doesn't copy the data
#' (It returns nothing)
#' 
#'
convert_datetime <- function(dt,time_pattern="time") {
  time_vars <- names(dt)[grepl(time_pattern,names(dt))]
  for (i in 1:length(time_vars)) {
    cat("converting: ",time_vars[i],"\n")
    expr <- paste0(time_vars[i]," := lubridate::as_datetime(",time_vars[i],")")
    dt[,eval(parse(text = expr))]
  }
}

#' Add numeric time variables
#' 
#' Adds the following
#' tos_hr: hours passed since entry to ICU (tos = time of stay)
#' timeofday_hr: time of day in hours (e.g. 12:30pm = 12.5 or 6pm = 18)
#' med_starttime_tos_hr: medicine start time in hours since entry to ICU
#' med_endtime_tos_hr: medicine end time in hours since entry to ICU
#' los_dy: length of stay in ICU in days
#'
add_numeric_timevars <- function(dt) {
  cat("adding numeric time variables\n")
  dt[,tos_hr := round(as.numeric(difftime(time,intime,units = "mins"))/60,2)]
  dt[,timeofday_hr := round(lubridate::hour(time) + 
                                       lubridate::minute(time)/60,2)]
  dt[,med_starttime_tos_hr := round(as.numeric(difftime(med_starttime,
                                                                 intime,units = "mins"))/60,2)]
  dt[,med_endtime_tos_hr := round(as.numeric(difftime(med_endtime,
                                                               intime,units = "mins"))/60,2)]
  dt[,los_dy := as.numeric(difftime(outtime,intime,
                                          units = "mins"))/(60*24)]
}

#' Add the insulin variables
#'
#'
add_insulinvars <- function(dt) {
  # infusion
  cat("creating insulin infusion variables\n")
  dt[insulin_admin == "INFUSION",insulin_infusion_rate := insulin_rate]
  dt[insulin_admin == "INFUSION",insulin_infusion_amount := insulin_amount]
  dt[insulin_admin == "INFUSION",insulin_infusion_start := med_starttime_tos_hr]
  dt[insulin_admin == "INFUSION",insulin_infusion_end := med_endtime_tos_hr]
  dt[insulin_admin == "INFUSION",insulin_infusion_type := insulin_type]
  # bolus push
  cat("creating insulin bolus push variables\n")
  dt[insulin_admin == "BOLUS_PUSH",insulin_push_amount := insulin_amount]
  dt[insulin_admin == "BOLUS_PUSH",insulin_push_time := med_starttime_tos_hr]
  dt[insulin_admin == "BOLUS_PUSH",insulin_push_type := insulin_type]
  # bolus injection: short
  cat("creating insulin bolus injection variables\n")
  dt[insulin_admin == "BOLUS_INJECTION" & insulin_type == "Short",insulin_s_inject_amount := insulin_amount]
  dt[insulin_admin == "BOLUS_INJECTION" & insulin_type == "Short",insulin_s_inject_time := med_starttime_tos_hr]
  dt[insulin_admin == "BOLUS_INJECTION" & insulin_type == "Short",insulin_s_inject_type := insulin_type]
  # bolus injection: intermediate
  dt[insulin_admin == "BOLUS_INJECTION" & insulin_type == "Intermediate",insulin_m_inject_amount := insulin_amount]
  dt[insulin_admin == "BOLUS_INJECTION" & insulin_type == "Intermediate",insulin_m_inject_time := med_starttime_tos_hr]
  dt[insulin_admin == "BOLUS_INJECTION" & insulin_type == "Intermediate",insulin_m_inject_type := insulin_type]
  # bolus injection: long
  dt[insulin_admin == "BOLUS_INJECTION" & insulin_type == "Long",insulin_l_inject_amount := insulin_amount]
  dt[insulin_admin == "BOLUS_INJECTION" & insulin_type == "Long",insulin_l_inject_time := med_starttime_tos_hr]
  dt[insulin_admin == "BOLUS_INJECTION" & insulin_type == "Long",insulin_l_inject_type := insulin_type]
}