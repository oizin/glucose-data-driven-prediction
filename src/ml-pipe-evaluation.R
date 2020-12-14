
#' Evaluation plots for predicting blood glucose
#'
#' @param dt the test dataset (containing certain variables)
#' 
evaluation_plots_outcome <- function(folder_path,dt,model_name,print_plot=FALSE) {
  
  if (class(bst) == "xgb.Booster") {
    # variable importance
    tmp =  xgb.importance(model = bst)[1:30]
    tmp$Feature <- factor(tmp$Feature,levels=  tmp$Feature,ordered=TRUE)
    p = ggplot(tmp,aes(x = Feature,y = Gain)) +
      geom_bar(stat="identity",fill="lightblue") +
      coord_flip() +
      theme_minimal(base_size = 14)
    if (print_plot == TRUE) print(p)
    ggsave(plot = p,filename = paste0(folder_path,"/Importance_",model_name,".png"),width  = 10,height = 5)
  } else if (class(bst) == "catboost.Model") {
    tmp <- data.table(Feature = colnames(X_train),
                      Gain = catboost::catboost.get_feature_importance(bst)[,1])
    tmp <- tmp[order(-Gain)]
    tmp$Feature <- factor(tmp$Feature,levels=  tmp$Feature,ordered=TRUE)
    p = ggplot(tmp[1:30,],aes(x = Feature,y = Gain)) +
      geom_bar(stat="identity",fill="lightblue") +
      coord_flip() +
      theme_minimal(base_size = 14)
    if (print_plot == TRUE) print(p)
    ggsave(plot = p,filename = paste0(folder_path,"/Importance_",model_name,".png"),width  = 10,height = 5)
  } else {
    stop("new class of model?")
  }
  
  # compare distributions
  p <- ggplot(dt, aes(glucose_b_shift1, glucose_bst)) + 
    geom_point(alpha = 0.1) + 
    theme_classic() +
    geom_abline(intercept = 0,slope = 1,col="red",linetype=2) +
    labs(x = "Predicted blood glucose (mg/dL)","Measured blood glucose (mg/dL)")
  p <- ggExtra::ggMarginal(p, type = "histogram",size=3,binwidth=20)
  if (print_plot == TRUE) print(p)
  ggsave(plot = p,filename = paste0(folder_path,"/Prediction_dist_",model_name,".png"),width  = 10,height = 5)
  
  # compare error distributions
  p = ggplot(dt,aes(x = error1)) +
    geom_histogram(col="white",fill="lightblue",binwidth=10) +
    theme_minimal(base_size = 14) +
    coord_cartesian(xlim=c(-250,250)) +
    labs(x = "Prediction error")
  if (print_plot == TRUE) print(p)
  ggsave(plot = p,filename = paste0(folder_path,"/Prediction_error_",model_name,".png"),width  = 10,height = 5)
  
  # compare error distributions by outcome
  dt[,glucose_bins := cut(glucose_b_shift1,c(0,80,120,200,Inf),right = FALSE)]
  p = ggplot(dt,aes(x = error1)) +
    geom_histogram(col="white",fill="lightblue",binwidth=10) +
    theme_bw(base_size = 14) +
    coord_cartesian(xlim=c(-250,250)) +
    labs(x = "Prediction error") +
    facet_wrap(~glucose_bins,scales = "free_y")
  if (print_plot == TRUE) print(p)
  ggsave(plot = p,filename = paste0(folder_path,"/Prediction_error_by_val_",model_name,".png"),width  = 10,height = 5)
  
  # MAD by glucose level
  tmp = dt[,.(MAD=mean(deviation),.N),by=cut(glucose_b_shift1,c(seq(0,500,50),Inf),right = FALSE)]
  tmp$cut <- ordered(tmp$cut)
  p = ggplot(tmp,aes(x = cut,y = MAD)) +
    geom_point(aes(size=N)) +
    scale_y_continuous(breaks = seq(0,0.5,0.1)) +
    coord_cartesian(ylim = c(0,0.5)) +
    theme_minimal(base_size = 16) +
    labs(x = "Blood glucose (mg/dL)",y = "Mean absolute deviation (%)") +
    scale_size_continuous(name = "Number of\ndata points") +
    theme(axis.text.x = element_text(angle = 45))
  if (print_plot == TRUE) print(p)
  ggsave(plot = p,filename = paste0(folder_path,"/MAD_by_glucose_",model_name,".png"),width  = 8,height = 5)
  
  # MSE by glucose level
  tmp = dt[tstep > 0,.(MSE=sqrt(mean(error2)),.N),by=cut(glucose_b_shift1,c(seq(0,500,50),Inf),right = FALSE)]
  tmp$cut <- ordered(tmp$cut)
  p = ggplot(tmp,aes(x = cut,y = MSE)) +
    geom_point(aes(size=N)) +
    scale_y_continuous(breaks = seq(0,200,20)) +
    coord_cartesian(ylim = c(0,200)) +
    theme_minimal(base_size = 16) +
    labs(x = "Blood glucose (mg/dL)",y = "Root mean square error (mg/dL)") +
    scale_size_continuous(name = "Number of\ndata points")+
    theme(axis.text.x = element_text(angle = 45))
  if (print_plot == TRUE) print(p)
  ggsave(plot = p,filename = paste0(folder_path,"/MSE_by_glucose_",model_name,".png"),width = 8,height = 5)
  
  # MSE by time step
  tmp = dt[tstep > 0,.(MSE=sqrt(mean(error2)),.N),by=tstep]
  p = ggplot(tmp,aes(x = tstep,y = MSE)) +
    geom_point() +
    coord_cartesian(xlim = c(1,48*5),ylim=c(0,100)) +
    scale_y_continuous(breaks = seq(0,200,20)) +
    scale_x_continuous(breaks = seq(0,300,by=12),labels = seq(0,300,by=12)/2) +
    theme_minimal(base_size = 16) +
    labs(x = "Time in ICU (hours)",y = "Root mean square error (mg/dL)")
  if (print_plot == TRUE) print(p)
  ggsave(plot = p,filename = paste0(folder_path,"/MAD_by_time_",model_name,".png"),width = 8,height = 5)
  
  # MSE by time of day
  tmp = dt[tstep > 0,.(MSE=sqrt(mean(error2)),.N),by=.(timeofday_hr=round(timeofday_hr,0))]
  p = ggplot(tmp,aes(x = timeofday_hr,y = MSE)) +
    geom_point() +
    coord_cartesian(xlim = c(0,24),ylim=c(0,100)) +
    scale_y_continuous(breaks = seq(0,200,20)) +
    scale_x_continuous(breaks = seq(0,24,by=4),labels = seq(0,24,by=4)) +
    theme_minimal(base_size = 16) +
    labs(x = "Time of day (hours)",y = "Root mean square error (mg/dL)")
  if (print_plot == TRUE) print(p)
  ggsave(plot = p,filename = paste0(folder_path,"/MAD_by_time_day_",model_name,".png"),width = 8,height = 5)
  
  # MAD by ICU type
  tmp = dt[tstep > 0,.(MAD=mean(deviation),.N),by=first_careunit]
  p = ggplot(tmp,aes(x = first_careunit,y = MAD)) +
    geom_point(size=2) +
    scale_y_continuous(breaks = seq(0,0.4,0.1)) +
    coord_cartesian(ylim = c(0,0.4)) +
    theme_minimal(base_size = 16) +
    labs(x = "ICU type",y = "Mean absolute deviation (%)")
  if (print_plot == TRUE) print(p)
  ggsave(plot = p,filename = paste0(folder_path,"/MAD_by_ICU_",model_name,".png"),width = 8,height = 5)
  
  # MSE by ICU type
  tmp = dt[tstep > 0,.(MSE=sqrt(mean(error2)),.N),by=first_careunit]
  p = ggplot(tmp,aes(x = first_careunit,y = MSE)) +
    geom_point() +
    coord_cartesian(ylim=c(0,200)) +
    scale_y_continuous(breaks = seq(0,200,20)) +
    theme_minimal(base_size = 16) +
    labs(x = "ICU type",y = "Root mean square error (mg/dL)")
  if (print_plot == TRUE) print(p)
  ggsave(plot = p,filename = paste0(folder_path,"/MSE_by_ICU_",model_name,".png"),width = 8,height = 5)
  
  # MSE by outcome
  tmp = dt[tstep > 0 & !is.na(hospital_expire_flag),
                             .(MSE=sqrt(mean(error2)),.N),by=hospital_expire_flag]
  p = ggplot(tmp,aes(x = factor(hospital_expire_flag),y = MSE)) +
    geom_point() +
    coord_cartesian(ylim=c(0,80)) +
    scale_y_continuous(breaks = seq(0,200,20)) +
    theme_minimal(base_size = 16) +
    labs(x = "Died in Hospital",y = "Root mean square error (mg/dL)")
  if (print_plot == TRUE) print(p)
  ggsave(plot = p,filename = paste0(folder_path,"/MSE_by_outcome_",model_name,".png"),width = 8,height = 5)
  
  # MSE by diagnosis
  tmp = dt[tstep > 0,.(MSE=sqrt(mean(error2)),.N),by=diagnosis]
  p = ggplot(tmp[N>200],aes(x = factor(diagnosis),y = MSE)) +
    geom_point() +
    coord_cartesian(ylim=c(0,80)) +
    scale_y_continuous(breaks = seq(0,200,20)) +
    theme_minimal(base_size = 16) +
    labs(x = "Patient diagnosis",y = "RMSE (mg/dL)") + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=5))
  if (print_plot == TRUE) print(p)
  ggsave(plot = p,filename = paste0(folder_path,"/MSE_by_diagnosis_",model_name,".png"),width = 8,height = 5)
  
  # MSE by admission type
  tmp = dt[tstep > 0,.(MSE=sqrt(mean(error2)),.N),by=admission_type]
  p = ggplot(tmp,aes(x = factor(admission_type),y = MSE)) +
    geom_point() +
    coord_cartesian(ylim=c(0,80)) +
    scale_y_continuous(breaks = seq(0,200,20)) +
    theme_minimal(base_size = 16) +
    labs(x = "Admission type",y = "Root mean square error (mg/dL)")
  if (print_plot == TRUE) print(p)
  ggsave(plot = p,filename = paste0(folder_path,"/MSE_by_admission_type_",model_name,".png"),width = 8,height = 5)
  
  # CLARKE error grid
  tmp <- getClarkeZones(dt[tstep > 0,glucose_b_shift1], dt[tstep > 0,glucose_bst])
  tmp_text <- 100*round(prop.table(table(tmp)),4)
  p = plotClarkeGrid(dt[tstep > 0,glucose_b_shift1], dt[tstep > 0,glucose_bst],pointsize=1) +
    coord_cartesian(xlim = c(0,500),ylim = c(0,500)) +
    labs(y = "Predicted blood glucose (mg/dL)",x = "True blood glucose (mg/dL)",
         title = paste(names(tmp_text),tmp_text,sep=":",collapse = ", ")) +
    theme_bw(base_size = 16)
  if (print_plot == TRUE) print(p)
  ggsave(plot = p,filename = paste0(folder_path,"/CEGA_",model_name,".png"),width = 8,height = 5)
  
  # CLARKE error grid
  tmp <- getClarkeZones(dt[tstep > 0 & last_careunit == "CSRU",glucose_b_shift1], 
                        dt[tstep > 0 & last_careunit == "CSRU",glucose_bst])
  tmp_text <- 100*round(prop.table(table(tmp)),4)
  p = plotClarkeGrid(dt[tstep > 0 & last_careunit == "CSRU",glucose_b_shift1],
                 dt[tstep > 0 & last_careunit == "CSRU",glucose_bst],pointsize=1) +
    coord_cartesian(xlim = c(0,500),ylim = c(0,500)) +
    labs(y = "Predicted blood glucose (mg/dL)",x = "True blood glucose (mg/dL)",
         title = paste(names(tmp_text),tmp_text,sep=":",collapse = ", "),
         subtitle = "(CSRU only)") +
    theme_bw(base_size = 16)
  if (print_plot == TRUE) print(p)
  ggsave(plot = p,filename = paste0(folder_path,"/CEGA_CSRU_",model_name,".png"),width = 8,height = 5)
}

