

#' Check join
#'
#'
check_join <- function(n,data,join_name) {
  if(nrow(data) == n) {
    cat(paste0("success,joined ", join_name,"\n"))
  } else {
    stop(paste0("FAILURE on ", join_name,"\n"))
  }
}

#' Read in a dataset but keep only if icustay_ids in ids
#'
#'
#'
#'
fread_ids <- function(path,id_var,ids) {
  dt <- fread(path)
  dt <- dt[get(id_var) %in% ids,]
  dt
}

#'
#'
#'
#'
if_exists_archive <- function(path) {
  if (file.exists(path)) {
    path_list <- strsplit(path,"/")
    path_filename <- path_list[[1]][length(path_list[[1]])]
    path_filename <- paste0("archived_",format(Sys.time(),"%Y%m%d%H%M"),path_filename)
    path_list[[1]][length(path_list[[1]])] <- c("archive")
    path_list[[1]] <- c(path_list[[1]],path_filename)
    copy_path <- paste(path_list[[1]],collapse = "/")
    file.copy(path,copy_path)
    cat("file moved to archive\n")
  } else {
    cat("no file moved to archive\n")
  }
}