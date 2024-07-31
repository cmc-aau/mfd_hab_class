#####
# Block-wise parallelised wrapper for the vifcor function form usdm package
#####

# parallel library required

df_splitter <- function(df, block_size){
  N <- ncol(df)
  #print(N)
  n_splits <- (N%/%block_size) +1
  #print(n_splits)
  vec_indices <- (1:N%/%((N%/%n_splits)+1))
  #print(vec_indices)
  df.list <- lapply(as.list(0:(n_splits-1)), function(x) {df[,vec_indices==x]})
  return(df.list)
}


vifcor_recursive <- function(df, threshold, block_size, threads, selected_vars=c()){
  
  N <- ncol(df)
  if(length(selected_vars) != 0){
    df <- df[, sample(selected_vars)]
  }
 
 print(paste0("Number of variables: ", N, ". Number of remaining variables: ", ncol(df), "."))

  if(length(selected_vars)==N){
    return(selected_vars)

    } else {
    if (N <= block_size){
      v <- vifcor(df, th = threshold)
      selected_vars <- colnames(df)[!colnames(df)%in%v@excluded]
      return(selected_vars)
      
    } else {
      df.list <- df_splitter(df, block_size)
      v.list <- unlist(mclapply(df.list, vifcor, th = threshold, mc.cores = threads))
      to_remove_vars <- unlist(lapply((1:length(v.list)), function(x) {v.list[[x]]@excluded}))
      selected_vars <- colnames(df)[!colnames(df) %in% to_remove_vars]
      return(vifcor_recursive(df, threshold, block_size, threads, selected_vars=selected_vars))
    }
  }
}


vifcor_blockwise <- function(df, threshold, block_size, threads){
  N <- ncol(df)
  if(N > block_size){
    selected_vars <- vifcor_recursive(df, threshold, block_size, threads)
    #v <- vifcor(df[, selected_vars], th = threshold)
    #selected_vars <- colnames(df)[!colnames(df)%in%v@excluded]
    # REMOVE COMMENTS ABOVE TO ENABLE ALL VS ALL SCREENING WITH LEFTOVER VARIABLES. LONG TIME, FULL ACCURACY.
    return(selected_vars)
    
  } else {
    v <- vifcor(df, th = threshold)
    selected_vars <- colnames(df)[!colnames(df)%in%v@excluded]
    return(selected_vars)
  }
}


