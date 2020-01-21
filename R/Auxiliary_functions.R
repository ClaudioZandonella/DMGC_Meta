###################################
####    Auxiliary functions    ####
###################################


#----    I_squared_function    ----

# compute I squared using formula reported in
# http://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate (section: Multilevel Models)

#----    I_squared    ----
I_squared<-function(model){
  W <- diag(1/model$vi)
  X <- model.matrix(model)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  100 * sum(model$sigma2) / (sum(model$sigma2) + (model$k-model$p)/sum(diag(P)))
}


#----    get_legend    ----

get_legend<-function(a_gplot){
  tmp <- ggplot_gtable(ggplot_build(a_gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

#-----    mutate_cond    ----

# mutate_cond create a simple function for data frames or data tables 
# that can be incorporated into pipelines. 
# This function is like mutate but only acts on the rows satisfying the condition.

# https://stackoverflow.com/questions/34096162/dplyr-mutate-replace-several-columns-on-a-subset-of-rows

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

#------    compute_CI    ----

# Compute 95% CI given estimated value and SE
compute_CI <- function(estimate, SE, data){
  col_estimate = match(estimate,names(data))
  col_SE = match(SE,names(data))
  names = paste0(estimate,c("_lb","_ub"))
  
  CI = data.frame(lb= data[,col_estimate] - 1.96* data[,col_SE],
                  ub= data[,col_estimate] + 1.96* data[,col_SE])
  
  names(CI)=names
  
  return(CI)
}

#----    get_info_moderator_analysis    ----

get_info_moderator_analysis <- function(mod_rma_mv){
  
  data.frame(n_studies = mod_rma_mv$s.nlevels,  # number of studies
             n_effects = mod_rma_mv$k,          # number effects
             Q_value = mod_rma_mv$QM,           # Q value
             Q_df= mod_rma_mv$m,                # df Q-value
             Q_pvalue = mod_rma_mv$QMp)         # p-value
}

#----    