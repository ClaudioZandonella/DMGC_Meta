###################################
####    Auxiliary functions    ####
###################################


#----    I_squared    ----

# compute I squared using formula reported in
# http://www.metafor-project.org/doku.php/tips:i2_multilevel_multivariate (section: Multilevel Models)

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


#----    