naming <- function(name_object) {
 deparse(substitute(name_object))
}


performance_joint <- function(perf_object){
  mcc <- as.data.frame(h2o.mcc(perf_object))
  f1 <- as.data.frame(h2o.F1(perf_object))
  f0_5 <- as.data.frame(h2o.F0point5(perf_object))
  f2 <- as.data.frame(h2o.F2(perf_object))
  acc <- as.data.frame(h2o.accuracy(perf_object))
  max_mcc <- mcc[which.max(mcc$absolute_mcc),]
  max_f1 <- f1[which.max(f1$f1),]
  max_f0_5 <- f0_5[which.max(f0_5$f0point5),]
  max_f2 <- f2[which.max(f2$f2),]
  max_acc <- acc[which.max(acc$accuracy),]
  max_values <- cbind(max_mcc, max_f1, max_f0_5, max_f2, max_acc)
  name <- naming(perf_object)
  rownames(max_values) <- "model"
  print(max_values)
}



