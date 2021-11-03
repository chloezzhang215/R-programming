##hopkins statistics
calculate_hopkins_statistic = function(dataset, n){
  i = sample(nrow(dataset), n, replace = FALSE)
  min = apply(dataset, 2, min) 
  max = apply(dataset, 2, max) 
  temp = matrix(NA, nrow=n, ncol=ncol(dataset))
  
  for(i in 1:ncol(dataset)) {
    temp[,i] = runif(n, min=min[i], max=max[i])
  }
  s_dist = sum(rowMins(as.matrix(pdist(dataset[i, ], dataset[-i, ]))))
  r_dist = sum(rowMins(as.matrix(pdist(temp, dataset))))
  
  return (r_dist/(r_dist + s_dist))
}