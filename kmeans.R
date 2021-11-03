##kmeans
manual_kmeans_step = function(dataset,centers) {
  dis = tibble( A= rep(0,nrow(dataset)), B=0, C=0)
  c1 = list()
  for (i in 1:nrow(dataset)) {
    dis$A[i] = as_vector((dataset[i,1]-centers[1,2])^2 + (dataset[i,2]-centers[1,3])^2 +
                           (dataset[i,3]-centers[1,4])^2)^(0.5)
    dis$B[i] = as_vector((dataset[i,1]-centers[2,2])^2 + (dataset[i,2]-centers[2,3])^2 + 
                           (dataset[i,3]-centers[2,4])^2)^(0.5)
    dis$C[i] = as_vector((dataset[i,1]-centers[3,2])^2 + (dataset[i,2]-centers[3,3])^2 + 
                           (dataset[i,3]-centers[3,4])^2)^(0.5)
  }
  dis = dis %>% mutate(cluster = case_when(A<=B & A<=C ~ 'A',
                                           B<=A & B<=C ~ 'B',
                                           C<=A & C<=B ~ 'C'))
  c1$cluster = dis$cluster
  dataset$cluster = dis$cluster
  c1$centers = dataset %>% group_by(cluster) %>% summarize_all('mean')
  c1
}



