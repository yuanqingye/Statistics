dataset = data.table(iris)
dataset = dataset[1:51,]
ratio  = 0.2
sample_on_level = function(dataset,ratio,level_col){
  set.seed(1)
  if(sum(ratio) < 1)
    ratio = c(ratio,1-sum(ratio))
  ratio = sort(ratio,decreasing = TRUE)
  num_part = length(ratio)
  # cum_ratio = cumsum(ratio)
  # cum_ratio = c(0,cum_ratio)
  dataset[,c("index","group_index"):=.(.I,sample(num_part,.N,replace = TRUE,prob = ratio)),by = level_col]
  # dataset[,group_num := as.numeric(cut(group_index,round(cum_ratio*.N))),by = level_col]
  return(dataset)
}

