calc_adjret_PCA <- function(ve.xts,field,first_pass=FALSE) {
  #print(paste("ve.xts",ve.xts,"coln",coln,"field",field))
  ticker <- sub("var.env$","",ve.xts,fixed=TRUE)
  de.xts <- paste("data.env$",ticker,"$",ticker,field,sep="")
  cmd_string <- paste(ve.xts," <- (stats::lag(",de.xts,",-1)-",de.xts,")/",de.xts,sep="")
  #print(cmd_string)
  eval(parse(text=cmd_string))
}

calc_wrapper <- function(stx_list){
  PCA_matrix <- NULL
  for (ticker in stx_list){
    ve.xts <- paste0("var.env$",ticker)
    calc_adjret_PCA(ve.xts, ".Adjusted")
    command_string <- paste0("PCA_matrix <- cbind(PCA_matrix,",ve.xts,")")
    eval(parse(text=command_string))
  }
  return (PCA_matrix)
}

Correlated_stocks <- function(PCA_matrix){
  PCA_correlation = cor(PCA_matrix)
  num_stocks = 50
  stocks = colnames(PCA_correlation)
  results = list()
  
  for (j in 1:491){
  
    stock = sort(PCA_correlation[,j], decreasing=TRUE)[1:num_stocks]
    corr_stock = names(stock)
    
    string = ""
    for (i in 2:49){
      string = paste0(string,corr_stock[i],"+")
    }
    string = paste0(string,corr_stock[50])
    
    command_string <- paste0("fit <- lm(",stocks[j],"~0+",string,",data=PCA_matrix)")
    eval(parse(text=command_string))
    step <- stepAIC(fit, direction="both")
    result = step$coefficients
    results[[j]] = result
    names(results)[j] <- stocks[j] 
  }
  return(results) 
}
