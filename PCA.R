calc_adjret_PCA <- function(ve.xts,field,first_pass=FALSE) {
  #print(paste("ve.xts",ve.xts,"coln",coln,"field",field))
  ticker <- sub("var.env$","",ve.xts,fixed=TRUE)
  de.xts <- paste("data.env$",ticker,"$",ticker,field,sep="")
  cmd_string <- paste(ve.xts," <- (stats::lag(",de.xts,",-1)-",de.xts,")/",de.xts,sep="")
  #print(cmd_string)
  eval(parse(text=cmd_string))
}

calc_wrapper <- function(stx_list){
  var.env$Stock_Return <- NULL
  for (ticker in stx_list){
    ve.xts <- paste0("var.env$",ticker)
    calc_adjret_PCA(ve.xts, ".Adjusted")
    command_string <- paste0("var.env$Stock_Return <- cbind(var.env$Stock_Return,",ve.xts,")")
    eval(parse(text=command_string))
  }
  return (var.env$Stock_Return)
}

#Finds top 50 correlated stocks and runs stepwise regression for each stock in var.env$Stock_Return
Correlated_stocks <- function(){
  PCA_correlation <- cor(var.env$Stock_Return)
  num_stocks <- 50
  stocks <- colnames(PCA_correlation)
  var.env$unc_res <- list()
  var.env$unc_R2 <- list()
  
  for (j in 1:491){
  
    stock <- sort(PCA_correlation[,j], decreasing=TRUE)[1:num_stocks]
    corr_stock <- names(stock)
    
    string <- ""
    for (i in 2:49){
      string <- paste0(string,corr_stock[i],"+")
    }
    string <- paste0(string,corr_stock[50])
    
    command_string <- paste0("fit <- lm(",stocks[j],"~0+",string,",data=var.env$Stock_Return)")
    eval(parse(text=command_string))
    step <- stepAIC(fit, direction="both")
    result <- step$coefficients
    var.env$unc_res[[j]] <- result
    names(var.env$unc_res)[j] <- stocks[j]
    var.env$unc_R2[[j]] <- summary(step)$adj.r.squared
    names(var.env$unc_R2)[j] <- stocks[j]
  }
}

Pos_Coff_Correlation <- function(){
  PCA_correlation <- cor(var.env$Stock_Return)
  j<-1
  num_stocks <- length(var.env$unc_res[[j]])
  lower_bound <- as.list(rep(0, num_stocks))
  initial_value <- var.env$unc_res[[j]]
  initial_value[initial_value<0] <- 0
  corr_stock <- names(var.env$unc_res[[j]])
  string <- ""
  for (i in 1:(num_stocks-1)){
    string <- paste0(string,corr_stock[i],"+")
  }
  string <- paste0(string,corr_stock[num_stocks])
  
  stock_return = var.env$Stock_Return[,j]
  var.env$Stock_Return.df <- as.data.frame(var.env$Stock_Return)
  for (k in 1:length(var.env$Stock_Return.df)){
    if (names(var.env$Stock_Return)[k] %in% corr_stock){
      stock_return <- data.frame(stock_return,var.env$Stock_Return[,k])
    }
  }
  command_string <- paste0("fit <- nls(",stocks[1],"~0+",string,",data=stock_return,start=initial_value,lower=lower_bound)")
  eval(parse(text=command_string))
}

PCA_Analysis <- function(){
  #june 2006 - May 2007 PCA analysis
  pr.out<-prcomp(var.env$Stock_Return[104:354], center=TRUE, scale=TRUE)
  var.env$eig.array <- pr.out$rotation
  var.env$top10 <- list()
  var.env$bottom10 <- list()
  for (i in 2:6){
    sorted <- sort(var.env$eig.array[,i], decreasing=TRUE)
    var.env$top10[[i-1]] <- names(sorted[1:10])
    var.env$bottom10[[i-1]] <- names(tail(sorted, 10))
  }
  names(var.env$top10) <- names(var.env$eig.array[1,][2:6])
  names(var.env$bottom10) <- names(var.env$eig.array[1,][2:6])

  pr.var<-pr.out$sdev^2
  pve<-pr.var/sum(pr.var)
  plot(pve, xlab="Principal component", ylab="Proportion of Variance Explained", ylim=c(0,1), type='b')
  plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variation Explained", ylim=c(0,1),type='b')
}

Returns <- function(stx_list.loaded){
  var.env$Stock_Return <- calc_wrapper(stx_list.loaded)
  var.env$Stock_Return <- var.env$Stock_Return[,colSums(is.na(var.env$Stock_Return))==1]
  var.env$Stock_Return <- na.omit(var.env$Stock_Return)
}