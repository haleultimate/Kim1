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
  stocks <- colnames(PCA_correlation)
  var.env$unc_res <- list()
  var.env$unc_R2 <- list()
  var.env$pos_res <- list()
  var.env$pos_R2 <- list()
  var.env$one_res <- list()
  
  #Loops through each stock in Stock_Return
  for (j in 1:5){
    
    #gets top 50 correlated stocks
    num_stocks <- 50
    stock <- sort(PCA_correlation[,j], decreasing=TRUE)[1:num_stocks]
    corr_stock <- names(stock)
    string <- ""
    for (i in 2:49){
      string <- paste0(string,corr_stock[i],"+")
    }
    string <- paste0(string,corr_stock[50])
    
    #Finds best fit using stepwise regression
    command_string <- paste0("fit <- lm(",stocks[j],"~0+",string,",data=var.env$Stock_Return)")
    eval(parse(text=command_string))
    step <- stepAIC(fit, direction="both",trace=FALSE)
    
    #Stores results in unc_res and and adjusted R^2 in unc_R2
    result <- step$coefficients
    var.env$unc_res[[j]] <- result
    names(var.env$unc_res)[j] <- stocks[j]
    var.env$unc_R2[[j]] <- summary(step)$adj.r.squared
    names(var.env$unc_R2)[j] <- stocks[j]
    
    num_stocks <- length(var.env$unc_res[[j]])
    
    lower_bound <- as.list(rep(0, num_stocks))
    
    #sets initial variable values equal to values found in the unconstrained model
    #converts any negative values to zero
    initial_value <- var.env$unc_res[[j]]
    initial_value[initial_value<0] <- 0
    for (i in 1:num_stocks){
      names(initial_value)[i] <- unlist(strsplit(names(step$coefficients)[i],"[.]"))[1]
    }
    
    #Creates string for nls()
    corr_stock <- names(var.env$unc_res[[j]])
    string <- ""
    for (i in 1:num_stocks){
      string <- paste0(string,"+",unlist(strsplit(names(step$coefficients)[i],"[.]"))[1],"*",corr_stock[i])
    }
    
    var.env$Stock_Return.df <- as.data.frame(var.env$Stock_Return)
    
    command_string <- paste0("fit <- nls(",stocks[j],"~0",string,",data=var.env$Stock_Return.df,alg=\"port\",start=initial_value,lower=lower_bound)")
    eval(parse(text=command_string))
    
    #Removes any zero valued coefficients
    result <- summary(fit)$coefficients[,1]
    result[result==0] <- NA
    result
    result <- na.omit(result)
    
    #Stores results in pos_res and adjusted R^2 in pos_R2
    var.env$pos_res[[j]] <- result
    names(var.env$pos_res)[j] <- stocks[j]
    actual <- var.env$Stock_Return[,j]
    predict <- predict(fit)
    R2 <- 1- (sum((actual-predict )^2)/sum((actual-mean(actual))^2))
    var.env$pos_R2[[j]] <- 1- ((1-R2)*(length(var.env$Stock_Return[,j])-1)/(length(var.env$Stock_Return[,j]) - length(var.env$pos_res[[j]]) -1))
    names(var.env$pos_R2)[j] <- stocks[j]
    
    #Recalculates coefficient values so they sum to one and stores in one_res
    sum <- 0
    for (i in 1:length(var.env$pos_res[[j]])){
      sum <- sum + var.env$pos_res[[j]][i]
    }
    var.env$one_res[[j]] <- var.env$pos_res[[j]]/sum
    names(var.env$one_res)[j] <- stocks[j]
  }
}

PCA_Analysis <- function(){
  #June 2006 - May 2007 PCA analysis
  pr.out<-prcomp(var.env$Stock_Return[104:354], center=TRUE, scale=TRUE)
  
  #Matrix of PCA components
  var.env$eig.array <- pr.out$rotation
  
  #Top 10 and Bottom 10 stocks for 2-6 PCA components
  var.env$top10 <- list()
  var.env$bottom10 <- list()
  for (i in 2:6){
    sorted <- sort(var.env$eig.array[,1], decreasing=TRUE)
    var.env$top10[[i-1]] <- names(sorted[1:10])
    var.env$bottom10[[i-1]] <- names(tail(sorted, 10))
  }
  names(var.env$top10) <- names(var.env$eig.array[1,][2:6])
  names(var.env$bottom10) <- names(var.env$eig.array[1,][2:6])

  #Plot of explained variance
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