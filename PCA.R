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
  
  #Loops through each stock in Stock_Return
  for (j in 1:491){
  
    stock <- sort(PCA_correlation[,j], decreasing=TRUE)[1:num_stocks]
    corr_stock <- names(stock)
    
    #gets names of top 50 correlated stocks
    string <- ""
    for (i in 2:49){
      string <- paste0(string,corr_stock[i],"+")
    }
    string <- paste0(string,corr_stock[50])
    
    #Finds best fit using stepwise regression
    command_string <- paste0("fit <- lm(",stocks[j],"~0+",string,",data=var.env$Stock_Return)")
    eval(parse(text=command_string))
    step <- stepAIC(fit, direction="both",trace=FALSE)
    
    #Stores results in unc_res and unc_R2
    result <- step$coefficients
    var.env$unc_res[[j]] <- result
    names(var.env$unc_res)[j] <- stocks[j]
    var.env$unc_R2[[j]] <- summary(step)$adj.r.squared
    names(var.env$unc_R2)[j] <- stocks[j]
  }
}

Pos_Coff_Correlation <- function(){
  PCA_correlation <- cor(var.env$Stock_Return)
  j <- 1
  num_stocks <- length(var.env$unc_res[[j]])
  
  lower_bound <- as.list(rep(0, num_stocks))
  
  #sets initial variable values equal to values found in the unconstrained model and converts any negative values to zero
  initial_value <- var.env$unc_res[[j]]
  initial_value[initial_value<0] <- 0
  for (i in 1:num_stocks){
    names(initial_value[i]) <- unlist(strsplit(names(step$coefficients)[i],"[.]"))[1]
  }
  
  corr_stock <- names(var.env$unc_res[[j]])
  string <- ""
  for (i in 1:num_stocks){
    string <- paste0(string,"+",corr_stock[i])
  }
  string <- paste0(string,corr_stock[num_stocks])
  
  stock_return = var.env$Stock_Return[,j]
  var.env$Stock_Return.df <- as.data.frame(var.env$Stock_Return)
  
  string <- "A.Adjusted ~ beta1*FOX.Adjusted + beta2*FOXA.Adjusted + beta3*CSCO.Adjusted + beta4*PX.Adjusted + beta5*FMC.Adjusted + beta6*ROK.Adjusted + beta7*MOLX.Adjusted + beta8* CMI.Adjusted + beta9*ETN.Adjusted + beta10*BEN.Adjusted + beta19*RRD.Adjusted + beta11*WMB.Adjusted + beta12*PH.Adjusted + beta13*INTC.Adjusted + beta14*MMM.Adjusted + beta15*TMO.Adjusted + beta16*BMS.Adjusted + beta17*JEC.Adjusted + beta18*TER.Adjusted"
  
  command_string <- paste0("fit <- nls(",stocks[j],"~0",string,",data=var.env$Stock_Return.df,alg=\"port\",start=initial_value,lower=lower_bound)")
  eval(parse(text=command_string))

  g <- function(x)
    function(beta1,beta2,beta3)
      beta1+ beta2*x+beta3*x^2
  
  out <- nls(y~g(x)(beta1,beta2,beta3),data=data.frame(x,y),
             alg="port",
             start=list(beta1=1,beta2=1,beta3=1),
             lower=list(beta1=1,beta2=1,beta3=1))
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