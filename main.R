#main.R

print(paste("Start time:",Sys.time()))
if (!exists("stx_list.loaded")) stx_list.loaded <- NULL 
rm(list = ls(all=TRUE)[!ls(all=TRUE) %in% c("data.env","stx_list.loaded")]) #clean workspace except for (data.env, stx.list.old) so we don't have to reload data

source("init.R")            
load_custom_libraries()
load_packages()
stock_list_PCA()   
stx_list.loaded <- load_stock_history_PCA(stx_list.loaded)  

#code below should be set up as functions where possible
PCA_matrix <- calc_wrapper(stx_list.loaded)
PCA_matrix <- PCA_matrix[,colSums(is.na(PCA_matrix))==1]
PCA_matrix <- na.omit(PCA_matrix)

Unconstrained_results <- Correlated_stocks(PCA_matrix)

lower_bound = as.list(rep(0, 50))

stock = sort(PCA_correlation[,1], decreasing=TRUE)[1:num_stocks]
corr_stock = names(stock)
string = ""
for (i in 2:49){
  string = paste0(string,corr_stock[i],"+")
}
string = paste0(string,corr_stock[50])
PCA_matrix.df <- as.data.frame(PCA_matrix)
command_string <- paste0("fit <- nls(",stocks[1],"~0+",string,",data=PCA_matrix.df,start=lower_bound,lower=lower_bound)")
eval(parse(text=command_string))

#PCA analysis
pr.out=prcomp(PCA_matrix[104:354,], center=TRUE, scale = TRUE)

pr.out$center
pr.out$scale
rot = pr.out$rotation

biplot(pr.out, scale=0)
pr.var=pr.out$sdev^2
pve=pr.var/sum(pr.var)
plot(pve, xlab="Principal component", ylab="Proportion of Variance Explained", ylim=c(0,1), type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variation Explained", ylim=c(0,1),type='b')

#june 2006 - May 2007 PCA analysis
pr.out=prcomp(PCA_matrix[104:354,], center=TRUE, scale = TRUE)
top10 = list()
bottom10 = list()
for (i in 2:6){
  sorted = sort(rot[,i], decreasing = TRUE)
  top10[[i-1]] = names(sorted[1:10])
  bottom10[[i-1]] = names(tail(sorted, 10))
}
names(top10) <- names(rot[1,][2:6])
names(bottom10) <- names(rot[1,][2:6])

