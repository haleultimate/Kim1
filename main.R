#main.R

print(paste("Start time:",Sys.time()))
if (!exists("stx_list.loaded")) stx_list.loaded <- NULL 
rm(list = ls(all=TRUE)[!ls(all=TRUE) %in% c("data.env","stx_list.loaded")]) #clean workspace except for (data.env, stx.list.old) so we don't have to reload data

source("init.R")     
set_up_environments()
load_custom_libraries()
load_packages()
stock_list_PCA()   
stx_list.loaded <- load_stock_history_PCA(stx_list.loaded)  

print("stocks loaded")
print(stx_list.loaded)
#code below should be set up as functions where possible
print("calculating returns")
Returns(stx_list.loaded)
#Correlated_stocks()
print("calculating PCA")
PCA_Analysis()

