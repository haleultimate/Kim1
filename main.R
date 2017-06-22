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
Returns(stx_list.loaded)

Correlated_stocks()
Pos_Coff_Correlation()
PCA_Analysis ()

