
#######        Index       ###########
#table.index = number of uninque interaction table
#current.index = the current interaction table index
#iter = iteration number
#node = number of splitting point in each iteration
#SplitVar.index = number of splitting variable in each iteration
########################################



check_element = function(List,element) all(element %in% List)

diff_extract = function(DBM1, lossdata, best.iter){
      
    Var_list = list()
    relativity_list = list()
    table.index=1
    for (iter in 1:best.iter){
      tree = pretty.gbm.tree(DBM1,iter)
      if (nrow(tree) != 1){
          SplitVar = unique(tree$SplitVar) 
          SplitVar = sort(SplitVar[SplitVar != -1]) + 1
          if (!any(sapply(Var_list,check_element,SplitVar)) || length(Var_list) == 0){
              Var_list[[table.index]]= SplitVar
              current.index = table.index
              table.index= table.index +1 
              grid.list = list()
              for (var in 1:length(SplitVar)){
                  grid.list[[var]] = sort(as.matrix(unique(lossdata[DBM1$var.names[SplitVar[var]]])),na.last = T)
              }
              var.permutation = cbind(expand.grid(grid.list),0)
              names(var.permutation) = c(DBM1$var.names[SplitVar],"relativity")   
              relativity_list[[current.index]] = var.permutation
          } else{
              current.index = which(sapply(Var_list,check_element,SplitVar))[1]
              var.permutation = relativity_list[[current.index]]
              SplitVar = Var_list[[current.index]]
          }
            
          node=1
          if(nrow(tree) !=1){
              x = matrix(1,nrow(var.permutation), sum(tree$SplitVar == -1))
              for (i in which(tree$SplitVar == -1)){      
                temp = i -1
                repeat{
                    LRM = as.integer(which(tree[,3:5] == temp)/nrow(tree)-0.001)+1
                    temp = which(tree[,3:5] == temp) %% nrow(tree) - 1
                    if (DBM1$var.type[tree$SplitVar[temp+1]+1] == 0 ){                  #continuous
                      var.permutation.col = which( tree$SplitVar[temp+1]+1 == SplitVar)
                      if (LRM==1) {
                        index = var.permutation[,var.permutation.col] < tree$SplitCodePred[temp+1]
                        index[is.na(index)] = F
                        x[,node]=  x[,node]*index
                      }else if(LRM==2){
                        index = var.permutation[,var.permutation.col] >= tree$SplitCodePred[temp+1]
                        index[is.na(index)] = F
                        x[,node]=  x[,node]*index
                      }else if (LRM ==3){
                        x[,node]=  x[,node]*(is.na(var.permutation[,var.permutation.col]))
                      }
                    } else if (DBM1$var.type[tree$SplitVar[temp+1]+1] != 0 ){ #categorical
                      var.permutation.col = which( tree$SplitVar[temp+1]+1 == SplitVar)
                      index = DBM1$c.splits[[tree$SplitCodePred[temp+1]+1]]
                      if (LRM==1) {
                        index = which (index == -1)
                        index = levels(var.permutation[,var.permutation.col])[index]
                        x[,node]=  x[,node]*(var.permutation[,var.permutation.col] %in% index ) 
                      }else if(LRM==2){
                        index = which (index == 1)
                        index = levels(var.permutation[,var.permutation.col])[index]
                        x[,node]=  x[,node]*(var.permutation[,var.permutation.col] %in% index )   
                      }else if (LRM ==3){
                        x[,node]=  x[,node]*(is.na(var.permutation[,var.permutation.col]))
                      }
                    }            
                    if (temp ==0) break
                  } # repeat
                  node=node+1
              } # for (i in which(tree$SplitVar == -1))
              relativity.index = ncol(relativity_list[[current.index]])
              relativity_list[[current.index]][relativity.index] = relativity_list[[current.index]][relativity.index] + x %*%  tree$SplitCodePred[which(tree$SplitVar == -1)]
          }
      
      }
    }
    
    ##################----------------------##################
    # Calculate number of variables
    ##################----------------------##################
    
    n_parameter = 0
    for (i in 1:length(relativity_list)){
      n_parameter = n_parameter + nrow(relativity_list[[i]])
    }
    
    return (list(Var_list,relativity_list,n_parameter))
}

