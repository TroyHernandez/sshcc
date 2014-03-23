# BinaryL1NN.R
library(class)

# This can be generalized for any classification method.
BinaryL1NN <- function(x, test, y){
  
  if(is.null(dim(y))){
    y <- as.matrix(y, ncol = 1)
  }
  
  #depth is the depth of the hierarchical tree.
  depth <- length(unique(y[, ncol(y)])) - 1
  if(depth == 0){
#     stop("There is only one class!")
  }
  
  # Initializing values.
  y.mat <- matrix("", nrow = nrow(y), ncol = depth)
  wt.mat <- matrix(0, nrow = depth, ncol = ncol(x))
  node.list <- rep("", depth)
  
  # Creating testing matrix
  if(length(test) > 0){
    if(is.null(dim(test))){
      test <- t(as.matrix(test, nrow = 1))
    }
    y.mat.test <- matrix("", nrow = nrow(test),
                         ncol = max(1, depth))
  }
  
  # Finding relevant labels
  temp.y.ind <- 1
  temp.y <- y[, temp.y.ind]
  while(temp.y.ind < ncol(y) && length(unique(temp.y)) == 1){
    temp.y.ind <- temp.y.ind + 1
    temp.y <- y[, temp.y.ind]
  }
  
  #Creating new labels for binary tree assignment
  new.y <- OLRAssign(x, temp.y, method = "binary", warm.start = TRUE)
  node.list[1] <- paste(unique(new.y), collapse = "..")
  
  node.tree <- as.list(rep("", length(unique(new.y))))
  names(node.tree) <- unique(new.y)
  
  if(length(unique(new.y)) == 1){
    y.mat[1:nrow(y.mat), 1] <- new.y
    if(length(test) > 0){
      y.mat.test[1:nrow(y.mat.test), 1] <- unique(new.y)
    }
    wt.mat[1, 1:ncol(wt.mat)] <- NA
    node.tree[unique(new.y)] = unique(new.y)
  }else{
    y.mat[, 1] <- new.y
    #Could insert cv here.
    knn.weights <- L1NNEquilibrium(x = x, y = new.y, cv = TRUE)
    wt.mat[1, ] <- knn.weights
    if(length(test) > 0){
      temp.test.mat <- t(t(test) * wt.mat[1, ])
      if(ncol(temp.test.mat) != ncol(x)){
        temp.test.mat <- t(temp.test.mat)
      }
      y.mat.test[, 1] <- as.character(knn(t(t(x) * wt.mat[1, ]),
                                          temp.test.mat, new.y))
    }
    
    y.mat.col <- 2
    
    if(ncol(y.mat) > 1){
      for(i in 1:length(unique(new.y))){
        
        temp.ind <- which(new.y == unique(new.y)[i])
        if(length(test) > 0){
          temp.ind.test <- which(y.mat.test[, 1] == unique(new.y)[i])          
        }else{
          temp.ind.test <- NULL
        }
        
        if(length(unique(y[temp.ind, 1])) == 1 && ncol(y) == 1){
          y.mat[temp.ind, min(ncol(y.mat), y.mat.col)] <-
            unique(y[temp.ind, 1])
          if(length(temp.ind.test) > 0){
            y.mat.test[temp.ind.test, min(ncol(y.mat), y.mat.col)] <-
              unique(y[temp.ind, 1])              
          }
        }else{
          temp.test.mat2 <- as.matrix(test[temp.ind.test, ],
                                      ncol = length(temp.ind.test))
          if(ncol(temp.test.mat2) != ncol(x)){
            temp.test.mat2 <- t(temp.test.mat2)
          }
          
          if(length(unique(y[temp.ind, 1])) > 1){
            # Stay with the current category with multiple sub-categories.
            bl1nn <- BinaryL1NN(x = x[temp.ind, ], test = temp.test.mat2,
                                y = y[temp.ind, ])
          }else{
            # Go to the next category/column with multiple categories.
            bl1nn <- BinaryL1NN(x = x[temp.ind, ], test = temp.test.mat2,
                                y = y[temp.ind, -1])
          }
          
          # Don't want to allow them to mess up wt.mat, node.list, y.mat.col
          if(!is.na(bl1nn$wt.mat[1])){
            
            y.mat[temp.ind, y.mat.col:(y.mat.col + ncol(bl1nn$y.mat) - 1)] <-
              bl1nn$y.mat
            
            #Write answers to wt.mat
            wt.mat[y.mat.col:(y.mat.col + ncol(bl1nn$y.mat) - 1), ] <-
              bl1nn$wt.mat
            
            node.list[y.mat.col:(y.mat.col + ncol(bl1nn$y.mat) - 1)] <-
              bl1nn$node.list
            
            node.tree[[i]] <- bl1nn$node.tree
            
            if(length(temp.ind.test) > 0){
              y.mat.test[temp.ind.test,
                         y.mat.col:(y.mat.col + ncol(bl1nn$y.mat) - 1)] <-
                bl1nn$y.mat.test
            }
            
            #Write previous columns results 
            for(j in y.mat.col:(y.mat.col + ncol(bl1nn$y.mat) - 1)){
              empty.ind <- which(y.mat[-temp.ind, j] == "")
              if(length(empty.ind) > 0){
                y.mat[-temp.ind, j][empty.ind] <-
                  y.mat[-temp.ind, (j-1)][empty.ind]
              }
              
              if(length(temp.ind.test) > 0){
                empty.ind.test <- which(y.mat.test[-temp.ind.test, j] == "")
                if(length(empty.ind.test) > 0){
                  y.mat.test[-temp.ind.test, j][empty.ind.test] <-
                    y.mat.test[-temp.ind.test, (j-1)][empty.ind.test]
                }
              }
            }
            
            y.mat.col <- y.mat.col + ncol(bl1nn$y.mat)
          }
        }
      }
    }
  }
  
  #   cat("Node.list", head(node.list),"\n")
  y.mat <- HierarchyCleanup(y.mat)
  if(length(test) > 0){
    y.mat.test <- HierarchyCleanup(y.mat.test)
  }else{
    y.mat.test <- NULL
  }
  #   if(sum(node.list == "")>0){
  #     stop("Node.list broke.")
  #   }
  list(y.mat = y.mat, y.mat.test = y.mat.test,
       wt.mat = wt.mat, node.list = node.list, node.tree = node.tree)
  #   if(y.mat.col==38){
  #     stop("This fucking thing.")
  #   }
}

#########################################################

predict.BinaryL1NN <- function(y.mat, wt.mat, train, test){
  if(is.null(dim(y.mat))){
    y.mat <- as.matrix(y.mat, ncol = 1)
    wt.mat <- t(as.matrix(wt.mat, nrow = 1))
  }
  if(is.null(dim(test))){
    test <- as.matrix(test, nrow = 1)
  }
  if(ncol(train) != ncol(wt.mat)){
    stop("train cols need to equal wt.mat cols.\n")
  }
  #   y.mat <- model$y.mat
  #   wt.mat <- model$wt.mat
  predict.mat <- matrix("", nrow(test), ncol(y.mat))
  
  cl <- y.mat[, 1]
  fit <- as.character(knn(t(t(train) * wt.mat[1, ]),
                          t(t(test) * wt.mat[1, ]), cl))
  
  for(i in 1:length(unique(fit))){
    if( length(strsplit(as.character(unique(fit)[i]),
                        ".", fixed = T)[[1]]) == 1 ){
      temp.ind <- which(fit == unique(fit)[i])
      predict.mat[temp.ind, ] <- unique(fit)[i]
    }else{
      temp.ind <- which(fit == unique(fit)[i])
      predict.mat[temp.ind, ] <- predict.BinaryL1NN(y.mat[temp.ind, -1],
                                                    wt.mat[-1, ],
                                                    train[temp.ind, ],
                                                    test[temp.ind, ])
    }
  }
  predict.mat
}

# predict.BinaryL1NN <- function(y.mat, wt.mat, train, test){
#   if(is.null(dim(y.mat))){
#     y.mat <- as.matrix(y.mat, ncol = 1)
#     wt.mat <- t(as.matrix(wt.mat, nrow = 1))
#   }
#   if(is.null(dim(test))){
#     test <- as.matrix(test, nrow = 1)
#   }
#   if(ncol(y.mat) != nrow(wt.mat)){
#     stop("y.mat cols need to equal wt.mat rows.\n")
#   }
#   #   y.mat <- model$y.mat
#   #   wt.mat <- model$wt.mat
#   predict.mat <- matrix("", nrow(test), ncol(y.mat))
#   
#   cl <- y.mat[, 1]
#   fit <- knn(t(t(train) * wt.mat[1, ]), t(t(test) * wt.mat[1, ]), cl)
#   
#   for(i in 1:length(unique(fit))){
#     if( length(strsplit(as.character(unique(fit)[i]),
#                         ".", fixed = T)[[1]]) == 1 ){
#       temp.ind <- which(fit == unique(fit)[i])
#       predict.mat[temp.ind, ] <- unique(fit)[i]
#     }else{
#       temp.ind <- which(fit == unique(fit)[i])
#       predict.mat[temp.ind, ] <- predict.BinaryL1NN(y.mat[temp.ind, -1],
#                                                     wt.mat[-1, ],
#                                                     train[temp.ind, ],
#                                                     test[temp.ind, ])
#     }
#   }
#   predict.mat
# }
