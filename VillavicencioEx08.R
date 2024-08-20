  #Sebastian M. Villavicencio
  #CMSC 150 A2L
  
  source("VillavicencioEx04.R")
  generate_col_labels <- function(n){ # n is the number of intervals
    col_labels = c()
    for(i in 1:n){
      col_labels = c(col_labels, paste("a",i, sep = ""))
      col_labels = c(col_labels, paste("b",i, sep = ""))
      col_labels = c(col_labels, paste("c",i, sep = ""))
    }
    
    col_labels = c(col_labels, "f(x)")
    return (col_labels);
  }
  
  compute_y <- function(qsi.fxns, x_val, x){ # used to compute the value of y using the qsi.fxns
    for(i in 1:(length(x_val)-1)){
      if(x >= x_val[i] && x <= x_val[i+1]) # check if x is within the rage of the function
        return(qsi.fxns[[i]](x));
    }
    return(NA)
  }
  
  poly.qsi <- function(data, x){
    data <- data[order(data$x),]
    x_val = data$x
    y_val = data$y
    
    if (length(x_val)!=length(y_val)) return (NA)
    if (length(x_val) < 2 || length(y_val) < 2) return (NA)
    
    n = length(x_val)-1 #number of intervals
    # nrow = 3n-1 because there is no a1
    mat =  matrix(0, nrow = (3*n)-1, ncol = (3*n)+1, byrow=FALSE, dimnames = list(c(1:((3*n)-1)), generate_col_labels(n)))
    
    row = 1
    for(i in 3:(n+1)){ # condition 1, internal knots
                       # starts with 3 since R starts with index 1
      mat[row,paste("a",i-2, sep="")] = (x_val[i-1])^2
      mat[row,paste("b",i-2, sep="")] = x_val[i-1]
      mat[row,paste("c",i-2, sep="")] = 1
      mat[row,"f(x)"] = y_val[i-1]
      row = row + 1
      mat[row,paste("a",i-1, sep="")] = (x_val[i-1])^2
      mat[row,paste("b",i-1, sep="")] = x_val[i-1]
      mat[row,paste("c",i-1, sep="")] = 1
      mat[row,"f(x)"] = y_val[i-1]
      row = row + 1
    }
    
    # condition 2, end points
    mat[row,paste("a",1, sep="")] = (x_val[1])^2
    mat[row,paste("b",1, sep="")] = x_val[1]
    mat[row,paste("c",1, sep="")] = 1
    mat[row,"f(x)"] = y_val[1]
    row = row + 1
    mat[row,paste("a",n, sep="")] = (x_val[n+1])^2
    mat[row,paste("b",n, sep="")] = x_val[n+1]
    mat[row,paste("c",n, sep="")] = 1
    mat[row,"f(x)"] = y_val[n+1]
    row = row+1
    
    for(i in 3:(n+1)){  #condition 3, first derivative of interior knots must be equal
      mat[row,paste("a",i-2, sep="")] = 2*(x_val[i-1])
      mat[row,paste("a",i-1, sep="")] = -2*(x_val[i-1])
      mat[row,paste("b",i-2, sep="")] = 1
      mat[row,paste("b",i-1, sep="")] = -1
      row = row + 1
    }
    
    mat = mat[,-c(1)] # remove the a1 column
    gauss = GaussianElimination(mat)
    
    sol_set = matrix(0, nrow = 1, ncol = 1, dimnames = list("a1")) # put a1 as the first element of the solutionset
    sol_set = rbind(sol_set, gauss$solutionSet) # combine the solution sets
    
    # arrange the function
    temp_fxn = "function (x) "
    qsi.fxns = c()
    for(i in 1:(3*n)){
      if(i%%3 == 1){
        temp_fxn = paste(temp_fxn,round(sol_set[i], digits=4),"*x^2 + ", sep = "")
      } else if(i%%3 == 2){
        temp_fxn = paste(temp_fxn, round(sol_set[i], digits=4),"*x + ", sep = "")
      }else {
        temp_fxn = paste(temp_fxn, round(sol_set[i], digits=4), sep = "")
        qsi.fxns = c(qsi.fxns, eval(parse(text=temp_fxn)))
        temp_fxn = "function (x) "
      }
    }
    
    retList = list("qsi.fxns" = qsi.fxns, "y" = compute_y(qsi.fxns, x_val, x))
    return(retList)
  }
  # from lecture slides
  # data = list (x values, y values)
  # df = data.frame(
  #   "x" = c(3.0, 4.5, 7.0, 9.0),
  #   "y" = c(2.5, 1.0, 2.5, 0.5)
  # )
  # poly.qsi(df,5)
