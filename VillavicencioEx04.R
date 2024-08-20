# Sebastian M. Villavicencio 2020-06249
# CMSC 150 A2L

GaussianElimination <- function(matrix){
  a=matrix
  n = length(a[,1]) # number of rows
  
  solution_mat = matrix(, nrow = length(a[1,])-1)
 
  for (i in 1:(n-1)){
    max_value = max(abs(a[i:n,i]))
    for (k in i:n) # find pivot row
      if(abs(a[k,i]) == max_value)
        pivot_row = k
    if (a[pivot_row, i] == 0) # no unique solution exists 
      return(NA)
    else{ # partial pivoting
      temp_row = a[pivot_row,]
      a[pivot_row,] = a[i,]
      a[i,] = temp_row
    }
    for (j in (i+1):n){
      pivot_element = a[i,i]
      multiplier = a[j,i]/pivot_element
      normalized_row = multiplier * a[i,]
      a[j,] = a[j,] - normalized_row
    }
  }
  solution_mat[n,1] = a[n, n+1]/a[n,n]
  for (i in (n-1):1) # backward substitution
    solution_mat[i,1] = (a[i,n+1] - sum(a[i,(i+1):n] * solution_mat[(i+1):n]))/a[i,i]
  retList = list("solutionSet" = solution_mat, "matrix" = a)
  return(retList)
}

GaussJordanElimination <- function(matrix){
  a = matrix
  n = length(a[,1]) # number of rows
 
  for (i in 1:n){
    if (i != n+1){
      max_value = max(abs(a[i:n,i]))
      for (k in i:n)# find pivot row
        if(abs(a[k,i]) == max_value) 
          pivot_row = k
      if (a[pivot_row, i] == 0) 
        return(NA)
      else{ # partial pivoting
        temp_row = a[pivot_row,]
        a[pivot_row,] = a[i,]
        a[i,] = temp_row
      }
      a[i,] = a[i,]/a[i,i] #normalize pivot row
      for (j in 1:n){
        if (i != j){
          temporary_vector = a[j,i] * a[i,]
          a[j,] = a[j,] - temporary_vector
        }
      }
    }
  }
  retList = list("solutionSet" = matrix(a[,ncol(a)]), "matrix" = a)
  return(retList)
}

