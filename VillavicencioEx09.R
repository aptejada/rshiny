#Sebastian M. Villavicencio
#CMSC 150 A2L
create_identity <- function(size){ # creates an identity matrix
  mat = matrix(0, nrow = size, ncol = size)
  for(i in 1:size)
    mat[i,i] = 1
  return (mat);
}

construct_tableau <- function(inputs){ # construct tableau for the shipping problem
                                       # constructs the dual problem
  if(sum(inputs$supplies)<sum(inputs$demands)) return (NULL) # if there are more demand than the supply
  num_warehouse = 5 # no. of warehouse
  num_plant = 3 # no. of plants
  num_col = num_warehouse * num_plant # no.of warehouse * no.of plants 
  num_constraints = num_warehouse + num_plant + 1 # each plant and warehouse has a constraint + objective function
  
  # initial matrix and the RHS
  mat = matrix(0, ncol = num_col, nrow = num_constraints, byrow = FALSE, dimnames = list(c(1:(num_constraints-1), "obj_function"), c(paste("x", 1:(num_col), sep = ""))))
  rhs = matrix(0, nrow = num_constraints, dimnames = list(rownames(mat),"RHS"))
  
  curr_row = 1
  
  # plant constraints(supply) inequality sign is <=
  # warehouse constraints (demand) inequality sign is >= 
  # one of the two must be multiplied to -1 so that they have the same inequality sign
  
  # fill in the initial matrix and the RHS
  # plant constraints(supply)
  for(i in 1:num_col){   
    mat[curr_row,i] = -1
    if(i%%num_warehouse == 0){
      rhs[curr_row,"RHS"] = -1*inputs$supplies[i/num_warehouse]
      curr_row=curr_row+1
    }  
  }
  
  # warehouse constraints(demands)
  temp = c()
  for(i in 1:num_plant)
    temp = cbind(temp,create_identity(num_warehouse))
  mat[curr_row:(nrow(mat)-1),]= temp[1:nrow(temp),]
  rhs[curr_row:(nrow(rhs)-1)]= inputs$demands[1:(num_warehouse)]
  
  # objective function
  counter = 1
  for(i in inputs$costs){
    for(j in i){
      mat[nrow(mat), counter] = j
      counter = counter +1
    }
  }
  rhs[nrow(rhs)] = 1
  mat = cbind(mat, rhs)
  
  mat = t(mat) # transpose of the matrix
  
  # dual of the problem 
  tableau = mat
  numrow = nrow(tableau)
  numcol = ncol(tableau)
  
  tableau[numrow,] = -1*tableau[numrow,]
  
  # add slack variables
  temp = cbind(create_identity(numrow), tableau[,numcol]) # bind the last column of the tableau to an identity matrix
  tableau = tableau [,-c(numcol)] # remove the last column
  tableau = cbind(tableau, temp)
  tableau[nrow(tableau), ncol(tableau)] = 0
  
  # rename rows and columns
  rownames(tableau) = NULL
  colnames(tableau) = c(paste("S", 1:(numcol-1), sep=""), paste("x",1:(numrow-1), sep=""), "Z", "Solution")
  
  return(tableau)
}

get_pivot_col <- function(list){ # returns the index of the pivot column
  pivot_column = 1
  for(i in 2:length(list)-1){
    if(list[i] < list[pivot_column])  
      pivot_column = i
  }
  return (pivot_column)
}

get_pivot_row <- function(list, solution){ # returns the index of the pivot column
  for(i in 1:(length(list)-1)){
    if (list[i] == 0) next
    if(solution[i]/list[i] > 0){
      pivot_row = i
      break
    }
  }

  for(j in 1:(length(list)-1)){
    if (list[j] == 0) next
    test_ratio = solution[j]/list[j]
    if((test_ratio > 0) && ((test_ratio) < (solution[pivot_row]/list[pivot_row])))
      pivot_row = j
  }
  return(pivot_row)
}

get_basic_sol <- function(tableau, isMax){ # returns the basic solution of the tableau
  vars = colnames(tableau)
  vars= vars[!vars %in% 'Solution'] # remove "Solution" in the list of variables
  
  basic_sol = matrix(0, nrow=length(vars), dimnames = list(vars,"Solution"))
  if(isMax == TRUE){ # the solution is on the last column
    for(i in vars){
      col_to_check = tableau[,i]
      if(!is.na(match(1,col_to_check))){
        row = match(1,col_to_check)
        col_to_check[row] = 0
        if(all(col_to_check == 0)) basic_sol[i,1] = tableau[row,"Solution"]
        else basic_sol[i,1] = 0
      }
    }
  }else if (isMax == FALSE){ # the solution is on the last row
    for(i in vars)
      basic_sol[i,1] = tableau[nrow(tableau), i]
    basic_sol["Z",1] = tableau[nrow(tableau), ncol(tableau)]
  }
  return(basic_sol)
}

simplex <- function(tableau, isMax, problem){
  if(is.null(tableau)) return (NA)
  if(problem == TRUE) isMax = FALSE #the shipping problem is always minimization
  
  while(!all(tableau[nrow(tableau),] >= 0)){ # loop until all values on the last row is positive
    pivot_col = get_pivot_col(tableau[nrow(tableau),]);
    pivot_row = get_pivot_row(tableau[,pivot_col], tableau[,ncol(tableau)])

    pivot_element = tableau[pivot_row, pivot_col]

    tableau[pivot_row,] = tableau[pivot_row,]/pivot_element #normalize pivot row

    for(row in 1:nrow(tableau)){
      if(row == pivot_row) next
      C = tableau[row, pivot_col]
      tableau[row,] = tableau[row,] - (tableau[pivot_row,]*C)
    }
  }
  
  basic.solution = get_basic_sol(tableau, isMax)
  opt.val = basic.solution["Z","Solution"]
  
  if (problem == FALSE) retList = list("final.tableau" = tableau, "basic.solution" = basic.solution, "opt.val" = opt.val)
  else if(problem == TRUE){ # shipping.num must be added to the retList
    vars = paste("x", 1:15, sep="")
    temp = c()
    for(i in vars) 
      temp = c(temp, basic.solution[i, "Solution"])
    shipping.num = matrix(temp, nrow = 3, ncol = 5, byrow = TRUE, dimnames = list(c("DEN", "PHO", "DAL"), c("SAC","SL","ALB","CHI","NYC")))
    
    retList = list("final.tableau" = tableau, "basic.solution" = basic.solution, "opt.val" = opt.val, "shipping.num"=shipping.num)
  }
  return(retList)
}

# maximization problem from the Simplex Method handout
# tableau2 = matrix(c(7,11,1,0,0,0,0,77,10,8,0,1,0,0,0,80,1,0,0,0,1,0,0,9,0,1,0,0,0,1,0,6,-150,-175,0,0,0,0,1,0), nrow = 5, byrow = TRUE, dimnames = list(c(),c("x1", "x2", "S1", "S2", "S3", "S4", "Z", "Solution")))
# simplex(tableau2, TRUE, FALSE)

# minimization from the Simplex Method handout
# tableau3 = matrix(c(1,7,1,0,0,14,2,6,0,1,0,20,-4,-20,0,0,1,0), nrow = 3, byrow=TRUE, dimnames = list(c(), c("S1", "S2", "x1", "x2", "Z", "Solution")))
# simplex(tableau3, FALSE, FALSE)

# shipping problem
inputs = list("supplies"=c(310,260,280), "demands" = c(180,80,200,160,220), "costs" = list(c(10,8,6,5,4),c(6,5,4,3,6),c(3,4,5,5,9)))
# inputs = list("supplies"=c(200,200,200), "demands" = c(100,100,100,100,100), "costs" = list(c(5,6,7,8,9),c(6,7,8,9,10),c(3,5,7,11,13)))
# inputs = list("supplies"=c(1400,400,200), "demands" = c(431,332,350,450,400), "costs" = list(c(30,29,31,35,33),c(26,24,23,25,27),c(11,13,15,20,17)))
# inputs = list("supplies"=c(100,100,100), "demands" = c(20,20,20,20,20), "costs" = list(c(5,5,5,5,5),c(5,5,5,5,5),c(5,5,5,5,5)))
# inputs = list("supplies"=c(50,50,50), "demands" = c(201,25,90,60,70), "costs" = list(c(30,29,31,35,33),c(26,24,23,25,27),c(11,13,15,20,17)))

tableau1 = construct_tableau(inputs) # when solving the shipping problem, construct the tableau first using the 
simplex(tableau1, FALSE, TRUE) 


