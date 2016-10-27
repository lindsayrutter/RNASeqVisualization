library(combinat)
# Number of elements in array
n = 6
# Array
dat <- 1:n

# All possible permutations for array
ans <- permn(dat)
lengthAns <- length(ans)
ansDF <- data.frame()
# Place first permutation in final answer data frame
ansDF <- rbind(ansDF, ans[[1]])

# Look at the rest of the possible permutations. Determine for each one if it is truly unique from all the previously-listed possible permutations. If it is unique from them, then add it to the final answer data frame
for (i in 2:lengthAns){
  j = i
  k = TRUE
  while (k && j > 1){
    j = j-1
    if(setequal(ans[[i]][1:(n/2)], ans[[j]][1:(n/2)]))
      k = FALSE
    if(setequal(ans[[i]][1:(n/2)], ans[[j]][(n/2+1):(n)]))
      k = FALSE
  }
  if (k){
    ansDF <- rbind(ansDF, ans[[i]])
  }
}

# At this point, ansDF contains all unique possible ways to split the array into two-equally sized groups.
