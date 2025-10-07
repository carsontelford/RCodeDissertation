
# Complete the 'miniMaxSum' function below.
#
# The function accepts INTEGER_ARRAY arr as parameter.
#

arr <- c(1,2,3,4,5)
miniMaxSum <- function(arr) {
  combinations <- combn(arr, 4)
  ncombos <- ncol(combinations)
  
  mysums <- numeric(ncombos)
  for(i in 1:ncombos){
    calcsum <- sum(combinations[,i])
    mysums[i] <- calcsum
  }
  mymin <- min(mysums)
  mymax <- max(mysums)
  
  cat(mymin, mymax)
  
}
miniMaxSum(arr)



# Complete the 'birthdayCakeCandles' function below.
#
# The function is expected to return an INTEGER.
# The function accepts INTEGER_ARRAY candles as parameter.
#
candles <- c(4,4,1,3)
birthdayCakeCandles <- function(candles) {
  mytab <- table(candles)
  mymax <- max(mytab)
  mymax
}
birthdayCakeCandles(candles)



# Complete the 'timeConversion' function below.
#
# The function is expected to return a STRING.
# The function accepts STRING s as parameter.
#

timeConversion <- function(s) {
  
  # define input as a time
  s2 <- strptime(s, format="%I:%M:%S%p")
  s3 <- format(s2, "%H:%M:%S")
  return(s3)
  
}




# Complete the 'gradingStudents' function below.
#
# The function is expected to return an INTEGER_ARRAY.
# The function accepts INTEGER_ARRAY grades as parameter.
#
grades <- c(4,73,67,38,33)
gradingStudents <- function(grades) {
  
  multiples <- seq(0,100,by=5)
  
  newgrades <- sapply(grades, function(g){
    if(g<38){
      g
    }
    else if((g+1) %in% multiples){
      g+1
    }
    else if((g+2)%in% multiples){
      g+2
    }
    else{g}
  })
  newgrades <- newgrades[newgrades>=40]
  return(newgrades)
  
}

gradingStudents(newgrades)




# Complete the 'kangaroo' function below.
#
# The function is expected to return a STRING.
# The function accepts following parameters:
#  1. INTEGER x1 =  starting point of kangaroo 1
#  2. INTEGER v1 = distance per time step of kang 1
#  3. INTEGER x2 = starting point kangaroo 2
#  4. INTEGER v2 = distance per time step of kang 2
# RETURN: YES if they ever land on same spot, NO if they dont after 10k iters
kangaroo <- function(x1, v1, x2, v2) {
  
  # initialize the point they are at
  k1 <- x1
  k2 <- x2
  
  # iterate through the points they will be, updating k
  for(i in 1:10000){
    k1 <- k1+v1
    k2 <- k2+v2
    
    if(k1==k2){
      return("YES")
    }
  }
  return("NO")
  
}
kangaroo(0, 3, 4, 2)





kangaroofunc <- function(x1, v1, x2, v2){
  k1 <- x1
  k2 <- x2
  
  for (i in 1:10000) {
    k1 <- k1+v1
    k2 <- k2+v2
    
    if(k1 == k2){
      return("TRUE")
    }
  }
  return("FALSE")
}
kangaroofunc(0, 3, 4, 2)





arr <- c(1, 2, 3, 4, 5)
d <- 11
rotateLeft <- function(d, arr) {
  n <- length(arr)
  d <- d %% n
  if(d==0){return(arr)}
  
  firstnums <- arr[(d+1):n]
  lastnums <- arr[1:d]
  newvec <- c(firstnums, lastnums)
  return(newvec)
  
}
rotateLeft(d, arr)









# Complete the 'matchingStrings' function below.
#
# The function is expected to return an INTEGER_ARRAY.
# The function accepts following parameters:
#  1. STRING_ARRAY stringList
#  2. STRING_ARRAY queries
#

matchingStrings <- function(stringList, queries) {
  counts <- integer(length(queries))
  
  for(i in seq_along(queries)){
    counts[i] <- sum(stringList == queries[i])
  }
  return(counts)
  
}
stringList <- c("aba", "baba", "aba", "xzxb")
queries <- c("aba", "xzxb", "ab")
matchingStrings(stringList, queries)






# binary trees
node3 <- list(data = 3, left = NULL, right = NULL)
node4 <- list(data = 4, left = NULL, right = NULL)
node2 <- list(data = 2, left = node3, right = node4)
node5 <- list(data = 5, left = NULL, right = NULL)
root <- list(data = 1, left = node2, right = node5)

preOrder <- function(root) {
  if (is.null(root)) return()  # stop recursion at empty nodes
  cat(root$data, " ")
  preOrder(root$left)
  preOrder(root$right)
}

preOrder(root) 






#### ChatGPT exmaples ####
##### Arrays #####

#' Task: You are given a numeric vector of patient IDs and a second vector of 
#' test results (same length). For each patient, return the maximum test result 
#' observed.
patient_ids <- c(101, 102, 103, 101, 104, 102, 105, 101)
test_results <- c(5.6, 7.2, 6.1, 8.3, 5.9, 7.5, 6.8, 9.0)

maxresult <- function(patient_ids, test_results){
  
  mydf <- data.frame(patient_ids=patient_ids,
                       test_results=test_results)
  
  aggdf <- aggregate(test_results~patient_ids,data=mydf,FUN="max")
  return(aggdf)
}
maxresult(patient_ids, test_results)


# return max and min
maxminresult <- function(patient_ids, test_results){
  
  patientid <- unique(patient_ids)
  maxresult <- tapply(test_results, patient_ids, FUN="max")
  minresult <- tapply(test_results, patient_ids, FUN="min")
  
  returndf <- data.frame(patientid,maxresult, minresult)
  
  return(returndf)
}
maxminresult(patient_ids, test_results)



#### 2. Sliding windows ####
#' Problem: Given a numeric vector of daily glucose readings for 
#' a patient, find the maximum average glucose over any 3 
#' consecutive days.
glucose <- c(110, 120, 105, 130, 125, 115)

myfunc <- function(glucose){
  
  # initialize vector of the averages
  windowsize <- 3
  n <- length(glucose)
  averages <- numeric(n-windowsize+1)
  
  # calculate values
  for(i in 1:length(averages)){
    averages[i] <- mean(glucose[i:(i+windowsize-1)])
  }
  maxval <- max(averages)
  return(maxval)
}
myfunc(glucose)


#' Given a string, find the length of the longest substring with all
#'  unique characters.
string <- c("abcabcbb")
longest_unique_substring <- function(s) {
  chars <- strsplit(s, "")[[1]]
  n <- length(chars)
  
  seen <- list()   # to store last seen index of a character
  left <- 1        # left side of sliding window
  max_len <- 0
  
  for (right in 1:n) {
    ch <- chars[right]
    
    # If character was seen and is inside the current window, move left pointer
    if (!is.null(seen[[ch]]) && seen[[ch]] >= left) {
      left <- seen[[ch]] + 1
    }
    
    # Update last seen position
    seen[[ch]] <- right
    
    # Update max length
    window_len <- right - left + 1
    max_len <- max(max_len, window_len)
  }
  
  return(max_len)
}

# Example
longest_unique_substring(string)




#### 2b. Moving window max Number of Patients in a Time Window ####
#' This is a sliding window problem: you need to efficiently compute sums 
#' of consecutive subarrays of size. return max.

patients <- c(5, 1, 3, 7, 2, 6, 2, 4)
k <- 3

movingwindow <- function(patients, k){
  
  mylength <- length(patients)
  # to hold the vals
  avgs <- numeric(mylength-k+1)
  
  # fill avgs
  for(i in 1:length(avgs)){
    curmean <- sum(patients[i:(i+k-1)])
    avgs[i] <- curmean
  }
  avg_max <- max(avgs)
  return(avg_max)
}
movingwindow(patients,k)




#### 3. String manipulation ####
#' Problem: Check if a DNA sequence only contains valid 
#' nucleotides (A, C, G, T).
dna <- "ACGTACGTTAGX"

myfunc <- function(dna){
  nucleotides <- strsplit(dna, "")[[1]]
  
  # check if all chars are true nucleotides
  for (i in 1:length(nucleotides)) {
    if(!nucleotides[i] %in% c("A","C","G","T")){
      return(FALSE)}
  }
}
myfunc(dna)



#### 4. Binary Tree Traversal (Recursive) ####
#' Problem: Implement preorder traversal of a simple tree structure.
#' Task: Return a numeric vector of node values in preorder (root, left, right).

# Define a simple tree as a list

tree <- list(
  value = 1,
  left = list(value = 2, left = NULL, right = NULL),
  right = list(value = 3, left = NULL, right = NULL)
)

# Recursive preorder traversal function
preorder_traversal <- function(tree) {
  # Base case: if node is NULL, return empty vector
  if (is.null(tree)) return(c())
  
  # Visit root
  result <- tree$value
  
  # Visit left subtree
  result <- c(result, preorder_traversal(tree$left))
  
  # Visit right subtree
  result <- c(result, preorder_traversal(tree$right))
  return(result)
}

# Run the traversal
preorder_traversal(tree)




#### 5. Graph Traversal — Connected Components ####
#' Problem: Given a list of patient interactions (edges), count the 
#' number of connected patient clusters.

edges <- list(
  c(1,2), c(2,3), c(4,5), c(6,6)
)

count_patient_clusters <- function(edges) {
  clusters <- list()
  
  for (i in edges) {
    # Find clusters that overlap with the current edge
    overlapping <- sapply(clusters, function(cl) any(cl %in% i))

    if (any(overlapping)) {
      # Merge overlapping clusters with the edge
      merged_cluster <- unique(c(i, unlist(clusters[overlapping])))
      clusters <- clusters[!overlapping]        # remove old overlapping clusters
      clusters <- c(clusters, list(merged_cluster))  # add merged cluster
    } else {
      # Edge forms a new cluster
      clusters <- c(clusters, list(i))
    }
  }
  
  return(length(clusters))
}

count_patient_clusters(edges)



#### 6. Mini Tabular / Data Wrangling — Column Operations ####
#' Problem: You have a “lab results” table. Compute average lab 
#' value per test type.

labs <- data.frame(
  patient_id = c(101,101,102,103,103),
  test_type = c("A1C","Chol","A1C","Chol","A1C"),
  value = c(7.2, 180, 6.8, 200, 7.5)
)

myfunc <- function(labs){
  mycalc <- aggregate(value~test_type,data=labs,FUN="mean")
  mycalc
}
myfunc(labs)


# now return num tests, min, mean, and max per id
labs <- data.frame(
  patient_id = c(101,101,102,103,103,
                 101,101,102,103,103),
  test_type = c("A1C","Chol","A1C","Chol","A1C",
                "A1C","Chol","A1C","Chol","A1C"),
  value = c(7.2, 180, 6.8, 200, 7.5,
            8.2, 190, 8.8, 300, 8.5)
)
myfunc <- function(labs){
  mincalc <- aggregate(value~patient_id+test_type,data=labs,FUN="min")
  meancalc <- aggregate(value~patient_id+test_type,data=labs,FUN="mean")
  maxcalc <- aggregate(value~patient_id+test_type,data=labs,FUN="max")
  nvisitscalc <- aggregate(value~patient_id+test_type,data=labs,FUN="length")
  
  finaldf <- data.frame(
    patient_id = mincalc$patient_id,
    test_type = mincalc$test_type,
    minval = mincalc$value,
    meanval = meancalc$value,
    maxval = maxcalc$value,
    ntests = nvisitscalc$value
  )
  return(finaldf)
}
myfunc(labs)





#### 7. Filtering — SQL-like WHERE clause ####
#' Problem: Find all patients with A1C > 7.
labs <- data.frame(
  patient_id = c(101,101,102,103,103),
  test_type = c("A1C","Chol","A1C","Chol","A1C"),
  value = c(7.2, 180, 6.8, 200, 7.5)
)

myfunc <- function(labs){
  newdf <- subset(labs, test_type=="A1C" & value>7)
  return(newdf)
}
myfunc(labs)



# now return df with patients who have A1C>7 and CHOL>190
labs <- data.frame(
  patient_id = c(101,101,102,103,103,104),
  test_type  = c("A1C","Chol","A1C","Chol","A1C","Chol"),
  value      = c(7.2, 180, 6.8, 200, 7.5, 195)
)




#### 8. Hash map: ice cream parlor ####
#' You are given a vecotr of ice cream prices. you have m dollars
#' to spend and can get 2 ice creams. Select the two
#' that will cost all the money you have. Return the indices
#' of prices of the 2 flavors they buy sorted ascending.

prices <- c(1,3,4,5,6)
m <- 8

hashfunc <- function(prices,m){
  
  # initialize list of prices i have already seen
  seen <- list()
  
  # start looking at the prices and record them
  for (i in seq_along(prices)) {
    
    # calculate complement of the current price
    complement <- m-prices[i]
    
    # check if ive seen this complement before
    if(as.character(complement) %in% names(seen)){
      
      # combine current i with the complement (j)
      j <- seen[[as.character(complement)]]
      return(sort(c(j,i)))
    }
   
    # if i havent seen this complement before, record it and keep looping
    seen[[as.character(prices[i])]] <- i
  }
  return(NULL)
}

hashfunc(prices, m)



#### 9. Sort intervals ####
# Sort by start time
appointments <- data.frame(
  start = c(1, 3, 2, 6, 8),
  end   = c(3, 5, 4, 7, 10)
)

sortfunc <- function(appointments){
  sorted <- appointments[order(appointments$start),]
  return(sorted)
}
sortfunc(appointments)



#### 10. Binary search ####
#' Find Next Available Appointment. You have a vector of appointment times 
#' (in minutes since clinic opening). A patient wants the next available 
#' appointment after a given time. Use binary search to efficiently find
#'  it.
appointments <- c(120,60, 210, 150, 300)  # sorted appointment times in minutes
target_time <- 130                           # patient wants next available slot after 130

binarysearch <- function(target_time, appointments){
  appointments <- sort(appointments)
  
  # start process initializing left and right
  left <- 1
  right <- length(appointments)
  result <- NA
  
  while(left <= right){
    mid <- floor((left+right)/2)
    if(appointments[mid] == target_time){
      return(mid)}
    else if(appointments[mid] <= target_time){
      left <- mid+1
    }
    else{
      result <- appointments[mid] 
      right <- mid-1
    }
    }
return(result)
}
binarysearch(target_time, appointments)




#### 11. groupAnagrams ####
#' Given a list/vector of strings, group the strings that are anagrams 
#' of each other together.
words <- c("eat", "tea", "tan", "ate", "nat", "bat")

groupAnagrams <- function(words) {
  # initialize hashmap list
  hashmap <- list()
  
  for (i in words) {
    # record current string of letters
    key <- paste0(sort(strsplit(i, "")[[1]]), collapse="")
    
    if (!is.null(hashmap[[key]])) {
      # if the key exists in the hashmap, add the word to the list
      hashmap[[key]] <- c(hashmap[[key]], i)
      
      # if it doesnt exist already, make a new list for it w/in the hashmap
    } else {
      hashmap[[key]] <- c(i)
    }
  }
  
  return(unname(hashmap))  # remove the keys, return only the grouped anagrams
}
groupAnagrams(words)




#### 12. Merge Patient Appointment Intervals ####
#' A clinic has a list of scheduled patient appointments represented as 
#' intervals. Each interval has a start and end time (in minutes since 
#' clinic opening). Some appointments may overlap.
#' Write a function that merges all overlapping intervals and returns the 
#' resulting list sorted by start time.

intervals <- data.frame(
  start = c(30, 60, 45, 120, 150),
  end   = c(75, 90, 70, 180, 200)
)

mergeoverlap <- function(intervals){
  # sort
  intervals <- intervals[order(intervals$start),]
  
  # Initialize merged list with the first interval
  merged <- data.frame(start = intervals$start[1], end = intervals$end[1])
  
  for (i in 2:nrow(intervals)) {
    last_merged <- merged[nrow(merged), ]
    current <- intervals[i, ]
    
    # Check for overlap
    if (current$start <= last_merged$end) {
      # Merge intervals by updating the end
      merged$end[nrow(merged)] <- max(last_merged$end, current$end)
    } else {
      # No overlap, add as a new interval
      merged <- rbind(merged, current)
    }
  }
  return(merged)
}
mergeoverlap(intervals)



#### 13. Count Unique Patients by Visit Rule ####
#' A clinic logs patient visits with the following vector of patient IDs. Some 
#' patients visit multiple times. You need to count the number of unique 
#' patients who only count a patient if they visited at least 2 times.
visits <- c(101, 102, 103, 101, 104, 102, 105, 101)

visitfunc <- function(visits){
  count <- data.frame(table(visits))
  count <- count[count$Freq>=2,]
  return(count)
}
visitfunc(visits)



#### 14. Longest unique substring ####
s <- "abcabcbb"

longest_unique_substring <- function(s) {
  n <- nchar(s)
  if (n == 0) return(0)
  
  chars <- strsplit(s, "")[[1]]   # split into characters
  seen <- list()                  # store last seen index of each char
  left <- 1
  max_len <- 0
  
  for (right in 1:n) {
    c <- chars[right]
    
    # If character was seen and inside the window, move left
    if (!is.null(seen[[c]]) && seen[[c]] >= left) {
      left <- seen[[c]] + 1
    }
    
    # Update last seen position of this char
    seen[[c]] <- right
    
    # Update max length
    max_len <- max(max_len, right - left + 1)
  }
  
  return(max_len)
}




#### 15. FizzBuzz  ####
# Write a program that prints the numbers from 1 to 100, but:
#   For multiples of 3, print "Fizz" instead of the number.
#   For multiples of 5, print "Buzz" instead of the number.
#   For numbers which are multiples of both 3 and 5, print "FizzBuzz".

myfunc <- function(numbers){

  # initialize vecs to compare
  multiples3 <- seq(3,99,by=3)
  multiples5 <- seq(5,100,by=5)
  
  # intiialize results vector
  myresult <- character(length(numbers))
  
  # start filling the vec
  for(i in seq_along(numbers)){
    if(numbers[i] %in% multiples3 & numbers[i] %in% multiples5){
      myresult[i] <- "FizzBuzz"
    }
    else if(numbers[i] %in% multiples3){
      myresult[i] <- "Fizz"
    }
    else if (numbers[i] %in% multiples5){
      myresult[i] <- "Buzz"
    }
    else{myresult[i] <- i}
  }
  return(myresult)
}

numbers <- seq(1,100,by=1)
myfunc(numbers)


craps11 <- 0
roulette11 <- 0
pcraps <- .5

for(i in 1:1000000){
  
  # if crap is selected
  if(runif(1)>.5){
    if(runif(1)<(2/36)){
      craps11 <- craps11+1
    }
  }
  
  # if roulette
  else{
    if(runif(1) < (1/38)){
      roulette11 <- roulette11+1
    }
  }
}


probroulette <- (roulette11)/(roulette11+craps11)
probroulette

probcraps <- (craps11)/(roulette11+craps11)
probcraps



