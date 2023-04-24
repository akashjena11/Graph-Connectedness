setwd("~/Documents/Rstuff/hw7")

graph_connectedness_fun_1 = function(x){
  # requires expm package
  # x is a graph adjacency matrix
  # it should be symmetric
  # it should be non-negative
  # some checks
  if(!('matrix'%in%class(x))){warning('x replaced with as.matrix(x)')
    x = as.matrix(x)}
  if(nrow(x)!=ncol(x)) stop('x is not square')
  if(!is.numeric(x)) stop('x is not numeric')
  if(any(x<0)){warning('x replaced with abs(x)'); x = abs(x)}
  if(nrow(x)==1){warning('x is a 1x1 adjacency matrix'); return(TRUE)}
  if(!isSymmetric(x)){warning('x replaced with sign(x+t(x))'); x = sign(x+t(x))}
  n = nrow(x) - 1
  diag(x) = 1 # add self-loops at each node
  all((x%^%n) > 0) # %ˆ% defined in expm package
}

#install.packages("expm")
library(expm)

library(expm)
set.seed(1234567890)
generate_graph = function(force_linear = FALSE,
                          force_disconnected = FALSE,
                          n = 100L, prob = .1){
  if(!force_linear){
    x = rep(0L,n^2)
    s = sample.int(n^2,n^2*prob)
    x[s] = 1L
    attr(x,'dim') = c(n,n)
    x = sign(x+t(x))
    diag(x) = 0
  }else{
    x = cbind(rbind(c(0),diag(n-1)),c(0))
    x = x+t(x)
  }
  if(force_disconnected){s = sample.int(n,floor(n/2)); x[s,-s] = 0; x[-s,s] = 0}
  return(matrix(as.integer(x),n,n))
}

x_1 = generate_graph(n=150)

Rprof(filename="hw7q1.out", interval=0.001, filter.callframe=TRUE)
graph_connectedness_fun_1(x_1)
Rprof(NULL)


### 1
summaryRprof('hw7q1.out')$by.total

#The operation %^% takes the most time, which is 46.15% of the total time.




### 2
graph_connectedness_fun_2 = function(x){
  # x is a graph adjacency matrix
  # it should be symmetric
  # it should be non-negative
  # some checks
  if(!('matrix'%in%class(x))){warning('x replaced with as.matrix(x)')
    x = as.matrix(x)}
  if(nrow(x)!=ncol(x)) stop('x is not square')
  if(!is.numeric(x)) stop('x is not numeric')
  if(any(x<0)){warning('x replaced with abs(x)'); x = abs(x)}
  if(nrow(x)==1){warning('x is a 1x1 adjacency matrix'); return(TRUE)}
  if(!isSymmetric(x)){warning('x replaced with sign(x+t(x))'); x = sign(x+t(x))}
  n = nrow(x) - 1
  diag(x) = 1 # add self-loops at each node
  state = c(1,rep(0,n)) # start at e_1
  for(i in 1:n){ # loop the multiplication, no real way around this
    state = x%*%state
  }
  all(state>0)
}

#Running Rprof()
Rprof(filename="hw7q2.out", interval=0.001, filter.callframe=TRUE)
graph_connectedness_fun_2(x_1)
Rprof(NULL)

summaryRprof('hw7q1.out') #sampling.time = 0.039
summaryRprof('hw7q2.out') #sampling.time = 0.032

#The second function takes less time. The ratio is 0.032/0.039 = 0.82.
#So the second funtion takes 82% of the time taken by the first one.

x_2 = generate_graph(n=500)
graph_connectedness_fun_2(x_2) #NA
graph_connectedness_fun_1(x_2) #TRUE





### 3
graph_connectedness_fun_3 = function(x){
  #x is a graph adjacency matrix
  # it should be symmetric
  # it should be non-negative
  # some checks
  if(!('matrix'%in%class(x))){warning('x replaced with as.matrix(x)')
    x = as.matrix(x)}
  if(nrow(x)!=ncol(x)) stop('x is not square')
  if(!is.numeric(x)) stop('x is not numeric')
  if(any(x<0)){warning('x replaced with abs(x)'); x = abs(x)}
  if(nrow(x)==1){warning('x is a 1x1 adjacency matrix'); return(TRUE)}
  if(!isSymmetric(x)){warning('x replaced with sign(x+t(x))'); x = sign(x+t(x))}
  n = nrow(x) - 1
  diag(x) = 1 # add self-loops at each node
  state = c(1,rep(0,n)) # start at e_1
  for(i in 1:n){ # loop the multiplication, no real way around this
    state = sign(x%*%state)
  }
  all(state>0)
}


graph_connectedness_fun_3(x_2) #Return TRUE

#Running Rprof()
Rprof(filename="hw7q3.out", interval=0.001, filter.callframe=TRUE)
graph_connectedness_fun_3(x_2)
Rprof(NULL)

summaryRprof('hw7q3.out')$by.total
#The operation %*% is taking the most amount of time (72% of total).

x_3 = generate_graph(n=1000)
system.time(graph_connectedness_fun_3(x_3)) #The time taken was 1.546 seconds.

Rprof(filename="hw7_task3.out", interval=0.001, filter.callframe=TRUE)
graph_connectedness_fun_3(x_3)
Rprof(NULL)

summaryRprof('hw7_task3.out')$by.total
#The operation %*% is taking the most amount of time (93.74% of total).



### 4
graph_connectedness_fun_4 = function(x){
  # x is a graph adjacency matrix
  # it should be symmetric
  # it should be non-negative
  # some checks
  if(!('matrix'%in%class(x))){warning('x replaced with as.matrix(x)')
    x = as.matrix(x)}
  if(nrow(x)!=ncol(x)) stop('x is not square')
  if(!is.numeric(x)) stop('x is not numeric')
  if(any(x<0)){warning('x replaced with abs(x)'); x = abs(x)}
  if(nrow(x)==1){warning('x is a 1x1 adjacency matrix'); return(TRUE)}
  if(!isSymmetric(x)){warning('x replaced with sign(x+t(x))'); x = sign(x+t(x))}
  n = nrow(x) - 1
  diag(x) = 1 # add self-loops at each node
  state = c(1,rep(0,n)) # start at e_1
  for(i in 1:n){ # loop the multiplication, no real way around this
    state = sign(x%*%state)
    if(all(state>0)) return(TRUE)
  }
  all(state>0)
}


#Running Rprof()
Rprof(filename="hw7q4.out", interval=0.001, filter.callframe=TRUE)
graph_connectedness_fun_4(x_3)
Rprof(NULL)

summaryRprof('hw7q4.out')$by.total
#Top five operations in terms of time taken are as follows.
#all.equal.numeric takes 33.85%
#diag<- takes 32.31%
#is.na takes 7.69%
#any takes 4.62%
#findCenvVar takes 4.62%


x_4 = generate_graph(n=1000,force_disconnected=TRUE)
system.time(graph_connectedness_fun_4(x_4)) #The time taken was 1.587 seconds.



### 5
graph_connectedness_fun_5 = function(x){
  # x is a graph adjacency matrix
  # it should be symmetric
  # it should be non-negative
  # some checks
  if(!('matrix'%in%class(x))){warning('x replaced with as.matrix(x)')
    x = as.matrix(x)}
  if(nrow(x)!=ncol(x)) stop('x is not square')
  if(!is.numeric(x)) stop('x is not numeric')
  if(any(x<0)){warning('x replaced with abs(x)'); x = abs(x)}
  if(nrow(x)==1){warning('x is a 1x1 adjacency matrix'); return(TRUE)}
  if(!isSymmetric(x)){warning('x replaced with sign(x+t(x))'); x = sign(x+t(x))}
  n = nrow(x) - 1
  diag(x) = 1 # add self-loops at each node
  state = c(1,rep(0,n)) # start at e_1
  old_state = rep(NA,n+1)
  for(i in 1:n){ # loop the multiplication, no real way around this
    old_state = state
    state = sign(x%*%state)
    if(all(state==old_state)) break
  }
  all(state>0)
}

system.time({graph_connectedness_fun_5(x_3)}) #The time taken was 0.028 seconds.
system.time({graph_connectedness_fun_5(x_4)}) #The time taken was 0.029 seconds.

x_5 = generate_graph(n=5000)

#Running Rprof()
Rprof(filename="hw7q5.out", interval=0.001, filter.callframe=TRUE)
graph_connectedness_fun_5(x_5)
Rprof(NULL)

summaryRprof('hw7q5.out')$by.total

#The operation %*% is 9th in terms of total time spent.
#The top 5 operations in terms of time spent are as follows.
#all.equal.numeric 22.65%
#diag<- 20.96%
#is.na 19.76%
#graph_connectedness_fun_5 13.01%
#as.vector 8.31%



### 6
graph_connectedness_fun_6 = function(x){
  # x is a graph adjacency matrix
  # it should be symmetric
  # it should be non-negative
  # some checks
  if(!('matrix'%in%class(x))){warning('x replaced with as.matrix(x)')
    x = as.matrix(x)}
  if(nrow(x)!=ncol(x)) stop('x is not square')
  if(!is.numeric(x)) stop('x is not numeric')
  if(any(x<0)){warning('x replaced with abs(x)'); x = abs(x)}
  if(nrow(x)==1){warning('x is a 1x1 adjacency matrix'); return(TRUE)}
  #if(!isSymmetric(x)){warning('x replaced with sign(x+t(x))'); x = sign(x+t(x))}
  n = nrow(x) - 1
  diag(x) = 1 # add self-loops at each node
  state = c(1,rep(0,n)) # start at e_1
  old_state = rep(NA,n+1)
  for(i in 1:n){ # loop the multiplication, no real way around this
    old_state = state
    state = sign(x%*%state)
    if(all(state==old_state)) break
  }
  all(state>0)
}

system.time(graph_connectedness_fun_6(x_5)) #The time taken was 0.446 seconds.

x_6 = generate_graph(n=10000)

#Running Rprof()
Rprof(filename="hw7q6.out", interval=0.001, filter.callframe=TRUE)
graph_connectedness_fun_6(x_6)
Rprof(NULL)

summaryRprof('hw7q6.out')$by.total
#%*% ranks 3rd in terms of time taken (out of 4 functions).
#The top 5 operations in terms of time taken are as follows.
#diag<- takes 51.24%
#graph_connectedness_fun_6 takes 26.94%
#%*% takes 17.96%
#any takes 3.85%




### 7
graph_connectedness_fun_7 = function(x){
  # x is a graph adjacency matrix
  # it should be symmetric
  # it should be non-negative
  # some checks
  if(!('matrix'%in%class(x))){warning('x replaced with as.matrix(x)')
    x = as.matrix(x)}
  if(nrow(x)!=ncol(x)) stop('x is not square')
  if(!is.numeric(x)) stop('x is not numeric')
  if(any(x<0)){warning('x replaced with abs(x)'); x = abs(x)}
  if(nrow(x)==1){warning('x is a 1x1 adjacency matrix'); return(TRUE)}
  #if(!isSymmetric(x)){warning('x replaced with sign(x+t(x))'); x = sign(x+t(x))}
  n = nrow(x) - 1
  #diag(x) = 1 # add self-loops at each node
  state = c(1,rep(0,n)) # start at e_1
  old_state = rep(NA,n+1)
  for(i in 1:n){ # loop the multiplication, no real way around this
    old_state = state
    state = sign(x%*%state+state)
    if(all(state==old_state)) break
  }
  all(state>0)
}

system.time(graph_connectedness_fun_7(x_6)) #The time taken was 2.413 seconds


x_7 = generate_graph(n=10000,force_disconnected=TRUE)

#Running Rprof()
Rprof(filename="hw7q7.out", interval=0.001, filter.callframe=TRUE)
graph_connectedness_fun_7(x_7)
Rprof(NULL)

summaryRprof('hw7q7.out')$by.total
#%*% is ranked 1st among 3 functions.
#any is ranked 3rd.
#The top functions are as follows.
#%*% takes 60.64%
#graph_connectedness_fun_7 takes 30.41%
#any takes 8.94%



### 8
graph_connectedness_fun_8 = function(x){
  # x is a graph adjacency matrix
  # it should be symmetric
  # it should be non-negative
  # some checks
  if(!('matrix'%in%class(x))){warning('x replaced with as.matrix(x)')
    x = as.matrix(x)}
  if(nrow(x)!=ncol(x)) stop('x is not square')
  if(!is.numeric(x)) stop('x is not numeric')
  #if(any(x<0)){warning('x replaced with abs(x)'); x = abs(x)}
  if(nrow(x)==1){warning('x is a 1x1 adjacency matrix'); return(TRUE)}
  #if(!isSymmetric(x)){warning('x replaced with sign(x+t(x))'); x = sign(x+t(x))}
  n = nrow(x) - 1
  #diag(x) = 1 # add self-loops at each node
  state = c(1,rep(0,n)) # start at e_1
  old_state = rep(NA,n+1)
  for(i in 1:n){ # loop the multiplication, no real way around this
    old_state = state
    state = sign(x%*%state + state)
    if(all(state==old_state)) break
  }
  all(state>0)
}

system.time(graph_connectedness_fun_8(x_6)) #The time taken was 1.881 seconds
system.time(graph_connectedness_fun_8(x_7)) #The time taken was 1.991 seconds


x_8 = generate_graph(n=500,force_linear=TRUE)

#Running Rprof()
Rprof(filename="hw7q8.out", interval=0.001, filter.callframe=TRUE)
graph_connectedness_fun_8(x_8)
Rprof(NULL)

summaryRprof('hw7q8.out')$by.total
#%*% ranks 1st in terms of time taken (out of two functions).
#The order is as follows.
#%*% takes 88.4%
#graph_connectedness_fun_8 takes 11.6%



### 9
graph_connectedness_fun_9 = function(x){
  # x is a graph adjacency matrix
  # it should be symmetric
  # it should be non-negative
  # some checks
  if(!('matrix'%in%class(x))){warning('x replaced with as.matrix(x)')
    x = as.matrix(x)}
  if(nrow(x)!=ncol(x)) stop('x is not square')
  if(!is.numeric(x)) stop('x is not numeric')
  #if(any(x<0)){warning('x replaced with abs(x)'); x = abs(x)}
  if(nrow(x)==1){warning('x is a 1x1 adjacency matrix'); return(TRUE)}
  #if(!isSymmetric(x)){warning('x replaced with sign(x+t(x))'); x = sign(x+t(x))}
  n = nrow(x) - 1
  np1 = n+1
  #diag(x) = 1 # add self-loops at each node
  state = c(TRUE,rep(FALSE,n)) # start at e_1
  old_state = rep(NA,length(state))
  while(TRUE){ # loop the multiplication, no real way around this
    old_state = state
    state = (.rowSums(x[,state,drop=FALSE],np1,sum(state))>0) | state
    if(all(state==old_state)) break
  }
  all(state)
}

system.time(graph_connectedness_fun_9(x_6)) #0.807 seconds
system.time(graph_connectedness_fun_9(x_7)) #0.329 seconds
system.time(graph_connectedness_fun_9(x_8)) #0.358 seconds

x_9 = generate_graph(n=20000)

#Running Rprof()
Rprof(filename="hw7q9.out", interval=0.001, filter.callframe=TRUE)
graph_connectedness_fun_9(x_9)
Rprof(NULL)

summaryRprof('hw7q9.out')$by.total
#.rowSums ranks 1 among 3 operations.
#The order is as follows.
#.rowSums takes 99.96%
#graph_connectedness_fun_9 takes 0.03%
#c takes 0.01%



### 10
graph_connectedness_fun_10 = function(x){
  # x is a graph adjacency matrix
  # it should be symmetric
  # it should be non-negative
  # some checks
  if(!('matrix'%in%class(x))){warning('x replaced with as.matrix(x)')
    x = as.matrix(x)}
  if(nrow(x)!=ncol(x)) stop('x is not square')
  if(!is.numeric(x)) stop('x is not numeric')
  #if(any(x<0)){warning('x replaced with abs(x)'); x = abs(x)}
  if(nrow(x)==1){warning('x is a 1x1 adjacency matrix'); return(TRUE)}
  #if(!isSymmetric(x)){warning('x replaced with sign(x+t(x))'); x = sign(x+t(x))}
  #diag(x) = 1 # add self-loops at each node
  np1 = nrow(x)
  state = 1 # start at e_1
  old_state = NA
  while(TRUE){ # loop the multiplication, no real way around this
    old_state = state
    state = sort(unique(c(state,state[.rowSums(x[,state,drop=FALSE],
                                               np1,length(state))>0])))
    if(all(state==old_state)) break
  }
  all(state)
}


system.time(graph_connectedness_fun_10(x_6)) #0.015 seconds
system.time(graph_connectedness_fun_10(x_7)) #0.001 seconds
system.time(graph_connectedness_fun_10(x_8)) #0.000 seconds
system.time(graph_connectedness_fun_10(x_9)) #0.001 seconds

x_10 = generate_graph(n=20000, force_disconnected=TRUE)

#Running Rprof()
Rprof(filename="hw7q10.out", interval=0.001, filter.callframe=TRUE)
graph_connectedness_fun_10(x_10)
Rprof(NULL)

summaryRprof('hw7q10.out')$by.total
#.rowSums ranks 1 (out of 2 functions).
#The order is as follows.
#.rowSums takes 100%
#graph_connectedness_fun_10 takes 0%



### 11
graph_connectedness_fun_9_v2 = function(x){
  # x is a graph adjacency matrix
  # it should be symmetric
  # it should be non-negative
  # some checks
  if(!('matrix'%in%class(x))){warning('x replaced with as.matrix(x)')
    x = as.matrix(x)}
  if(nrow(x)!=ncol(x)) stop('x is not square')
  if(!is.numeric(x)) stop('x is not numeric')
  #if(any(x<0)){warning('x replaced with abs(x)'); x = abs(x)}
  if(nrow(x)==1){warning('x is a 1x1 adjacency matrix'); return(TRUE)}
  #if(!isSymmetric(x)){warning('x replaced with sign(x+t(x))'); x = sign(x+t(x))}
  #diag(x) = 1 # add self-loops at each node
  state = c(TRUE,rep(FALSE,nrow(x) - 1)) # start at e_1
  available_indices = 2:length(state) # 2:nrow(x)
  RSg0 = NA
  while(TRUE){ # loop the multiplication, no real way around this
    RSg0 = (.rowSums(x[available_indices,state,drop=FALSE],
                     length(available_indices),sum(state))>0)
    if(all(RSg0)) return(TRUE)
    if(all(!RSg0)) return(FALSE)
    state[available_indices[RSg0]] = TRUE
    available_indices = available_indices[!RSg0]
  }
}


graph_connectedness_fun_10_v2 = function(x){
  # x is a graph adjacency matrix
  # it should be symmetric
  # it should be non-negative
  # some checks
  if(!('matrix'%in%class(x))){warning('x replaced with as.matrix(x)')
    x = as.matrix(x)}
  if(nrow(x)!=ncol(x)) stop('x is not square')
  if(!is.numeric(x)) stop('x is not numeric')
  #if(any(x<0)){warning('x replaced with abs(x)'); x = abs(x)}
  if(nrow(x)==1){warning('x is a 1x1 adjacency matrix'); return(TRUE)}
  #if(!isSymmetric(x)){warning('x replaced with sign(x+t(x))'); x = sign(x+t(x))}
  #diag(x) = 1 # add self-loops at each node
  state = 1 # start at 1
  available_indices = 2:nrow(x)
  RSg0 = NA
  while(TRUE){ # loop the multiplication, no real way around this
    RSg0 = .rowSums(x[available_indices,state,drop=FALSE],
                    length(available_indices),length(state))>0
    if(all(RSg0)) return(TRUE)
    if(all(!RSg0)) return(FALSE)
    state = c(state,available_indices[RSg0])
    available_indices = available_indices[!RSg0]
  }
}

#9 version 2
system.time(graph_connectedness_fun_9_v2(x_6)) #0.385 seconds
system.time(graph_connectedness_fun_9_v2(x_7)) #0.508 seconds
system.time(graph_connectedness_fun_9_v2(x_8)) #0.163 seconds
system.time(graph_connectedness_fun_9_v2(x_9)) #0.831 seconds
system.time(graph_connectedness_fun_9_v2(x_10)) #2.189 seconds


#10 version 2
system.time(graph_connectedness_fun_10_v2(x_6)) #0.101 seconds
system.time(graph_connectedness_fun_10_v2(x_7)) #0.186 seconds
system.time(graph_connectedness_fun_10_v2(x_8)) #0.138 seconds
system.time(graph_connectedness_fun_10_v2(x_9)) #0.347 seconds
system.time(graph_connectedness_fun_10_v2(x_10)) #0.917 seconds


x_11 = generate_graph(n=1000, force_linear=TRUE)

#Running Rprof() for version 2 of function 9
Rprof(filename="hw7q11_9v2.out", interval=0.001, filter.callframe=TRUE)
graph_connectedness_fun_9_v2(x_11)
Rprof(NULL)

summaryRprof('hw7q11_9v2.out')$by.total


#Running Rprof() for version 2 of function 10
Rprof(filename="hw7q11_10v2.out", interval=0.001, filter.callframe=TRUE)
graph_connectedness_fun_10_v2(x_11)
Rprof(NULL)

summaryRprof('hw7q11_10v2.out')$by.total

#10_v2 is better than 9_v2. Because .rowSums is taking less time in 10_v2.
