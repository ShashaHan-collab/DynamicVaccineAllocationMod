library(Matrix)
library(gurobi)
require(zoo)
##################################################################################
## The file includes a list of functions used in simulating the scenario without vaccines, 
## the uniform allocation and the optimalallocations with myopic stratgies
##################################################################################


# Caulate the disease transmission probabity beta
cal_beta <- function(C,N,R0,gamma){
  prop = N/sum(N) 
  M = C 
  nage = length(N)
  for(i in 1:nage){ 
    for(j in 1:nage){
      M[i,j] <- C[i,j]*prop[i]/prop[j]
    }
  }
  eig = eigen(M)
  beta = R0*gamma/max(Re(eig$values))  
}


# The comparment models with nage age groups and five states
SIRfunc <- function(time, state, parms){
  ncompartment = 5
  nage = length(state)/ncompartment
  S    = as.matrix(state[1:nage])
  V    = as.matrix(state[(nage+1):(2*nage)])
  U    = as.matrix(state[(2*nage+1):(3*nage)])
  I    = as.matrix(state[(3*nage+1):(4*nage)])
  R    = as.matrix(state[(4*nage+1):(5*nage)])
  
  V = pmax(V,0)
  U = pmax(U,0)
  I = pmax(I,0)
  with(as.list(parms),{
    N = S+V+U+I+R
    dS = - as.matrix(v) -as.matrix((S - v)*beta)*(C%*%as.matrix(I/N))
    dV = + as.matrix(v) -as.matrix((V+v)*w) -as.matrix((V+v)*(1-w)*beta)*(C%*%as.matrix(I/N))
    dU = + as.matrix((V+v)*w)*as.matrix(1-eff)-as.matrix(U*beta)*(C%*%as.matrix(I/N))
    dI = + as.matrix((S + U + V*(1-w) - v*w)*beta)*(C%*%as.matrix(I/N)) - gamma*as.matrix(I)
    dR = + gamma*as.matrix(I) + as.matrix((V+v)*w)*as.matrix(eff)
    out=c(dS,dV,dU,dI,dR)
    list(out)
  })
}


# Allocate vaccines 
allocation_model <- function(param,policy){
  SIRfunc <- function(time, state, parms){
    ncompartment = 5
    nage = length(state)/ncompartment
    S    = as.matrix(state[1:nage])
    V    = as.matrix(state[(nage+1):(2*nage)])
    U    = as.matrix(state[(2*nage+1):(3*nage)])
    I    = as.matrix(state[(3*nage+1):(4*nage)])
    R    = as.matrix(state[(4*nage+1):(5*nage)])
    
    V = pmax(V,0)
    U = pmax(U,0)
    I = pmax(I,0)
    with(as.list(parms),{
      N = S+V+U+I+R
      dS = - as.matrix(v) -as.matrix((S - v)*beta)*(C%*%as.matrix(I/N))
      dV = + as.matrix(v) -as.matrix((V+v)*w) -as.matrix((V+v)*(1-w)*beta)*(C%*%as.matrix(I/N))
      dU = + as.matrix((V+v)*w)*as.matrix(1-eff)-as.matrix(U*beta)*(C%*%as.matrix(I/N))
      dI = + as.matrix((S + U + V*(1-w) - v*w)*beta)*(C%*%as.matrix(I/N)) - gamma*as.matrix(I)
      dR = + gamma*as.matrix(I) + as.matrix((V+v)*w)*as.matrix(eff)
      out=c(dS,dV,dU,dI,dR)
      list(out)
    })
  }
  
  cpt <-  param$cpt
  eff <- param$eff
  w <- param$w
  N <- param$N
  beta <- param$beta
  gamma <- param$gamma
  nage <- param$nage
  Timelim <- param$Timelim
  ntiers <- param$niters
  epoch <- param$epoch
  
  # Track the populations that eligible for vaccinations
  T12 <- param$T12*param$vc.tier
  names(T12) <- paste0("tier12",1:nage)
  T3 <- param$T3 *param$vc 
  Vca <- cbind(t(T12),t(T3))
  Vca[is.na(Vca)] <- 0
  
  # Initial conditions
  I_0 = as.matrix(rep(1, nage))   
  S_0 = as.matrix(N - I_0)
  R_0 = as.matrix(rep(0, nage)) 
  V_0 = as.matrix(rep(0, nage))
  U_0 = as.matrix(rep(0, nage))
  v = as.matrix(rep(0, nage))
  vparameters=c(gamma=gamma,beta=beta, eff = eff,v = v, w = w, C=C)
  inits=c(S=S_0,V=V_0,U=U_0,I=I_0,R=R_0)
  

  vt = seq(0,epoch,1)
  # Update states until the day vaccinations start
  svir.infec = as.data.frame(lsoda(inits, vt, SIRfunc, parms = vparameters)) 
  svir.infec <- svir.infec %>% filter(time <= (epoch - 1))

 
  Vcount <- as.data.frame(array(0,c(Timelim+1, nage*ntiers)))
  colnames(Vcount) <- paste0("V",1:(nage*ntiers),sep= "")
  Vcount$time <- 0:Timelim
  
  
  # Start allocating vaccines 
  t <- epoch 
  for (t in epoch:Timelim){
    S_t <- svir.infec %>% filter(row_number()==n()) %>%
      select(paste0("S",1:nage,sep = ""))%>%
      t(.)
    V_t <- svir.infec %>% filter(row_number()==n()) %>%
      select(paste0("V",1:nage,sep = ""))%>%
      t(.)
    U_t <- svir.infec %>% filter(row_number()==n()) %>%
      select(paste0("V",1:nage,sep = ""))%>%
      t(.)
    I_t <- svir.infec %>% filter(row_number()==n()) %>%
      select(paste0("I",1:nage,sep = ""))%>%
      t(.)
    R_t <- svir.infec %>% filter(row_number()==n()) %>%
      select(paste0("R",1:nage,sep = ""))%>%
      t(.)
 
    # Update allocated vaccines
    v.update <- va_alloc(beta,S_t,Vca,I_t,cpt,C,N,param,policy) 
    Vcount[t, ] <- c(v.update,t-1)
    Vca.occup <- Vcount %>% filter(time == (t-1)) %>%
      select(paste0("V",1:(nage*ntiers),sep = ""))
    Vca <- pmax(0,Vca - Vca.occup)
    
    #  Sum every nage columns to get the total allocation for an age group
    v <- sapply(seq(1,nage,by=1),function(i) sum(v.update[c(i,i+nage)]))
    vparameters2 =c(gamma=gamma,beta=beta, eff = eff,v = v, w = w, C=C)
    inits=c(S=S_t,V=V_t,U=U_t,I=I_t,R=R_t)
    
    # Progress to the next day
    vt = seq(0,1,1) 
    svir.update = as.data.frame(lsoda(inits, vt, SIRfunc, parms = vparameters2))
    latest <- svir.update %>% filter(time == 1)
    latest$time <- t
    svir.infec[t+1,] <- latest
  } 
  
  #  Sum every nage columns to get the total allocation for an age group
  Vcount.out <- as.data.frame(sapply(seq(1,nage,by=1),function(i) rowSums(Vcount[,c(i,i+nage)])))
  Vcount.out$time <- 0:Timelim
  
  out <- list()
  out$svir <- svir.infec
  out$vaccine <- Vcount.out
  return(out)
}

## allocation functions
va_alloc <- function(beta,S_t,Vca,I_t,cpt,C,N,param,policy){
  ntiers <- length(Vca)/length(S_t)
  
  # Check the first tier 
  if(max(Vca) == 0){
    x = rep(0,nage*ntiers) 
  }else{ 
    # Check reamingins in the first tier, allocate according proportionally to the populations 
    if(sum(Vca[1:nage]) >= cpt){
      Sd <-  c(S_t, rep(0,nage))
      x12 <- pmin(Sd,Vca)/sum(pmin(Sd,Vca))*cpt
      x <- x12
    }else if(sum(Vca[1:nage]) < cpt){
      # Below is for double-check, which in fact equals x12 <- rep(0,nage*ntiers)
      x12 <- c(Vca[1:nage],rep(0,nage)) 
      cpt <-  max(0,cpt-sum(Vca[1:nage]))
      Vca3 <- Vca[(nage + 1): (nage*ntiers)]
      if(policy == "minInfec"){
        x3 <- opt_infec_alloc(beta,S_t,Vca3,I_t,cpt,C, N) 
      }else if(policy == "minHosp"){
        x3 <- opt_hosp_alloc(beta,S_t,Vca3,I_t,cpt,C,N,param$hosp) 
      }else if(policy == "minDeath"){
        x3 <- opt_death_alloc(beta,S_t,Vca3,I_t,cpt,C, N,param$deathr) 
      }else if(policy == "minICU"){
        x3 <- opt_icu_alloc(beta,S_t,Vca3,I_t,cpt,C,N,param$icur) 
      }else if(policy == "minSymp"){
        x3 <- opt_symp_alloc(beta,S_t,Vca3,I_t,cpt,C,N,param$symp) 
      }else if(policy == "dynUnif"){
        x3 <- dynamic_unif_alloc(S_t,Vca3,cpt) 
      }
      x3 <- c(rep(0,nage), x3)
      x <- x12 + x3
    }
  }
  return(x)
}


##################################################################
# Allocation strategies under specific policies
##################################################################

# Uniform allocation
dynamic_unif_alloc <- function(S,Vca3,cpt){
  x <- pmin(S,Vca3)/sum(pmin(S,Vca3))*cpt 
  return(x)
}

# Optimal allocation minimizing daily infections
opt_infec_alloc <- function(beta,S,Vca,I,cpt,C,N){
  lambda <- beta*C%*%as.matrix(I/N)
  
  model <- list()
  
  model$A          <- matrix(rep(1,length(S)), nrow=1, byrow=T) # contraint matrix
  model$obj        <- t(lambda) # the coefficients of objectives
  model$modelsense <- 'max'
  model$rhs        <- c(cpt) # right hand side of constraints 
  model$sense      <- c('<=')
  model$lb         <- 0 # lower bound of allocation variables
  model$ub         <- pmin(S,Vca) # upper bound of allocation variables
  
  result <- gurobi(model,list(OutputFlag = 0))
  return(result$x)
}

# Optimal allocation minimizing daily symptomatic cases
opt_symp_alloc <- function(beta,S,Vca,I,cpt,C,N,symp){
  lambda <- beta*C%*%as.matrix(I/N)
  lambda <- lambda *symp
  model <- list()
  
  model$A          <- matrix(rep(1,length(S)), nrow=1, byrow=T) # contraint matrix
  model$obj        <- t(lambda) # the coefficients of objectives
  model$modelsense <- 'max'
  model$rhs        <- c(cpt) # right hand side of constraints 
  model$sense      <- c('<=')
  model$lb         <- 0 # lower bound of allocation variables
  model$ub         <- pmin(S,Vca) # upper bound of allocation variables
  
  result <- gurobi(model,list(OutputFlag = 0))
  return(result$x)
}

# Optimal allocation minimizing daily hospitlaizations
opt_hosp_alloc <- function(beta,S,Vca,I,cpt,C,N,hosp){
  lambda <- beta*C%*%as.matrix(I/N)
  lambda <- lambda *hosp
  model <- list()
  
  model$A          <- matrix(rep(1,length(S)), nrow=1, byrow=T) # contraint matrix
  model$obj        <- t(lambda) # the coefficients of objectives
  model$modelsense <- 'max'
  model$rhs        <- c(cpt) # right hand side of constraints 
  model$sense      <- c('<=')
  model$lb         <- 0 # lower bound of allocation variables
  model$ub         <- pmin(S,Vca) # upper bound of allocation variables
  
  result <- gurobi(model,list(OutputFlag = 0))
  return(result$x)
}

# Optimal allocation minimizing daily ICUs
opt_icu_alloc <- function(beta,S,Vca,I,cpt,C,N,icur){
  lambda <- beta*C%*%as.matrix(I/N)
  lambda <- lambda *icur
  model <- list()
  
  model$A          <- matrix(rep(1,length(S)), nrow=1, byrow=T) # contraint matrix
  model$obj        <- t(lambda) # the coefficients of objectives
  model$modelsense <- 'max'
  model$rhs        <- c(cpt) # right hand side of constraints 
  model$sense      <- c('<=')
  model$lb         <- 0 # lower bound of allocation variables
  model$ub         <- pmin(S,Vca) # upper bound of allocation variables
  
  result <- gurobi(model,list(OutputFlag = 0))
  return(result$x)
}

# Optimal allocation minimizing daily deaths
opt_death_alloc <- function(beta,S,Vca,I,cpt,C,N,deathr){
  lambda <- beta*C%*%as.matrix(I/N)
  lambda <- lambda *deathr
  
  model <- list()
  
  model$A          <- matrix(rep(1,length(S)), nrow=1, byrow=T) # contraint matrix
  model$obj        <- t(lambda) # the coefficients of objectives
  model$modelsense <- 'max'
  model$rhs        <- c(cpt) # right hand side of constraints 
  model$sense      <- c('<=')
  model$lb         <- 0 # lower bound of allocation variables
  model$ub         <- pmin(S,Vca) # upper bound of allocation variables
  
  result <- gurobi(model,list(OutputFlag = 0))
  return(result$x)
}





