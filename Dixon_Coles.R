####################################################################################
# PART 1
####################################################################################

tau <- Vectorize(function(xx, yy, lambda, mu, rho){
  if (xx == 0 & yy == 0){return(1 - (lambda*mu*rho))
  } else if (xx == 0 & yy == 1){return(1 + (lambda*rho))
  } else if (xx == 1 & yy == 0){return(1 + (mu*rho))
  } else if (xx == 1 & yy == 1){return(1 - rho)
  } else {return(1)}
})


DClogLik <- function(y1, y2, lambda, mu, rho=0){
  #rho=0, independence
  #y1: home goals
  #y2: away goals
  sum(log(tau(y1, y2, lambda, mu, rho)) + log(dpois(y1, lambda)) + log(dpois(y2, mu)))
}

DCmodelData <- function(df){
  
  team.names <- unique(c(levels(df$HomeTeam), levels(df$AwayTeam)))
  
  # attack, with sum-to-zero constraint
  ## home
  hm.a <- model.matrix(~ HomeTeam - 1, data=df)
  hm.a[df$HomeTeam == team.names[length(team.names)], ] <- -1
  hm.a <- hm.a[,1:(length(team.names)-1)]
  
  # away
  am.a <- model.matrix(~ AwayTeam -1, data=df)
  am.a[df$AwayTeam == team.names[length(team.names)], ] <- -1
  am.a <- am.a[,1:(length(team.names)-1)]
  
  # defence, same as before 
  hm.d <- model.matrix(~ HomeTeam - 1, data=df)
  am.d <- model.matrix(~ AwayTeam -1, data=df)
  
  return(list(homeTeamDMa=hm.a, homeTeamDMd=hm.d,
              awayTeamDMa=am.a, awayTeamDMd=am.d,
              homeGoals=df$FTHG, awayGoals=df$FTAG,
              teams=team.names)) 
}


DCoptimFn <- function(params, DCm){
  
  home.p <- params[1]
  rho.p <- params[2]
  
  nteams <- length(DCm$teams)
  attack.p <- matrix(params[3:(nteams+1)], ncol=1) #one column less
  defence.p <- matrix(params[(nteams+2):length(params)], ncol=1) 
  
  # need to multiply with the correct matrices
  lambda <- exp(DCm$homeTeamDMa %*% attack.p + DCm$awayTeamDMd %*% defence.p + home.p)
  mu <- exp(DCm$awayTeamDMa %*% attack.p + DCm$homeTeamDMd %*% defence.p)
  
  
  return(
    DClogLik(y1=DCm$homeGoals, y2=DCm$awayGoals, lambda, mu, rho.p) * -1
  )
}

#DCattackConstr <- function(params, DCm, ...){
#  nteams <- length(DCm$teams)
#  attack.p <- matrix(params[3:(nteams+2)], ncol=1)
#  return((sum(attack.p) / nteams) - 1)
#}


dta <- read.csv('http://www.football-data.co.uk/mmz4281/1617/E0.csv') # For 16/17 Premier League Season
dcm <- DCmodelData(dta)
nteams <- length(dcm$teams)

#initial parameter estimates
attack.params <- rep(.1, times=nteams-1) # one less parameter
defence.params <- rep(-0.8, times=nteams)
home.param <- 0.06
rho.init <- 0.03
par.inits <- c(home.param, rho.init, attack.params, defence.params)

#informative names
#skip the last team
names(par.inits) <- c('HOME', 'RHO', 
                      paste('Attack', dcm$teams[1:(nteams-1)], sep='.'),
                      paste('Defence', dcm$teams, sep='.'))






library(alabama)
res <- optim(par=par.inits, fn=DCoptimFn, DCm=dcm, method='BFGS')


res
res$par



parameters <- res$par
#compute 'missing team' attack parameter
missing.attack <- sum(parameters[3:(nteams+1)]) * -1

#put it in the parameters vector
parameters <- c(parameters[1:(nteams+1)], missing.attack, parameters[(nteams+2):length(parameters)])
names(parameters)[nteams+2] <- paste('Attack.', dcm$teams[nteams], sep='')

#increase attack by one
parameters[3:(nteams+2)] <- parameters[3:(nteams+2)] + 1  

#decrease defence by one
parameters[(nteams+3):length(parameters)] <- parameters[(nteams+3):length(parameters)] - 1 

######################################################################
parameters

res$par <- parameters
######################################################################

####################################################################################
# PART 2
####################################################################################

# Expected goals home
res$par
lambda <- exp(res$par['HOME'] + res$par['Attack.Watford'] + res$par['Defence.Leicester'])
lambda

# Expected goals away
mu <- exp(res$par['Attack.Leicester'] + res$par['Defence.Watford'])
mu

maxgoal <- 10 # will be useful later
probability_matrix <- dpois(0:maxgoal, lambda) %*% t(dpois(0:maxgoal, mu))
probability_matrix

scaling_matrix <- matrix(tau(c(0,1,0,1), c(0,0,1,1), lambda, mu, res$par['RHO']), nrow=2)
probability_matrix[1:2, 1:2] <- probability_matrix[1:2, 1:2] * scaling_matrix
probability_matrix

HomeWinProbability <- sum(probability_matrix[lower.tri(probability_matrix)])

DrawProbability <- sum(diag(probability_matrix))

AwayWinProbability <- sum(probability_matrix[upper.tri(probability_matrix)])

HomeWinProbability
DrawProbability
AwayWinProbability

######################################################################
awayG <- numeric(maxgoal)
for (gg in 2:maxgoal){
  awayG[gg-1] <- sum(diag(probability_matrix[,gg:(maxgoal+1)]))
}
awayG[maxgoal] <- probability_matrix[1,(maxgoal+1)]

homeG <- numeric(maxgoal)
for (gg in 2:maxgoal){
  homeG[gg-1] <- sum(diag(probability_matrix[gg:(maxgoal+1),]))
}
homeG[maxgoal] <- probability_matrix[(maxgoal+1),1]

goaldiffs <- c(rev(awayG), sum(diag(probability_matrix)), homeG)
names(goaldiffs) <- -maxgoal:maxgoal
