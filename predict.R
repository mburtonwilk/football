##########################################################################################
#read football data
df <- read.csv('http://www.football-data.co.uk/mmz4281/1617/E0.csv') # for PL
df0 <- read.csv('http://www.football-data.co.uk/mmz4281/1617/E1.csv') # for Championship
df1 <- read.csv('http://www.football-data.co.uk/mmz4281/1617/E2.csv') # for League One
df2 <- read.csv('http://www.football-data.co.uk/mmz4281/1617/E1.csv') # for League Two
##########################################################################################


##########################################################################################
# do the modelling
# position data into format compatible with glm function

df <- apply(df, 1, function(row){
  
  data.frame(team=c(row['HomeTeam'], row['AwayTeam']),
             
             opponent=c(row['AwayTeam'], row['HomeTeam']),
             
             goals=c(row['FTHG'], row['FTAG']),
             
             home=c(1, 0))
  
})

df <- do.call(rbind, df)



# ensure we've not ended up with factors!

df$goals <- as.numeric(as.character(df$goals))



# fit the model

model <- glm(goals ~ home + team + opponent, 
             
             family=poisson(link=log), data=df)

summary(model)


# let's make some predictions!

# average home goals
av_home_goals <- predict(model, 
                         
                         data.frame(home=1, team="Leicester", 
                                    
                                    opponent="Arsenal"), 
                         
                         type="response")


# average away goals
av_away_goals <- predict(model, 
                         
                         data.frame(home=0, team="Arsenal", 
                                    
                                    opponent="Leicester"), 
                         
                         type="response")


##########################################################################################


# get probabilities per goal (assuming goals scored in a game ranges from 0 to 10, for each team)
home_goals <- dpois(0:10, av_home_goals) 

away_goals <- dpois(0:10, av_away_goals)



# convert probability vectors into score matrix

m <- home_goals %o% away_goals
rownames(m) <- 0:10
colnames(m) <- 0:10
print(m)

# no scientific format please
options(scipen = 999)

########################################################################
# get probabilities for home, draw, away win
home <- sum(m[lower.tri(m)])
home

draw <- sum(diag(m))
draw

away <- sum(m[upper.tri(m)])
away
########################################################################


########################################################################
# Probability[Under 2.5]
m[1,1] + m[1,2] + m[1,3] + m[2,1] + m[2,2] + m[3,1]

# Probability[Over 2.5]
1 - (m[1,1] + m[1,2] + m[1,3] + m[2,1] + m[2,2] + m[3,1])
########################################################################


########################################################################
# Under/Over a certain market value
market <- 2.5
prob <- 0

for(i in as.numeric(rownames(m))){
  for(j in as.numeric(colnames(m))){
    
    if( i + j > market){
      prob <- prob + m[i+1, j+1]
    }
  }
}
# prob. Over[.....]
cat(prob * 100, '%')
########################################################################
