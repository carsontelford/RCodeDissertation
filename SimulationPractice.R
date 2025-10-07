
#### Simulation practice ####

#### 1. Dice roll ####
#' Sim rolling 2 dice 100000 times. Estimate prob of getting sum of 7,11,doubles.
#' Compare with exact theoretical probs.

nsims <- 100000
n7s <- 0
n11s <- 0
doubles <- 0
for (i in 1:nsims) {
  
  # roll  dice: randomly sample numbers 1:6
  roll1 <- sample(1:6,1)
  roll2 <- sample(1:6,1)
  mysum <- roll1+roll2
  
  if(roll1==roll2){
    doubles <- doubles+1
  }
  if(mysum == 7){
    n7s <- n7s+1
  }
  if(mysum==11){
    n11s <- n11s+1
  }
}

prob7 <- n7s/nsims
prob11 <- n11s/nsims
probdoubles <- doubles/nsims
vals <- c(prob7, prob11, probdoubles)
vals


#### 2. Coin flip sequences ####
#' Estimate prob of getting seq HTH in coin 1 million coin flips.
set.seed(5)
nsims <- 2000000
flips <- runif(nsims)
HT <- ifelse(flips<.5,"H","T")
HTstr <- paste(HT, collapse="")
numHTH <- str_count(HTstr, "HTH")
head(numHTH)
probHTH <- numHTH/(nsims-2)
probHTH



#### 3. Draw cards prob aces ####
#' Draw 5 cards from 52 card deck. Est prob of getting at least 2 aces.

nsims <- 100000
nSuccess <- 0
for (i in 1:nsims) {
  
  # draw cards without replacement and count aces each time
  mydraws <- sample(1:52, 5, replace=F)
  nAces <- length(mydraws[mydraws<5])
  
  # update nSuccesses
  if(nAces>=2){nSuccess <- nSuccess+1}
  
}
nSuccess/nsims



#### 4. Conditional Prob bayes ####
#' There are 2 bags:
#'  Bag A has 3 red and 7 blue.
#'  Bag B has 5 red and 5 blue
#'  Pick a bag at random, then drw a red ball.
#'  Estimate probability the red bal came from Bag A

nsims <- 1000000
pRed_A <- 3/10
pRed_B <- 5/10

nRedA <- 0
nRedB <- 0

for(i in 1:nsims){
  
  # random choose bag A or B 50/50
  sim <- runif(1)
  
  # pick bag A
  if(sim<.5){
    
    # pick a ball from bag A
    ballsim <- runif(1)
    # if it comes back red 
    if(ballsim<=pRed_A){
      nRedA <- nRedA+1
    }
  }
  else{
    
    # pick a ball from bag B
    ballsim <- runif(1)
    # if the ball comes out red
    if(ballsim<=pRed_B){
      nRedB <- nRedB+1
    }
  }
}
totalReds <- nRedA+nRedB
probA <- nRedA/totalReds
probB <- nRedB/totalReds
probA
probB



#### 5. Birthday Problem ####
#' There are 50 people in a room.
#' Estimate prob that at least 2 people share same bday
#' Repeat for 1000 trials

npeople <- 50
nsims <- 10000
nPos <- 0
for(i in 1:nsims){
  
  # for each person, sim a birthday with replacement
  bdays <- sample(1:365,50,replace=T)
  repeatedBdays <- duplicated(bdays)
  countRepeats <- length(repeatedBdays[repeatedBdays=="TRUE"])
  # record that there were shared bdays in this iter
  if(countRepeats>0){
    nPos <- nPos+1
  }
}
nPos
nPos/nsims


#### 6. Gamblers ruin ####
#' Gambler has 10$ and bets 1$ on a fair coin until reaching 0 or 20$

nsims <- 10000
wins <- 0

for (i in 1:nsims) {
  
  dollars <- 10
  # repeat until i reach 0 or 20
  while(dollars>0 && dollars<20){
    
    # flip the coin
    flip <- runif(1)
    
    # heads make 1
    if(flip<.5){
      dollars <- dollars+1
    }
    # tails lose 1
    else if(flip>=.5){
      dollars <- dollars-1
    }
  }
  if(dollars==20){
    wins <- wins+1
  }
}
prob20 <- wins/nsims
prob20



#### 7. Biased coins Bayes ####
#' Coin A has 70% prob of heads
#' Coin B has 40% prob heads
#' You randomly pick a coin 50/50 and flip 3 times, getting 2 heads
#' what is prob it was Coin A?

nsims <- 10000
coinAsuccess <- 0
coinBsuccess <- 0

for(i in 1:nsims){
  
  # randomly pick a coin
  choose <- runif(1)
  
  # choose coin A
  if(choose<.5){
    # flip coin A 3 times
    flipsA <- runif(3)
    
    # see how many are heads given 70% prob
    countHeads <- length(flipsA[flipsA<.7])
    
    # 70% chance it was heads. if it was heads, record
    if(countHeads==2){
      coinAsuccess <- coinAsuccess+1
    }
  }
  
  # or choose coin B
  else{
    
    # flip B 3 times
    flipsB <- runif(3)
    
    # count headds given 40% chance each flip is heads
    countHeads <- length(flipsB[flipsB<.4])
    
    if(countHeads==2){
      coinBsuccess <- coinBsuccess+1
    }
  }
}

nsuccesses <- coinAsuccess+coinBsuccess
probA <- coinAsuccess/nsuccesses
probA
