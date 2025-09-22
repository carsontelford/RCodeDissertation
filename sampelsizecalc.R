library(powerSurvEpi)


# estimate SS
powerConLogistic.con(
  N=NULL,
  power = 0.8,
  OR = 1.025,
  sigma = 20,
  nD = 1,
  nH = 2,
  R2 = 0,
  alpha = 0.05,
  nTests = 1
)
# i need 49 pairs




# estimate power 
library(powerSurvEpi)
powerConLogistic.con(N = 76,
                    power = NULL,
                    OR = 1.05,
                    sigma = 10,
                    nD = 1,
                    nH = 2,
                    R2 = 0,
                    alpha = 0.05,
                    nTests = 2)

# estimate OR 
library(powerSurvEpi)
OR = powerConLogistic.con(N = 76,
                          power = .8,
                          OR = NULL,
                          sigma = 10,
                          nD = 1,
                          nH = 2,
                          R2 = 0,
                          alpha = 0.05,
                          nTests = 2)
print(OR) 
