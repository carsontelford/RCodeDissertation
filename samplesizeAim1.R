
# Install and load required packages
library(simr)
library(lme4)

# Set parameters for simulation
event_rate <- 0.0038
n_obs_per_cluster <- 9
n_clusters <- 4572
risk_ratio <- 1.15
icc <- 0.05  # Low ICC, you can adjust this if needed
exposure_effect <- log(risk_ratio)  # Log-transformed effect size for risk ratio

# Simulate a dataset with random intercepts for clusters
set.seed(123)  # Set seed for reproducibility
sim_data <- expand.grid(cluster = 1:n_clusters, obs = 1:n_obs_per_cluster)
sim_data$exposure <- rnorm(nrow(sim_data))  # Continuous exposure variable

# Generate binary outcome with logit link
sim_data$outcome <- rbinom(nrow(sim_data), size = 1, prob = event_rate)
table(sim_data$outcome)

# Create a GLMM model with random intercepts for clusters
glmm_model <- glmer(outcome ~ exposure + (1 | cluster), data = sim_data, 
                    family = binomial(link = "logit"))
summary(glmm_model)

# Define a power simulation for 1000 simulations
power_simulation <- powerSim(glmm_model, nsim = 10)

# Show the results
summary(power_simulation)





# positive association between xposure and outcome
# Install and load required packages
library(simr)
library(lme4)

# Set parameters for simulation
event_rate <- 0.0038
n_obs_per_cluster <- 9
n_clusters <- 4572
risk_ratio <- 1.05
icc <- 0.05  # Low ICC, you can adjust this if needed
exposure_effect <- log(risk_ratio)  # Log-transformed effect size for risk ratio
exposure_effect

# Simulate a dataset with random intercepts for clusters
set.seed(123)  # Set seed for reproducibility
sim_data <- expand.grid(cluster = 1:n_clusters, obs = 1:n_obs_per_cluster)
sim_data$exposure <- rnorm(nrow(sim_data))  # Continuous exposure variable

# Adjust the logit of the outcome to ensure a positive association between exposure and outcome
logit_prob <- exposure_effect * sim_data$exposure
sim_data$prob_outcome <- plogis(logit_prob)  # Transform logit to probability

# Generate binary outcome based on the probability
sim_data$outcome <- rbinom(nrow(sim_data), size = 1, prob = sim_data$prob_outcome)

# Create a GLMM model with random intercepts for clusters
glmm_model <- glmer(outcome ~ exposure + (1 | cluster), data = sim_data, 
                    family = binomial(link = "logit"))
summary(glmm_model)

# Define a power simulation for 1000 simulations
power_simulation <- powerSim(glmm_model, nsim = 100)

# Show the results
summary(power_simulation)












# district random effect

# Install and load required packages
library(simr)
library(lme4)

# Set parameters for simulation
event_rate <- 0.0038
n_obs_per_farm <- 9
n_farms <- 4572
n_districts <- 200  # Fewer districts (adjust as needed)
risk_ratio <- 1.15
icc <- 0.05  # Low ICC, you can adjust this if needed
exposure_effect <- log(risk_ratio)  # Log-transformed effect size for risk ratio

# Simulate a dataset with random intercepts for districts
set.seed(123)  # Set seed for reproducibility
sim_data <- expand.grid(farm = 1:n_farms, obs = 1:n_obs_per_farm)

# Assign each farm to a district (assign random districts to farms)
sim_data$district <- rep(sample(1:n_districts, n_farms, replace = TRUE), each = n_obs_per_farm)

# Simulate continuous exposure variable
sim_data$exposure <- rnorm(nrow(sim_data))

# Generate binary outcome with logit link
sim_data$outcome <- rbinom(nrow(sim_data), size = 1, prob = event_rate)

# Create a GLMM model with random intercepts for districts
glmm_model <- glmer(outcome ~ exposure + (1 | farm), data = sim_data, 
                    family = binomial(link = "logit"))
summary(glmm_model)

# Define a power simulation for 1000 simulations
power_simulation <- powerSim(glmm_model, nsim = 5)

# Show the results
summary(power_simulation)

