#### 3. Descriptive analysis ####

library(tidyverse)
library(terra)
library(sf)
library(exactextractr)
library(raster)
library(tidyr)

rvf <- read.csv("Aim 1/Covariate Data/Final Merged Dataset/mergeddf_21Jul25.csv")
summary(rvf$crop_percent)
rvf <- rvf %>%
  mutate(crop_percent10 = crop_percent*10) %>%
  mutate(
    popmeanlog = log(popmean+1),
    popmean_scale = scale(popmean),
    popmeanlog_scale = scale(popmeanlog),
    livestockdenslog = log(livestockdens+1),
    livestockdens_scale = scale(livestockdens),
    livestockdenslog_scale = scale(livestockdenslog),
    cattledenslog = log(cattledens+1),
    cattledens_scale = scale(cattledens),
    cattledenslog_scale = scale(cattledenslog),
    chirpssum_scale = scale(chirpssum),
    slope_scale = scale(slope),
    chirpssum_cov_scale = scale(chirpssum_cov),
    elevation_scale = scale(elevation),
    twi_scale = scale(twi))

#### Description of covar distributions ####
myvars <- colnames(rvf)
myvars
myvars <- myvars[c(6:22, 59:67)]
myvars

# percentile bins
for (i in myvars) {
  # 1. Bin var into 5% percentile intervals (20 bins)
  breaks_tile <- quantile(rvf[[i]], probs = seq(0, 1, 0.05), na.rm = TRUE)
  breaks_tile <- unique(breaks_tile)  # <-- remove duplicate breakpoints
  
  # If not enough unique breaks, skip variable
  if (length(breaks_tile) < 3) {
    message(paste("Skipping", i, "- not enough unique breakpoints"))
    next
  }
  
  rvf_binned_tile <- rvf %>%
    mutate(myvar_bin = cut(.data[[i]], breaks = breaks_tile)) %>%
    group_by(myvar_bin) %>%
    summarise(
      myvar_mean = mean(.data[[i]], na.rm = TRUE),
      rvf_rate = mean(rvfbin, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    filter(!is.na(myvar_mean))
  
  # 2. Plot
  myplot1 <- ggplot(rvf_binned_tile, aes(x = myvar_mean, y = rvf_rate)) +
    geom_point(size = 2, color = "darkred") +  # binned points
    geom_smooth(method = "loess", se = TRUE, color = "blue", fill = "lightblue", span = 1) +
    coord_cartesian(ylim = c(0, NA)) +
    labs(
      x = i,
      y = "Proportion of Farms with RVF Outbreak",
      title = paste("Smoothed Risk of RVF Outbreak (Binned):", i)
    ) +
    theme_minimal()
  
  print(myplot1)
}


# actual value bins
for (i in myvars) {
  
  # 1. Bin var into 5% percentile intervals (20 bins)
  range_i <- range(rvf[[i]], na.rm = TRUE)
  breaks <- seq(range_i[1], range_i[2], length.out = 11)  # 10 bins
  
  rvf_binned <- rvf %>%
    mutate(myvar = .data[[i]],
           myvar_bin = cut(myvar, breaks = breaks)) %>%
    group_by(myvar_bin) %>%
    summarise(
      myvar_mean = mean(myvar, na.rm = TRUE),
      rvf_rate = mean(rvfbin, na.rm = TRUE),
      n = n()
    ) %>%
    filter(!is.na(myvar_mean))
  
  # 2. Plot
  myplot1 <- ggplot(rvf_binned, aes(x = myvar_mean, y = rvf_rate)) +
    geom_point(size = 2, color = "darkred") +  # binned points
    geom_smooth(method = "loess", se = TRUE, color = "blue", fill = "lightblue", span = 1) +
    coord_cartesian(ylim = c(0, NA)) +
    labs(
      x = i,
      y = "Proportion of Farms with RVF Outbreak",
      title = paste("Smoothed Risk of RVF Outbreak (Binned):", i)
    ) +
    theme_minimal()
  
  print(myplot1)
}


# whisker plots
for (i in myvars) {
  myplot2 <- ggplot(rvf, aes(x = factor(rvfbin), y = .data[[i]])) +
    geom_boxplot(fill = c("gray", "red")) +
    labs(x = "RVF Outbreak (0 = No, 1 = Yes)", y = i,
         title = paste(i, "by RVF Outbreak Status")) +
    theme_minimal()
  print(myplot2)
}


#### Create geog groups for rand int ####
mygriddf <- rvf %>%
  distinct(x,y,ID)
mygridrast <- rast(mygriddf, type="xyz")
plot(mygridrast)

# 2. Aggregate to low resolution (e.g., factor of 2)
fact <- 2
r_lowres_template <- aggregate(mygridrast, fact = fact, fun = mean)
plot(r_lowres_template)

# 3. Assign unique IDs to coarse cells
r_lowres_id <- r_lowres_template
values(r_lowres_id) <- 1:ncell(r_lowres_id)
plot(r_lowres_id)

# 4. Now disaggregate
r_highres_group_id <- disagg(r_lowres_id, fact = fact, method = "near")
plot(r_highres_group_id)

#5. Extract group IDs for original points in mygriddf
mygriddf$ID_agg <- extract(r_highres_group_id, mygriddf[, c("x", "y")])[,2]

# Check
head(mygriddf)
mygriddf <- mygriddf %>%
  dplyr::select(ID,ID_agg)

# now merge the agged regional ID to rvf df
rvf <- rvf %>%
  left_join(mygriddf)


#### map change over time ####
mynames <- colnames(rvf)
mynames
mynames <- mynames[c(6, 16, 20, 25:27, 62)]

for (i in mynames) {
  df_raster <- rvf %>%
    dplyr::select(x,y,year,i)
  p <- ggplot(df_raster) +
    geom_raster(aes(x = x, y = y, fill = !!sym(i))) +
    coord_equal() +
    scale_fill_viridis_c() +
    theme_bw() +
    facet_wrap(~year) +
    ggtitle(i)
  
  print(p)  # You need to print the plot inside the loop
}

#### Temporal trends ####
# select sample, including case cells 
id_random_map <- rvf %>%
  distinct(ID) %>%
  mutate(random_number = sample(1:100, n(), replace = TRUE))

tempdf <- rvf %>%
  group_by(ID) %>%
  mutate(cumcases = sum(rvfn)) %>%
  ungroup() %>%
  mutate(evercase = ifelse(cumcases>0,1,0)) %>%
  # merge in random numner 1-100
  left_join(id_random_map) %>%
  mutate(random_number = ifelse(cumcases>0,1,random_number)) 

table(tempdf$random_number)

tempdf <- tempdf %>%
  filter(random_number <20)


#' plot time series for each var
#' for outbreak pixels vs nonoutbreak grid cells
varnames <- names(tempdf)
varnames
varnames <- varnames[6:12]

library(rlang)

for (i in varnames) {
  # Summarize to get the average for each year and evercase
  myplotdf <- tempdf %>%
    group_by(evercase, year) %>%
    summarise(mean_val = mean(!!sym(i), na.rm = TRUE), .groups = "drop")
  
  # Plot the average lines only
  myplot <- ggplot(myplotdf) +
    geom_line(aes(x = year, 
                  y = mean_val, 
                  color = factor(evercase))) +
    
    scale_color_manual(values = c("0" = "gray50", "1" = "red")) +
    labs(x = "Year", 
         y = i,
         color = "Ever Case") +
    theme_minimal()
  
  print(myplot)
}


#' plot time series for each var
#' for outbreak pixels vs nonoutbreak grid cells
varnames <- names(tempdf)
varnames
varnames <- varnames[6:12]

library(rlang)

for (i in varnames) {
  # Summarize to get the average for each year and evercase
  myplotdf <- tempdf %>%
    group_by(rvfbin, year) %>%
    summarise(mean_val = mean(!!sym(i), na.rm = TRUE), .groups = "drop")
  
  # Plot the average lines only
  myplot <- ggplot(myplotdf) +
    geom_line(aes(x = year, 
                  y = mean_val, 
                  color = factor(rvfbin))) +
    
    scale_color_manual(values = c("0" = "gray50", "1" = "red")) +
    labs(x = "Year", 
         y = i,
         color = "Outbreak") +
    theme_minimal()
  
  print(myplot)
}



#### univariate analysis ####
##### no control for cropland #####
# plot relationship with each var
library(mgcv)
library(ggplot2)

myvars <- colnames(rvf)
myvars
# myvars <- myvars[c(6:7, 12:22, 58:65)]
myvars <- myvars[c(6,9,12,14:22, 59:67)]

# Loop through all covariates
for (v in myvars) {
  # Fit GAM with spline + random intercept for year
  formula_str <- paste0("rvfbin ~ s(", v, ", k = 5) + s(ID_agg, bs = 're')")
  gammod <- gam(as.formula(formula_str), data = rvf, family = binomial)
  p <- plot(gammod, rug=T)
  print(p)
}

##### again controlling for crop_pct #####
myvars <- colnames(rvf)
myvars
myvars <- myvars[c(62, 69, 71, 73, 59)]
myvars

aic_results <- data.frame(variable = character(), AIC = numeric(), 
                          crop_pct_coef = numeric(),
                          stringsAsFactors = FALSE)
for (v in myvars) {
  formula_str <- paste0("rvfbin ~ crop_percent10 + s(", v, ", k = 5) + s(ID_agg, bs = 're')")
  gammod <- mgcv::gam(as.formula(formula_str), data = rvf, family = binomial)
  summary(gammod)
  myaic <- AIC(gammod)
  print(myaic)
  my_coef <- coef(gammod)["crop_percent"]
  
  # Append to results
  aic_results <- rbind(aic_results, data.frame(variable = v, AIC = myaic,
                                               crop_pct_coef = my_coef))
  print(aic_results)
}

# View or save the result
aic_results$OR <- exp(aic_results$crop_pct_coef)
print(aic_results)








#### Functional form ####
# crop_percent
mod_linear <- glmer(rvfbin ~ crop_percent10 + (1 | ID_agg), data = rvf, family = binomial)
mod_log    <- glmer(rvfbin ~ log(crop_percent10 + 0.01) + (1 | ID_agg), data = rvf, family = binomial)  # add 0.01 to avoid log(0)
AIC(mod_linear, mod_log)

# livestock dens
colnames(rvf)
mod_linear <- glmer(rvfbin ~ crop_percent + livestockdens + (1 | ID_agg), data = rvf, family = binomial)
mod_linear_scale <- glmer(rvfbin ~ crop_percent + livestockdens_scale + (1 | ID_agg), data = rvf, family = binomial)
mod_log <- glmer(rvfbin ~ crop_percent + livestockdenslog + (1 | ID_agg), data = rvf, family = binomial)
mod_log_scale <- glmer(rvfbin ~ crop_percent + livestockdenslog_scale + (1 | ID_agg), data = rvf, family = binomial)
AIC(mod_linear,mod_linear_scale, mod_log, mod_log_scale)
summary(mod_log_scale)
# note: use scaled

# cattledens
mod_linear <- glmer(rvfbin ~ crop_percent + cattledens + (1 | ID_agg), data = rvf, family = binomial)
mod_linear_scale <- glmer(rvfbin ~ crop_percent + cattledens_scale + (1 | ID_agg), data = rvf, family = binomial)
mod_log <- glmer(rvfbin ~ crop_percent + cattledenslog + (1 | ID_agg), data = rvf, family = binomial)
mod_log_scale <- glmer(rvfbin ~ crop_percent + cattledenslog_scale + (1 | ID_agg), data = rvf, family = binomial)
AIC(mod_linear,mod_linear_scale, mod_log, mod_log_scale)
summary(mod_log)
# note: use scaled


# chirpssum
mod_linear <- glmer(rvfbin ~ crop_percent + chirpssum + (1 | ID_agg), data = rvf, family = binomial)
mod_linear_scale    <- glmer(rvfbin ~ crop_percent + chirpssum_scale + (1 | ID_agg), data = rvf, family = binomial)  # add 0.01 to avoid log(0)
AIC(mod_linear, mod_linear_scale)
summary(mod_linear)
summary(mod_linear_scale)
# note: use chirpssum_scale 


# popmean
mod_linear <- glmer(rvfbin ~ crop_percent + popmean + (1 | ID_agg), data = rvf, family = binomial)
mod_linear_scale <- glmer(rvfbin ~ crop_percent + popmean_scale + (1 | ID_agg), data = rvf, family = binomial)
mod_log <- glmer(rvfbin ~ crop_percent + popmeanlog + (1 | ID_agg), data = rvf, family = binomial)
mod_log_scale <- glmer(rvfbin ~ crop_percent + popmeanlog_scale + (1 | ID_agg), data = rvf, family = binomial)
AIC(mod_linear,mod_linear_scale, mod_log, mod_log_scale)
summary(mod_log)
# note: use log


# slope
mod_linear <- glmer(rvfbin ~ crop_percent + slope + (1 | ID_agg), data = rvf, family = binomial)
mod_linear_scale    <- glmer(rvfbin ~ crop_percent + slope_scale + (1 | ID_agg), data = rvf, family = binomial)  # add 0.01 to avoid log(0)
AIC(mod_linear, mod_linear_scale)
summary(mod_linear_scale)
# note: use linearscale

# elevation
mod_linear <- glmer(rvfbin ~ crop_percent + elevation + (1 | ID_agg), data = rvf, family = binomial)
mod_linear_scale    <- glmer(rvfbin ~ crop_percent + elevation_scale + (1 | ID_agg), data = rvf, family = binomial)  # add 0.01 to avoid log(0)
AIC(mod_linear, mod_linear_scale)
summary(mod_linear_scale)
# note: use linear scaled


# chirpssum_cov
mod_linear <- glmer(rvfbin ~ crop_percent + chirpssum_cov + (1 | ID_agg), data = rvf, family = binomial)
mod_linear_scale    <- glmer(rvfbin ~ crop_percent + chirpssum_cov_scale + (1 | ID_agg), data = rvf, family = binomial)  # add 0.01 to avoid log(0)
AIC(mod_linear, mod_linear_scale)
summary(mod_linear_scale)
# note: use linear scale

# twi
mod_linear <- glmer(rvfbin ~ crop_percent + twi + (1 | ID_agg), data = rvf, family = binomial)
mod_linear_scale    <- glmer(rvfbin ~ crop_percent + twi_scale + (1 | ID_agg), data = rvf, family = binomial)  # add 0.01 to avoid log(0)
AIC(mod_linear, mod_linear_scale)
summary(mod_linear_scale)
# note: use linear scale




#### Multivar model ####
# correlation matrix
cordf <- rvf %>%
  dplyr::select(crop_percent10,livestockdenslog,cattledenslog_scale,
                chirpssum_scale, popmeanlog, slope_scale,
                elevation_scale, chirpssum_cov_scale,
                twi_scale)
cordf2 <- cor(cordf)

# check vif
myformula <- as.formula(rvfbin ~ crop_percent10 + # exposure
                          livestockdenslog+ # livestock presence
                         chirpssum_scale + # rain increases water pooling
                          chirpssum_cov_scale + # variability in precip increases water pooling
                          slope_scale + # topo susc to pooling
                          popmeanlog)
mod <- glm(myformula, data = rvf, family = "binomial")
summary(mod)
car::vif(mod)



##### best fitting model #####
# prep data frame to staore all model results
results <- data.frame(model = character(),
                      AIC = numeric(),
                      logLik = numeric(),
                      OR = numeric(),
                      Estimate = numeric(),
                      SE = numeric(),
                      zval = numeric(),
                      pval = numeric(),
                      stringsAsFactors = FALSE)

# compare between topo susc vars
formula1 <- as.formula(rvfbin ~ crop_percent10 + # exposure
                         livestockdenslog + # livestock presence
                         chirpssum_scale + # rain increases water pooling
                         chirpssum_cov_scale + # variability in precip increases water pooling
                         twi_scale + # topo susc to pooling
                         popmeanlog + # human pop leads to livestock and importation
                         (1|ID_agg))
formula2 <- as.formula(rvfbin ~ crop_percent10 + # exposure
                         livestockdenslog + # livestock presence
                         chirpssum_scale + # rain increases water pooling
                         chirpssum_cov_scale + # variability in precip increases water pooling
                         elevation_scale + # topo susc to pooling
                         popmeanlog + # human pop leads to livestock and importation
                         (1|ID_agg))
formula3 <- as.formula(rvfbin ~ crop_percent10 + # exposure
                         livestockdenslog + # livestock presence
                         chirpssum_scale + # rain increases water pooling
                         chirpssum_cov_scale + # variability in precip increases water pooling
                         slope_scale + # topo susc to pooling
                         popmeanlog + # human pop leads to livestock and importation
                         (1|ID_agg))

myformulas <- list(
  formula1,
  formula2,
  formula3
)

# Loop over formulas
for (i in seq_along(myformulas)) {
  formula <- myformulas[[i]]
  print(formula)
  
  mod <- glmer(formula, data = rvf, family = "binomial")
  print(summary(mod))
  coefs <- summary(mod)$coefficients[2,]  # 2nd row for crop_percent
  
  results <- rbind(results, data.frame(
    model = i,
    AIC = AIC(mod),
    logLik = as.numeric(logLik(mod)),
    OR = exp(coefs["Estimate"]),
    Estimate = coefs["Estimate"],
    SE = coefs["Std. Error"],
    zval = coefs["z value"],
    pval = coefs["Pr(>|z|)"],
    stringsAsFactors = FALSE
  ))
}
print(results)

#' result: best fitting model is mod 3
# sensitivity if i use cattle dens without shoats
formula4 <- as.formula(rvfbin ~ crop_percent10 + # exposure
                         cattledenslog + # livestock presence
                         chirpssum_scale + # rain increases water pooling
                         chirpssum_cov_scale + # variability in precip increases water pooling
                         slope_scale + # topo susc to pooling
                         popmeanlog + # human pop leads to livestock and importation
                         (1|ID_agg))

# see effect of removing each confounder on exposure estimate
formula5 <- as.formula(rvfbin ~ crop_percent10 + # exposure
                         livestockdenslog + # livestock presence
                         chirpssum_scale + # rain increases water pooling
                         chirpssum_cov_scale + # variability in precip increases water pooling
                         slope_scale + # topo susc to pooling
                         popmeanlog + # human pop leads to livestock and importation
                         (1|ID_agg))
formula6 <- as.formula(rvfbin ~ crop_percent10 + # exposure
                         # livestockdenslog + # livestock presence
                         chirpssum_scale + # rain increases water pooling
                         chirpssum_cov_scale + # variability in precip increases water pooling
                         slope_scale + # topo susc to pooling
                         popmeanlog + # human pop leads to livestock and importation
                         (1|ID_agg))
formula7 <- as.formula(rvfbin ~ crop_percent10 + # exposure
                         livestockdenslog + # livestock presence
                         # chirpssum_scale + # rain increases water pooling
                         chirpssum_cov_scale + # variability in precip increases water pooling
                         slope_scale + # topo susc to pooling
                         popmeanlog + # human pop leads to livestock and importation
                         (1|ID_agg))
formula8 <- as.formula(rvfbin ~ crop_percent10 + # exposure
                         livestockdenslog + # livestock presence
                         chirpssum_scale + # rain increases water pooling
                         # chirpssum_cov_scale + # variability in precip increases water pooling
                         slope_scale + # topo susc to pooling
                         popmeanlog + # human pop leads to livestock and importation
                         (1|ID_agg))
formula9 <- as.formula(rvfbin ~ crop_percent10 + # exposure
                         livestockdenslog + # livestock presence
                         chirpssum_scale + # rain increases water pooling
                         chirpssum_cov_scale + # variability in precip increases water pooling
                         # slope_scale + # topo susc to pooling
                         popmeanlog + # human pop leads to livestock and importation
                         (1|ID_agg))
formula10 <- as.formula(rvfbin ~ crop_percent10 + # exposure
                          livestockdenslog + # livestock presence
                         chirpssum_scale + # rain increases water pooling
                         chirpssum_cov_scale + # variability in precip increases water pooling
                         slope_scale + # topo susc to pooling
                         # popmeanlog + # human pop leads to livestock and importation
                         (1|ID_agg))

# check effect measure modification of crop_percent by livestock dens
formula11 <- as.formula(rvfbin ~ crop_percent10 + # exposure
                          livestockdenslog + # livestock presence
                          crop_percent10*livestockdenslog+
                          chirpssum_scale + # rain increases water pooling
                          chirpssum_cov_scale + # variability in precip increases water pooling
                          slope_scale + # topo susc to pooling
                          popmeanlog + # human pop leads to livestock and importation
                          (1|ID_agg))
formula12 <- as.formula(rvfbin ~ crop_percent10 + # exposure
                          livestockdenslog + # livestock presence
                          crop_percent10*livestockdenslog+
                          # chirpssum_scale + # rain increases water pooling
                          # chirpssum_cov_scale + # variability in precip increases water pooling
                          # slope_scale + # topo susc to pooling
                          # popmeanlog + # human pop leads to livestock and importation
                          (1|ID_agg))

# Define formulas
myformulas <- list(
  formula3,
  formula4,
  formula5,
  formula6,
  formula7,
  formula8,
  formula9,
  formula10,
  formula11,
  formula12
)

# Loop over formulas
for (i in seq_along(myformulas)) {
  formula <- myformulas[[i]]
  print(formula)
  mod <- glmer(formula, data = rvf, family = "binomial")
  summary(mod)
  coefs <- summary(mod)$coefficients[2,]  # 2nd row for crop_percent
  
  results <- rbind(results, data.frame(
    model = i,
    AIC = AIC(mod),
    logLik = as.numeric(logLik(mod)),
    OR = exp(coefs["Estimate"]),
    Estimate = coefs["Estimate"],
    SE = coefs["Std. Error"],
    zval = coefs["z value"],
    pval = coefs["Pr(>|z|)"],
    stringsAsFactors = FALSE
  ))
}

# View results
print(results)
write.csv(results, "Aim 1/Model fit results/GLMER_results.csv")


#### GAM fit ####
library(mgcv)
library(ggplot2)

mod4_gam <- mgcv::gam(rvfbin ~ crop_percent10 + s(livestockdenslog)+
                        s(chirpssum_scale) + s(chirpssum_cov_scale)+
                        s(twi_scale) + s(popmeanlog) + 
                        s(ID_agg, bs = 're'), 
                      data = rvf, family = binomial)

my_coef <- coef(mod4_gam)["crop_percent10"]
my_coef
exp(my_coef)
exp(my_coef)^10
AIC(mod4_gam)



#### Spatial autocorrelation test ####
formula4
mod4 <- glmer(rvfbin ~ crop_percent10 + cattledenslog + chirpssum_scale + chirpssum_cov_scale + 
                 slope_scale + popmeanlog + (1 | ID_agg),
               family="binomial",data=rvf)
summary(mod4)
resid_vals <- resid(mod4, type = "pearson")  # or type = "deviance"
coords <- cbind(rvf$x, rvf$y)
# Define neighbors based on a distance threshold (e.g., 10 km)
library(spdep)
nb <- dnearneigh(coords, 0, .1)  # 0 to 10,000 meters
lw <- nb2listw(nb, style = "W")
moran_test <- moran.test(resid_vals, lw)
moran_test


library(ggplot2)
rvf$resid <- resid_vals
ggplot(rvf, aes(x = x, y = y, color = resid)) +
  geom_point() + scale_color_gradient2(midpoint = 0) +
  theme_minimal()


