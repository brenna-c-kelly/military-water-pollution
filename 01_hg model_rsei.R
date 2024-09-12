
library(sf)
library(INLA)
library(spdep)
# library(broom)
library(dplyr)
# library(ggExtra)
library(ggplot2)
library(moments)
library(stringr)
library(regclass)
library(tidyverse)
library(wesanderson)

## data
# rsei_fed <- read.csv("data/rsei_fed_2023.csv")
# 
# test <- read.csv("data/rsei_fed_zenodo.csv")
# 
# aggregate(test$total, by = list(test$year), FUN = skewness)
# 
# ggplot(test, aes(x = total)) +
#   geom_histogram(stat = "bin") +
#   facet_wrap(~ year)
# 
# 
# rsei_fed_na <- rsei_fed |>
#   filter(!is.na(state))
# write.csv(rsei_fed_na, "data/rsei_fed_zenodo.csv", row.names = FALSE)
# 
# rsei_fed <- rsei_fed[which(rsei_fed$geoid != 0), ]

#rsei_fed$state.x <- str_pad(rsei_fed$state.x, 2, pad = "0")

# only ones that are kept the whole time


rsei_fed$fips <- str_pad(rsei_fed$fips, 5, pad = 0)
rsei_fed$idarea <- as.numeric(as.factor(rsei_fed$fips))
rsei_fed$idarea1 <- rsei_fed$idarea
rsei_fed$idtime <- 1 + rsei_fed$submission.year - min(rsei_fed$submission.year)

rsei_fed$rsei_bin <- ifelse(rsei_fed$rsei.score > 0, 1, 0)

## glms
summary(
  glm(rsei_bin ~ 1, data = rsei_fed, family = binomial)
)

gam_dat <- rsei_fed[which(rsei_fed$rsei_bin == 1), ]

summary(
  glm(rsei.score ~ 1, data = gam_dat,
      family = Gamma)
)


# prep for inla hg
n = nrow(rsei_fed)

z = as.vector(rsei_fed$rsei_bin == 1)
y = ifelse(z == 1, rsei_fed$rsei.score, NA)

#z = as.vector(rsei_fed$rsei_cancer_bin == 1)
#y = ifelse(z == 1, rsei_fed$rsei.score.cancer, NA)
#mean(y, na.rm = TRUE)
#summary(rsei_fed$rsei.score)

nothing1 <- rep(NA, n)
nothing2 <- rep(NA, n)

zNA = as.vector(c(z, nothing1))
yNA = as.vector(c(nothing2, y))

outcome.matrix <- matrix(c(zNA, yNA), ncol = 2)

mu_z <- c(rep(1, n), nothing1) # Binomial 
mu_y <- c(nothing2, rep(1,  n)) # Gamma 

## Create index vectors
id_space <- rsei_fed$idarea
id_time <- rsei_fed$idtime

## Queens case
###### take care of space later
# geometry <- st_read("data/geometry.shp")
# dat_nb <- poly2nb(geometry)

## Write to file
# nb2INLA("map.adj", dat_nb)
# g <- inla.read.graph(filename = "map.adj")


# doubling
# id_space_z <- c(id_space, nothing1) # Binomial
# id_space_y <- c(nothing2, id_space) # Gamma
# id_space_z2 <- c(id_space, nothing1) # Binomial
# id_space_y2 <- c(nothing2, id_space) # Gamma

id_time_z <- c(id_time, nothing1) # Binomial
id_time_y <- c(nothing2, id_time) # Gamma


# list
data_hg <- list(outcome.matrix = outcome.matrix, 
                # id_space_z = id_space_z, id_space_y = id_space_y,
                # id_space_z2 = id_space_z2, id_space_y2 = id_space_y2,
                id_time_z = id_time_z, id_time_y = id_time_y,
                mu_z = mu_z, mu_y = mu_y)

# inla
f_hg <- outcome.matrix ~ 
  # space and time effects
  # f(id_space_z, model = "bym2", graph = g) +
  # f(id_space_y, model = "bym2", graph = g) + 
  #id_time_z + id_time_y +
  f(id_time_z, model = "rw1") + 
  f(id_time_y, model = "rw1") + 
  #x_epa_z + x_epa_y +
  # intercepts
  mu_z + mu_y



system.time(
  res <- inla(f_hg, family = c("binomial", "gamma"), data = data_hg, # binomial = logit; gamma = log
              control.compute = list(dic = TRUE, waic = TRUE),
              control.inla = list(int.strategy = "eb"),
              control.fixed = list(expand.factor.strategy = "inla"),
              verbose = TRUE)
)

summary(res)

#save(res, file = "res_pop_0.1_0.9.RData")

#test <- load("res_pop_0.1_0.9.RData")




# race + pov:       149525.19
# race + pop:       152202.35
# race + pov + pop: 152278.44
# pov + pop_l:      151696.26
# race * pov + pop: 
# race + pop_c.2.8: 150995.12
# race + pop_c no y 151063.17
# race + pop0.1, 0.25, 0.75, 0.9    151254.71
# race + pop0.1,0.9 150157.79 ####
# race + popquint   151059.29

# race*pop_c:       150193.51
# race + pop_l:     151338.17




# w/ 0.3, 0.9   150536.52

# s, t, intercept     150043.97
# s, t, int, pop      36381213.28
# all effects -pop    didn't run, max correction warnings
# all effects         12367088.76, nan produced
# all effects -pov    12445770.37, nan produced
# all fx, pov*his     12196906.61, nan produced
# all fx, race*pov    11771494.03, nan produced, max correction
# 
# no intx; race/pov/gin   10229835.82
# race*pov, gin           10109090.23


table(rsei_fed$rsei.score == 0, rsei_fed$rsei_score_bin)
# 145181.02 ar1
# 144560.49 rw1

plot(aggregate(rsei_fed$rsei.score, by = list(rsei_fed$year), FUN = mean))

#add redlining? epa region? gini index
summary(res)
#151205.86
#151172.93  w pop
# 145120.19 wo pop
150759.09

#150749.38
#150820.71

rsei_fed_20 <- rsei_fed %>%
  filter(year == 2020)

summary(rsei_fed$rsei_score_bin)


# no intx 151188.64
summary(res1)
summary(res)

print(round(res$summary.fixed, 3))
print(res$summary.fixed)
round(res$summary.fixed, 3)


cuya <- rsei_fed %>%
  filter(geoid == "39035")

rsei_11 <- rsei_fed %>%
  filter(year == 2011)
rsei_20 <- rsei_fed %>%
  filter(year == 2020)

# plots
plot(res, plot.fixed.effects = FALSE,
     plot.random.effects = FALSE,
     plot.hyperparameters = TRUE,
     plot.predictor = FALSE, cex = 1.25)


#sapply(rsei_fed, function(x) sum(is.na(x)))

table(rsei_fed$rsei.media == "")

summary(is.na(rsei_fed))

#intercepts
res$marginals.fixed$mu_z %>%
  as.data.frame() %>%
  mutate(x = plogis(x)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line()
plogis(res$summary.fixed$mean[1])
round(res$summary.fixed, 2)

# inla.dmarginal(0, res$marginals.fixed$x_gin_y)

inla.hpdmarginal(0.90, res$marginals.fixed$x_gin_y)[2]

pal <- wes_palette("Zissou1", n = 10, type = "continuous")
v_colors =  viridis(q_colors, )
col = viridis(22, option = "plasma")

test <- data.frame(inla.smarginal(res$marginals.fixed$x_gin_y))
test$ci <- "100%"
test$ci[test$x > inla.hpdmarginal(0.99, res$marginals.fixed$x_gin_y)[1] &
          test$x < inla.hpdmarginal(0.99, res$marginals.fixed$x_gin_y)[2]] <- "99%"
test$ci[test$x > inla.hpdmarginal(0.95, res$marginals.fixed$x_gin_y)[1] &
          test$x < inla.hpdmarginal(0.95, res$marginals.fixed$x_gin_y)[2]] <- "95%"
test$ci[test$x > inla.hpdmarginal(0.90, res$marginals.fixed$x_gin_y)[1] &
          test$x < inla.hpdmarginal(0.90, res$marginals.fixed$x_gin_y)[2]] <- "90%"


test_90 <- test %>%
  filter(ci == "90%")
test_99 <- test %>%
  filter(ci == "99%")
ggplot(test_99, aes(x = x, y = y)) +
  geom_line(alpha = 0) +
  geom_area(aes(fill = x), alpha = 0.5)# +
geom_line(test_90, mapping = aes(x = x, y = y), alpha = 0) +
  geom_area(, alpha = 0.5)

inla.smarginal(res$marginals.fixed$x_gin_y) %>% # smoothed
  as.data.frame() %>%
  #split(by = ci)
  mutate(x = exp(x)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line(alpha = 1) +
  #geom_area(aes(fill = ci, alpha = 0.5), palette = "plasma") +
  #geom_hline(yintercept = ,
  #           linetype = "dashed") +
  geom_vline(xintercept = exp(inla.hpdmarginal(0.90, res$marginals.fixed$x_gin_y)[1]),
             linetype = "dashed") +
  geom_vline(xintercept = exp(inla.hpdmarginal(0.90, res$marginals.fixed$x_gin_y)[2]),
             linetype = "dashed") +
  geom_vline(xintercept = exp(inla.hpdmarginal(0.99, res$marginals.fixed$x_gin_y)[1])) +
  geom_vline(xintercept = exp(inla.hpdmarginal(0.99, res$marginals.fixed$x_gin_y)[2])) +
  theme_bw()
exp(res$summary.fixed$mean[2])



library(tibble)

model <- data.frame(exp(res$summary.fixed))
model <- tibble::rownames_to_column(model, "term")
model$credible <- ifelse(model$X0.025quant > 1, "credible", "not credible")

names(res$marginals.fixed)[1]

inla.smarginal(paste0("res$marginals.fixed$", names(res$marginals.fixed)[1]))
names <- names(res$marginals.fixed)
inla.smarginal(res$marginals.fixed$)


for(i in res$marginals.fixed) {
  print(inla.smarginal(i) %>% # smoothed
          as.data.frame() %>%
          mutate(x = exp(x)) %>%
          ggplot(aes(x = x, y = y)) +
          geom_line(alpha = 1)) #+
  #geom_area(color = col[i])
}

inla.smarginal(res$marginals.fixed$x_gin_y)

ggplot(data = model, 
       aes(x = mean, y = term, xmin = X0.025quant, xmax = X0.975quant,
           color = credible)) +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  xlim(c(-10, 150))
labs(title = "Model Estimates of Brain and Body Weight on REM Sleep",
     x = "Coefficient Estimate",
     y = "Predictor",
     caption = "Models fit with OLS. Error bars show the 95% confidence interval.") +
  scale_y_discrete(labels = c("Intercept", "Hours Awake", "Body Weight", "Brain Weight")) +
  ggpubr::theme_pubclean(flip = TRUE)

ggplot(data = model, 
       aes(x = mean, y = term, xmin = X0.025quant, xmax = X0.975quant,
           color = credible)) +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  xlim(c(-10, 150))
labs(title = "Model Estimates of Brain and Body Weight on REM Sleep",
     x = "Coefficient Estimate",
     y = "Predictor",
     caption = "Models fit with OLS. Error bars show the 95% confidence interval.") +
  scale_y_discrete(labels = c("Intercept", "Hours Awake", "Body Weight", "Brain Weight")) +
  ggpubr::theme_pubclean(flip = TRUE)


res$marginals.fixed$x_gin_y %>%
  as.data.frame() %>%
  mutate(x = exp(x)) %>%
  ggplot(aes(x = x, y = y)) %>%
  ggline(aes(x = as.numeric(x), y = as.numeric(y)))
# + 
geom_hline(yintercept = mean(tri_df_clean$freq), linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed")
#ggtitle("Interaction poverty/nh_white_z")# +
#geom_ribbon(aes(ymin = -theta, ymax = theta), alpha = 0.5)
p1 + guides(fill = guide_legend(title = "Standardized Percent in Poverty"))



#
plot(res, plot.fixed.effects = FALSE,
     plot.random.effects = FALSE,
     plot.hyperparameters = TRUE,
     plot.predictor = FALSE, cex = 1.25)


res1$marginals.fixed$mu_y %>%
  as.data.frame() %>%
  mutate(x = exp(x)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line()

summary(rsei_fed$black)

res$marginals.fixed$x_gin_y %>%
  as.data.frame() %>%
  mutate(x = plogis(x)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line()

summary(res1)

inla.dmarginal(0, res$marginals.fixed$x_gin_y)
inla.zmarginal(res$marginals.fixed$x_gin_y)
summary(res1)
print(res1$summary.fixed)
print(round(res$summary.fixed, 3))

inla.dmarginal(0, res$marginals.fixed$x_pop_y)

inla.hpdmarginal(0.99, res$marginals.fixed$x_pov_y)


save(res, file = "inla_results.RData")
test <- readRDS("inla_results.RData")



plot(rsei_fed[which(rsei_fed$rsei.score > 0), "population_10k_l"],
     rsei_fed[which(rsei_fed$rsei.score > 0), "rsei.score"])

aggregate(rsei_fed$population_10k, by = list(rsei_fed$rsei_score_bin), sd)


