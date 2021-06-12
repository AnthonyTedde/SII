library(magrittr)

tag_sample_10 <- c(45, 55, 68, 55, 51, 44, 42, 45, 53, 37)
tag_sample_10_hat <- mean(tag_sample_10)
tag_sample_10_sd <- sd(tag_sample_10)
freedom <- length(tag_sample_10) - 1
alpha <- 5e-2
critical_regions <- qt(p = alpha / 2, df = freedom) %>% 
  c(., -1* .)

# compute the t statistics
get_t <- function(mu_0, sample_hat, sample_sd){ 
  (tag_sample_10_hat - mu_0) / (tag_sample_10_sd / sqrt(freedom + 1))
}

# plot the student distribution
plt <- function(t, freedom, critical_regions){ 
  data.frame(x = -4: 4) %>% 
    ggplot2::ggplot(ggplot2::aes(x)) +
    ggplot2::stat_function(fun = dt, args = list(df = freedom)) +
    ggplot2::geom_vline(data = data.frame(x = critical_regions),
                        mapping = ggplot2::aes(xintercept = x), color = "darkred") +
    ggplot2::geom_point(data = data.frame(t, y = 0),
                        mapping = ggplot2::aes(x = t, y = y), size = 5,
                        color = "blue") 
}

################################################################################
# mu_0 := 40
################################################################################

# hypotheses:
# H0: mu_0, H1: mu_1 < mu_0 < mu_1
mu_0 <- 40
# Sample randomly taken from normal population
# Significance: 5%  
t <- get_t(mu_0, tag_sample_10_hat, tag_sample_10_sd)
plt(t, freedom, critical_regions)

# ===> h0 is rejected under the 5% significance level. 
# We potentially make a level-1 error: Rejection of a true H0.

################################################################################
# mu_0 := 49
################################################################################
 
# hypotheses:
# H0: mu_0, H1: mu_1 < mu_0 < mu_1
mu_0 <- 49
# Sample randomly taken from normal population
# Significance: 5%  
t <- get_t(mu_0, tag_sample_10_hat, tag_sample_10_sd)
plt(t, freedom, critical_regions)

# ===> h0 is accepted under the 5% significance level. 
# We potentially make a level-2 error: Acceptance of a false H0.


################################################################################
# mu_0 := 50
################################################################################

# hypotheses:
# H0: mu_0, H1: mu_1 < mu_0 < mu_1
mu_0 <- 50
# Sample randomly taken from normal population
# Significance: 5%  
t <- get_t(mu_0, tag_sample_10_hat, tag_sample_10_sd)
plt(t, freedom, critical_regions)

# ===> h0 is accepted under the 5% significance level. 
# We potentially make a level-2 error: Acceptance of a false H0.


################################################################################
# mu_0 := 51
################################################################################

# hypotheses:
# H0: mu_0, H1: mu_1 < mu_0 < mu_1
mu_0 <- 51
# Sample randomly taken from normal population
# Significance: 5%  
t <- get_t(mu_0, tag_sample_10_hat, tag_sample_10_sd)
plt(t, freedom, critical_regions)

# ===> h0 is accepted under the 5% significance level. 
# We potentially make a level-2 error: Acceptance of a false H0.


################################################################################
# mu_0 := 60
################################################################################

# hypotheses:
# H0: mu_0, H1: mu_1 < mu_0 < mu_1
mu_0 <- 60
# Sample randomly taken from normal population
# Significance: 5%  
t <- get_t(mu_0, tag_sample_10_hat, tag_sample_10_sd)
plt(t, freedom, critical_regions)

# ===> h0 is rejected under the 5% significance level. 
# We potentially make a level-1 error: rejection of a true H0.