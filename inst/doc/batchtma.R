## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE------------------------------------------------------------
#  # install.packages("remotes")  # The "remotes" package needs to be installed
#  remotes::install_github("stopsack/batchtma")

## ---- eval = FALSE------------------------------------------------------------
#  remotes::install_github("stopsack/batchtma", build_vignettes = TRUE)

## ---- message = FALSE---------------------------------------------------------
library(batchtma)
library(tidyverse)

## -----------------------------------------------------------------------------
set.seed(123)  # for reproducibility
df <- tibble(
  # Batches:
  batch = rep(paste0("batch", LETTERS[1:4]), times = 100),
  batchnum = rep(c(1, 5, 2, 3), times = 100),
  # Participants:
  person = rep(letters[1:10], each = 40),
  # Instead of a confounder, we will use a random variable for now:
  random = runif(n = 400, min = -2, max = 2),
  # The true (usually unobservable biomarker value):
  true = rep(c(2, 2.5, 3, 5, 6, 8, 10, 12, 15, 12), each = 40),
  # The observed biomarker value with random error ("noise"):
  noisy = true + runif(max = true / 3, n = 400) * 4)

df

## -----------------------------------------------------------------------------
df %>% plot_batch(marker = noisy, batch = batch, color = person)

## -----------------------------------------------------------------------------
df <- df %>%
  # Multiply by batch number to differentially change variance by batch,
  # divide by mean batch number to keep overall variance the same:
  mutate(noisy_batch = noisy * batchnum / mean(batchnum) + 
           # Similarly, change mean value per batch, keeping overall mean the same:
           batchnum * 3 - mean(batchnum) * 3)

df %>% plot_batch(marker = noisy_batch, batch = batch, color = person)

## -----------------------------------------------------------------------------
df %>% 
  adjust_batch(markers = noisy_batch, batch = batch, 
               method = simple) %>%
  plot_batch(marker = noisy_batch_adj2, batch = batch, color = person)

## -----------------------------------------------------------------------------
df %>% 
  adjust_batch(markers = noisy_batch, batch = batch, 
               method = standardize, confounders = random) %>%
  plot_batch(marker = noisy_batch_adj3, batch = batch, color = person)

## -----------------------------------------------------------------------------
df %>% 
  adjust_batch(markers = noisy_batch, batch = batch, 
               method = ipw, confounders = random) %>%
  plot_batch(marker = noisy_batch_adj4, batch = batch, color = person)

## -----------------------------------------------------------------------------
df %>% 
  adjust_batch(markers = noisy_batch, batch = batch, 
               method = quantreg, confounders = random) %>%
  plot_batch(marker = noisy_batch_adj5, batch = batch, color = person)

## -----------------------------------------------------------------------------
df %>% 
  adjust_batch(markers = noisy_batch, batch = batch, 
               method = quantnorm) %>%
  plot_batch(marker = noisy_batch_adj6, batch = batch, color = person)

## -----------------------------------------------------------------------------
set.seed(123)  # for reproducibility
df <- df %>%
  # Make confounder associated with batch:
  mutate(confounder = round(batchnum + runif(n = 200, max = 2)),
         # Make biomarker values associated with confounder:
         noisy_conf = noisy + confounder * 3 - mean(confounder) * 3)

df %>% plot_batch(marker = noisy_conf, batch = batch, color = confounder)

## -----------------------------------------------------------------------------
df <- df %>% 
  # Add batch effects to confounded biomarker values:
  mutate(noisy_conf_batch = noisy_conf * batchnum / mean(batchnum) + 
           batchnum * 3 - mean(batchnum) * 3)

df %>% plot_batch(marker = noisy_conf_batch, batch = batch, color = confounder)

## -----------------------------------------------------------------------------
df %>% 
  adjust_batch(markers = noisy_conf_batch, batch = batch, 
               method = standardize, confounders = confounder) %>%
  plot_batch(marker = noisy_conf_batch_adj3, batch = batch, color = confounder)

## -----------------------------------------------------------------------------
df %>% 
  adjust_batch(markers = noisy_conf_batch, batch = batch, 
               method = ipw, confounders = confounder) %>%
  plot_batch(marker = noisy_conf_batch_adj4, batch = batch, color = confounder)

## -----------------------------------------------------------------------------
df %>% 
  adjust_batch(markers = noisy_conf_batch, batch = batch, 
               method = quantreg, confounders = confounder) %>%
  plot_batch(marker = noisy_conf_batch_adj5, batch = batch, color = confounder)

## -----------------------------------------------------------------------------
df %>% 
  adjust_batch(markers = noisy_conf_batch, batch = batch, 
               method = simple, confounders = confounder) %>%
  plot_batch(marker = noisy_conf_batch_adj2, batch = batch, color = confounder)

## -----------------------------------------------------------------------------
df %>% 
  adjust_batch(markers = noisy_conf_batch, batch = batch, 
               method = quantnorm, confounders = confounder) %>%
  plot_batch(marker = noisy_conf_batch_adj6, batch = batch, color = confounder)

## -----------------------------------------------------------------------------
# df2 is the new dataset that also contains "noisy_conf_batch_adj2":
df2 <- df %>% adjust_batch(markers = noisy_conf_batch, batch = batch, 
                           method = standardize, confounders = confounder)

# Show overview of model diagnostics:
diagnose_models(df2)

## -----------------------------------------------------------------------------
fit <- diagnose_models(data = df2)$model_fits[[1]][[1]]
summary(fit)

## -----------------------------------------------------------------------------
diagnose_models(df2)$adjust_parameters

## -----------------------------------------------------------------------------
tibble(fitted    = fitted.values(fit),
       residuals = residuals(fit)) %>%
  ggplot(mapping = aes(x = fitted, y = residuals)) +
  geom_point() +
  theme_minimal()

