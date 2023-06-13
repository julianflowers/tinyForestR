devtools::install_github('biologicalrecordscentre/sparta')

library(sparta)
needs(snowfall, bayesplot, tidybayes)

time <- data.frame(start = c(2000, 2005, 2010, 2015, 2020),
                   end = c(2004, 2009, 2014, 2019, 2022))


tf_nbn <-fread("data/nbn_tf.csv")

tf_nbn_sparta <- tf_nbn |>
  select(site = tfID, taxa = species, year, month) |>
  mutate(date = lubridate::make_date(year, month, day = 1L))


tf_nbn_sparta <- tf_nbn_sparta |>
  drop_na()


results <- dataDiagnostics(taxa = tf_nbn_sparta$taxa,
                           site = tf_nbn_sparta$site,
                           time_period = tf_nbn_sparta$date,
                           progress_bar = TRUE)

## monthly list lengths by tfid
results$VisitListLength

## annual records
results$RecordsPerYear

## list lengths have increased per over time
results$modelList |>
  summary()


## telfer

tf_nbn_sparta$tp <- date2timeperiod(tf_nbn_sparta$date, time)

tf_nbn_sparta

telfer_results <- telfer(taxa = tf_nbn_sparta$taxa,
                         site = tf_nbn_sparta$site,
                         time_period = tf_nbn_sparta$tp,
                         minSite = 2)


## reporting rate

rr <- siteSelectionMinL(taxa = tf_nbn_sparta$taxa,
                        site = tf_nbn_sparta$site,
                        time_period = tf_nbn_sparta$tp,
                        minL = 2)

rr |>
  count(site)

myDataSubset  <- siteSelection(taxa = tf_nbn_sparta$taxa,
                               site = tf_nbn_sparta$site,
                               time_period = tf_nbn_sparta$tp,
                               minL = 2,
                               minTP = 5,
                               LFirst = TRUE)

myDataSubset |>
  arrange(site)

spp <- pluck(myDataSubset, "taxa") |>
  unique()


RR_out <- reportingRateModel(taxa = tf_nbn_sparta$taxa,
                             site = tf_nbn_sparta$site,
                             time_period = tf_nbn_sparta$date,
                             list_length = TRUE,
                             site_effect = TRUE,
                             species_to_include = spp[1:10],
                             overdispersion = FALSE,
                             family = 'Bernoulli',
                             print_progress = TRUE)
)

str(RR_out)

RR_out |>
  mutate(lower = year.estimate - year.stderror,
         upper = year.estimate + year.stderror) |>
  ggplot(aes(species_name, year.estimate)) +
  geom_point() +
  geom_linerange(aes(ymin = lower, ymax = upper)) +
  coord_flip()


sfInit(parallel = TRUE, cpus = 8)
sfExport('tf_nbn_sparta')

RR_mod_function <- function(taxa_name){

  library(sparta)

  RR_out <- reportingRateModel(species_to_include = taxa_name,
                               taxa = tf_nbn_sparta$taxa,
                               site = tf_nbn_sparta$site,
                               time_period = tf_nbn_sparta$date,
                               list_length = TRUE,
                               site_effect = TRUE,
                               overdispersion = FALSE,
                               family = 'Bernoulli',
                               print_progress = FALSE)
}

system.time({
  para_out <- sfClusterApplyLB(spp[1:200], RR_mod_function)
})


RR_out_combined <- map_df(para_out, rbind)

RR_out_combined |>
  mutate(lower = year.estimate - year.stderror,
         upper = year.estimate + year.stderror) |>
  ggplot(aes(fct_rev(species_name), year.estimate)) +
  geom_point() +
  geom_linerange(aes(ymin = lower, ymax = upper)) +
  coord_flip()


WSS_out <- WSS(taxa = tf_nbn_sparta$taxa,
               site = tf_nbn_sparta$site,
               time_period = tf_nbn_sparta$date,
               minL = 2,
               minTP = 5,
               print_progress = TRUE)

WSS_out[1:20,] |>
  mutate(lower = year.estimate - year.stderror,
         upper = year.estimate + year.stderror) |>
  ggplot(aes(fct_rev(species_name), year.estimate)) +
  geom_point() +
  geom_linerange(aes(ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0, colour = "red") +
  coord_flip()

## occupancy

system.time({
  occ_out <- occDetModel(taxa = tf_nbn_sparta$taxa,
                         site = tf_nbn_sparta$site,
                         survey = tf_nbn_sparta$date,
                         species_list = spp[1],
                         write_results = FALSE,
                         n_iterations = 200,
                         burnin = 15,
                         n_chains = 3,
                         thinning = 3,
                         seed = 123)
})

formattedOccData <- formatOccData(taxa = tf_nbn_sparta$taxa,
                                  site = tf_nbn_sparta$site,
                                  survey = tf_nbn_sparta$date)


formattedOccData$spp_vis[, 1:50] |>
  dim()

formattedOccData$occDetdata |>
  arrange(site) |>
  tail()

occdata <- bind_cols(formattedOccData$occDetdata, formattedOccData$spp_vis)

occdata_dt <- setDT(occdata)

occdata_dt[, 1:6]

snowfall::sfStop()

sfInit(parallel = TRUE, cpus = 6)

sfExport("formattedOccData")

occ_mod_function <- function(taxa_name){

  library(sparta)

  occ_out <- occDetFunc(taxa_name = taxa_name,
                        n_iterations = 200,
                        burnin = 15,
                        occDetdata = formattedOccData$occDetdata,
                        spp_vis = formattedOccData$spp_vis,
                        write_results = TRUE,
                        seed = 123)
}



occDetFunc(taxa_name = "Abdera biflexuosa")

system.time({
  para_out_1 <- sfClusterApplySR("Abdera biflexuosa", occ_mod_function)
})

formattedOccData$spp_vis[,1:2] |>
  filter(str_detect(`Abdera biflexuosa`, "TRUE"))

library(furrr)
plan(multisession)
options(future.globals.maxSize= 891289600)

system.time({
  para_out_1 <- future_map(spp[3], occ_mod_function)
})


sfStop()
para_out[[1]]

map(para_out, "species_name")

snowfall::sfCL

needs(brms, R2jags, posterior, lme4)

occdata <- bind_cols(formattedOccData$occDetdata, formattedOccData$spp_vis)

head(occdata)

occdata_dt <- data.table::setDT(occdata)

occdata_dt |>
  write_rds("large-data/occupancy_data_1.rds")

which(str_detect(colnames(occdata), "Poecile") == TRUE)

test_data <- occdata_dt[,c(2:4, 4730)] |>
  janitor::clean_names() |>
  mutate_at(.vars = 4, \(x) ifelse(x == "FALSE", 0, 1)) |>
  data.frame()

c <- colnames(test_data)
c

tdw <- test_data |>
  mutate(f = factor(poecile_montanus)) |>
  group_by(site, tp, f) |>
  summarise(n = n(),
            meanL = mean(l)) |>
  pivot_wider(names_from = "f", values_from = "n", values_fill = 0)

tdw |>
  save("data/tdw_1.rda")

load("data/tdw_1.rda")

tdw |>
  head()

test_mod_intercept <- stan_glmer(cbind(`0`, `1`) ~ 1 + (1|site), data = tdw, family = "binomial")

prior_summary(test_mod_intercept)

print(test_mod_intercept, digits = 2)

summary(test_mod_intercept,
        pars = c("(Intercept)", "sigma", "Sigma[site:(Intercept), (Intercept)]"),
        probs = c(0.025, 0.975),
        digits = 2)



sims <- as.matrix(test_mod_intercept)
sig <- as.matrix(test_mod_intercept, pars = "sigma")
colnames(sims)

in_sims <- as.matrix(test_mod_intercept, regex_pars = "(Intercept)")
map(s_sims, mean)
s_sims <- as.matrix(test_mod_intercept, regex_pars = "b\\[\\(Intercept\\) site\\:")

a_sims <- as.numeric(in_sims) + s_sims

test_mod_pred_1 <- stan_glmer(cbind(`0`, `1`) ~ 1 + meanL + tp + (1|site), data = tdw, family = "binomial", cores = 6, seed = 123)

prior_summary(test_mod_pred_1)

print(test_mod_pred_1, digits = 2)

pp_check(test_mod_pred_1)



plot(test_mod, regex_pars = "meanL")
