#!/usr/bin/Rscript
###############################################################################
# FIND THE BEST HYPERPARAMETERS FOR DEEP LEARNING MODEL
#------------------------------------------------------------------------------
# Last update 2018-12-02
# Based on train_13.R
###############################################################################

prefix = 'train_14'
log_file <- file(paste0(prefix, ".R.log"), open = "wt")
sink(file = log_file, append = TRUE, type = 'message')
message(Sys.time(), ' Initializing...')

suppressMessages(library(sits))
suppressMessages(library(dplyr))

setwd("/home/alber/Documents/data/experiments/prodes_reproduction")

# load the samples
sample_files <- "./data/samples/cluster/Samples_cluster.Rdata"
load(sample_files)
stopifnot(exists("subgroups"))
#unique(subgroups$label2)
#nrow(subgroups)
samples.tb <- subgroups %>% dplyr::mutate(label = label2) %>% 
  dplyr::select(-c(id_neuron, neuron_label, id_sample, label2)) %>%
  sits::sits_select_bands(bands = c("nir", "red", "swir2"))
#sits::sits_bands(samples.tb)
# sample_files <- list.files('./data/samples', full.names = TRUE,
#                            pattern = 'validated_prodes_[0-9]{3}_[0-9]{3}_[0-9]{4}-[0-9]{2}-[0-9]{2}.Rdata')
# samples.tb <- NULL
# for(spath in sample_files){
#   load(spath)
#   samples.tb <- dplyr::bind_rows(samples.tb, samples.tb)
# }
stopifnot(exists("samples.tb"))
#samples.tb <- samples.tb %>% dplyr::filter(label %in% c("forest", "deforestation", "flood")) # ---------------------------------

param_lst <- list()
n <- 1:20
for(i in n){
  n_layers <- sample(2:6, size = 1)
  param_lst[[i]] = list(
    units            = rep(sample(seq(600, 1000, 100), size = 1), n_layers),
    activation       = sample(c('selu', 'sigmoid'), size = 1),
    dropout_rates    = rep(sample(seq(0.3, 0.5, 0.1), size = 1), n_layers),
    optimizer        = keras::optimizer_adam(),
    epochs           = sample(seq(200, 300, 100), size = 1),
    batch_size       = sample(c(64, 256, 320), size = 1),
    validation_split = 0.2,
    model_name       = paste0(prefix, "_model_", i) 
  )
}

for(p in param_lst){
  #options(keras.fit_verbose = 0)
  method <- sits_deeplearning(
    units            = p$units,
    activation       = p$activation,
    dropout_rates    = p$dropout_rates,
    optimizer        = p$optimizer,
    epochs           = p$epochs,
    batch_size       = p$batch_size,
    validation_split = p$validation_split)
  model <- sits::sits_train(samples.tb, method)
  
  sits::sits_save_keras(model,
                        hdffile = paste0('./02_train_model/', prefix, '/', p$model_name, '.h5'),
                        rdsfile = paste0('./02_train_model/', prefix, '/', p$model_name, '.rds'))
  
  met <- environment(model)$history$metrics
  message('EXPERIMENT')
  message(names(p))
  message('optimizer = ', attributes(p$optimizer)$class[1])
  message(paste0(names(p), " = ", p, '\n'))
  msgs <- paste0(names(met), ' = ', met)
  for(m in msgs){
    message(m)
  }
  message('---')
  rm(method)
  rm(model)
  rm(met)
}
message('Finished!')
message(Sys.time())
message(' ')
