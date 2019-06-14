# plot_comparison_ts_l8_mod.R

# COMPARE TIME SERIES COMMING FROM L8MOD and MOD13Q1
# last update 20180920

setwd("/home/alber/Documents/data/experiments/prodes_reproduction/Rpackage/sits.prodes")
library(devtools)
devtools::load_all()

#library(tidyverse)
#library(sits)

cores <- floor(parallel::detectCores() * 3/4)
sample_directory <- "/home/alber/Documents/data/experiments/prodes_reproduction/data/samples"

# Filter for the original sample files taken from bicks made using interpolation
org_regex <- "validated_prodes_[0-9]{3}_[0-9]{3}_[0-9]{4}-[0-9]{2}-[0-9]{2}_oldinterpolation.Rdata"

# Report the number of bands on a brick
# find /home/alber/shared/ -type f -iname "*MYD13Q1_233067*" -exec number_of_bands {} \;
# find /home/alber/shared/ -type f -iname "*STARFM_233067*" -exec number_of_bands {} \;

if (!file.exists("/home/alber/Documents/data/experiments/prodes_reproduction/tempdir/samples_tb.Rdata")) {
    # load the  samples
    samples_tb <- sample_directory %>%
        list.files(full.names = TRUE, pattern = org_regex) %>%
        stringr::str_subset(pattern = "(233_067|226_064)") %>%
        dplyr::as_tibble() %>% dplyr::rename(files_org = value) %>%
        dplyr::mutate(
            files_itp = stringr::str_replace(files_org, "_oldinterpolation[.]Rdata", "_intepolated.Rdata"),
            files_sfm = stringr::str_replace(files_org, "_oldinterpolation[.]Rdata", "_starfm.Rdata")) %>%
        dplyr::filter(file.exists(files_org), file.exists(files_itp),
                      file.exists(files_sfm)) %>%
        ensurer::ensure_that(nrow(.) > 0) %>%
        dplyr::mutate(ts_org = load_samples(files_org, sat = "Landsat8"),
                      ts_itp = load_samples(files_itp, sat = "Landsat8"),
                      ts_sfm = load_samples(files_sfm, sat = "Landsat8"))

    # make sure the number of samples are the same
    stopifnot(Reduce(setdiff, lapply(list(samples_tb$ts_org, samples_tb$ts_itp,
                                          samples_tb$ts_sfm), vapply, nrow,
                                     numeric(1))) == numeric(0))

    # make sure each time series is 23-observation long
    stopifnot(all(sapply(lapply(
        list(ts_org = samples_tb$ts_org, ts_itp = samples_tb$ts_itp,
             ts_sfm = samples_tb$ts_sfm), function(x){
                 lapply(x, function(y){vapply(y$time_series, nrow, numeric(1))})
             }), function(x) {
                 all(vapply(x, function(y) all(y == 23), logical(1)))
             })))


    # arrange the time series
    common_bands <- ""
    samples_tb$time_series <- purrr::pmap(
        list(org_tb = samples_tb$ts_org, itp_tb = samples_tb$ts_itp,
             sfm_tb = samples_tb$ts_sfm),
        function(org_tb, itp_tb, sfm_tb){
            common_bands <<- Reduce(intersect, lapply(list(org_tb, itp_tb, sfm_tb), sits::sits_bands))
            org_tb <- org_tb %>% sits::sits_select_bands_(common_bands) %>% dplyr::rename(ts_org = time_series)
            itp_tb <- itp_tb %>% sits::sits_select_bands_(common_bands) %>% dplyr::rename(ts_itp = time_series) %>% dplyr::select(ts_itp)
            sfm_tb <- sfm_tb %>% sits::sits_select_bands_(common_bands) %>% dplyr::rename(ts_sfm = time_series) %>% dplyr::select(ts_sfm)
            rdm_tb <- sfm_tb %>% unlist(recursive = FALSE) %>%
                lapply(replace_bands_with_random) %>% tibble::tibble(ts_rdm = .)
            org_tb %>% dplyr::bind_cols(itp_tb, sfm_tb, rdm_tb) %>%
                return()})
    samples_tb <- samples_tb %>% dplyr::select(-dplyr::one_of(c("ts_org", "ts_itp", "ts_sfm")))

    # compare the time series - compute similarity parameters
    samples_tb$metrics <- parallel::mclapply(samples_tb$time_series,
                                             function(tb){
                                                 res <- lapply(common_bands, function(band){
                                                     met <- lapply(1:nrow(tb),
                                                                   function(i){
                                                                       ts_sfm <- tb %>% dplyr::slice(i) %>% dplyr::pull(ts_sfm) %>% dplyr::bind_rows() %>% dplyr::pull(band)
                                                                       ts_itp <- tb %>% dplyr::slice(i) %>% dplyr::pull(ts_itp) %>% dplyr::bind_rows() %>% dplyr::pull(band)
                                                                       ts_sfm <- ts_sfm[1:min(length(ts_sfm), length(ts_itp))]
                                                                       ts_itp <- ts_itp[1:min(length(ts_sfm), length(ts_itp))]
                                                                       m <- matrix(c(ts_sfm, ts_itp), nrow = 2, ncol = length(ts_sfm))
                                                                       dtw_dist <- NA
                                                                       try(
                                                                           dtw_dist <- m %>% dtw::dtwDist() %>% proxy::as.dist() %>% as.vector(),
                                                                           silent = TRUE
                                                                       )
                                                                       ts_lm <- lm(formula = ts_sfm ~ ts_itp)
                                                                       res <- c(ts_lm$coefficients, summary(ts_lm)$r.squared, dtw_dist)
                                                                       names(res) <- c("intercept", "slope", "r.squared", "dtw_dist")
                                                                       return(res)
                                                                   })
                                                     do.call(rbind, met) %>% dplyr::as_tibble() %>%
                                                         return()
                                                 })
                                                 if (length(res) == length(common_bands))
                                                     names(res) <- common_bands
                                                 return(res)
                                             }, mc.cores = cores)

    samples_tb$time_series2 <- purrr::pmap(list(samples_tb$time_series, samples_tb$metrics),
                                           function(x, y){
                                               z <- do.call(cbind, y) %>% dplyr::as_tibble()
                                               x %>% dplyr::bind_cols(z) %>%
                                                   return()
                                           })
    save(samples_tb, file = "/home/alber/Documents/data/experiments/prodes_reproduction/tempdir/samples_tb.Rdata")
}else{
    load(file = "/home/alber/Documents/data/experiments/prodes_reproduction/tempdir/samples_tb.Rdata")
    common_bands <- c("ndvi", "nir", "red", "swir2", "c_msavi", "c_nbr", "c_ndvi", "c_savi", "c_evi2")
}

# plot DTW distance
for (band in common_bands) {
    print(band)
    g <- samples_tb %>% dplyr::pull(time_series2) %>% dplyr::bind_rows() %>%
        dplyr::mutate(pYear = lubridate::year(end_date)) %>%
        ggplot2::ggplot() +
        ggplot2::geom_density(ggplot2::aes_string(x = paste0(band, ".dtw_dist"), colour = "label")) +
        ggplot2::facet_wrap(~ pYear, nrow = 2) +
        ggplot2::xlim(0, 7.5) + ggplot2::ylim(0, 1.2) +
        ggplot2::ggtitle(paste0("DTW distance between bricks StarFM and Interp for band ",
                                toupper(band)))
    print(g)
    ggplot2::ggsave(filename = file.path(".", "vignettes", "img", paste0("sfm_vs_interp_", band, ".png")))
}

print("Done!")

