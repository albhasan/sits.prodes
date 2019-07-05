#' @title Simulate a sits_tibble made of random observations
#' @name sim_sits_tibble
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description This function simulates a sits_tibble using random data. The
#' time series are simulated using a cosine function.
#'
#' @param n_samples     A length-one numeric. The number of samples per label.
#' @param label         A character. The labels for returned tibble. The default is "label_A".
#' @param lon_mean      A length-one numeric. The central longitude of the random samples. The default is 65 degrees est.
#' @param lon_sd        A length-one numeric. The standard deviation of the longitude of the random samples. The default is 1.
#' @param lat_mean      A length-one numeric. The central latitude of the random samples. The default is 5 degrees south.
#' @param lat_sd        A length-one numeric. The standard deviation of the latitude of the random samples. The default is 1.
#' @param date_start    A length-one character.The start date. The default is "2000/01/01".
#' @param date_end      A length-one character.The start date. The default is "2000/12/31".
#' @param obs_freq      A length-one numeric. The number of observations in the period. The default are 23.
#' @param n_vi          A length-one numeric. The number of vegetation indexes on each sample. The default is 1.
#' @return random_st    A sits_tibble
#'
#' @examples
#' library(sits.prodes)
#' library(sits)
#' my_st <- sim_sits_tibble(10, label = c("A", "B"))
#' sits_plot(my_st)
#'
#' @export
sim_sits_tibble <- function(n_samples, label = "label_A", lon_mean = -65,
                            lon_sd = 1, lat_mean = -5, lat_sd = 1,
                            date_start = "2000/01/01",
                            date_end = "2000/12/31",
                            obs_freq = 23,
                            n_vi = 1){
    # number if samples
    n <- n_samples * length(label)

    # simulate the time series
    index = seq(from = as.Date(date_start), to = as.Date(date_end), length.out = obs_freq)
    ts.lst <- lapply(1:n, FUN = function(x, obs_freq, index, n_vi){
        tmp.lst <- list()
        tmp.lst[["Index"]] <- index
        for (i in 1:n_vi) {
            tmp.lst[[paste0("vi", i)]] <- simulate_ts(t_vector = 1:obs_freq)
        }
        return(tibble::as_tibble(tmp.lst))
    }, obs_freq = obs_freq, index = index, n_vi = n_vi)

    # build the sits_tibble
    random_st <- tibble::tibble(longitude   = stats::rnorm(n, lon_mean, lon_sd),
                                latitude    = stats::rnorm(n, lat_mean, lat_sd),
                                start_date  = rep(as.Date(date_start), times = n),
                                end_date    = rep(as.Date(date_end), times = n),
                                label       = rep(label, each = n_samples),
                                coverage    = rep("random_coverage", times = n),
                                time_series = ts.lst)
    class(random_st) <- class(sits::sits_tibble())
    #sits:::.sits_test_tibble(random_st)
    return(random_st)
}



#' @title Simulate a time series of a Vegetation Index (VI). Alber Sanchez
#' @name simulate_ts
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#'
#' @description this function simulates a time series of vegetation indexes using a cosine function.
#'
#' @param t_vector    Numeric. A vector of time indexes.
#' @param m           A length-one numeric. The mean of the VI.
#' @param amplitude   A length-one numeric. The amplitude of the VI wave.
#' @param freq        A length-one numeric. The frequency of the VI wave.
#' @param phase_shift A length-one numeric. The phase shift of the VI wave.
#' @param noise_mean  A length-one numeric. The mean of the noise.
#' @param noise_sd    A length-one numeric. The stabndard deviation of the noise.
#' @return            A numeric vector
#'
simulate_ts <- function(t_vector, m = 0.5, amplitude = 0.15,
                        freq = 16/365, phase_shift = 0,
                        noise_mean = 0, noise_sd = 0.2){
    angular_freq <- 2 * pi * freq
    noise <- stats::rnorm(length(t_vector), noise_mean, noise_sd)
    return(m + amplitude * cos((angular_freq * t_vector) + phase_shift) + noise)
}
