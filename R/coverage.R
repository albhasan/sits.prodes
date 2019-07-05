#' @title Get the areas
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description    Get the areas sumarized by label.
#' @param cov      A coverage.
#' @param label    A character. The name of the stratification field.
#' @return         A simple feature object of type point (sf).
#' @export
cov_get_areas <- function(cov, label){
  area <- count <- lab_area <- tmp_area <- value <- zone <- NULL
  stopifnot(all(colnames(cov) %in% c("area", "count", "lab_area", "tmp_area", "value",
                                     "zone")))

  if (cov_type(cov) == "vector") {
    lab_area <- cov %>%
      dplyr::mutate(tmp_area = sf::st_area(.)) %>%
      sf::st_set_geometry(value = NULL) %>%
      dplyr::group_by_(label) %>%
      dplyr::summarise(area = sum(tmp_area))
    colnames(lab_area) <- c("label", "area")
  } else if (cov_type(cov) == "raster") {
    if (raster::isLonLat(cov)) {
      lab_area <- cov %>%
        raster::area() %>%
        raster::zonal(cov, 'sum') %>%
        dplyr::as_tibble() %>%
        dplyr::select(label = zone, area = sum)
    }else{
      cov_r <- raster::res(cov)
      lab_area <- cov %>%
        raster::freq() %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(area = count * prod(cov_r)) %>%
        dplyr::select(label = value, area)
    }
  }else{
    stop("Unknown coverage!")
  }
  return(lab_area)
}


#' @title Get values from a coverage
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description         Get teh values in the coverage at the given points
#' @param cov           A coverage.
#' @param sample_points A simple feature object of type point (sf).
#' @return              A simple feature object of type point (sf).
#' @export
cov_get_values <- function(cov, sample_points){
  cov_points <- NULL
  # Match CRS
  if (cov_srs(cov) != cov_srs(sample_points)) {
    sample_points <- sf::st_transform(sample_points, crs = cov_srs(cov))
  }
  #
  if (cov_type(cov) == "vector") {
    cov_points <- sample_points %>%
      sf::st_join(cov)
  } else if (cov_type(cov) == "raster") {
    cov_points <- cov %>%
      raster::extract(methods::as(sample_points, "Spatial"), sp = TRUE,
                      method = 'simple') %>%
      sf::st_as_sf()
  }else{
    stop("Unknown coverage!")
  }
  return(cov_points)
}


#' @title Transform a coverage's SRS
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Transform a coverage from its spatial reference system to another
#' @param cov         A coverage.
#' @param proj4string A Character. A proj4string description of the target SRS
#' @return            An projected object of the same class of cov
#' @export
cov_proj <- function(cov, proj4string){
  pcov <- cov
  if (cov_type(cov) == "vector") {
    if (sf::st_crs(cov)$proj4string != proj4string)
      pcov <- sf::st_transform(cov, crs = proj4string)
  } else if (cov_type(cov) == "raster") {
    if (raster::crs(cov, asText = TRUE) != proj4string)
      pcov <- raster::projectRaster(cov, crs = proj4string, method = "ngb")
  }else{
    stop("Unknown coverage!")
  }
  return(pcov)
}


#' @title Read a coverage from a file on disk.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Read a coverage from a file on disk.
#' @param cov_path A character. The path to a coverage file.
#' @return         A coverage
#' @export
cov_read <- function(cov_path){
  cov <- NULL
  cov_ext <- cov_path %>%
    basename() %>%
    stringr::str_split(pattern = '[.]') %>%
    unlist() %>%
    dplyr::last()
  if (cov_ext %in% "shp") {
    cov <- cov_path %>% sf::read_sf(stringsAsFactors = FALSE, quiet = TRUE)
  } else if (cov_ext %in% c("tif", "tiff", "TIF", "TIFF")) {
    cov <- cov_path %>% raster::raster()
  }else{
    stop("Unknown coverage!")
  }
  return(cov)
}


#' @title Get random sample points
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description    Get approximately n samples out of the given coverage
#' @param cov      A coverage.
#' @param n        A numeric. The number of samples.
#' @return         A simple feature object (sf) of type point.
#' @export
cov_sample_random <- function(cov, n){
  sample_points <- NULL
  if (cov_type(cov) == "vector") {
    suppressMessages(
      samples_sfc <- cov %>%
        sf::st_sample(size = n)
    )
    # cast back to sf
    sf_samples <- data.frame(id = 1:length(samples_sfc))
    sf::st_geometry(sf_samples) <- samples_sfc
    suppressMessages(
      sample_points <- sf::st_join(sf_samples, cov)
    )
  } else if (cov_type(cov) == "raster") {
    s_pts <- cov %>% raster::sampleRandom(size = n, sp = TRUE)
    sample_points <- cov %>% raster::extract(s_pts, sp = TRUE) %>%
      sf::st_as_sf()
  }else{
    stop("Unknown coverage!")
  }
  return(sample_points)
}


#' @title Get random sample points
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description    Get approximately n samples out of the given coverage. Each
#' label gets approximately the same number of samples
#' @param cov      A coverage.
#' @param n        A numeric. The number of samples per class.
#' @param label    A character. The name of the stratification field.
#' @param cores    A numeric. The number of cores for parallel processing. The
#' defailt is one.
#' @return         A simple feature object of type point (sf).
#' @export
cov_sample_stratified <- function(cov, n, label = NULL, cores = 1L){
  sample_points <- NULL
  samples_per_class <- n
  if (cov_type(cov) == "vector") {
    cov["tmp_label"] <- sf::st_set_geometry(cov[label], NULL)
    label_vec <- unique(cov$tmp_label)
    if (length(n) != length(label_vec))
      samples_per_class <- rep(n[1], times = length(label_vec))
    samples_sf_ls <- parallel::mclapply(
      1:length(label_vec), FUN = function(x, cov, label_vec, samples_per_class){
        tmp_label <- NULL
        stopifnot("tmp_label" %in% colnames(tmp_label))
        lab = label_vec[x]
        sf_obj <- cov %>%
          dplyr::filter(tmp_label == lab) %>%
          cov_sample_random(n = samples_per_class[x])
        return(sf_obj)
      },
      cov = cov,
      label_vec = label_vec,
      samples_per_class = samples_per_class,
      mc.cores = cores)
    # cast to sf
    sample_points <- do.call(rbind, samples_sf_ls)
    tmp_label <- NULL
    sample_points <- sample_points %>%
      dplyr::select(-tmp_label)
  } else if (cov_type(cov) == "raster") {
    sample_points <- cov %>%
      raster::sampleStratified(size = max(n), sp = TRUE) %>% sf::st_as_sf()
    df_ls <- sample_points %>% split(sample_points[[2]])
    if (length(n) != length(df_ls))
      samples_per_class <- rep(n[1], times = length(df_ls))
    sub_df_ls <- lapply(1:length(df_ls), function(x, df_ls, samples_per_class){
      df <- df_ls[[x]]
      return(df[sample(nrow(df), samples_per_class[x]), ])
    }, df_ls = df_ls, samples_per_class = samples_per_class)
    sample_points <- do.call(rbind, sub_df_ls)
  }else{
    stop("Unknown coverage!")
  }
  return(sample_points)
}


#' @title Get a coverage's SRS.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Get a coverage's spatial reference system.
#' @param cov      A coverage.
#' @return         A character. The proj4string description of the coverage's SRS
#' @export
cov_srs <- function(cov){
  proj4string <- NULL
  if (cov_type(cov) == "vector") {
    proj4string <- sf::st_crs(cov)[["proj4string"]]
  } else if (cov_type(cov) == "raster") {
    proj4string <- raster::projection(cov, asText = TRUE)
  }else{
    stop("Unknown coverage!")
  }
  return(proj4string)
}


#' @title Get the type of coverage.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Test if the given object is either a vector or a raster.
#' @param cov      A coverage.
#' @return         A character
#' @export
cov_type <- function(cov){
  res <- NA
  if (sum(class(cov) %in% c("sf")) > 0) {
    res <- "vector"
  } else if (sum(class(cov) %in% c("RasterLayer")) > 0) {
    res <- "raster"
  }
  return(res)
}


#' @title Is the given object a coverage?.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Test if the given object is supported.
#' @param cov      A coverage.
#' @return         A logical.
#' @export
is_cov <- function(cov){
  res <- FALSE
  if (!is.na(cov_type(cov)))
    res <- TRUE
  return(res)
}

#' @title Validate a coverage
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Compare two coverages.
#'
#' @param cov_res A coverage.
#' @param cov_ref A coverage.
#' @param lab_res A character. The name of the label field in the results coverage.
#' @param lab_ref A character. The name of the label field in the referece coverage.
#' @param n_samples A length-one integer. The number of samples to use during validation. The default is 1000.
#' @param cores     A length-one integer. The number cores to use. The default is 1
validate_sampling <- function(cov_res, cov_ref, lab_res, lab_ref,
                              n_samples = 10000, cores = 1){

  # get sample points
  s_points <- cov_ref %>%
    cov_sample_stratified(n = n_samples,
                          label = lab_ref,
                          cores = cores) %>%
    dplyr::transmute(sample_id = 1:nrow(.))

  # get reference sample points
  ref_points <- cov_ref %>% cov_get_values(s_points)

  # get values at sample points
  label_id <- sample_id <- NULL
  res_points <- cov_res %>%
    ensurer::ensure_that(c("sample_id", "label_id") %in% colnames(.)) %>%
    cov_get_values(s_points) %>%
    sf::st_set_geometry(value = NULL) %>%
    dplyr::select(sample_id, label_id = dplyr::contains("mixl8")) %>%
    dplyr::mutate(lab_res = dplyr::recode(label_id,
                                          "0" = "Unknown",
                                          "1" = "Deforestation",
                                          "2" = "Forest",
                                          "3" = "No forest",
                                          "4" = "Water")) %>%
    dplyr::select(sample_id, lab_res)

  # control
  if (nrow(ref_points) != nrow(res_points))
    warning("nrow(ref_points) != nrow(res_points)")

  # Join the data, select and rename label fields
  val_tb <- ref_points %>%
    sf::st_set_geometry(value = NULL) %>%
    dplyr::left_join(res_points, by = "sample_id") %>%
    dplyr::select("lab_ref" = lab_ref, "lab_res" = lab_res)

  # validate
  if (sum(stats::complete.cases(val_tb)) != nrow(val_tb)) {
    warning(paste0("Some samples are labeled as NAs: ",
                   sum(stats::complete.cases(val_tb)), "/", nrow(val_tb)))
    val_tb <- stats::na.omit(val_tb)
  }

  # control - check if all the result labels are in the reference values
  stopifnot(unique(val_tb[["lab_res"]]) %in% unique(val_tb[["lab_ref"]]))

  # Compute confusion matrix
  flevels <- val_tb %>%
    dplyr::select(lab_ref, lab_res) %>%
    unlist() %>% unique()
  con_mat <- caret::confusionMatrix(factor(val_tb[["lab_ref"]], flevels),
                                    factor(val_tb[["lab_res"]], flevels))

  # Compute areas
  cov_areas <- cov_get_areas(cov_ref, label = lab_ref)

  # Asses accuracy
  class_areas <- as.vector(cov_areas$area)
  names(class_areas) <- cov_areas$label
  acc <- asses_accuracy_area(
    error_matrix = as.matrix(as.data.frame.matrix(con_mat$table)),
    class_areas
  )
  # NOTE:
  # The function sits::sits_accuracy_area() internally calls dtwSat:::.twdtwAssess()
  # dtwSat:::.twdtwAssess(x = error_matrix, mapped_area = class_areas, conf.int = 0.95, rm.nosample = TRUE)
  return(acc)
}
