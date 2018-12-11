################################################################################
# COVERGAE IS AN ABSTRACTION OF THE COMMON FEATURE OF RASTER AND VECTOR OBJECTS
# alber sanchez alber.ipia@inpe.br
#-------------------------------------------------------------------------------
# Last update 2018-07-03
#-------------------------------------------------------------------------------
# TODO:
# - DO we need support for SP objects?
#-------------------------------------------------------------------------------
# NOTES:
################################################################################

#' @title Get the areas
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description    Get the areas sumarized by label.
#' @param cov      A coverage.
#' @param label    A character. The name of the stratification field.
#' @return         A simple feature object of type point (sf).
cov_get_areas <- function(cov, label){
  lab_area <- NULL
  if(cov_type(cov) == "vector"){  
    lab_area <- cov %>% dplyr::mutate(tmp_area = sf::st_area(.)) %>% 
      sf::st_set_geometry(NULL) %>%
      dplyr::group_by_(label) %>% 
      dplyr::summarise(area = sum(tmp_area))
    colnames(lab_area) <- c("label", "area") # TODO: dplyr-ize!
  }else if(cov_type(cov) == "raster"){
    if(raster::isLonLat(cov)){
      lab_area <- cov %>% raster::area() %>% zonal(cov, 'sum') %>% 
        dplyr::as_tibble() %>% dplyr::select(label = zone, area = sum)
    }else{
      cov_r <- raster::res(cov)
      lab_area <- cov %>% raster::freq() %>% dplyr::as_tibble() %>%
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
cov_get_values <- function(cov, sample_points){
  cov_points <- NULL
  # Match CRS
  if(cov_srs(cov) != cov_srs(sample_points)){
    sample_points <- sf::st_transform(sample_points, crs = cov_srs(cov))
  }
  #
  if(cov_type(cov) == "vector"){
    cov_points <- sample_points %>% sf::st_join(cov)
  }else if(cov_type(cov) == "raster"){
    cov_points <- cov %>% 
      raster::extract(as(sample_points, "Spatial"), sp = TRUE, method='simple') %>%
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
cov_proj <- function(cov, proj4string){
  pcov <- cov
  if(cov_type(cov) == "vector"){
    if(sf::st_crs(cov)$proj4string != proj4string)
      pcov <- sf::st_transform(cov, crs = proj4string)
  }else if(cov_type(cov) == "raster"){
    if(raster::crs(cov, asText = TRUE) != proj4string)
      pcov <- raster::projectRaster(cov, crs = proj4string, method="ngb")
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
cov_read <- function(cov_path){
  cov <- NULL
  cov_ext <- cov_path %>% basename() %>% stringr::str_split(pattern = '[.]') %>% 
    unlist() %>% dplyr::last()
  if(cov_ext %in% "shp"){
    cov <- cov_path %>% sf::read_sf(stringsAsFactors = FALSE, quiet = TRUE)
  }else if(cov_ext %in% c("tif", "tiff", "TIF", "TIFF")){
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
cov_sample_random <- function(cov, n){
  sample_points <- NULL
  if(cov_type(cov) == "vector"){
    suppressMessages(
      samples_sfc <- sf::st_sample(cov, size = n)
    )
    # cast back to sf
    sf_samples <- data.frame(id = 1:length(samples_sfc))
    sf::st_geometry(sf_samples) <- samples_sfc
    suppressMessages(
      sample_points <- sf::st_join(sf_samples, cov)
    )
  }else if(cov_type(cov) == "raster"){
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
cov_sample_stratified <- function(cov, n, label = NULL, cores = 1L){
  sample_points <- NULL
  samples_per_class <- n
  if(cov_type(cov) == "vector"){
    cov["tmp_label"] <- sf::st_set_geometry(cov[label], NULL)
    label_vec <- unique(cov$tmp_label)
    if(length(n) != length(label_vec))
      samples_per_class <- rep(n[1], times = length(label_vec))
    samples_sf_ls <- parallel::mclapply(1:length(label_vec),
                                        FUN = function(x, cov, label_vec, samples_per_class){
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
    sample_points <- sample_points %>% dplyr::select(-tmp_label)
  }else if(cov_type(cov) == "raster"){
    sample_points <- cov %>% 
      raster::sampleStratified(size = max(n), sp = TRUE) %>% sf::st_as_sf()
    df_ls <- sample_points %>% split(sample_points[[2]])
    if(length(n) != length(df_ls))
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
cov_srs <- function(cov){
  proj4string <- NULL
  if(cov_type(cov) == "vector"){
    proj4string <- sf::st_crs(cov)[["proj4string"]]
  }else if(cov_type(cov) == "raster"){
    proj4string <- raster::projection(cov, asText=TRUE)
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
cov_type <- function(cov){
  res <- NA
  if(sum(class(cov) %in% c("sf")) > 0){
    res <- "vector"
  }else if(sum(class(cov) %in% c("RasterLayer")) > 0){
    res <- "raster"
  }
  return(res)
}


#' @title Is the given object a coverage?.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Test if the given object is supported.
#' @param cov      A coverage.
#' @return         A logical.
is_cov <- function(cov){
  res <- FALSE
  if(!is.na(cov_type(cov)))
    res <- TRUE
  return(res)
}
