################################################################################
# UTIL FUNCTIONS FOR REPRODUCING PROCES USING DEEP LEARNING
# alber sanchez alber.ipia@inpe.br
#-------------------------------------------------------------------------------
# Last update 2018-09-13
################################################################################


#' @title Compute PRODES areas.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Compute the area of each of the PRODES' labels
#' 
#' @param pd_polygons An sf object (polygons).
#' @return A tibble.
#' @examples
#' \dontrun{
#' pd_polygons <- sf::st_read("prodes_233_067.shp", stringsAsFactors = FALSE)
#' prodes_compute_area(pd_polygons)
#' }
prodes_compute_area <- function(pd_polygons){
  # Aggregate area for each label
  lab_csum <- pd_polygons %>% 
    dplyr::mutate(tmp_area = units::drop_units(sf::st_area(.))) %>% 
    sf::st_set_geometry(NULL) %>% dplyr::group_by(mainclass, ano) %>% 
    dplyr::summarise(area = sum(tmp_area))
  # Forest area in the reference year (the last & only one)
  for_ref_area <- lab_csum %>% dplyr::filter(mainclass == "FLORESTA")
  stopifnot(nrow(for_ref_area) == 1)
  # Reverse cum_sum of deforestation
  def_csum <- lab_csum %>% dplyr::filter(mainclass == "DESMATAMENTO") %>%
    dplyr::arrange(desc(ano)) %>%
    dplyr::mutate(lbl_area_csum = cumsum(area)) %>% 
    dplyr::arrange(ano)
  # Estimate forest cover by adding the deforested area of former years
  # a_forest(x-1) = a_forest(x) + a_deforest(x)
  for_adj <- dplyr::tibble(mainclass = "FLORESTA", 
                           ano = def_csum[['ano']] - 1, 
                           area =  for_ref_area[["area"]] + 
                             def_csum[["lbl_area_csum"]])
  # Assemble results
  res <- lab_csum %>%
    dplyr::bind_rows(for_adj) %>%
    dplyr::group_by(mainclass, ano) %>%
    dplyr::arrange(.by_group = TRUE)
  return(res)
}






#' @title Rasterize PRODES.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Rasterize a PRODES map
#' 
#' @param ref_path     A length-one character. Path to a PRODES shp
#' @param img_path     A length-one character. Path to a directory
#' @param pyear        A length-one nueric. A PRODES year
#' @param cov_res      A results coverage.
#' @param level_key_pt A list for recoding values for translating portuguesse to english. See named list on help(recode)
#' @param level_key    A list for recoding values. See named list on help(recode)
#' @return A raster object
prodes_rasterize <- function(ref_path, img_path, pyear, cov_res, level_key_pt, 
                             level_key){
  tmp_raster_path <- file.path(img_path, 
                               stringr::str_replace(basename(ref_path), ".shp", 
                                                    stringr::str_c("_", pyear, ".tif")))
  tmp_vector_path <- file.path(img_path, 
                               stringr::str_replace(basename(ref_path), ".shp", 
                                                    stringr::str_c("_", pyear, ".shp")))

  # prepare the vector to rasterize
  tmp_vector <- cov_read(ref_path) %>%
    dplyr::filter(class_name %in% c(class_name_filter, paste0('d', pyear))) %>%
    dplyr::mutate(lab_ref_pt = dplyr::recode(mainclass, !!!level_key_pt)) %>%
    dplyr::mutate(lab_ref = dplyr::recode(lab_ref_pt, !!!level_key)) %>%
    cov_proj(proj4string = cov_srs(cov_res)) %>%
    dplyr::select(lab_ref)

  # rasterize
  # store # raster::dataType(cov_res) <- "FLT8S"
  raster::writeRaster(raster::setValues(cov_res, NA), 
                      filename = tmp_raster_path, overwrite=TRUE)
  suppressWarnings(sf::st_write(tmp_vector, dsn = tmp_vector_path, 
                                delete_dsn = TRUE, quiet = TRUE))
  cov_ref <- gdalUtils::gdal_rasterize(src_datasource = tmp_vector_path,
                                       dst_filename = tmp_raster_path,
                                       a = "lab_ref",
                                       l = stringr::str_replace(basename(tmp_vector_path), ".shp", ""),
                                       output_Raster = TRUE)
  cov_ref <- cov_ref[[1]] # Cast to raster layer

  # clean tmp files
  fdbf <- paste0(substring(tmp_vector_path, 1, nchar(tmp_vector_path)-3), "dbf")
  fprj <- paste0(substring(tmp_vector_path, 1, nchar(tmp_vector_path)-3), "prj")
  fshx <- paste0(substring(tmp_vector_path, 1, nchar(tmp_vector_path)-3), "shx")
  if (file.exists(tmp_vector_path)) file.remove(tmp_vector_path)
  if (file.exists(fdbf)) file.remove(fdbf)
  if (file.exists(fprj)) file.remove(fprj)
  if (file.exists(fshx)) file.remove(fshx)
  return(cov_ref)
}