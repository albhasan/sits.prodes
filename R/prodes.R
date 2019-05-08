################################################################################
# UTIL FUNCTIONS FOR REPRODUCING PROCES USING DEEP LEARNING
# alber sanchez alber.ipia@inpe.br
#-------------------------------------------------------------------------------
# Last update 2018-09-13
################################################################################


#' @title Match PRODES and MAPBIOMAS. 
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Rasterize a PRODES vector map to match a reference raster resolution.
#' 
#' @param file_pd    A length-one character. Path to a PRODES map (a shapefile).
#' @param file_rt    A length-one character. Path to raster of reference (a tif).
#' @param raster_path  A length-one character. An optional file path to store the resulting raster.
#' @param tile       A length-one character. The Landsat scene id (6 numbers).
#' @param year_pd    A integer. The years of deforestation to keep in the output.
#' @param prodes_lbl A tibble mapping the labels of PRODES from Portuguesse to English. It must contais the PRODES' labels in Portuguese (label_ld_pt, character) and english (label_pd, character), and its ID (id_pd, an integer that must have a one-to-one relationship to label_id).
#' @return           A length-one character. The path to a raster file. 
prodes2raster <- function(file_pd, file_rt, raster_path, tile, year_pd, prodes_lbl){
    fname <- tools::file_path_sans_ext(basename(file_pd))

    # prepare labels for recoding and reclassification
    unique_label_pt <- prodes_lbl %>% dplyr::group_by(label_pd_pt) %>%
        dplyr::slice(1) %>% dplyr::ungroup()
    unique_id_pd    <- prodes_lbl %>% dplyr::group_by(id_pd) %>%
        dplyr::slice(1) %>% dplyr::ungroup()
    key_label_pt <- unique_label_pt %>% dplyr::pull(label_pd) %>% as.list()
    key_id_pd    <- unique_id_pd    %>% dplyr::pull(id_pd)    %>% as.list()
    names(key_label_pt) <- unique_label_pt %>% dplyr::pull(label_pd_pt)
    names(key_id_pd) <-    unique_id_pd    %>% dplyr::pull(label_pd)
 
    # rasterization 
    mb_raster <- raster::raster(file_rt) 
    sf::st_read(dsn = dirname(file_pd), layer = fname, 
                stringsAsFactors = FALSE, quiet = TRUE) %>%
        dplyr::mutate(label    = dplyr::recode(mainclass, !!!key_label_pt),
                      label_id = dplyr::recode(label,     !!!key_id_pd)) %>%
        dplyr::filter(class_name %in% c(names(key_label_pt),
                      apply(expand.grid(c('d', 'r'), year_pd), 1, paste0, 
                      collapse = ''))) %>%
        dplyr::select(label_id) %>%
        vector2raster(raster_r = mb_raster,
                      vector_field = "label_id",
                      raster_path = raster_path) %>%
        attr("file") %>% attr("name") %>%
        return()
}


#' @title Convert the PRODES' scene format to LANDSAT's.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Insert missing 0s in PRODES' landsat scene id.
#' 
#' @param prodes_scene A length-one character. A PRODES scene id.
#' @return             A length-one character. A Landsat scene id. 
prodes2scene <- function(prodes_scene){
    gsub('^([0-9]{3})([0-9]+)$', '\\10\\2', prodes_scene)
}

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
#' @param pyear        A numeric. A PRODES year
#' @param cov_res      A results coverage.
#' @param level_key_pt A list for recoding values for translating portuguesse to english. See named list on help(recode)
#' @param level_key    A list for recoding values. See named list on help(recode)
#' @param class_name_filter A character. Constrain rasterization to these classes (labels).
#' @return A raster object
prodes_rasterize <- function(ref_path, pyear, cov_res, level_key_pt, 
                             level_key, class_name_filter = "FLORESTA"){
    .Deprecated("prodes2raster")
    stopifnot(length(ref_path) == 1)

    tmp_raster_path <- file.path(paste0(tempfile(pattern = tools::file_path_sans_ext(basename(ref_path))), ".tif"))
    tmp_vector_path <- file.path(paste0(tempfile(pattern = tools::file_path_sans_ext(basename(ref_path))), ".shp"))

    # prepare the vector to rasterize
    tmp_vector <- ref_path %>% cov_read() %>%
        dplyr::filter(class_name %in% c(class_name_filter, paste0('d', pyear))) %>%
        dplyr::mutate(lab_ref_pt = dplyr::recode(mainclass, !!!level_key_pt)) %>%
        dplyr::mutate(lab_ref = dplyr::recode(lab_ref_pt, !!!level_key)) %>%
        cov_proj(proj4string = cov_srs(cov_res)) %>%
        dplyr::select(lab_ref) %>%
        dplyr::mutate(lab_ref = as.integer(lab_ref))

    # rasterize
    # store # raster::dataType(cov_res) <- "FLT8S"
    raster::writeRaster(raster::setValues(cov_res, NA), 
                        filename = tmp_raster_path, overwrite = TRUE)
    # https://lists.osgeo.org/pipermail/gdal-dev/2006-March/008130.html
    suppressWarnings(
        sf::st_write(tmp_vector, dsn = tmp_vector_path, 
                     delete_dsn = TRUE, quiet = TRUE, delete_layer = TRUE)
    )
    cov_ref <- gdalUtils::gdal_rasterize(src_datasource = tmp_vector_path,
                                         dst_filename = tmp_raster_path,
                                         a = "lab_ref",
                                         l = stringr::str_replace(basename(tmp_vector_path), ".shp", ""),
                                         output_Raster = TRUE) %>%
        .[[1]] # Cast to raster layer

    # clean tmp files
    if (file.exists(tmp_vector_path)) 
        res <- file.remove(tmp_vector_path)
    res <- lapply(c("dbf", "prj", "shx"), function(ext){
        f <- paste0(substring(tmp_vector_path, 1, nchar(tmp_vector_path)-3), ext)
        if(file.exists(f))
            file.remove(f)
    })

    return(cov_ref)
}
