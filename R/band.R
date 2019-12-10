
#' @title Build band compositions for the given image's files.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Build band compositions for the given image's files.
#'
#' @param band_tb A tibble of files.
#' @param out_dir A lenght-one character. Path to a directory for storing the results.
#' @return       A tibble.
#' @export
compose_bands_s2 <- function(band_tb, out_dir = tempdir()) {
    # TODO: Validate and add to package's data.
    S2_COMPOSITION <-tribble(
        ~id,          ~composition_name,         ~bands,
        "agr",        "Agriculture",             c("B11", "B08", "B02"),
        "atm",        "Atmospheric penetration", c("B12", "B11", "B8A"),
        "fal_ir",     "False color infrared",    c("B08", "B04", "B03"),
        "fal_ur",     "False color urban",       c("B12", "B11", "B04"),
        "hea_veg",    "Helthy vegetation",       c("B08", "B11", "B02"),
        "lan_wat",    "Land/water",              c("B08", "B11", "B04"),
        "nat_cor",    "Natural color",           c("B04", "B03", "B02"),
        "nat_no_atm", "Natural color with atmospheric removel", c("B12", "B08", "B03"),
        "sho_ir",     "Short infrared",          c("B12", "B08", "B04"),
        "veg",        "Vegetation analysis",     c("B11", "B08", "B04")
    )



    prefix <- band_tb %>%
        dplyr::pull(file_path) %>%
        dplyr::first() %>%
        basename() %>%
        tools::file_path_sans_ext()
    S2_COMPOSITION <- S2_COMPOSITION %>%
        dplyr::mutate(out_file = purrr::map_chr(paste0(prefix, '_', .$id, '_'),
                                                tempfile, tmpdir = out_dir,
                                                fileext = ".vrt"))

    vrt_files <- character()
    for (i in seq(nrow(S2_COMPOSITION))) {
        composition_bands <- S2_COMPOSITION %>%
            dplyr::pull(bands) %>%
            magrittr::extract2(i)
        out_file <- S2_COMPOSITION %>%
            dplyr::pull(out_file) %>%
            magrittr::extract2(i)
        vrt_files[i] <- NA
        if (all(composition_bands %in% dplyr::pull(band_tb, band))) {
            vrt_files[i] <- band_tb %>%
                dplyr::filter(band %in% composition_bands) %>%
                dplyr::pull(file_path) %>%
                rev() %>%
                sits.starfm::gdal_build_vrt(out_filename = out_file,
                                            resolution = "average",
                                            resampling = "nearest",
                                            separate = TRUE)
        }
    }

    S2_COMPOSITION %>%
        dplyr::mutate(compositions = vrt_files) %>%
        return()
}

