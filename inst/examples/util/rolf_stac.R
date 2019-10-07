#### import libraries ####
library(dplyr)
library(magrittr)

#### fetch S3 ####
url <- aws.s3::get_bucket_df("mod13q1-test", max = Inf)$Key
url <- paste0("http://", "mod13q1-test", ".s3.amazonaws.com/", url)
files_name <- gsub("^.*/(.+)$", "\\1", url)
products   <- gsub("^(.{7}).*$", "\\1", files_name)
tiles      <- gsub("^.{8}(.{6}).*(_[0-9]+_[0-9]+)_[0-9]+\\.tif$", "\\1", files_name)
collection <- gsub("^.{15}(.{3}).*$", "\\1", files_name)
bands      <- gsub("^.*(250m.*)(_[0-9]+){3}\\.tif$", replacement = "\\1", x = files_name)
dates      <- as.integer(gsub("^.*_([0-9]+)\\.tif$", replacement = "\\1", x = files_name))
dates      <- sits::timeline_2000_2017[dates]

#### fetch localhost ####
url  <- list.files(path = "~/Downloads/MODIS/geotif", pattern = "*\\.tif", full.names = TRUE)
files_name <- gsub("^.*/(.+)$", "\\1", url)
products   <- gsub("^(.{7}).*$", "\\1", files_name)
tiles      <- gsub("^.{8}(.{6}).*(_[0-9]+_[0-9]+)_.*\\.tif$", "\\1\\2", files_name)
collection <- gsub("^.{15}(.{3}).*$", "\\1", files_name)
bands      <- gsub("^.*(250m.*).tif$", replacement = "\\1", x = files_name)
dates      <- as.character(as.Date(gsub("^.*A([0-9]{7}).*$", replacement = "\\1", x = files_name), format = "%Y%j"))
rm(files_name)


#### metadata functions ####
.file.open <- function(file) {

    if (grepl("^https?://.+", file)) {

        file = paste0("/vsicurl/", file)
    }

    brick <- tryCatch(raster::brick(file, values = FALSE), error = function(e) NULL)

    if (is.null(brick)) {

        stop(sprintf("Error while opening brick '%s'.\nFile is unreachable.", file))
    }

    return(brick)
}

.file.metadata <- function(file) {

    brick <- .file.open(file = file)

    extent <- raster::extent(brick)

    result <-
        tibble::tibble(
            nrow      = raster::nrow(brick),
            ncol      = raster::ncol(brick),
            time_len  = raster::nlayers(brick),
            crs       = as.character(raster::crs(brick)),
            xmin      = extent@xmin,
            xmax      = extent@xmax,
            ymin      = extent@ymin,
            ymax      = extent@ymax,
            xres      = raster::xres(brick),
            yres      = raster::yres(brick),
            data_type = as.character(raster::dataType(brick)))

    return(result)
}

.files.metadata <- function(files) {

    progress_bar <- length(files) >= 10

    if (progress_bar) {

        pb <- utils::txtProgressBar(min = 0, max = length(files), style = 3)
    }

    metadata <- lapply(
        files, function(file) {
            result <- .file.metadata(file = file)

            if (progress_bar) {
                utils::setTxtProgressBar(pb = pb, value = utils::getTxtProgressBar(pb) + 1)
            }

            return(result)
        })

    if (progress_bar) {
        close(pb)
    }

    metadata <- do.call(rbind, metadata)

    metadata <- tibble::as_tibble(metadata)

    return(metadata)
}

#### fetch metadata ####
rasters <-
    dplyr::tibble(product = products,
                  tile = tiles, collection = collection,
                  href = url, bands = bands, date = dates,
                  type = "image/tiff")

system.time(meta_data <- .files.metadata(rasters$href))

rasters <- dplyr::bind_cols(list(rasters, meta_data))

rasters <-
    EOCubes::MOD13Q1_bands %>%
    dplyr::mutate(band_long_name = paste0("250m_16_days_", band_long_name)) %>%
    dplyr::right_join(rasters, by = c("band_long_name" = "bands"))

rasters$keywords <- list(c("MOD13Q1", "Test Sites"))

#### cubes functions ####
cub_list <- function(. = NULL, .by = NULL, .index = FALSE, ...) {

    if (missing(.))
        return(list(...))

    dots <- substitute(list(...))[-1:0]
    .by <- substitute(.by)

    if (!is.data.frame(.))
        stop("You must inform a data frame as data input.")

    .by_values <- eval(.by, envir = ., enclos = environment())

    if (length(.by_values) == 0)
        .by_values <- rep(1, len = nrow(.))

    partitions <- by(., tapply(seq_len(length(.by_values)), .by_values), list)

    res <- lapply(partitions, function(.) {

        lapply(dots, function(expr) {
            eval(expr, envir = ., enclos = environment())
        })
    })

    names(res) <- NULL

    if (.index) {

        names(res) <- sapply(partitions, function(.) {

            eval(.by, envir = .[1,], enclos = environment())
        }, USE.NAMES = FALSE)
    }

    return(res)
}

get_spatial_crs <- function(crs) {

    list(type = "name",
         properties = list(name = crs))
}

get_spatial_extent <- function(.,
                               .xmin = "xmin",
                               .ymin = "ymin",
                               .xmax = "xmax",
                               .ymax = "ymax") {

    list(type = "Feature",
         bbox = c(min(.[[.xmin]]),
                  min(.[[.ymin]]),
                  max(.[[.xmax]]),
                  max(.[[.ymax]])),
         geometry = list(type = "Polygon",
                         coordinates = list(list(c(min(.[[.xmin]]),
                                              min(.[[.ymin]])),
                                            c(min(.[[.xmin]]),
                                              min(.[[.ymax]])),
                                            c(min(.[[.xmax]]),
                                              min(.[[.ymax]])),
                                            c(min(.[[.xmax]]),
                                              min(.[[.ymin]])),
                                            c(min(.[[.xmin]]),
                                              min(.[[.ymin]]))))))
}

get_temporal_extent <- function(., .date = "date") {

    .date <- substitute(.date)
    if (is.name(.date))
        .date <- deparse(.date)

    c(min(.[[.date]]), max(.[[.date]]))
}

cub_unique <- function(x) {

    res <- unique(x)
    if (length(res) > 1)
        stop(sprintf(paste("Field '%s's values have not unique values.",
                           "Values: %s\n", sep = "\n"),
                     deparse(substitute(x)), paste(res, collapse = ", ")), call. = FALSE)

    return(res)
}

cub_node <- function(., .name = NULL, ...) {

    dots <- substitute(list(...))[-1:0]
    .name <- substitute(.name)

    partitions <- .

    res <- lapply(partitions, function(.) {

        lapply(dots, function(expr) {
            eval(expr, envir = ., enclos = environment())
        })
    })

    if (!is.null(.name)) {

        names(res) <- sapply(partitions, function(.) {

            eval(.name, envir = ., enclos = environment())
        }, USE.NAMES = FALSE)
    }

    return(res)
}

cub_write_json <- function(x, project_path) {

    id <- x$id

    if (is.null(id))
        id <- "/"

    project_path <- path.expand(project_path)
    path <- paste(dirname(project_path), basename(project_path), id, sep = "/")

    if (!dir.exists(path))
        dir.create(path, showWarnings = FALSE, recursive = TRUE)

    file_path <- paste(path, "catalog.json", sep = "/")

    jsonlite::write_json(x, path = file_path, pretty = TRUE, auto_unbox = TRUE)

    return(file_path)
}

cub_remote <- function(remotes = NULL, name, base, curators, keywords) {

    res <- dplyr::tibble(name = name, base = base, curators = curators, keywords = list(unlist(keywords)))

    if (!is.null(remotes))
        res <- dplyr::bind_rows(list(remotes, res))

    return(res)
}

cub_curator <- function(curators = NULL, name, email) {

    res <- list(list(list(name = name, email = email)))

    if (!is.null(curators))
        res <- list(append(curators[[1]], res[[1]]))

    return(res)
}

#### make cubes ####

#### >localhost ####
project_folder <- "~/Documents/EOCubes/localhost"
published_location <- "~/Documents/EOCubes/localhost" # "http://eocubes-test.s3.amazonaws.com"

# complete definition
cube_def <-
    rasters %>%
    cub_list(.by = paste(product, collection, sep = "/"), .index = TRUE,
             id = cub_unique(paste(product, collection, sep = "/")),
             meta = list(crs = get_spatial_crs(cub_unique(crs)),
                         bands = cub_list(., .by = band_short_name, .index = TRUE,
                                          min = cub_unique(min),
                                          max = cub_unique(max),
                                          fill = cub_unique(fill),
                                          scale = cub_unique(scale),
                                          name = cub_unique(band_long_name))),
             tiles = cub_list(., .by = tile, .index = TRUE,
                              id = cub_unique(paste(product, collection, tile, sep = "/")),
                              extent = get_spatial_extent(.),
                              meta = list(cube = cub_unique(paste(published_location, product, collection, "catalog.json", sep = "/"))),
                              bands = cub_list(., .by = band_short_name, .index = TRUE,
                                               layers = cub_list(., .by = date, .index = FALSE,
                                                                 href = cub_unique(href),
                                                                 date = cub_unique(date)))))

# cube
cube_lst <-
    cube_def %>%
    cub_node(.,
             id = id,
             version = "0.6",
             description = "",
             keywords = c("USGS/NASA", "MOD13Q1", "Brazil"),
             meta = meta,
             tiles = cub_node(.$tiles,
                              href = paste(published_location, id, "catalog.json", sep = "/"),
                              extent = extent))

cube_lst %>%
    lapply(cub_write_json, project_folder)

# tiles
tiles_lst <- unname(cub_node(cube_def[[1]]$tiles,
                             id = id,
                             extent = extent,
                             meta = meta,
                             bands = cub_node(.$bands,
                                              layers = layers)))

tiles_lst %>%
    lapply(cub_write_json, project_folder)

#### make remotes ####

#### >eocubes ####
eocubes_lst <-
    cub_remote(name = "eocubes",
               base = "http://eocubes-test.s3.amazonaws.com",
               curators = cub_curator(name = "Rolf Simoes", email = "<rolf.simoes@inpe.br>") %>%
                   cub_curator(name = "Ricardo Cartaxo", email = "<ricardo.cartaxo@inpe.br>") %>%
                   cub_curator(name = "Alber Sanchez", email = "<alber.sanchez@inpe.br>"),
               keywords = c("INPE", "EOCubes", "R Package")) %>%
    cub_list(curators = unlist(curators, recursive = FALSE),
             keywords = unlist(keywords, recursive = FALSE),
             cubes = cub_node(list(), .name = id,
                              description = description,
                              href = paste(published_location, id, "catalog.json", sep = "/")))

eocubes_lst %>%
    lapply(cub_write_json, project_folder)

#### >localhost ####
localhost_lst <-
    cub_remote(name = "localhost",
               base = project_folder,
               curators = cub_curator(name = Sys.info()[["user"]], email = ""),
               keywords = c("EOCubes", "Local")) %>%
    cub_list(version = "0.6",
             description = "Local maintained cubes",
             keywords = unlist(keywords, recursive = FALSE),
             curators = unlist(curators, recursive = FALSE),
             cubes = cub_node(cube_lst, .name = id,
                              description = description,
                              keywords = keywords,
                              href = paste(published_location, id, "catalog.json", sep = "/")))

localhost_lst %>%
    lapply(cub_write_json, project_folder)

localhost_lst %>%
    jsonlite::toJSON(pretty = T)
##### >root #####

root_lst <-
    cub_list(version = "0.6",
             default = "localhost",
             remotes = cub_node(localhost_lst,
                                .name = "localhost",
                                description = description,
                                keywords = unlist(keywords),
                                href = cub_unique(paste(project_folder, "catalog.json", sep = "/"))))

root_lst %>%
    jsonlite::toJSON(pretty = T, auto_unbox = T)

root_lst %>% jsonlite::write_json(path = "~/.EOCubes/remotes.json", pretty = T)

# publish on AWS S3
input_folder <- path.expand(paste(project_folder, cube_name, sep = "/"))
files <- list.files(path = input_folder, recursive = T, full.names = T)
files_output <- gsub(sprintf("^%s/(.*)$", input_folder), paste0(cube_name, "/\\1"), files)
files <- c(path.expand(paste(project_folder, "catalog.json", sep = "/")), files)
files_output <- c("catalog.json", files_output)
sapply(seq_along(files), function(i) aws.s3::put_object(files[i], files_output[i], "eocubes-test", acl = "public-read"))

#### EOCubes use ####
mod13q1_006 <- cube("MOD13Q1/006")
cube_crs(mod13q1_006)
cube_bands(mod13q1_006)
cube_tiles(mod13q1_006)
city_shp <- sf::read_sf("~/Downloads/test.shp")
cube_tiles(mod13q1_006, which = tiles_intersects(mod13q1_006, city_shp))
h12v10_cube <- filter_tiles(mod13q1_006, which = tiles_intersects(mod13q1_006, city_shp))
test <- make_stacks(h12v10_cube,
                    bands = c("ndvi", "evi"),
                    start_dates = seq(as.Date("2000-01-01"), as.Date("2018-01-01"), by = "years"),
                    end_dates = seq(as.Date("2001-01-01"), as.Date("2019-01-01"), by = "years"),
                    count = 23)
