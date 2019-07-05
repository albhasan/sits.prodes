
# Check if the available bricks cover the scenes and years of a classification.

suppressMessages(library(dplyr))
library(sits.prodes)

# Check that the files meet the scenes and prodes-year requirements.
in_dir <- c("/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_interp",
            "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_mask_cloud",
            "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_raw",
            "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_simple",
            "/home/alber/Documents/data/experiments/prodes_reproduction/data/raster/brick_starfm") %>%
    ensurer::ensure_that(all(dir.exists(.)))

expected_scenes <- c("225063", "226064", "233067")
expected_years <- 2014:2016 %>% as.character()
expected_bands <- c("blue", "cloud", "dark", "ndvi", "nir", "red",
                    "substrate", "swir2", "vegetation")

res <- lapply(in_dir, identify_missing_bricks, expected_scenes, expected_years, expected_bands)
names(res) <- basename(in_dir)
