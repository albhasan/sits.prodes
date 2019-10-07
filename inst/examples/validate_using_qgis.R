library(foreign)

test <- expert_validation %>% slice(100)
qgis <- read.dbf("/home/alber/Desktop/prodes_225063_expert_samples.dbf", as.is = TRUE) %>%
    dplyr::as_tibble()
names(qgis) <- c("Label2014", "Label2017", "rep_prodes_52_dl_smooth3x3")


# check the labels
recode_ls <- list(
    "1" = "deforestation",
    "2"  = "forest"
)

qgis <- qgis %>%
    tidyr::drop_na() %>%
    dplyr::filter(!(Label2014 %in% c("Cloud Cover", "Coud Cover")),
                  !(Label2017 %in% c("Cloud Cover", "Coud Cover"))) %>%
    dplyr::mutate(recoded = dplyr::recode(.$rep_prodes_52_dl_smooth3x3, !!!recode_ls)) %>%
    dplyr::select(-rep_prodes_52_dl_smooth3x3) %>%
    dplyr::rename("rep_prodes_52_dl_smooth3x3" = recoded)

data_f_2014 <- factor(qgis[["Label2014"]], levels = unlist(recode_ls))
data_f_2017 <- factor(qgis[["Label2017"]], levels = unlist(recode_ls))
ref_f  <- factor(qgis[["rep_prodes_52_dl_smooth3x3"]], levels = unlist(recode_ls))
con_mat2014 <- caret::confusionMatrix(data = data_f_2014, reference = ref_f)
con_mat2017 <- caret::confusionMatrix(data = data_f_2017, reference = ref_f)
