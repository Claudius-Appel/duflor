## code to prepare `DATASET` dataset goes here

#### Setup functions ####
load_extdata <- function(path = NULL) {
    if (is.null(path)) {
        dir(system.file("extdata", package = "duflor"),full.names = T)
    } else {
        system.file("extdata", path, package = "duflor", mustWork = TRUE)
    }
}
#### Setup various values ####
spectrums <- getOption("duflor.default_hsv_spectrums")
#### GFA test images ####
gfa_test_path_cropped <- load_extdata("plant_cropped.jpg") # load images from bundled raw data files
gfa_test_path <- load_extdata("plant.jpg") # load extdata-images
gfa_test_plant_cropped <- duflor::load_image(gfa_test_path_cropped)
gfa_test_plant <- duflor::load_image(gfa_test_path)
#### WFA test images ####
wfa_test_path_cropped <- load_extdata("root_cropped.jpg") # load extdata-images
wfa_test_path <- load_extdata("root.jpg") # load extdata-images
wfa_test_plant_cropped <- duflor::load_image(wfa_test_path_cropped)
wfa_test_plant <- duflor::load_image(wfa_test_path)
#### retrieve HSV-matches ####
# gfa cropped
gfa_test_plant_cropped_HSV_matches <- duflor::extract_pixels_HSV(
    pixel.array = gfa_test_plant_cropped,
    lower_bound = spectrums$lower_bound,
    upper_bound = spectrums$upper_bound
)
# gfa noncropped
gfa_test_plant_HSV_matches <- duflor::extract_pixels_HSV(
    pixel.array = gfa_test_plant,
    lower_bound = spectrums$lower_bound,
    upper_bound = spectrums$upper_bound
)
# wfa cropped
wfa_test_plant_cropped_HSV_matches <- duflor::extract_pixels_HSV(
    pixel.array = wfa_test_plant_cropped,
    lower_bound = spectrums$lower_bound,
    upper_bound = spectrums$upper_bound
)
# wfa noncropped
wfa_test_plant_HSV_matches <- duflor::extract_pixels_HSV(
    pixel.array = wfa_test_plant,
    lower_bound = spectrums$lower_bound,
    upper_bound = spectrums$upper_bound
)
#### Save to internal rda file
usethis::use_data(
    gfa_test_path,
    wfa_test_path,
    gfa_test_plant_cropped,
    wfa_test_plant_cropped,
    # gfa_test_plant_HSV_matches,
    # wfa_test_plant_HSV_matches,
    # gfa_test_plant_cropped_HSV_matches,
    # wfa_test_plant_cropped_HSV_matches,

    internal = TRUE,overwrite = T)
