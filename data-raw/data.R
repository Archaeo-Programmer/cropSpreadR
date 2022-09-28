library(magrittr)
library(cropDiffusionR)

# Elevation (in meters) extracted from digital elevation model for samples in maize database.
# Get maize database and locations.
MDB_tmp <- cropDiffusionR::maizeDB %>%
  dplyr::left_join(
    .,
    readr::read_csv(stringr::str_remove(here::here(
      "Paper 2/Maize_Database_Locations_DONT_DISTRIBUTE.csv"), "cropSpreadR/"),
      col_types = readr::cols()
    ),
    by = "LabID"
  ) %>%
  dplyr::select(LabID, Long, Lat, Country, Elevation_masl) %>%
  sf::st_as_sf(coords = c("Long", "Lat"), crs = 4326)

# Get elevation at site locations for USA sites.
MDB_SWUS <- MDB_tmp %>%
  dplyr::filter(Country == "USA") %>%
  dplyr::select(-Country)

# Read in National Elevation dataset.
NED <-
  raster::raster("/Volumes/DATA/NED/EXTRACTIONS/UUSS_NED_1.tif")

# Set projection
raster::crs(NED) <- "EPSG:4326"

MDB_SWUS_elev <- data.frame(MDB_SWUS, elevation = terra::extract(x = NED, y = MDB_SWUS))

# Get elevation at site locations for Mexico sites.
MDB_Mexico <- MDB_tmp %>%
  dplyr::filter(Country == "Mexico") %>%
  dplyr::select(-Country)

MDB_Mexico_elev <- elevatr::get_elev_point(MDB_Mexico, prj = "EPSG:4326", src = "aws", z = 10, override_size_check = TRUE)

MDB_Mexico_elev <- dplyr::left_join(MDB_Mexico %>% sf::st_drop_geometry(), MDB_Mexico_elev %>% sf::st_drop_geometry(), by = "LabID") %>%
  dplyr::select(elev_units)

MDB_elev <- dplyr::bind_rows(MDB_SWUS_elev, MDB_Mexico_elev)

usethis::use_data(MDB_elev,
                  overwrite = TRUE)



# Get annual accumulated GDD.


