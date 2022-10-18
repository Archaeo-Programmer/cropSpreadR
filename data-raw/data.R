library(magrittr)
library(cropDiffusionR)

# Elevation (in meters) extracted from digital elevation model for samples in maize database.
# Get maize database and locations.
MDB_tmp <- cropDiffusionR::maizeDB %>%
  dplyr::left_join(.,
                   readr::read_csv(
                     stringr::str_remove(
                       here::here("Paper 2/Maize_Database_Locations_DONT_DISTRIBUTE.csv"),
                       "cropSpreadR/"
                     ),
                     col_types = readr::cols()
                   ),
                   by = "LabID") %>%
  dplyr::select(LabID, Long, Lat, Country, Elevation_masl) %>%
  sf::st_as_sf(coords = c("Long", "Lat"), crs = 4326)

# Get elevation at site locations for USA sites.
MDB_SWUS <- MDB_tmp %>%
  dplyr::filter(Country == "USA") %>%
  dplyr::select(-Country)

# Read in National Elevation dataset.
swus_NED1 <- raster::raster(stringr::str_remove(here::here("datasets/UUSS_NED_1.tif"),
                                               "cropSpreadR/"))

# Set projection
raster::crs(swus_NED1) <- "EPSG:4326"

# Extract elevation data for each site.
MDB_SWUS_elev <-
  data.frame(MDB_SWUS, elevation = terra::extract(x = swus_NED1, y = MDB_SWUS))

# Get elevation at site locations for Mexico sites.
MDB_Mexico <- MDB_tmp %>%
  dplyr::filter(Country == "Mexico") %>%
  dplyr::select(-Country)

MDB_Mexico_elev <-
  elevatr::get_elev_point(
    MDB_Mexico,
    prj = "EPSG:4326",
    src = "aws",
    z = 10,
    override_size_check = TRUE
  )

MDB_Mexico_elev <-
  dplyr::left_join(
    MDB_Mexico %>% sf::st_drop_geometry(),
    MDB_Mexico_elev %>% sf::st_drop_geometry(),
    by = "LabID"
  ) %>%
  dplyr::select(elev_units)

MDB_elev <- dplyr::bind_rows(MDB_SWUS_elev, MDB_Mexico_elev)

usethis::use_data(MDB_elev,
                  overwrite = TRUE)

# Save the transformed NED data to save time in plotting.
# Decrease spatial resolution for plotting.
# swus_NED <- raster::aggregate(swus_NED1, fact = 11)
# Crop and mask to fit the Four Corner States
# swus_NED <-
#   raster::crop(swus_NED, cropDiffusionR::swus_states)
# swus_NED <-
#   raster::mask(swus_NED, cropDiffusionR::swus_states)
# Write this raster to data-raw, then can just read in.
# raster::writeRaster(swus_NED, filename = here::here("data-raw/swus_NED.tif"))

swus_NED <- raster::raster(here::here("data-raw/swus_NED.tif"))

usethis::use_data(swus_NED,
                  overwrite = TRUE)

# Get annual accumulated GDD.
# Calculate average days per month during 1970-2000.
month_days <-
  tibble::tibble(date = seq(
    lubridate::as_date("1970-01-01"),
    lubridate::as_date("2000-12-31"),
    "1 day"
  )) %>%
  dplyr::mutate(year = lubridate::year(date),
                month = lubridate::month(date)) %>%
  dplyr::group_by(year, month) %>%
  dplyr::count() %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(`days` = mean(n)) %>%
  dplyr::pull(days)

# Get PRISM monthly gdd (takes a few minutes to run)
PRISM_gdd_monthly <-
  # Need tmin and tmax to calculate the tavg
  c(tmin = "tmin", tmax = "tmax") %>%
  purrr::map(function(var) {
    # Read and stack rasters for 1970 to 2000.
    out_rast <-
      list.files(
        paste0("/Volumes/DATA/PRISM/EXTRACTIONS/SKOPE_4CORNERS/", var),
        full.names = TRUE,
        pattern = "(?:197[0-9]|19[8-9][0-9]|2000)"
      ) %>%
      raster::stack(quick = TRUE) %>%
      raster::readAll()

    # Create a z variable called months
    months <- names(out_rast) %>%
      stringr::str_extract("M(.*)$")

    # Take the mean of the raster layers by month and return
    out_rast %>%
      raster::setZ(months,
                   name = "months") %>%
      raster::zApply(by = months,
                     fun = mean,
                     name = 'months') %>%
      magrittr::divide_by(10)

  }) %>%
  # Get GDD for each month.
  purrr::reduce(
    .f = function(x, y) {
      paleomat::calc_gdd(
        tmin = x,
        tmax = y,
        t.base = 10,
        t.cap = 30
      )
    }
  ) %>%
  # Multiply by the number of days in a month to get the total accumulated GDD for each month.
  {
    . * month_days
  }

# Sum all 12 months.
PRISM_annual_gdd <- sum(PRISM_gdd_monthly, na.rm = FALSE)

usethis::use_data(PRISM_annual_gdd,
                  overwrite = TRUE)


# Get growing season GDD.
# Get annual accumulated GDD.
# Calculate average days per month during 1970-2000 for May to September.
month_days_GS <-
  tibble::tibble(date = seq(
    lubridate::as_date("1970-01-01"),
    lubridate::as_date("2000-12-31"),
    "1 day"
  )) %>%
  dplyr::mutate(year = lubridate::year(date),
                month = lubridate::month(date)) %>%
  dplyr::filter(month %in% 5:9) %>%
  dplyr::group_by(year, month) %>%
  dplyr::count() %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(`days` = mean(n)) %>%
  dplyr::pull(days)

# Get PRISM monthly gdd for May to September (takes a few minutes to run)
PRISM_gdd_monthly_GS <-
  # Need tmin and tmax to calculate the tavg
  c(tmin = "tmin", tmax = "tmax") %>%
  purrr::map(function(var) {
    # Read and stack rasters for 1970 to 2000.
    out_rast <-
      list.files(
        paste0("/Volumes/DATA/PRISM/EXTRACTIONS/SKOPE_4CORNERS/", var),
        full.names = TRUE,
        pattern = "^Y(19[7-9]|2000).*M0[5-9]\\b"
      ) %>%
      raster::stack(quick = TRUE) %>%
      raster::readAll()

    # Create a z variable called months
    months <- names(out_rast) %>%
      stringr::str_extract("M(.*)$")

    # Take the mean of the raster layers by month and return
    out_rast %>%
      raster::setZ(months,
                   name = "months") %>%
      raster::zApply(by = months,
                     fun = mean,
                     name = 'months') %>%
      magrittr::divide_by(10)

  }) %>%
  # Get GDD for each month.
  purrr::reduce(
    .f = function(x, y) {
      paleomat::calc_gdd(
        tmin = x,
        tmax = y,
        t.base = 10,
        t.cap = 30
      )
    }
  ) %>%
  # Multiply by the number of days in a month to get the total accumulated GDD for each month.
  {
    . * month_days_GS
  }

# Sum together the 5 months of the growing season.
PRISM_gs_gdd <- sum(PRISM_gdd_monthly_GS, na.rm = FALSE)

usethis::use_data(PRISM_gs_gdd,
                  overwrite = TRUE)


# Get counties shapefiles for the SWUS.
SWUS_counties <-
  sf::st_as_sf(
    maps::map(
      "county",
      fill = TRUE,
      plot = FALSE,
      regions = "colorado|utah|arizona|new mexico"
    )
  ) %>%
  sf::st_transform(crs = 4326) %>%
  tidyr::separate(ID, into = c("REGION", "NAME"), sep = ",") %>%
  dplyr::filter(REGION %in% c("colorado", "utah", "arizona", "new mexico")) %>%
  sf::st_make_valid() %>%
  dplyr::mutate(
    county_st = paste(NAME, REGION, sep = ", "),
    centroid = purrr::map(geom, ~ sf::st_centroid(.x))
  ) %>%
  tidyr::separate(centroid,
                  into = c("centroid_long", "centroid_lat"),
                  sep = ", ") %>%
  dplyr::mutate(dplyr::across(dplyr::starts_with("centroid"), ~ readr::parse_number(.x)))

usethis::use_data(SWUS_counties,
                  overwrite = TRUE)


# Get all four corners radiocarbon data, and then calculate county SPDs.
sf::sf_use_s2(FALSE)

# Get maize database and add location data.
MDB <- cropDiffusionR::maizeDB %>%
  dplyr::left_join(.,
                   readr::read_csv(
                     stringr::str_remove(
                       here::here("Paper 2/Maize_Database_Locations_DONT_DISTRIBUTE.csv"),
                       "cropSpreadR/"
                     ),
                     col_types = readr::cols()
                   ),
                   by = "LabID") %>%
  dplyr::mutate(Long1 = Long, Lat1 = Lat) %>%
  # Convert to spatial object
  sf::st_as_sf(coords = c("Long1", "Lat1"),
               crs = 4326) %>%
  sf::st_make_valid() %>%
  # Join to counties data to get county and state information.
  sf::st_join(SWUS_counties) %>%
  dplyr::mutate(county_st = ifelse(!is.na(REGION) &
                                     !is.na(NAME), paste(NAME, REGION, sep = ", "), NA)) %>%
  sf::st_drop_geometry() %>%
  suppressMessages()

# Get p3k14c data and filter out sites that do not have location data. We are able to provide locations for a few additional sites.
bad_loc <- p3k14c::p3k14c_data %>%
  dplyr::filter(Province %in% c("Arizona", "New Mexico", "Colorado", "Utah")) %>%
  dplyr::filter(LocAccuracy == 0 & !is.na(SiteName)) %>%
  dplyr::select(-c(Long, Lat)) %>%
  # Add sites with locations from the maize database to the p3k14c data.
  dplyr::left_join(
    .,
    MDB %>% dplyr::select(SiteName, Long, Lat) %>% dplyr::distinct(SiteName, .keep_all = TRUE),
    by = "SiteName"
  ) %>%
  # Remove any sites that do not have locations.
  dplyr::filter(!is.na(Long))

incomplete_SiteID <- toupper(c("42BEà", "42SA", "5LPà", "LAà", "42à", "42EMà", "42MDà", "42UNà", "42WNà", "5STà", "BB:6:à", "X:12:2à", "BB:", "AA:", "EE:", paste0(LETTERS, ":")))

# Get all four corners data.
sw_rc <- p3k14c::p3k14c_data %>%
  dplyr::filter(Province %in% c("Arizona", "New Mexico", "Colorado", "Utah")) %>%
  # Remove sites with no location information.
  dplyr::filter(LocAccuracy > 0) %>%
  # Add sites/samples back in that we provided location data from the maize database.
  dplyr::bind_rows(., bad_loc) %>%
  # Keep only distinct from LabID.
  dplyr::distinct(LabID, .keep_all = TRUE) %>%
  # Establish the calibration curve to use and covert d13C to numeric.
  dplyr::mutate (calib = "intcal20",
                 d13C = as.numeric(d13C),
                 db = "p3k14c") %>%
  dplyr::bind_rows(
    MDB %>% dplyr::select(dplyr::intersect(names(
      p3k14c::p3k14c_data
    ), names(MDB))) %>% dplyr::mutate(db = "mine", calib = "intcal20"),
    .
  ) %>%
  dplyr::group_by(LabID) %>%
  dplyr::arrange(LabID, db) %>%
  dplyr::slice(1) %>%
  dplyr::mutate(
    # Convert SiteIDs to uppercase for consistency.
    SiteID = trimws(toupper(SiteID)),
    # Some siteIDs are incomplete/incorrect, so replace with NA.
    SiteID = ifelse(SiteID %in% incomplete_SiteID, NA_character_, SiteID),
    # Remove the parentheses from many of the Arizona sample site IDs.
    SiteID = trimws(stringr::str_replace(SiteID, " \\s*\\([^\\)]+\\)", "")),
    # Also, remove the starting AZ on some of the Arizona sample site IDs.
    SiteID = stringr::str_replace(SiteID, "^(AZ )", "")
  ) %>%
  # Remove the leading 0s on some siteIDs so that these are consistent.
  # First, we separate the beginning part of the SiteID from the ending number.
  tidyr::separate(
    SiteID,
    into = c("SiteID1", "SiteID2"),
    "(?<=[[:alpha:]][[:alpha:]])",
    remove = FALSE
  ) %>%
  # Then, convert to numeric to remove any leading 0s. This does not apply to Arizona sites, since the numbering systems are different.
  dplyr::mutate(
    SiteID2 = as.numeric(SiteID2),
    SiteID = ifelse(
      Province != "Arizona" &
        SiteID2 > 0 &
        !SiteName %in% c("CKL-1-190810-8-1", "PY0810-1-1", "LA 18091"),
      paste0(SiteID1, SiteID2),
      SiteID
    )
  ) %>%
  dplyr::select(-c(SiteID1, SiteID2)) %>%
  # Fill in any missing site IDs by SiteName.
  dplyr::group_by(SiteName) %>%
  tidyr::fill(SiteID, .direction = "updown") %>%
  # Fill in any missing site names by SiteID.
  dplyr::group_by(SiteID) %>%
  tidyr::fill(SiteName, .direction = "updown") %>%
  dplyr::mutate(SiteIDName = dplyr::coalesce(SiteID, SiteName)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Province) %>%
  dplyr::arrange(Province, !is.na(SiteIDName)) %>%
  dplyr::mutate(SiteIDName = ifelse(is.na(SiteIDName), paste(Province, dplyr::row_number()), SiteIDName)) %>%
  # Convert to spatial object
  sf::st_as_sf(coords = c("Long", "Lat"),
               crs = 4326) %>%
  sf::st_make_valid() %>%
  # Join to counties data to get county and state information.
  sf::st_join(SWUS_counties) %>%
  dplyr::mutate(county_st = paste(NAME, REGION, sep = ", ")) %>%
  dplyr::select(LabID,
                SiteID,
                SiteName,
                SiteIDName,
                county_st,
                NAME,
                REGION,
                Taxa,
                Age,
                Error,
                calib) %>%
  sf::st_drop_geometry() %>%
  suppressMessages()

usethis::use_data(sw_rc,
                  overwrite = TRUE)


# Put data into a nested format in the dataframe, then create the SPDs by county.
by_county <- sw_rc %>%
  # Trim unnecessary variables.
  dplyr::select(Age, Error, calib, NAME, REGION) %>%
  sf::st_drop_geometry() %>%
  # Keep only data up to 9000 years old.
  dplyr::filter(Age < 9000) %>%
  # Get total number of samples by name and region.
  dplyr::add_count(NAME, REGION) %>%
  # Group by name, region, and n to nest data.
  dplyr::group_by(NAME, REGION, n) %>%
  tidyr::nest() %>%
  dplyr::mutate(spds = purrr::map(data, makeSPD))

SPD_county <- by_county %>%
  dplyr::mutate(spds = purrr::map(
    spds,
    ~ dplyr::mutate(
      .x,
      presence = ifelse(PrDens == 0, 0, 1),
      change = presence - dplyr::lag(presence, default = 0),
      firstdiff = PrDens - dplyr::lag(PrDens, default = 0),
      change_words = dplyr::case_when(
        change == 1 ~ "Begin occupation",
        change == -1 ~ "End occupation",
        TRUE ~ "nothing"
      )
    )
  )) %>%
  dplyr::mutate(county_st = paste(NAME, REGION, sep = ", "))

usethis::use_data(SPD_county,
                  overwrite = TRUE)


# Get a shapefile for the Balsas Basin.
download.file(
  url = "https://birdscanada.org/download/gislab/bcr_terrestrial_shape.zip?_ga=2.159175994.1630070136.1666017185-75333408.1666017185",
  destfile = here::here("data-raw/BCR_Terrestrial.zip"))

unzip(zipfile = here::here("data-raw/BCR_Terrestrial.zip"),
      exdir = here::here("data-raw"))

# Remove the zip file.
file.remove(here::here("data-raw/BCR_Terrestrial.zip"))
# Remove other unnecessary files.
file.remove(list.files(here::here("data-raw/BCR_Terrestrial"), pattern = "BCR_Terrestrial_dissolve|BCR_Terrestrial_master_International", full.names = TRUE))

# Read in shapefile.
Balsas_Basin <- sf::read_sf(here::here("data-raw/BCR_Terrestrial/BCR_Terrestrial_master.shp")) %>%
  dplyr::filter(BCRNAME == "CUENCA DEL RIO BALSAS")

usethis::use_data(Balsas_Basin,
                  overwrite = TRUE)



