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
NED <-
  raster::raster("/Volumes/DATA/NED/EXTRACTIONS/UUSS_NED_1.tif")

# Set projection
raster::crs(NED) <- "EPSG:4326"

MDB_SWUS_elev <-
  data.frame(MDB_SWUS, elevation = terra::extract(x = NED, y = MDB_SWUS))

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


# Calculate county SPDs.
MDB <- cropDiffusionR::maizeDB %>%
  dplyr::left_join(.,
                   readr::read_csv(
                     stringr::str_remove(
                       here::here("Paper 2/Maize_Database_Locations_DONT_DISTRIBUTE.csv"),
                       "cropSpreadR/"
                     ),
                     col_types = readr::cols()
                   ),
                   by = "LabID")

counties10sf <-
  sf::st_as_sf(maps::map("county", fill = TRUE, plot = FALSE)) %>%
  sf::st_transform(., crs = 4326) %>%
  tidyr::separate(ID, into = c("REGION", "NAME"), sep = ",") %>%
  dplyr::filter(REGION %in% c("colorado", "utah", "arizona", "new mexico")) %>%
  sf::st_make_valid()

bad_loc <- p3k14c::p3k14c_data %>%
  dplyr::filter(Province %in% c("Arizona", "New Mexico", "Colorado", "Utah")) %>%
  dplyr::filter(LocAccuracy == 0 & !is.na(SiteName)) %>%
  dplyr::select(-c(Long, Lat)) %>%
  dplyr::left_join(
    .,
    MDB %>% dplyr::select(SiteName, Long, Lat) %>% dplyr::distinct(SiteName, .keep_all = TRUE),
    by = "SiteName"
  ) %>%
  dplyr::filter(!is.na(Long))

sf::sf_use_s2(FALSE)

Four_corners <- p3k14c::p3k14c_data %>%
  dplyr::filter(Province %in% c("Arizona", "New Mexico", "Colorado", "Utah")) %>%
  dplyr::filter(LocAccuracy > 0) %>%
  dplyr::bind_rows(., bad_loc) %>%
  dplyr::distinct(LabID, .keep_all = TRUE) %>%
  dplyr::mutate (calib = "intcal20",
                 d13C = as.numeric(d13C)) %>%
  sf::st_as_sf(# Convert to spatial object
    coords = c("Long", "Lat"),
    crs = 4326) %>%
  sf::st_make_valid() %>%
  sf::st_join(counties10sf)

by_county <- Four_corners %>%
  dplyr::select(#trim unnecessary variables
    Age, Error, calib, NAME, REGION) %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(Age < 9000) %>%
  dplyr::group_by(NAME) %>% # group by region
  dplyr::mutate(n = n()) %>%
  dplyr::group_by(NAME, REGION, n) %>% # group by region
  tidyr::nest()

by_county$spds <- purrr::map(by_county$data, makeSPD)

by_county2 <- by_county %>%
  # dplyr::select(-data)
  mutate(spds = map(
    spds,
    ~ mutate(
      .x,
      presence = ifelse(PrDens == 0, 0, 1),
      change = tsibble::difference(presence, default = first(presence)),
      firstdiff = tsibble::difference(PrDens, default = first(PrDens)),
      change_words = ifelse(
        change == 1,
        "Begin occupation",
        ifelse(change == -1, "End occupation", 0)
      )
    )
  ))

by_county_changes <- by_county2 %>%
  tidyr::unnest(cols = c(spds)) %>%
  dplyr::filter(change_words != "0")
