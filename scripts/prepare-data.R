# prepare-data.R

nid_download_date <- ymd("2025-05-07")


###### State info

us_state_data <- tibble(
  state_abb = state.abb,
  state = state.name,
  state_area = state.area,
  state_region = state.region,
  state_div = state.division
)

state_boundaries_sf <- us_states() |>
  # `dams` data uses `state_abb` rather than 'state_abbr`, which `dam` gets from R's built-in `state.abb` data frame.
  # next 3 lines are a workaround since "rename(state_abb = state_abbr)" doesn't work
  #   "Error in rename.sf(us_states(), state_abb = state_abbr) : internal error: can't find `agr` columns"
  mutate(state_abb = state_abbr) |>
  relocate(state_abb, .after = state_abbr) |>
  select(-state_abbr) |>
  filter(!state_abb %in% c("DC", "HI", "AK", "PR")) |>
  arrange(state_abb) |>
  st_transform("NAD83") # US Government standard CRS

continental_us_border <- st_union(state_boundaries_sf)

bbox_continental <- continental_us_border |>
  st_bbox()


###### US River data

rivers_fname <- here("data/raw/nws/rs16my07") # subset of US river data
rivers_us <- st_read(rivers_fname, crs = "NAD83",
                     quiet = TRUE)

###### Get Missouri River data from OSM
# it's too big to do in one call (OSM query times out)

mo_river_osm_fname <- here("data/processed/mo-river-osm.rds")
# mo_river_osm_points_fname <- here("data/processed/mo-river-osm-points.rds")
# mo_river_dams_osm_fname <- here("data/processed/mo-river-dams-osm.rds")

### River lines and points (points include additional tributary detail)

# TODO: Do I need to keep Missouri River data from OSM? Is it still used anywhere?

if (!file.exists(mo_river_osm_fname)) {
  #| !file.exists(mo_river_osm_points_fname)) {

  # Features from https://wiki.openstreetmap.org/wiki/Map_features#Water
  # requires osmdata package

  # Relation: Missouri River (1756890)
  # https://www.openstreetmap.org/relation/1756890

  # mo_river_data <- my_bbox |>
  #   opq()%>%
  #   add_osm_feature(key = "waterway",
  #                   value = c("river", "stream")) %>%
  #   osmdata_sf()
  ### The above times out, so doing it by state

  # as suggested by ChatGPT
  query <- opq(
    bbox = "Montana",
    timeout = 120
  ) %>%
    add_osm_feature(key = "waterway", value = "river") %>%
    add_osm_feature(key = "name", value = "Missouri River")
  mo_river_data_mt <- osmdata_sf(query)

  query <- opq(
    bbox = "North Dakota",
    timeout = 120
  ) %>%
    add_osm_feature(key = "waterway", value = "river") %>%
    add_osm_feature(key = "name", value = "Missouri River")
  mo_river_data_nd <- osmdata_sf(query)

  query <- opq(
    bbox = "South Dakota",
    timeout = 120
  ) %>%
    add_osm_feature(key = "waterway", value = "river") %>%
    add_osm_feature(key = "name", value = "Missouri River")
  mo_river_data_sd <- osmdata_sf(query)

  query <- opq(
    bbox = "Nebraska",
    timeout = 120
  ) %>%
    add_osm_feature(key = "waterway", value = "river") %>%
    add_osm_feature(key = "name", value = "Missouri River")
  mo_river_data_ne <- osmdata_sf(query)

  query <- opq(
    bbox = "Iowa",
    timeout = 120
  ) %>%
    add_osm_feature(key = "waterway", value = "river") %>%
    add_osm_feature(key = "name", value = "Missouri River")
  mo_river_data_ia <- osmdata_sf(query)

  query <- opq(
    bbox = "Kansas",
    timeout = 120
  ) %>%
    add_osm_feature(key = "waterway", value = "river") %>%
    add_osm_feature(key = "name", value = "Missouri River")
  mo_river_data_ks <- osmdata_sf(query)

  query <- opq(
    bbox = "Missouri",
    timeout = 120
  ) %>%
    add_osm_feature(key = "waterway", value = "river") %>%
    add_osm_feature(key = "name", value = "Missouri River")
  mo_river_data_mo <- osmdata_sf(query)

  query <- opq(
    bbox = "Illinois",
    timeout = 120
  ) %>%
    add_osm_feature(key = "waterway", value = "river") %>%
    add_osm_feature(key = "name", value = "Missouri River")
  mo_river_data_il <- osmdata_sf(query)

  mo_lines <- bind_rows(
    mo_river_data_mt$osm_multilines |> mutate(state = "mt"),
    mo_river_data_nd$osm_multilines |> mutate(state = "nd"),
    mo_river_data_sd$osm_multilines |> mutate(state = "sd"),
    mo_river_data_ne$osm_multilines |> mutate(state = "ne"),
    mo_river_data_ia$osm_multilines |> mutate(state = "ia"),
    mo_river_data_ks$osm_multilines |> mutate(state = "ks"),
    mo_river_data_mo$osm_multilines |> mutate(state = "mo"),
    mo_river_data_il$osm_multilines |> mutate(state = "il")
  ) |>
    select(-c(`name:ar`:`name:ur`)) |>
    filter(role == "main_stream") # omits "side_stream"

  write_rds(mo_lines, mo_river_osm_fname)
} else {
  mo_lines <- read_rds(mo_river_osm_fname)
}


### Dam info

dam_fname <- here("data/processed/dam_all.rds")

big_dam_height_cutoff <- 100 # ft

if (!file.exists(dam_fname)) {
  dams_raw <- st_read(here("data/raw/nation.gpkg")) |>
    remove_empty(which = c("rows", "cols"))

  dams_all <- dams_raw |>
    mutate(across(
      c(
        latitude,
        longitude,
        distance,
        damHeight,
        hydraulicHeight,
        structuralHeight,
        nidHeight,
        damLength,
        volume,
        yearCompleted,
        nidStorage,
        maxStorage,
        normalStorage,
        surfaceArea,
        drainageArea,
        maxDischarge,
        numberOfLocks,
        lengthOfLocks,
        widthOfLocks,
        secondaryLengthOfLocks,
        secondaryWidthOfLocks
      ),
      as.numeric
    )) |>
    mutate(across(
      c(
        latitude,
        longitude,
        distance,
        damHeight,
        hydraulicHeight,
        structuralHeight,
        nidHeight,
        damLength,
        volume,
        yearCompleted,
        nidStorage,
        maxStorage,
        normalStorage,
        surfaceArea,
        drainageArea,
        maxDischarge,
        numberOfLocks,
        lengthOfLocks,
        widthOfLocks,
        secondaryLengthOfLocks,
        secondaryWidthOfLocks
      ),
      function(x) if_else(x == 0, NA, x)
    ))

  write_rds(dams_all, dam_fname)
  
} else {
  
  dams_all <- read_rds(dam_fname)
  
}

# TODO: Consider saving this rather than recreating it for each chapter
dams <- dams_all |>
  filter(!state %in% c("Alaska", "Hawaii", "Puerto Rico", "Guam")) |>
  filter(
    !is.na(latitude),
    is.na(otherStructureId), # no secondary structures (e.g., most but not all dikes/levees)
    longitude < 0
  ) |> # remove bad data point
  filter(
    !str_detect(name, "[Dd]ike") | # no dikes unless with dam
      (str_detect(name, "[Dd]ike") &
        (str_detect(name, "Dam") | str_detect(otherNames, "Dam")))
  ) |>
  filter(
    !str_detect(name, "Levee") | # no levee unless with dam
      (str_detect(name, "Levee") &
        (str_detect(name, "Dam") | str_detect(otherNames, "Dam")))
  ) |>
  filter(
    !(state == "Florida" & str_detect(name, "Herbert Hoover Dike - CIZ [B-G]")),
    nidId != "OH00581", # Grand Lake St. Marys - East Embankment in Ohio (duplicate surface area with West Embankment)
    nidId != "WA00266", # North Dam; same surface area and nidStorage as Dry Falls Dam
    !(nidId == "CA00426" & str_detect(name, "Auxiliary")),
    !(nidId == "CA00376" & str_detect(name, "Auxiliary"))
  ) |>
  mutate(
    primaryPurposeId = ifelse(
      is.na(primaryPurposeId) | primaryPurposeId == "Other",
      "Other or not specified",
      primaryPurposeId
    ),
    primaryPurposeId = ifelse(
      primaryPurposeId == "Fire Protection, Stock, Or Small Fish Pond",
      "Fire Protection, Stock,\nOr Small Fish Pond",
      primaryPurposeId
    ),
    primaryOwnerTypeId = ifelse(
      is.na(primaryOwnerTypeId) |
        primaryOwnerTypeId %in% c("Other", "Not Listed"),
      "Other or not specified",
      primaryOwnerTypeId
    ),
    primaryDamTypeId = ifelse(
      is.na(primaryDamTypeId) | primaryDamTypeId == "Other",
      "Other or not specified",
      primaryDamTypeId
    ),
    yearCompleted = ifelse(
      is.na(primaryDamTypeId),
      "Not specified",
      yearCompleted
    ),
    nidHeightId = factor(
      nidHeightId,
      levels = rev(c(
        "Greater than 100 feet",
        "51-100 feet",
        "25-50 feet",
        "Less than 25 feet",
        "Undetermined"
      ))
    ),
    hazardId = factor(
      hazardId,
      levels = rev(c("High", "Significant", "Low", "Undetermined"))
    ),
    conditionAssessId = ifelse(
      is.na(conditionAssessId),
      "Missing",
      conditionAssessId
    ),
    conditionAssessId = factor(
      conditionAssessId,
      levels = rev(c(
        "Satisfactory",
        "Fair",
        "Poor",
        "Unsatisfactory",
        "Not Rated",
        "Missing",
        "Not Available"
      ))
    ),
    conditionAssessId = fct_collapse(
      conditionAssessId,
      Missing_or_not_rated = c("Missing", "Not Available", "Not Rated")
    ),
    eapId = ifelse(is.na(eapId), "Missing", eapId),
    eapId = factor(
      eapId,
      levels = rev(c("Yes", "No", "Not Required", "Missing"))
    )
  ) |>
  mutate(big_dam = nidHeight > big_dam_height_cutoff) |>
  select(
    -c(
      otherStructureId,
      separateStructuresCount,
      isAssociatedStructureId,
      designerNames,
      nonFederalDamOnFederalId,
      sourceAgency,
      stateFedId
    ),
    -c(congDist:nrcsWatershedAuthorizationId),
    -c(usaceDivision:nation),
    -c(
      femaRegion:aiannh,
      stateKey,
      outletGateTypes,
      countyState,
      city,
      distance
    ),
    -contains(c("Locks", "spillway"))
  ) |>
  left_join(
    # TODO: use this state data or remove
    us_state_data,
    by = "state"
  )

dams_no_geo <- dams |>
  st_drop_geometry()

n_dams <- nrow(dams_no_geo)
n_dams_big <- dams_no_geo |>
  filter(big_dam) |>
  nrow()

# TODO: Is this still used? If not remove it.
dams_no_geo_all_purposes <- dams_no_geo |>
  st_drop_geometry() |>
  select(
    name,
    nidId,
    state,
    primaryPurposeId,
    purposeIds
  ) |>
  separate_longer_delim(purposeIds, delim = ";") |>
  mutate(purpose_id = ifelse(is.na(purposeIds) | purposeIds == "-1",
                             "Other or not specified",
                             purposeIds),
         purpose_id = ifelse(
           purpose_id == "Fire Protection, Stock, Or Small Fish Pond",
           "Fire Protection, Stock,\nOr Small Fish Pond",
           purpose_id
         ),
  )

dams_by_state <- dams |>
  count(state, state_abb)


######  How wet is your state?

# TODO: can this be moved to one chapter?

wet <- readxl::read_xlsx(
  here("data/raw/how-wet-is-your-state-usgs.xlsx"),
  skip = 3
) |>
  clean_names() |>
  mutate(
    great_lakes_square_miles = if_else(
      great_lakes_square_miles == "—",
      "0",
      great_lakes_square_miles
    ),
    total_area_less_great_lakes = total_area_square_miles -
      coalesce(as.numeric(great_lakes_square_miles), 0)
  ) |>
  select(
    -contains("kilometer"),
    -c(
      inland_square_miles,
      coastal_square_miles,
      great_lakes_square_miles,
      territorial_square_miles
    )
  )

names(wet) <- paste0("usgs_", names(wet))
