#' Generate Synthetic ALccdfDB Data for Demonstrations
#'
#' @description These functions create realistic synthetic datasets that mirror
#'   the structure and types of real Alabama CCDF administrative data. They are
#'   useful for demonstrations, vignettes, and testing without requiring access
#'   to confidential data files.
#'
#'   All four generators share facility identities when called with the same
#'   \code{seed}, enabling cross-module linkage (programs <-> enrolled <->
#'   clients, programs <-> staff, programs <-> melissa).
#'
#' @name synthetic-data
NULL


# ---- Internal: Shared Facility Pool ----------------------------------------

#' Generate shared facility identities
#'
#' Creates deterministic facility records for all generators to share.
#' When called with the same seed and n, produces identical facilities.
#'
#' @param n Integer. Number of facilities to generate.
#' @param seed Integer. Random seed.
#' @return A tibble with facility_id, facility_name, facility_type, provider_id,
#'   county, region, facility_address.
#' @keywords internal
.alccdf_synthetic_facilities <- function(n, seed) {
  withr::with_seed(seed, {

    # ---- Facility type distribution (~45/15/10/20/10) ----
    type_probs <- c(0.45, 0.15, 0.10, 0.20, 0.10)
    type_labels <- c("Center", "Family Home", "Group Home",
                     "Faith-Based", "Excepted (University/Other)")
    type_prefixes <- c("FC", "FF", "FG", "FE", "FX")

    fac_type_idx <- sample(seq_along(type_labels), n, replace = TRUE,
                           prob = type_probs)
    fac_type <- factor(type_labels[fac_type_idx], levels = type_labels)

    # ---- Facility ID: prefix by type + sequential within type ----
    fac_id <- character(n)
    for (k in seq_along(type_labels)) {
      idx <- which(fac_type_idx == k)
      if (length(idx) > 0) {
        fac_id[idx] <- sprintf("%s%03d", type_prefixes[k], seq_along(idx))
      }
    }

    # ---- Realistic Alabama facility names (~30 per type) ----
    center_names <- c(
      "Little Stars Learning Center", "Bright Beginnings Academy",
      "Sunshine Child Development", "Growing Minds Center",
      "Happy Hearts Childcare", "ABC Learning Center",
      "Kids First Academy", "Rainbow Child Care Center",
      "Creative Kids Learning", "Stepping Stones Academy",
      "Tiny Treasures Center", "Discovery Day School",
      "Playful Minds Academy", "Blossom Child Care Center",
      "Little Explorers Academy", "Future Leaders Center",
      "Kiddie Corner Learning", "Smart Start Academy",
      "Tender Care Center", "Peachtree Learning Center",
      "Magnolia Day School", "Southern Star Academy",
      "Cotton State Child Care", "Heritage Learning Center",
      "Azalea Child Development", "Camellia Kids Academy",
      "Dogwood Learning Center", "Jubilee Child Care Center",
      "Red Mountain Academy", "Riverfront Learning Center"
    )
    family_home_names <- c(
      "Ms. Johnson's Family Day Care", "Smith Family Child Care",
      "Williams Family Home", "Brown's Little Ones",
      "Davis Family Day Care", "Wilson Home Child Care",
      "Moore Family Care Home", "Taylor's Tiny Steps",
      "Anderson Family Day Care", "Thomas Home Care",
      "Jackson Family Child Care", "White Family Day Home",
      "Harris Home Child Care", "Martin Family Care",
      "Thompson Family Day Care", "Garcia Family Home",
      "Robinson Family Child Care", "Clark Home Day Care",
      "Lewis Family Care Home", "Lee Family Day Care",
      "Walker Home Child Care", "Hall Family Day Care",
      "Allen Family Care Home", "Young Home Child Care",
      "King Family Day Care", "Wright Family Home Care",
      "Scott Family Day Care", "Green Home Child Care",
      "Baker Family Care", "Adams Family Day Home"
    )
    group_home_names <- c(
      "Northside Group Home", "Southview Group Care",
      "Eastside Children's Group", "Westgate Group Home",
      "Hilltop Group Child Care", "Meadowbrook Group Home",
      "Pinewood Group Care", "Lakeside Group Home",
      "Oak Grove Group Care", "Cedar Hill Group Home",
      "Maple Lane Group Care", "Elm Street Group Home",
      "Holly Ridge Group Care", "Birch Creek Group Home",
      "Ivy Lane Group Care", "Willow Park Group Home",
      "Aspen Group Child Care", "Cypress Group Home",
      "Juniper Group Care", "Spruce Hill Group Home",
      "Poplar Grove Group Care", "Hickory Group Home",
      "Walnut Creek Group Care", "Chestnut Group Home",
      "Hazel Group Child Care", "Laurel Group Home",
      "Sycamore Group Care", "Magnolia Group Home",
      "Pecan Grove Group Care", "Mimosa Group Home"
    )
    faith_names <- c(
      "First Baptist Child Care", "St. Paul's Early Learning",
      "Grace Community Childcare", "New Hope Church Day Care",
      "Mt. Zion Christian Academy", "Bethel Baptist Center",
      "Trinity Methodist Preschool", "Faith Temple Day Care",
      "Calvary Church Child Care", "Good Shepherd Academy",
      "Shiloh Baptist Learning", "Antioch Church Day Care",
      "Emmanuel Lutheran Preschool", "Pilgrim Rest Child Care",
      "St. Luke's Early Learning", "Macedonia Church Academy",
      "Providence Baptist Center", "Zion Hill Church Care",
      "Friendship Baptist Day Care", "Pleasant Hill Church Care",
      "Ebenezer Church Academy", "Mt. Olive Baptist Center",
      "Christ Church Preschool", "Covenant Community Care",
      "Liberty Church Day Care", "Cornerstone Church Academy",
      "Restoration Church Care", "Peace Lutheran Preschool",
      "St. Andrew's Child Care", "Redeemer Church Academy"
    )
    excepted_names <- c(
      "UA Early Learning Lab", "Auburn University Child Care",
      "UAB Child Development Center", "Troy University Preschool",
      "JSU Child Learning Center", "USA Kids Academy",
      "Samford University Preschool", "ASU Early Learning Center",
      "Alabama A&M Child Care", "UNA Kids Campus",
      "Montevallo Early Learning", "UAH Child Development",
      "AUM Campus Child Care", "Tuskegee University Day Care",
      "Spring Hill College Preschool", "Faulkner University Care",
      "Stillman College Child Center", "Judson College Learning",
      "Miles College Early Learning", "Talladega College Child Care",
      "Birmingham-Southern Kids Lab", "Oakwood University Care",
      "Huntingdon College Preschool", "Wallace State Child Center",
      "Shelton State Kids Academy", "Calhoun Community Care",
      "Lawson State Early Learning", "Gadsden State Child Center",
      "Drake State Kids Academy", "Snead State Learning Center"
    )

    name_pools <- list(center_names, family_home_names, group_home_names,
                       faith_names, excepted_names)
    fac_name <- character(n)
    for (k in seq_along(type_labels)) {
      idx <- which(fac_type_idx == k)
      if (length(idx) > 0) {
        pool <- name_pools[[k]]
        fac_name[idx] <- sample(pool, length(idx), replace = length(idx) > length(pool))
      }
    }

    # ---- Provider IDs ----
    provider_id <- sprintf("P%04d", seq_len(n))

    # ---- Alabama counties and regions ----
    al_counties <- c(
      "Jefferson", "Mobile", "Montgomery", "Madison", "Tuscaloosa",
      "Baldwin", "Lee", "Shelby", "Morgan", "Houston",
      "Etowah", "Calhoun", "Lauderdale", "Limestone", "Marshall",
      "DeKalb", "Colbert", "Russell", "Talladega", "Elmore",
      "St. Clair", "Autauga", "Coffee", "Dale", "Cullman",
      "Walker", "Blount", "Chambers", "Chilton", "Covington",
      "Escambia", "Jackson", "Tallapoosa", "Marion", "Franklin"
    )
    county_probs <- c(
      0.10, 0.08, 0.07, 0.07, 0.06,
      0.05, 0.05, 0.05, 0.04, 0.03,
      0.03, 0.03, 0.03, 0.02, 0.02,
      0.02, 0.02, 0.02, 0.02, 0.02,
      0.02, 0.01, 0.01, 0.01, 0.01,
      0.01, 0.01, 0.01, 0.01, 0.01,
      0.01, 0.01, 0.01, 0.01, 0.01
    )
    county <- sample(al_counties, n, replace = TRUE, prob = county_probs)
    region <- paste("Region", sample(1:7, n, replace = TRUE))

    # ---- Realistic Alabama addresses ----
    street_nums <- sample(100:9999, n, replace = TRUE)
    street_names <- c(
      "Main Street", "Broad Street", "Court Street", "Commerce Street",
      "Church Street", "Oak Avenue", "Elm Avenue", "Pine Street",
      "Magnolia Boulevard", "Peachtree Drive", "University Boulevard",
      "Highway 280", "Airport Boulevard", "Governors Drive",
      "McFarland Boulevard", "Memorial Parkway", "Beltline Road",
      "Eastern Boulevard", "Ross Clark Circle", "Quintard Avenue",
      "Noble Street", "Dexter Avenue", "Cloverdale Road",
      "Fairview Avenue", "Highland Avenue", "Morris Avenue"
    )
    city_map <- c(
      Jefferson = "Birmingham", Mobile = "Mobile",
      Montgomery = "Montgomery", Madison = "Huntsville",
      Tuscaloosa = "Tuscaloosa", Baldwin = "Daphne",
      Lee = "Auburn", Shelby = "Alabaster",
      Morgan = "Decatur", Houston = "Dothan",
      Etowah = "Gadsden", Calhoun = "Anniston",
      Lauderdale = "Florence", Limestone = "Athens",
      Marshall = "Albertville", DeKalb = "Fort Payne",
      Colbert = "Muscle Shoals", Russell = "Phenix City",
      Talladega = "Talladega", Elmore = "Wetumpka"
    )
    zip_map <- c(
      Jefferson = "35203", Mobile = "36602",
      Montgomery = "36104", Madison = "35801",
      Tuscaloosa = "35401", Baldwin = "36526",
      Lee = "36830", Shelby = "35007",
      Morgan = "35601", Houston = "36301",
      Etowah = "35901", Calhoun = "36201",
      Lauderdale = "35630", Limestone = "35611",
      Marshall = "35950", DeKalb = "35967",
      Colbert = "35661", Russell = "36867",
      Talladega = "35160", Elmore = "36092"
    )
    default_city <- "Prattville"
    default_zip <- "36067"

    streets <- sample(street_names, n, replace = TRUE)
    cities <- ifelse(county %in% names(city_map), city_map[county], default_city)
    zips <- ifelse(county %in% names(zip_map), zip_map[county], default_zip)
    facility_address <- paste0(street_nums, " ", streets, ", ", cities, ", AL ", zips)

    tibble::tibble(
      facility_id = fac_id,
      facility_name = fac_name,
      facility_type = fac_type,
      provider_id = provider_id,
      county = county,
      region = region,
      facility_address = facility_address
    )
  })
}


# ---- Exported: Synthetic Programs -------------------------------------------

#' Generate Synthetic Program Data
#'
#' @description Creates a synthetic \code{alccdf_program_clean} object with
#'   realistic facility characteristics, capacity, operating hours, tier
#'   ratings, and derived quality-adjusted capacity fields.
#'
#' @param n Integer. Number of facilities. Default 50.
#' @param seed Integer. Random seed for reproducibility. Default 42.
#'
#' @return An \code{alccdf_program_clean} S3 object.
#'
#' @examples
#' prog <- alccdf_synthetic_programs(n = 30, seed = 123)
#' prog
#' head(prog$data)
#'
#' @rdname synthetic-data
#' @export
alccdf_synthetic_programs <- function(n = 50, seed = 42) {
  .check_suggested("withr", "reproducible synthetic data generation")

  fac <- .alccdf_synthetic_facilities(n, seed)
  snapshot_date <- Sys.Date()

  withr::with_seed(seed + 1000, {

    ft_levels <- alccdf_facility_type_levels()
    tier_levels <- alccdf_facility_tier_levels()

    facility_type <- factor(as.character(fac$facility_type), levels = ft_levels)
    facility_type_raw <- as.character(fac$facility_type)

    # ---- License numbers ----
    license_number <- sprintf("LIC-%05d-%04d", sample(10000:99999, n), sample(1:9999, n))

    # ---- Facility tier (ordered factor; NA for Faith-Based and Excepted ~30%) ----
    fac_type_chr <- as.character(facility_type)
    gets_tier <- fac_type_chr %in% c("Center", "Family Home", "Group Home")
    tier_raw <- rep(NA_character_, n)
    n_rated <- sum(gets_tier)
    if (n_rated > 0) {
      tier_raw[gets_tier] <- sample(
        tier_levels, n_rated, replace = TRUE,
        prob = c(0.10, 0.20, 0.30, 0.25, 0.15)
      )
    }
    facility_tier <- factor(tier_raw, levels = tier_levels, ordered = TRUE)
    facility_tier_original <- facility_tier
    was_originally_rated <- !is.na(facility_tier)

    # ---- County and region as factors ----
    county <- factor(fac$county)
    region <- factor(fac$region)

    # ---- Addresses (raw + standardized) ----
    facility_address <- fac$facility_address
    facility_address2 <- toupper(facility_address)
    facility_address2 <- gsub("STREET", "ST", facility_address2)
    facility_address2 <- gsub("AVENUE", "AVE", facility_address2)
    facility_address2 <- gsub("BOULEVARD", "BLVD", facility_address2)
    facility_address2 <- gsub("DRIVE", "DR", facility_address2)
    facility_address2 <- gsub("ROAD", "RD", facility_address2)
    facility_address2 <- gsub("PARKWAY", "PKWY", facility_address2)

    # ---- Operating hours ----
    day_start <- sample(c("06:00", "06:30", "07:00", "07:30"), n, replace = TRUE,
                        prob = c(0.35, 0.30, 0.25, 0.10))
    day_end <- sample(c("17:00", "17:30", "18:00", "18:30", "19:00"), n, replace = TRUE,
                      prob = c(0.10, 0.20, 0.40, 0.20, 0.10))
    has_night <- sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.10, 0.90))
    night_start <- ifelse(has_night,
                          sample(c("18:00", "19:00", "20:00"), n, replace = TRUE),
                          NA_character_)
    night_end <- ifelse(has_night,
                        sample(c("06:00", "06:30", "07:00"), n, replace = TRUE),
                        NA_character_)

    # ---- Capacity ----
    day_capacity <- integer(n)
    day_capacity[fac_type_chr == "Center"] <-
      sample(20:200, sum(fac_type_chr == "Center"), replace = TRUE)
    day_capacity[fac_type_chr == "Family Home"] <-
      sample(4:8, sum(fac_type_chr == "Family Home"), replace = TRUE)
    day_capacity[fac_type_chr == "Group Home"] <-
      sample(6:12, sum(fac_type_chr == "Group Home"), replace = TRUE)
    day_capacity[fac_type_chr == "Faith-Based"] <-
      sample(15:60, sum(fac_type_chr == "Faith-Based"), replace = TRUE)
    day_capacity[fac_type_chr == "Excepted (University/Other)"] <-
      sample(20:80, sum(fac_type_chr == "Excepted (University/Other)"), replace = TRUE)

    night_capacity <- integer(n)
    night_idx <- which(has_night & fac_type_chr == "Center")
    if (length(night_idx) > 0) {
      night_capacity[night_idx] <- sample(5:30, length(night_idx), replace = TRUE)
    }

    has_day_capacity <- as.integer(day_capacity > 0)
    has_night_capacity <- as.integer(night_capacity > 0)
    total_capacity <- day_capacity + night_capacity
    night_to_day_ratio <- ifelse(day_capacity > 0,
                                 round(night_capacity / day_capacity, 4), 0)

    # ---- Age ranges ----
    age_min_choices <- c(0, 0, 0, 1, 2, 3)
    age_max_choices <- c(5, 6, 8, 10, 12, 13)
    day_age_min <- sample(age_min_choices, n, replace = TRUE)
    day_age_max <- sample(age_max_choices, n, replace = TRUE)
    day_age_max <- pmax(day_age_max, day_age_min + 2)
    day_age_range <- paste0(day_age_min, " - ", day_age_max, " Years")
    day_age_range_num <- day_age_max - day_age_min

    day_age_infants_toddlers <- as.integer(day_age_min < 3)
    day_age_preschool <- as.integer(day_age_min <= 3 & day_age_max >= 4)
    day_age_school_aged <- as.integer(day_age_max >= 6)

    night_age_range <- ifelse(has_night, "0 - 12 Years", NA_character_)
    night_age_min <- ifelse(has_night, 0, NA_real_)
    night_age_max <- ifelse(has_night, 12, NA_real_)

    # ---- Operating hours (numeric) ----
    start_hrs <- as.numeric(substr(day_start, 1, 2)) +
      as.numeric(substr(day_start, 4, 5)) / 60
    end_hrs <- as.numeric(substr(day_end, 1, 2)) +
      as.numeric(substr(day_end, 4, 5)) / 60
    day_operating_hours <- round(end_hrs - start_hrs, 1)
    night_operating_hours <- ifelse(has_night, round(runif(n, 10, 12), 1), NA_real_)

    operation_type <- ifelse(
      has_night & has_day_capacity == 1, "Day & Night",
      ifelse(has_day_capacity == 1, "Day Only", "Night Only")
    )

    # ---- Expiration and dates ----
    expiration_date <- Sys.Date() + sample(30:540, n, replace = TRUE)
    current_date <- rep(Sys.Date(), n)
    days_till_expire <- as.integer(expiration_date - current_date)

    # ---- Quality indicators ----
    is_high_quality <- !is.na(facility_tier) & facility_tier >= "Star 3"

    tier_num <- as.integer(facility_tier)
    tier_weight_linear <- ifelse(!is.na(tier_num), tier_num * 0.2, NA_real_)
    tier_weight_binary <- ifelse(!is.na(tier_num),
                                 ifelse(tier_num >= 3, 1.0, 0.0), NA_real_)
    tier_weight_exp <- ifelse(!is.na(tier_num),
                              c(0.1, 0.2, 0.4, 0.7, 1.0)[tier_num], NA_real_)

    capacity_qa_linear <- ifelse(!is.na(tier_weight_linear),
                                 day_capacity * tier_weight_linear, NA_real_)
    capacity_qa_binary <- ifelse(!is.na(tier_weight_binary),
                                 day_capacity * tier_weight_binary, NA_real_)
    capacity_qa_exp <- ifelse(!is.na(tier_weight_exp),
                              day_capacity * tier_weight_exp, NA_real_)

    # ---- Capacity flag ----
    capacity_flag <- rep("OK", n)
    fam_over <- fac_type_chr == "Family Home" & day_capacity > 8
    grp_over <- fac_type_chr == "Group Home" & day_capacity > 12
    capacity_flag[fam_over] <- "Review: Family Home > 8"
    capacity_flag[grp_over] <- "Review: Group Home > 12"

    # ---- Optional fields ----
    phone_area <- sample(c("205", "251", "256", "334", "938"), n, replace = TRUE)
    phone_number <- paste0("(", phone_area, ") ",
                           sprintf("%03d", sample(200:999, n, replace = TRUE)), "-",
                           sprintf("%04d", sample(1000:9999, n, replace = TRUE)))

    first_names <- c("Mary", "Patricia", "Linda", "Barbara", "Elizabeth",
                     "Jennifer", "Maria", "Susan", "Margaret", "Lisa",
                     "Nancy", "Karen", "Dorothy", "Sandra", "Ashley",
                     "Angela", "Donna", "Ruth", "Carol", "Michelle")
    last_names <- c("Smith", "Johnson", "Williams", "Brown", "Jones",
                    "Davis", "Miller", "Wilson", "Moore", "Taylor",
                    "Anderson", "Thomas", "Jackson", "White", "Harris")
    director_name <- paste(sample(first_names, n, replace = TRUE),
                           sample(last_names, n, replace = TRUE))

    consultant_names <- paste(
      sample(c("Dr.", "Ms.", "Mr."), n, replace = TRUE),
      sample(first_names, n, replace = TRUE),
      sample(last_names, n, replace = TRUE)
    )

    # ---- Build tibble ----
    df <- tibble::tibble(
      facility_id          = fac$facility_id,
      facility_name        = fac$facility_name,
      facility_type_raw    = facility_type_raw,
      facility_type        = facility_type,
      facility_tier        = facility_tier,
      county               = county,
      region               = region,
      facility_address     = facility_address,
      facility_address2    = facility_address2,
      provider_id          = fac$provider_id,
      license_number       = license_number,
      day_start            = day_start,
      day_end              = day_end,
      night_start          = night_start,
      night_end            = night_end,
      day_capacity         = as.integer(day_capacity),
      night_capacity       = as.integer(night_capacity),
      has_day_capacity     = has_day_capacity,
      has_night_capacity   = has_night_capacity,
      total_capacity       = as.integer(total_capacity),
      night_to_day_ratio   = night_to_day_ratio,
      day_age_range        = day_age_range,
      day_age_min          = day_age_min,
      day_age_max          = day_age_max,
      day_age_range_num    = day_age_range_num,
      day_age_infants_toddlers = day_age_infants_toddlers,
      day_age_preschool    = day_age_preschool,
      day_age_school_aged  = day_age_school_aged,
      night_age_range      = night_age_range,
      night_age_min        = night_age_min,
      night_age_max        = night_age_max,
      day_operating_hours  = day_operating_hours,
      night_operating_hours = night_operating_hours,
      operation_type       = operation_type,
      expiration_date      = expiration_date,
      current_date         = current_date,
      days_till_expire     = days_till_expire,
      facility_tier_original = facility_tier_original,
      was_originally_rated = was_originally_rated,
      is_high_quality      = is_high_quality,
      tier_weight_linear   = tier_weight_linear,
      tier_weight_binary   = tier_weight_binary,
      tier_weight_exp      = tier_weight_exp,
      capacity_qa_linear   = capacity_qa_linear,
      capacity_qa_binary   = capacity_qa_binary,
      capacity_qa_exp      = capacity_qa_exp,
      capacity_flag        = capacity_flag,
      snapshot_date        = snapshot_date,
      phone_number         = phone_number,
      director_name        = director_name,
      consultant           = consultant_names
    )
  })

  .make_alccdf_obj(
    data          = df,
    class_name    = "alccdf_program_clean",
    module        = "program",
    stage         = "clean",
    snapshot_date = snapshot_date,
    extra_meta    = list(
      program_type   = "synthetic",
      format_version = "synthetic",
      source_file    = "synthetic",
      n_rows_raw     = n,
      n_facilities   = n
    )
  )
}


# ---- Exported: Synthetic Subsidy --------------------------------------------

#' Generate Synthetic Subsidy Data
#'
#' @description Creates synthetic enrolled-children and clients subsidy data
#'   with PII separation, realistic dates, and cross-module facility linkage.
#'
#' @param n_enrolled Integer. Number of enrolled-children records. Default 80.
#' @param n_clients Integer. Number of client records. Default 40.
#' @param seed Integer. Random seed for reproducibility. Default 42.
#'
#' @return A named list with \code{$enrolled} and \code{$clients}, each an
#'   \code{alccdf_subsidy_clean} S3 object with \code{$data}, \code{$pii},
#'   and \code{$meta} components.
#'
#' @examples
#' sub <- alccdf_synthetic_subsidy(n_enrolled = 40, n_clients = 20, seed = 99)
#' sub$enrolled
#' head(sub$enrolled$data)
#' head(sub$enrolled$pii)
#'
#' @rdname synthetic-data
#' @export
alccdf_synthetic_subsidy <- function(n_enrolled = 80, n_clients = 40, seed = 42) {
  .check_suggested("withr", "reproducible synthetic data generation")

  # Use same base seed for facility pool so linkage works
  fac <- .alccdf_synthetic_facilities(50, seed)
  snapshot_date <- Sys.Date()

  # ---- Enrolled data ----
  enrolled_obj <- withr::with_seed(seed + 2000, {

    ne <- n_enrolled

    # Random IDs (PII-free)
    parent_id <- sprintf("P%06d", sample(100000:999999, ne))
    child_id <- sprintf("C%06d", sample(100000:999999, ne))
    case_id <- sprintf("C%03d", seq_len(ne))

    # Link ~75% of records to known facilities
    n_linked <- round(ne * 0.75)
    fac_idx <- c(
      sample(seq_len(nrow(fac)), n_linked, replace = TRUE),
      rep(NA_integer_, ne - n_linked)
    )
    fac_idx <- sample(fac_idx)  # shuffle

    fac_id <- ifelse(!is.na(fac_idx), fac$facility_id[fac_idx],
                     sprintf("FC%03d", sample(100:199, ne - n_linked, replace = TRUE)))
    fac_name <- ifelse(!is.na(fac_idx), fac$facility_name[fac_idx],
                       paste("Unknown Facility", seq_len(ne - n_linked)))
    prov_id <- ifelse(!is.na(fac_idx), fac$provider_id[fac_idx],
                      sprintf("P%04d", sample(5000:9999, ne - n_linked, replace = TRUE)))
    cty <- ifelse(!is.na(fac_idx), fac$county[fac_idx],
                  sample(c("Jefferson", "Mobile", "Montgomery"), ne - n_linked,
                         replace = TRUE))
    reg <- ifelse(!is.na(fac_idx), fac$region[fac_idx],
                  paste("Region", sample(1:7, ne - n_linked, replace = TRUE)))

    # Child demographics
    child_age <- round(runif(ne, 0, 12), 1)
    care_level <- ifelse(child_age < 1, "Infant",
                         ifelse(child_age < 3, "Toddler",
                                ifelse(child_age < 5, "PreSchool", "School Age")))

    eligibility_category <- sample(paste("Category", LETTERS[1:5]),
                                   ne, replace = TRUE,
                                   prob = c(0.30, 0.25, 0.20, 0.15, 0.10))

    # Dates
    elig_begin <- Sys.Date() - sample(30:365, ne, replace = TRUE)
    elig_end <- elig_begin + sample(180:540, ne, replace = TRUE)
    place_start <- elig_begin + sample(0:30, ne, replace = TRUE)
    has_place_end <- sample(c(TRUE, FALSE), ne, replace = TRUE, prob = c(0.3, 0.7))
    place_end <- ifelse(has_place_end, as.character(place_start + sample(90:365, ne, replace = TRUE)),
                        NA_character_)
    place_end <- as.Date(place_end)

    unit_of_care <- sample(c("Weekly", "Daily"), ne, replace = TRUE,
                           prob = c(0.75, 0.25))

    enrolled_df <- tibble::tibble(
      parent_id            = parent_id,
      child_id             = child_id,
      case_id              = case_id,
      child_age            = child_age,
      care_level           = care_level,
      eligibility_category = eligibility_category,
      eligibility_begin_date = elig_begin,
      eligibility_end_date = elig_end,
      placement_start_date = place_start,
      placement_end_date   = place_end,
      facility_id          = fac_id,
      facility_name        = fac_name,
      provider_id          = prov_id,
      county               = cty,
      region               = reg,
      unit_of_care         = unit_of_care,
      snapshot_date        = snapshot_date
    )

    # PII table
    first_names <- c("James", "Mary", "Robert", "Patricia", "John", "Jennifer",
                     "Michael", "Linda", "David", "Barbara", "William", "Elizabeth",
                     "Richard", "Susan", "Joseph", "Jessica", "Thomas", "Sarah",
                     "Charles", "Karen")
    last_names <- c("Smith", "Johnson", "Williams", "Brown", "Jones", "Garcia",
                    "Miller", "Davis", "Rodriguez", "Martinez", "Hernandez",
                    "Lopez", "Wilson", "Anderson", "Thomas", "Taylor")
    child_first <- c("Emma", "Liam", "Olivia", "Noah", "Ava", "Elijah",
                     "Sophia", "James", "Isabella", "Oliver", "Mia", "Benjamin",
                     "Charlotte", "Lucas", "Amelia", "Mason", "Harper", "Ethan",
                     "Evelyn", "Logan")

    pii <- tibble::tibble(
      parent_id   = parent_id,
      child_id    = child_id,
      parent_name = paste(sample(first_names, ne, replace = TRUE),
                          sample(last_names, ne, replace = TRUE)),
      child_name  = paste(sample(child_first, ne, replace = TRUE),
                          sample(last_names, ne, replace = TRUE)),
      parent_ssn  = sprintf("%03d-%02d-%04d",
                            sample(100:899, ne, replace = TRUE),
                            sample(10:99, ne, replace = TRUE),
                            sample(1000:9999, ne, replace = TRUE)),
      child_ssn   = sprintf("%03d-%02d-%04d",
                            sample(100:899, ne, replace = TRUE),
                            sample(10:99, ne, replace = TRUE),
                            sample(1000:9999, ne, replace = TRUE))
    )

    obj <- .make_alccdf_obj(
      data          = enrolled_df,
      class_name    = "alccdf_subsidy_clean",
      module        = "subsidy",
      stage         = "clean",
      snapshot_date = snapshot_date,
      extra_meta    = list(
        subsidy_type   = "enrolled",
        format_version = "synthetic",
        source_file    = "synthetic",
        n_rows_raw     = ne,
        has_pii        = TRUE,
        n_pii_records  = nrow(pii)
      )
    )
    obj$pii <- pii
    obj
  })

  # ---- Clients data ----
  clients_obj <- withr::with_seed(seed + 3000, {

    nc <- n_clients

    # Reuse some parent/child IDs from enrolled for overlap
    enrolled_parents <- enrolled_obj$data$parent_id
    enrolled_children <- enrolled_obj$data$child_id
    n_overlap <- min(round(nc * 0.4), length(enrolled_parents))

    overlap_idx <- sample(seq_along(enrolled_parents), n_overlap)
    new_n <- nc - n_overlap
    parent_id <- c(enrolled_parents[overlap_idx],
                   sprintf("P%06d", sample(100000:999999, new_n)))
    child_id <- c(enrolled_children[overlap_idx],
                  sprintf("C%06d", sample(100000:999999, new_n)))
    case_id <- c(enrolled_obj$data$case_id[overlap_idx],
                 sprintf("C%03d", seq(n_enrolled + 1, n_enrolled + new_n)))

    # Facility linkage for clients
    fac_idx <- sample(seq_len(nrow(fac)), nc, replace = TRUE)

    # Child demographics
    child_dob <- Sys.Date() - sample(365:4380, nc, replace = TRUE)
    child_age <- round(as.numeric(difftime(Sys.Date(), child_dob, units = "days")) / 365.25, 1)

    # Residential addresses (for Melissa merge)
    street_nums <- sample(100:9999, nc, replace = TRUE)
    res_streets <- c("Oak Lane", "Maple Drive", "Cedar Court", "Pine Way",
                     "Elm Street", "Birch Road", "Walnut Avenue", "Cypress Circle",
                     "Magnolia Place", "Dogwood Lane", "Holly Street", "Pecan Drive",
                     "Willow Court", "Hickory Lane", "Poplar Avenue")
    res_cities <- c("Birmingham", "Huntsville", "Mobile", "Montgomery",
                    "Tuscaloosa", "Auburn", "Dothan", "Decatur",
                    "Florence", "Gadsden", "Anniston", "Alabaster")
    res_zips <- c("35203", "35801", "36602", "36104",
                  "35401", "36830", "36301", "35601",
                  "35630", "35901", "36201", "35007")
    city_idx <- sample(seq_along(res_cities), nc, replace = TRUE)
    family_address <- paste0(
      street_nums, " ", sample(res_streets, nc, replace = TRUE), ", ",
      res_cities[city_idx], ", AL ", res_zips[city_idx]
    )

    funding_type <- sample(c("Type I", "Type II"), nc, replace = TRUE,
                           prob = c(0.60, 0.40))
    copay_weekly <- round(runif(nc, 0, 50), 2)

    placement_date <- Sys.Date() - sample(30:365, nc, replace = TRUE)

    clients_df <- tibble::tibble(
      parent_id        = parent_id,
      child_id         = child_id,
      case_id          = case_id,
      family_address   = family_address,
      funding_type     = funding_type,
      child_age        = child_age,
      child_dob        = child_dob,
      copay_weekly     = copay_weekly,
      provider_id      = fac$provider_id[fac_idx],
      provider_name    = fac$facility_name[fac_idx],
      provider_address = fac$facility_address[fac_idx],
      placement_date   = placement_date,
      county           = fac$county[fac_idx],
      snapshot_date    = snapshot_date
    )

    # PII table for clients
    first_names <- c("James", "Mary", "Robert", "Patricia", "John", "Jennifer",
                     "Michael", "Linda", "David", "Barbara", "William", "Elizabeth",
                     "Richard", "Susan", "Joseph", "Jessica", "Thomas", "Sarah",
                     "Charles", "Karen")
    last_names <- c("Smith", "Johnson", "Williams", "Brown", "Jones", "Garcia",
                    "Miller", "Davis", "Rodriguez", "Martinez", "Hernandez",
                    "Lopez", "Wilson", "Anderson", "Thomas", "Taylor")
    child_first <- c("Emma", "Liam", "Olivia", "Noah", "Ava", "Elijah",
                     "Sophia", "James", "Isabella", "Oliver", "Mia", "Benjamin",
                     "Charlotte", "Lucas", "Amelia", "Mason", "Harper", "Ethan",
                     "Evelyn", "Logan")

    pii <- tibble::tibble(
      parent_id   = parent_id,
      child_id    = child_id,
      parent_name = paste(sample(first_names, nc, replace = TRUE),
                          sample(last_names, nc, replace = TRUE)),
      child_name  = paste(sample(child_first, nc, replace = TRUE),
                          sample(last_names, nc, replace = TRUE)),
      parent_ssn  = sprintf("%03d-%02d-%04d",
                            sample(100:899, nc, replace = TRUE),
                            sample(10:99, nc, replace = TRUE),
                            sample(1000:9999, nc, replace = TRUE)),
      child_ssn   = sprintf("%03d-%02d-%04d",
                            sample(100:899, nc, replace = TRUE),
                            sample(10:99, nc, replace = TRUE),
                            sample(1000:9999, nc, replace = TRUE))
    )

    obj <- .make_alccdf_obj(
      data          = clients_df,
      class_name    = "alccdf_subsidy_clean",
      module        = "subsidy",
      stage         = "clean",
      snapshot_date = snapshot_date,
      extra_meta    = list(
        subsidy_type   = "clients",
        format_version = "synthetic",
        source_file    = "synthetic",
        n_rows_raw     = nc,
        has_pii        = TRUE,
        n_pii_records  = nrow(pii)
      )
    )
    obj$pii <- pii
    obj
  })

  list(enrolled = enrolled_obj, clients = clients_obj)
}


# ---- Exported: Synthetic Staff ----------------------------------------------

#' Generate Synthetic Staff Data
#'
#' @description Creates a synthetic \code{alccdf_staff_clean} object with
#'   staff professional data, career lattice levels, and PII separation.
#'   Uses facility names as the linkage key back to programs.
#'
#' @param n Integer. Number of staff records. Default 60.
#' @param seed Integer. Random seed for reproducibility. Default 42.
#'
#' @return An \code{alccdf_staff_clean} S3 object with \code{$data},
#'   \code{$pii}, and \code{$meta} components.
#'
#' @examples
#' staff <- alccdf_synthetic_staff(n = 30, seed = 99)
#' staff
#' head(staff$data)
#'
#' @rdname synthetic-data
#' @export
alccdf_synthetic_staff <- function(n = 60, seed = 42) {
  .check_suggested("withr", "reproducible synthetic data generation")

  # Same base seed for facility pool

  fac <- .alccdf_synthetic_facilities(50, seed)
  snapshot_date <- Sys.Date()

  withr::with_seed(seed + 4000, {

    ns <- n

    # ---- Random staff IDs ----
    random_staff_id <- sprintf("STF%06d", sample(100000:999999, ns))

    # ---- Link ~65% to known facility names, rest to unmatched ----
    n_linked <- round(ns * 0.65)
    fac_idx <- c(
      sample(seq_len(nrow(fac)), n_linked, replace = TRUE),
      rep(NA_integer_, ns - n_linked)
    )
    fac_idx <- sample(fac_idx)

    unmatched_names <- c(
      "Sunrise Preschool", "Tiny Angels Day Care",
      "Kidz Zone Learning", "Little Lambs Academy",
      "Precious Gems Child Care", "Shooting Stars Learning",
      "Playhouse Academy", "Little Scholars Center",
      "Happy Trails Day Care", "Bright Future Childcare"
    )
    n_unmatched <- sum(is.na(fac_idx))
    facility_name <- ifelse(
      !is.na(fac_idx),
      fac$facility_name[fac_idx],
      sample(unmatched_names, n_unmatched, replace = TRUE)
    )

    facility_county <- ifelse(
      !is.na(fac_idx),
      fac$county[fac_idx],
      sample(c("Jefferson", "Mobile", "Montgomery", "Madison", "Tuscaloosa"),
             n_unmatched, replace = TRUE)
    )
    facility_county <- stringr::str_to_title(facility_county)

    provider_type <- ifelse(
      !is.na(fac_idx),
      as.character(fac$facility_type[fac_idx]),
      sample(c("Center", "Family Home", "Group Home"), n_unmatched, replace = TRUE)
    )

    # ---- Alabama ZIP codes for staff residence (35004-36925 range) ----
    # Representative Alabama ZIPs by region: Birmingham, Huntsville, Mobile,
    # Montgomery, Tuscaloosa, Dothan, Gadsden, Auburn, Decatur, Florence, etc.
    al_zips <- c(
      # Birmingham metro (Jefferson/Shelby)
      "35004", "35007", "35022", "35023", "35040", "35068", "35071", "35080",
      "35094", "35111", "35124", "35126", "35173", "35203", "35205", "35206",
      "35209", "35210", "35211", "35212", "35215", "35216", "35222", "35223",
      "35226", "35233", "35235", "35242", "35243", "35244",
      # Tuscaloosa
      "35401", "35404", "35405", "35406", "35453", "35473", "35476",
      # Decatur / Morgan / Limestone
      "35601", "35603", "35611", "35613", "35620", "35640", "35670",
      # Huntsville / Madison
      "35749", "35756", "35758", "35801", "35802", "35803", "35805", "35806",
      "35810", "35824",
      # Gadsden / Etowah / Marshall / DeKalb
      "35901", "35903", "35905", "35950", "35957", "35967", "35968",
      # Florence / Lauderdale / Colbert
      "35630", "35632", "35645", "35661",
      # Montgomery metro
      "36064", "36066", "36067", "36092", "36104", "36106", "36107",
      "36108", "36109", "36110", "36111", "36116", "36117",
      # Anniston / Calhoun / Talladega
      "35150", "35160", "36201", "36205", "36206", "36207",
      # Dothan / Houston / Dale
      "36301", "36303", "36305", "36310", "36322", "36330", "36360",
      # Selma / Black Belt
      "36701", "36703", "36720", "36744", "36756", "36786",
      # Mobile / Baldwin
      "36502", "36507", "36526", "36527", "36532", "36535", "36560",
      "36571", "36575", "36602", "36604", "36606", "36608", "36609",
      "36611", "36618", "36619", "36695",
      # Auburn / Lee / Russell
      "36801", "36830", "36832", "36849", "36867", "36869",
      # Opelika / Chambers / Randolph
      "36853", "36854", "36862", "36874", "36879"
    )
    user_zip <- sample(al_zips, ns, replace = TRUE)

    al_user_counties <- c(
      "Jefferson", "Mobile", "Montgomery", "Madison", "Tuscaloosa",
      "Baldwin", "Lee", "Shelby", "Morgan", "Houston",
      "Etowah", "Calhoun", "Lauderdale", "Limestone", "Marshall",
      "DeKalb", "Colbert", "Russell", "Talladega", "Elmore"
    )
    user_county <- stringr::str_to_title(
      sample(al_user_counties, ns, replace = TRUE)
    )

    # ---- Professional fields ----
    currently_operating <- sample(c(TRUE, FALSE), ns, replace = TRUE,
                                  prob = c(0.90, 0.10))

    position <- sample(
      c("Teacher", "Director", "Assistant", "Aide", "Lead Teacher"),
      ns, replace = TRUE,
      prob = c(0.35, 0.10, 0.25, 0.15, 0.15)
    )

    career_lattice_level <- sample(
      paste("Level", 1:5), ns, replace = TRUE,
      prob = c(0.15, 0.25, 0.30, 0.20, 0.10)
    )

    # ---- Build main data tibble ----
    staff_df <- tibble::tibble(
      random_staff_id      = random_staff_id,
      user_zip             = user_zip,
      user_county          = user_county,
      facility_name        = facility_name,
      facility_county      = facility_county,
      provider_type        = provider_type,
      currently_operating  = currently_operating,
      position             = position,
      career_lattice_level = career_lattice_level,
      snapshot_date        = snapshot_date
    )

    # ---- PII table ----
    first_names <- c("Mary", "Patricia", "Linda", "Barbara", "Elizabeth",
                     "Jennifer", "Maria", "Susan", "Margaret", "Lisa",
                     "Nancy", "Karen", "Betty", "Dorothy", "Sandra",
                     "Ashley", "Angela", "Donna", "Ruth", "Carol",
                     "Michelle", "Emily", "Amanda", "Melissa", "Deborah")
    last_names <- c("Smith", "Johnson", "Williams", "Brown", "Jones",
                    "Davis", "Miller", "Wilson", "Moore", "Taylor",
                    "Anderson", "Thomas", "Jackson", "White", "Harris",
                    "Martin", "Thompson", "Garcia", "Martinez", "Robinson")

    pii <- tibble::tibble(
      random_staff_id = random_staff_id,
      staff_name      = paste(sample(first_names, ns, replace = TRUE),
                              sample(last_names, ns, replace = TRUE)),
      user_email      = paste0(
        tolower(paste0(
          substr(sample(first_names, ns, replace = TRUE), 1, 1),
          sample(last_names, ns, replace = TRUE)
        )),
        sample(c("@gmail.com", "@yahoo.com", "@outlook.com",
                 "@aol.com", "@hotmail.com"), ns, replace = TRUE)
      )
    )

    obj <- .make_alccdf_obj(
      data          = staff_df,
      class_name    = "alccdf_staff_clean",
      module        = "staff",
      stage         = "clean",
      snapshot_date = snapshot_date,
      extra_meta    = list(
        source_file    = "synthetic",
        format_version = "synthetic",
        n_rows_raw     = ns,
        has_pii        = TRUE,
        n_pii_records  = nrow(pii)
      )
    )
    obj$pii <- pii
    obj
  })
}


# ---- Exported: Synthetic Melissa Geocoding ----------------------------------

#' Generate Synthetic Melissa Geocoding Data
#'
#' @description Creates synthetic Melissa.com geocoding results for both
#'   program addresses and household (client) addresses. Returns plain
#'   tibbles (not S3 objects) matching the structure produced by
#'   \code{\link{melissa_import_programs}} and
#'   \code{\link{melissa_import_households}}.
#'
#' @param programs An optional \code{alccdf_program_clean} object. If
#'   \code{NULL} (default), uses \code{alccdf_synthetic_programs(seed = seed)}
#'   to generate addresses.
#' @param seed Integer. Random seed for reproducibility. Default 42.
#'
#' @return A named list with:
#'   \describe{
#'     \item{programs}{A tibble with geocoded program addresses}
#'     \item{households}{A tibble with geocoded household addresses}
#'   }
#'
#' @examples
#' mel <- alccdf_synthetic_melissa(seed = 42)
#' head(mel$programs)
#' head(mel$households)
#'
#' @rdname synthetic-data
#' @export
alccdf_synthetic_melissa <- function(programs = NULL, seed = 42) {
  .check_suggested("withr", "reproducible synthetic data generation")

  # Resolve program addresses

  if (is.null(programs)) {
    programs <- alccdf_synthetic_programs(n = 50, seed = seed)
  }
  prog_data <- programs$data

  withr::with_seed(seed + 5000, {

    np <- nrow(prog_data)

    # ---- Alabama bounding box ----
    lat_min <- 30.14
    lat_max <- 35.01
    lng_min <- -88.48
    lng_max <- -84.89

    # ---- Programs geocoding tibble ----
    # Address in UPPERCASE with county suffix (matching Melissa output format)
    prog_address <- toupper(prog_data$facility_address)
    prog_address <- paste0(prog_address, ", ", toupper(as.character(prog_data$county)),
                           " COUNTY")

    prog_lat <- round(runif(np, lat_min, lat_max), 6)
    prog_lng <- round(runif(np, lng_min, lng_max), 6)

    # Census geography
    prog_tract <- round(runif(np, 100, 999999), 0)
    prog_block <- round(runif(np, 1000, 9999), 0)
    prog_fips <- sample(c(1073, 1097, 1101, 1089, 1125, 1003, 1081, 1117,
                          1103, 1069, 1055, 1015, 1077, 1083, 1095),
                        np, replace = TRUE)

    al_counties <- as.character(prog_data$county)
    al_cities <- gsub(",.*", "", gsub("^[0-9]+ [A-Za-z ]+ , ", "",
                                      as.character(prog_data$facility_address)))
    # Extract city more reliably
    addr_parts <- strsplit(as.character(prog_data$facility_address), ", ")
    al_cities <- vapply(addr_parts, function(x) if (length(x) >= 2) x[2] else "Unknown", "")

    al_zips <- gsub(".*AL ", "", as.character(prog_data$facility_address))

    melissa_result_code <- sample(c("GS05", "GS06", "GS03", "GS01"),
                                  np, replace = TRUE,
                                  prob = c(0.70, 0.15, 0.10, 0.05))
    melissa_status_code <- sample(c("SE01", "SE01,GE02", "AE01"),
                                  np, replace = TRUE,
                                  prob = c(0.80, 0.15, 0.05))
    melissa_place_code <- sample(10000:99999, np, replace = TRUE)

    programs_tbl <- tibble::tibble(
      facility_address    = prog_address,
      latitude            = prog_lat,
      longitude           = prog_lng,
      census_tract        = prog_tract,
      census_block        = prog_block,
      fips_code           = prog_fips,
      melissa_county      = al_counties,
      melissa_place       = al_cities,
      melissa_place_code  = melissa_place_code,
      melissa_city        = al_cities,
      melissa_state       = rep("AL", np),
      melissa_zip         = al_zips,
      melissa_geo_zip     = al_zips,
      melissa_result_code = melissa_result_code,
      melissa_status_code = melissa_status_code
    )

    # ---- Households geocoding tibble ----
    nh <- 40
    hh_street_nums <- sample(100:9999, nh, replace = TRUE)
    hh_streets <- c("Oak Lane", "Maple Drive", "Cedar Court", "Pine Way",
                    "Elm Street", "Birch Road", "Walnut Avenue",
                    "Cypress Circle", "Magnolia Place", "Dogwood Lane",
                    "Holly Street", "Pecan Drive", "Willow Court",
                    "Hickory Lane", "Poplar Avenue", "Azalea Way",
                    "Jasmine Court", "Iris Circle", "Daisy Lane",
                    "Rose Boulevard")
    hh_cities <- c("Birmingham", "Huntsville", "Mobile", "Montgomery",
                   "Tuscaloosa", "Auburn", "Dothan", "Decatur",
                   "Florence", "Gadsden")
    hh_zips <- c("35203", "35801", "36602", "36104",
                 "35401", "36830", "36301", "35601",
                 "35630", "35901")
    hh_counties <- c("Jefferson", "Madison", "Mobile", "Montgomery",
                     "Tuscaloosa", "Lee", "Houston", "Morgan",
                     "Lauderdale", "Etowah")

    hh_city_idx <- sample(seq_along(hh_cities), nh, replace = TRUE)
    hh_address <- toupper(paste0(
      hh_street_nums, " ", sample(hh_streets, nh, replace = TRUE), ", ",
      hh_cities[hh_city_idx], ", AL ", hh_zips[hh_city_idx], ", ",
      hh_counties[hh_city_idx], " COUNTY"
    ))

    hh_lat <- round(runif(nh, lat_min, lat_max), 6)
    hh_lng <- round(runif(nh, lng_min, lng_max), 6)
    hh_tract <- round(runif(nh, 100, 999999), 0)
    hh_block <- round(runif(nh, 1000, 9999), 0)
    hh_fips <- sample(c(1073, 1097, 1101, 1089, 1125, 1003, 1081, 1117,
                        1103, 1069), nh, replace = TRUE)
    hh_result_code <- sample(c("GS05", "GS06", "GS03", "GS01"),
                             nh, replace = TRUE,
                             prob = c(0.70, 0.15, 0.10, 0.05))
    hh_status_code <- sample(c("SE01", "SE01,GE02", "AE01"),
                             nh, replace = TRUE,
                             prob = c(0.80, 0.15, 0.05))
    hh_place_code <- sample(10000:99999, nh, replace = TRUE)

    households_tbl <- tibble::tibble(
      facility_address    = hh_address,
      latitude            = hh_lat,
      longitude           = hh_lng,
      census_tract        = hh_tract,
      census_block        = hh_block,
      fips_code           = hh_fips,
      melissa_county      = hh_counties[hh_city_idx],
      melissa_place       = hh_cities[hh_city_idx],
      melissa_place_code  = hh_place_code,
      melissa_city        = hh_cities[hh_city_idx],
      melissa_state       = rep("AL", nh),
      melissa_zip         = hh_zips[hh_city_idx],
      melissa_geo_zip     = hh_zips[hh_city_idx],
      melissa_result_code = hh_result_code,
      melissa_status_code = hh_status_code
    )

    list(programs = programs_tbl, households = households_tbl)
  })
}
