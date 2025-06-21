library(here)
library(rvest)
library(stringr)
library(cli)

inmet_find_links <- function(
    url = "https://portal.inmet.gov.br/dadoshistoricos",
    xpath = "//article[@class='post-preview']/a"
    ) {
  
  download_links <- xml2::read_html(url) |> 
    rvest::html_elements(xpath = xpath) |> 
    rvest::html_attr("href")
  
  return(download_links)
}

inmet_download_data <- function(data_dir = NULL, overwrite = FALSE) {
  
  # Create directories with error handling
  data_dir <- here("static", "data-raw", "data", "inmet")
  dest_dir <- here("static", "data-raw", "data", "inmet", "zipped_files")
  
  # Use cli to create directories with informative messages
  cli::cli_alert_info("Creating directories...")
  try({
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  })
  
  # Fetch download links
  cli::cli_alert_info("Fetching download links...")
  download_links <- inmet_find_links()
  
  # Check if files already are downloaded
  check <- list.files(dest_dir, pattern = "\\.zip$")
  
  if (length(check) > 0) {
    cli::cli_warn("Zipped files detected in {data_dir}.")
    
    if (!overwrite) {
      cli::cli_warn("To overwrite files use `overwrite = TRUE`.")
      return(NULL)
    }
    
  }

  # Prepare destination files
  dest_file <- here(dest_dir, basename(download_links))
  
  # Download files with cli progress bar
  cli_alert_info("Starting download of {length(download_links)} files")
  cli_progress_bar(
    total = length(download_links),
    format = paste(
      "{cli::pb_spin} Downloading {current_year}",
      "({cli::pb_current}/{cli::pb_total})",
      "| {cli::pb_percent}"
    )
  )
  
  # Vectorized error handling
  safely_download <- function(url, destfile) {
    tryCatch(
      download.file(url, destfile = destfile, mode = "wb", quiet = TRUE),
      error = function(e) {
        cli_alert_danger("Failed to download {basename(destfile)}: {e$message}")
        return(NULL)
      }
    )
  }
  
  # Download loop
  for (i in seq_along(download_links)) {
    current_year <- gsub("\\.zip", "", basename(download_links)[[i]])
    
    safely_download(download_links[[i]], dest_file[[i]])
    
    cli_progress_update()
  }
  
  cli_progress_done()
  cli_alert_success("Download complete!")

}

inmet_download_data()

# Create directories with error handling
data_dir <- here("static", "data-raw", "data", "inmet")
dest_dir <- here("static", "data-raw", "data", "inmet", "zipped_files")

# Use cli to create directories with informative messages
cli_alert_info("Creating directories...")
try({
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
})

# Fetch download links
cli_alert_info("Fetching download links...")
url <- "https://portal.inmet.gov.br/dadoshistoricos"
download_links <- read_html(url) |> 
  html_elements(xpath = "//article[@class='post-preview']/a") |> 
  html_attr("href")

# Prepare destination files
dest_file <- here(dest_dir, basename(download_links))

# Download files with cli progress bar
cli_alert_info("Starting download of {length(download_links)} files")
cli_progress_bar(
  total = length(download_links),
  format = paste(
    "{cli::pb_spin} Downloading {current_year}",
    "({cli::pb_current}/{cli::pb_total})",
    "| {cli::pb_percent}"
  )
)

# Vectorized error handling
safely_download <- function(url, destfile) {
  tryCatch(
    download.file(url, destfile = destfile, mode = "wb", quiet = TRUE),
    error = function(e) {
      cli_alert_danger("Failed to download {basename(destfile)}: {e$message}")
      return(NULL)
    }
  )
}

# Download loop
for (i in seq_along(download_links)) {
  current_year <- gsub("\\.zip", "", basename(download_links)[[i]])
  
  safely_download(download_links[[i]], dest_file[[i]])
  
  cli_progress_update()
}

cli_progress_done()
cli_alert_success("Download complete!")


data_dir <- here("static", "data-raw", "data", "inmet", "zipped_files")
zip_file <- list.files(data_dir, pattern = "zip$", full.names = TRUE)
out_dir <- here(data_dir, gsub("\\.zip", "", basename(zip_file)))

cli::cli_progress_bar(
  total = length(dest_file),
  format = "{cli::pb_spin} Unzipping data for year: {current_year} [{cli::pb_current}/{cli::pb_total}]"
)

for (i in seq_along(zip_file)) {
  
  current_year <- gsub("\\.zip", "", basename(zip_file)[[i]])
  out_dir <- here(data_dir, paste0("inmet_", current_year))
  
  utils::unzip(zip_file[[i]], exdir = out_dir)
  cli::cli_progress_update()
  
}

get_colnames <- function(x, validate = TRUE, case_sensitive = FALSE, return_type = "vector") {
  # Comprehensive column name definitions with additional metadata
  col_names <- list(
    csv = list(
      names = c(
        "data_ymd",
        "hora_utc",
        "precipitacao_total_mm",
        "pressao_atm_hora_mB",
        "pressao_atm_max_mB",
        "pressao_atm_min_mB",
        "radiacao_global_kj_m2", 
        "temp_bulbo_hora",
        "temp_orvalho_hora",
        "temp_max",
        "temp_min",
        "temp_orvalho_max",
        "temp_orvalho_min",
        "umidade_rel_max",
        "umidade_rel_min",
        "umidade_rel_hora",
        "vento_direcao",
        "vento_rajada_max",
        "vento_velocidade",
        "drop_col"
      ),
      description = "Meteorological CSV data columns",
      type = "numeric"
    ),
    metadata = list(
      names = c(
        "source",
        "name_region",
        "name_state",
        "code_station", 
        "name_station",
        "date_start",
        "drop_col",
        "date_end"
      ),
      description = "Metadata columns for station information",
      type = "categorical"
    ),
    station = list(
      names = c(
        "name_station",
        "name_state",
        "st_situation",
        "st_lat",
        "st_lng",
        "st_z",
        "st_date_start_operation",
        "code_station"
      ),
      description = "Station-specific details",
      type = "mixed"
    )
  )

  # Input validation with enhanced error handling
  return_type <- match.arg(return_type)
  
  # Flexible matching with optional case sensitivity
  if (!case_sensitive) {
    x <- tolower(x)
    input_names <- tolower(names(col_names))
  } else {
    input_names <- names(col_names)
  }
  
  # Validate input
  if (validate) {
    tryCatch({
      # Check if input matches any predefined column set
      match_index <- match(x, input_names)
      
      if (is.na(match_index)) {
        cli::cli_alert_danger(
          "Invalid column set. Available sets: {paste(names(col_names), collapse = ', ')}"
        )
        stop("Invalid column set")
      }
    }, error = function(e) {
      cli::cli_alert_danger("Error in column set validation: {e$message}")
      return(NULL)
    })
  }
  
  # Retrieve column names
  result <- col_names[match(x, input_names)]
  
  # Optional verbose output
  if (validate) {
    cli::cli_alert_info(
      "Retrieved column set: {result[[1]]$description}"
    )
  }
  
  # Return based on specified type
  if (return_type == "vector") {
    result <- result[[1]]$names
  }
  
  return(result)
}

get_dim_station <- function(st_dir = NULL, verbose = FALSE) {
  # Use cli for informative messages
  cli::cli_h1("Retrieving Dimension Stations")
  
  # Default directory with error handling
  if (is.null(st_dir)) {
    tryCatch({
      st_dir <- here::here("static", "data-raw", "data", "inmet", "dim_stations")
    }, error = function(e) {
      cli::cli_alert_danger("Error creating default directory path: {e$message}")
      stop("Cannot determine default directory")
    })
  }
  
  # Validate directory existence
  if (!dir.exists(st_dir)) {
    cli::cli_alert_danger("Directory does not exist: {st_dir}")
    stop("Invalid directory path")
  }
  
  # Find CSV files with error handling
  csv_files <- list.files(st_dir, pattern = "\\.csv$", full.names = TRUE)
  
  # Validate file count
  if (length(csv_files) != 2) {
    cli::cli_alert_warning(
      "Expected 2 CSV files, found {length(csv_files)}: {paste(csv_files, collapse = ', ')}"
    )
    stop("Incorrect number of CSV files")
  }
  
  # Optimized file reading with error handling
  safely_fread <- function(file) {
    tryCatch({
      data.table::fread(file, 
                        showProgress = verbose, 
                        stringsAsFactors = FALSE, 
                        encoding = "UTF-8")
    }, error = function(e) {
      cli::cli_alert_danger("Error reading file {basename(file)}: {e$message}")
      return(NULL)
    })
  }
  
  # Parallel file reading (optional, requires future package)
  if (requireNamespace("future.apply", quietly = TRUE)) {
    future::plan(future::multisession)
    ls_dim_station <- future.apply::future_lapply(csv_files, safely_fread)
    future::plan(future::sequential)
  } else {
    ls_dim_station <- lapply(csv_files, safely_fread)
  }
  
  # Remove any NULL entries from failed reads
  ls_dim_station <- Filter(Negate(is.null), ls_dim_station)
  
  # Combine data tables
  dim_station <- data.table::rbindlist(ls_dim_station, fill = TRUE)
  
  # Get and set column names with error handling
  tryCatch({
    col_names <- get_colnames("station", return_type = "vector")
    data.table::setnames(dim_station, names(dim_station), col_names)
  }, error = function(e) {
    cli::cli_alert_danger("Error setting column names: {e$message}")
    stop("Column name assignment failed")
  })
  
  # Date parsing with error handling
  tryCatch({
    dim_station[, st_date_start_operation := lubridate::dmy(st_date_start_operation)]
  }, error = function(e) {
    cli::cli_alert_warning("Error parsing date column: {e$message}")
    # Optionally, keep original column or handle differently
  })
  
  # Optional verbose output
  if (verbose) {
    cli::cli_alert_success("Dimension stations data loaded successfully")
    cli::cli_alert_info("Rows: {nrow(dim_station)}, Columns: {ncol(dim_station)}")
  }
  
  return(dim_station)
}

get_metadata <- function(path, include_path = FALSE, verbose = FALSE) {
  # Validate input
  if (!file.exists(path)) {
    cli::cli_alert_danger("File not found: {path}")
    return(NULL)
  }
  
  # Expected column names
  col_names <- get_colnames("station")
  
  # Extract filename without extension
  name_file <- tools::file_path_sans_ext(basename(path))
  
  # Split filename and create metadata
  tryCatch({
    # Use stringr for robust splitting
    metadata_parts <- stringr::str_split(name_file, "_", simplify = TRUE)
    
    # Validate number of parts
    if (length(metadata_parts) != length(col_names)) {
      cli::cli_alert_warning(
        "Filename does not match expected format: {name_file}"
      )
      return(NULL)
    }
    
    # Create data.table with proper column names
    metadata_station <- data.table::data.table(
      matrix(metadata_parts, nrow = 1, dimnames = list(NULL, col_names))
    )
    
    metadata_station[, drop_col := NULL]
    
    # Optionally include file path
    if (include_path) {
      metadata_station[, path := path]
    }
    
    # Optional verbose output
    if (verbose) {
      cli::cli_alert_info("Metadata extracted successfully:")
      print(metadata_station)
    }
    
    return(metadata_station)
    
  }, error = function(e) {
    cli::cli_alert_danger("Error processing metadata: {e$message}")
    return(NULL)
  })
}

find_path_csv <- function(data_dir = NULL) {
  
  if (is.null(data_dir)) {
    data_dir <- here("static", "data-raw", "data", "inmet", "zipped_files")
  }
  
  path_files <- list.files(
    data_dir,
    pattern = "\\.CSV$|\\.csv$",
    recursive = TRUE,
    full.names = TRUE
    )
  
  return(path_files)
  
}

# Find all csv files


dim_station <- get_dim_station()


path <- path_files[5]
metadata <- get_metadata(path)
metadata[, name_station := NULL]
metadata[, name_state := NULL]
metadata <- merge(metadata, dim_station, by = "code_station", all.x = TRUE)

inmet_read_csv <- function(path, skip = 8) {
  
  dat <- data.table::fread(
    path,
    skip = skip,
    colClasses = c("Date", "character", rep("numeric", 18)),
    na.strings = "-9999",
    encoding = "Latin-1"
  )
  
  col_names <- get_colnames("csv")
  data.table::setnames(dat, names(dat), col_names)
  
  return(dat)
  
}

inmet_clean_data <- function(dat, include_metadata = TRUE) {
  
  if (!is.data.table(dat)) {
    data.table::setDT(dat)
  }
  
  dat[, data_ymd_hms := lubridate::ymd_hm(paste(data_ymd, hora_utc))]
  dat[, data_hora := lubridate::hour(data_ymd_hms)]
  dat[, data_dia := lubridate::day(data_ymd)]
  dat[, data_mes := lubridate::month(data_ymd)]
  
  return(dat)
  
}

import_inmet_csv <- function(path, include_metadata = TRUE) {
  
  dat <- inmet_read_csv(path)
  dat <- inmet_clean_data(dat)
  
  if (include_metadata) {
    metadata <- get_metadata(path)
    dat <- cbind(dat, metadata)
  }
  return(dat)
  
}

import_inmet <- function(station = "all", include_metadata = TRUE) {

  if (station == "all") {
    
    ls <- parallel::mclapply(path_files, import_inmet_csv, include_metadata)
    dat <- data.table::rbindlist(ls)
    return(dat)
    
  }
  
  available_stations <- c(
    unique(metadata_path$name_station),
    unique(metadata_path$code_station)
  )
  
  if (all(station %in% available_stations)) {
    
    sub_station <- metadata_path[code_station %in% station | name_station %in% station]
    sub_path <- sub_station[["path"]]
    ls <- parallel::mclapply(sub_path, import_inmet_csv, include_metadata)
    dat <- data.table::rbindlist(ls)
    return(dat)
    
  }
  
}

inmet_list_stations <- function() {
  
  path_metadata <- lapply(path_files, get_metadata, include_path = FALSE)
  metadata_path <- rbindlist(path_metadata)
  return(metadata_path)
  
}

inmet_list_stations()

import_inmet("BRASILIA")







metadata_path[name_station == "BRASILIA"]$path

get_metadata(path_files)

dim_station[name_station == "BRASILIA"]

import_inmet(path_files[231])

dat <- data.table::fread(
  path,
  skip = 8,
  colClasses = c("Date", "character", rep("numeric", 18)),
  na = "-9999",
  encoding = "Latin-1"
)

col_names <- get_colnames("csv")
data.table::setnames(dat, names(dat), col_names)

dat[, data_ymd_hms := lubridate::ymd_hm(paste(data_ymd, hora_utc))]
dat[, data_hora := lubridate::hour(data_ymd_hms)]
dat[, data_dia := lubridate::day(data_ymd)]
dat[, data_mes := lubridate::month(data_ymd)]

cbind(dat, metadata)

stringr::str_extract()

gsub("\\.CSV|\\.csv", "", basename(path))

dim_station[dc_nome == "BRASILIA"]

import_inmet <- function(path) {
  
  dat <- import_csv(path)
  
  
}

import_csv <- function(path) {
  
  dat <- data.table::fread(
    path,
    skip = 8,
    colClasses = c("Date", "character", rep("numeric", 18)),
    na = "-9999",
    encoding = "Latin-1"
  )
  
  return(dat)
  
}

find_station <- function(path) {
  
}

clean_inmet <- function(dat) {
  
  dat <- janitor::clean_names(dat)
  
  col_names <- c(
    "data_ymd",
    "hora_utc",
    "precipitacao_total_mm",
    "pressao_atm_hora_mB",
    "pressao_atm_max_mB",
    "pressao_atm_min_mB",
    "radiacao_global_kj_m2", 
    "temp_bulbo_hora",
    "temp_orvalho_hora",
    "temp_max",
    "temp_min",
    "temp_orvalho_max",
    "temp_orvalho_min",
    "umidade_rel_max",
    "umidade_rel_min",
    "umidade_rel_hora",
    "vento_direcao",
    "vento_rajada_max",
    "vento_velocidade",
    "drop_col"
  )
  
  data.table::setnames(dat, names(dat), col_names)
  
  dat[, data_ymd_hms := lubridate::ymd_hm(paste(data_ymd, hora_utc))]
  dat[, data_hora := lubridate::hour(data_ymd_hms)]
  dat[, data_dia := lubridate::day(data_ymd)]
  dat[, data_mes := lubridate::month(data_ymd)]
  return(dat)
  dat <- cbind(dat, dat_station)
  
}

dat_station <- dim_station[
  cd_estacao == code_station[[1]],
  .(dc_nome, cd_estacao)
  ]

dat <- data.table::fread(
  path_files[1],
  skip = 8,
  colClasses = c("Date", "character", rep("numeric", 18)),
  na = "-9999",
  encoding = "Latin-1"
  )

dat <- janitor::clean_names(dat)

col_names <- c(
  "data_ymd",
  "hora_utc",
  "precipitacao_total_mm",
  "pressao_atm_hora_mB",
  "pressao_atm_max_mB",
  "pressao_atm_min_mB",
  "radiacao_global_kj_m2", 
  "temp_bulbo_hora",
  "temp_orvalho_hora",
  "temp_max",
  "temp_min",
  "temp_orvalho_max",
  "temp_orvalho_min",
  "umidade_rel_max",
  "umidade_rel_min",
  "umidade_rel_hora",
  "vento_direcao",
  "vento_rajada_max",
  "vento_velocidade",
  "drop_col"
)

data.table::setnames(dat, names(dat), col_names)

dat[, data_ymd_hms := lubridate::ymd_hm(paste(data_ymd, hora_utc))]
dat[, data_hora := lubridate::hour(data_ymd_hms)]
dat[, data_dia := lubridate::day(data_ymd)]
dat[, data_mes := lubridate::month(data_ymd)]

dat <- cbind(dat, dat_station)