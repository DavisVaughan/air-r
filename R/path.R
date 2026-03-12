air_path <- function(..., version = NULL) {
  check_dots_empty0(...)

  path <- air_path_sitrep(
    verbose = FALSE,
    version = version,
    offer_download = TRUE
  )

  if (is.null(path)) {
    abort("Can't find air.")
  }

  path
}

air_sitrep <- function(..., version = NULL) {
  check_dots_empty0(...)

  air_path_sitrep(
    verbose = TRUE,
    version = version,
    offer_download = FALSE
  )

  invisible()
}

# Returns a path to an air binary or `NULL`
air_path_sitrep <- function(verbose, version, offer_download) {
  path <- air_path_which(verbose, version)
  if (!is.null(path)) {
    return(path)
  }

  path <- air_path_cache(verbose, version, offer_download)
  if (!is.null(path)) {
    return(path)
  }

  cli_failure(verbose, "Can't find air.")
  NULL
}

air_path_which <- function(verbose, version) {
  path <- unname(Sys.which("air"))

  if (string_is_empty(path)) {
    cli_info(verbose, "Can't find air via `which`.")
    return(NULL)
  }

  if (!is.null(version)) {
    actual_version <- air_version(path)

    if (!identical(version, actual_version)) {
      cli_info(
        verbose,
        "Found air via `which` at {.path {path}}, but version was incompatible.",
        bullets = c(
          "Expected: {version}",
          "Actual: {actual_version}"
        )
      )
      return(NULL)
    }
  }

  cli_success(verbose, "Found air via `which` at {.path {path}}.")
  path
}

air_path_cache <- function(verbose, version, offer_download) {
  if (is.null(version)) {
    abort("TODO: Resolve latest to a version number")
    # version <- resolve_latest_version()
  }

  path <- cache_get(version)

  if (!is.null(path)) {
    cli_info(verbose, "Found air {version} via cache.")
    return(path)
  }

  if (offer_download) {
    cli::cli_inform("Can't find air {version} in cache. Download it?")

    answer <- menu(choices = c("Yes", "No"))
    yes <- identical(answer, 1L)

    if (yes) {
      # Always verbose!
      path <- cache_download(verbose = TRUE, version = version)
      return(path)
    }
  }

  cli_info(verbose, "Can't find air via cache.")
  NULL
}
