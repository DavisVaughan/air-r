cache_get <- function(version) {
  path <- cache_path(version)

  if (file.exists(path)) {
    path
  } else {
    NULL
  }
}

cache_download <- function(verbose, version) {
  cli_info(verbose, "Downloading air {version}")
  path <- switch(
    system_os(),
    macos = cache_download_unix(version),
    linux = cache_download_unix(version),
    windows = cache_download_windows(version)
  )
  cli_success(verbose, "Cached air {version}")
  path
}

cache_download_unix <- function(version) {
  target_triple <- target_triple()
  stem <- cli::format_inline("air-{target_triple}")

  url <- cli::format_inline(
    "https://github.com/posit-dev/air/releases/download/{version}/{stem}.tar.gz"
  )

  from_directory <- file.path(tempdir(), "air", version)
  dir_create(from_directory)

  to_directory <- cache_version_directory(version)
  dir_create(to_directory)

  path_archive <- file.path(from_directory, "air.tar.gz")
  download.file(url, destfile = path_archive, quiet = TRUE)
  untar(path_archive, exdir = from_directory)

  from_path <- file.path(from_directory, stem, "air")
  to_path <- file.path(to_directory, "air")
  file.copy(from_path, to_path)

  to_path
}

cache_download_windows <- function(version) {
  target_triple <- target_triple()
  stem <- cli::format_inline("air-{target_triple}")

  url <- cli::format_inline(
    "https://github.com/posit-dev/air/releases/download/{version}/{stem}.zip"
  )

  from_directory <- file.path(tempdir(), "air", version)
  dir_create(from_directory)

  to_directory <- cache_version_directory(version)
  dir_create(to_directory)

  path_archive <- file.path(from_directory, "air.zip")
  download.file(url, destfile = path_archive, quiet = TRUE)
  unzip(path_archive, exdir = from_directory)

  from_path <- file.path(from_directory, "air.exe")
  to_path <- file.path(to_directory, "air.exe")

  if (!file.copy(from_path, to_path)) {
    cli::cli_abort("Failed to copy {.path {from_path}} to {.path {to_path}}.")
  }

  to_path
}

cache_path <- function(version) {
  file.path(cache_version_directory(version), air_executable_name())
}

cache_version_directory <- function(version) {
  file.path(cache_binaries_directory(), version)
}

cache_binaries_directory <- function() {
  file.path(cache_root(), "binaries")
}

cache_root <- function() {
  tools::R_user_dir("air", which = "cache")
}

air_executable_name <- function() {
  if (is_windows()) {
    "air.exe"
  } else {
    "air"
  }
}

target_triple <- function() {
  arch <- system_arch()
  target <- switch(
    system_os(),
    macos = "apple-darwin",
    windows = "pc-windows-msvc",
    linux = "unknown-linux-gnu"
  )
  paste0(arch, "-", target)
}

is_windows <- function() {
  system_os() == "windows"
}

# Returns `"macos"`, `"windows"`, `"linux"`, or `NULL` if unknown
system_os <- function() {
  switch(
    tolower(Sys.info()[["sysname"]]),
    "darwin" = "macos",
    "windows" = "windows",
    "linux" = "linux",
    abort("Unsupported OS")
  )
}

# Non-aarch64 machines are unlikely to all be x86_64 ones,
# but for Linux that's our universal non-aarch64 build and
# probably works for most people.
system_arch <- function() {
  if (identical(R.version$arch, "aarch64")) {
    "aarch64"
  } else {
    "x86_64"
  }
}

dir_create <- function(path) {
  if (dir.exists(path)) {
    return(invisible())
  }

  if (!dir.create(path, recursive = TRUE)) {
    cli::cli_abort("Failed to create directory at {.path {path}}.")
  }

  invisible()
}
