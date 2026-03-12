air_version <- function(command = air_path()) {
  result <- air_run(
    command = command,
    args = "--version"
  )

  if (result$status != 0L) {
    abort("Can't get version due to: {result$stderr}")
  }

  stdout <- result$stdout

  # TODO: Better parsing? Should air be able to report just the version number?
  stdout <- gsub("air", "", stdout, fixed = TRUE)
  stdout <- trimws(stdout)

  # TODO: Error if it isn't right
  stdout
}
