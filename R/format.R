format_text <- function(text) {
  check_string(text)

  # TODO!: Use `--isolated` when implemented.
  # We always want `format_text()` to use predictable defaults.
  # For anything more complicated, write to your project's file system
  # and use `format_file()`.

  result <- air_run(
    command = air_path(),
    args = c(
      "format",
      "--stdin-file-path",
      "dummy.R"
    ),
    stdin = text
  )

  check_status("Can't format text.", result$status, result$stderr)

  result$stdout
}

format_file <- function(path) {
  check_string(path)

  # TODO: Maybe pre check that the file exists?
  path <- normalizePath(path, mustWork = FALSE)

  result <- air_run(
    command = air_path(),
    args = c(
      "format",
      path
    )
  )

  check_status("Can't format file.", result$status, result$stderr)

  invisible()
}

check_status <- function(message, status, stderr, error_call = caller_env()) {
  if (status == 0L) {
    return(invisible())
  }

  if (!is.null(stderr)) {
    bullet <- paste0(
      "Reported error:\n\n",
      "```\n",
      stderr,
      "```"
    )
  } else {
    bullet <- NULL
  }

  abort(c(message, i = bullet), call = error_call)
}
