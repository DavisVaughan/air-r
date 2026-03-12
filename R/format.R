format_text <- function(
  text,
  ...,
  file = NULL,
  version = NULL
) {
  check_dots_empty0(...)

  if (is.null(file)) {
    # Process is launched from current working directory, so it is
    # `${pwd}/dummy.R` for configuration search purposes. Always want to force
    # it, since `dummy.R` is just a side effect of how we have to call it.
    file <- "dummy.R"
    force <- "--force"
  } else {
    force <- NULL
  }

  args <- c(
    "format",
    "--stdin-file-path",
    file,
    force
  )

  result <- air_run(
    command = air_path(version = version),
    args = args,
    stdin = text
  )

  if (result$status != 0L) {
    if (!is.null(result$stderr)) {
      bullet <- paste0(
        "Reported error:\n\n",
        "```\n",
        result$stderr,
        "```"
      )
    } else {
      bullet <- NULL
    }

    abort(c(
      "Can't format text.",
      i = bullet
    ))
  }

  result$stdout
}

# TODO
format_file <- function() {}
