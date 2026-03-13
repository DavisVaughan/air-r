air_path <- function() {
  path <- air_path_impl(verbose = FALSE)

  if (is.null(path)) {
    stop_cant_find_air()
  }

  path
}

air_path_sitrep <- function() {
  air_path_impl(verbose = TRUE)
  invisible()
}

# Returns a path to an air binary or `NULL`
air_path_impl <- function(verbose) {
  path <- air_path_which(verbose)
  if (!is.null(path)) {
    return(invisible(path))
  }

  cli_failure(verbose, "Can't find {.code air}.")
  NULL
}

air_path_which <- function(verbose) {
  path <- unname(Sys.which("air"))

  if (string_is_empty(path)) {
    cli_info(verbose, "Can't find {.code air} via {.code which}.")
    return(NULL)
  }

  cli_success(verbose, "Found {.code air} via {.code which} at {.path {path}}.")
  path
}

stop_cant_find_air <- function(error_call = caller_env()) {
  message <- paste(
    sep = "\n",
    cli::format_inline("Can't find {.code air}."),
    "",
    "The Air command line tool is required.",
    "Install the latest version of Air by running one of the following in your terminal.",
    "",
    "For macOS / Linux:",
    "",
    "```",
    "curl -LsSf https://github.com/posit-dev/air/releases/latest/download/air-installer.sh | sh",
    "```",
    "",
    "For Windows:",
    "",
    "```",
    "powershell -ExecutionPolicy Bypass -c \"irm https://github.com/posit-dev/air/releases/latest/download/air-installer.ps1 | iex\"",
    "```",
    "",
    cli::format_inline(
      "Visit {.url https://github.com/posit-dev/air} for more information."
    )
  )

  abort(message, call = error_call)
}
