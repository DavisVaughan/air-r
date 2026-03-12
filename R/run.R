air_run <- function(command, args, ..., stdin = NULL) {
  check_dots_empty0(...)

  # Never with color!
  args <- c(args, "--no-color")

  has_stdin <- !is.null(stdin)

  if (has_stdin) {
    # `processx::run()` only supports stdin from a file, but we REALLY want to
    # use `run()` anyways over managing a `process$new()` manually. It is way
    # more complicated and we don't get `run()`'s managed event loop that polls
    # both stdout and stderr simultaneously, which is useful for us because we
    # want to keep them separated. Ideally one day `processx::run()` will just
    # let us provide a string for stdin directly, Gabor said he'd like to, but
    # right now has no way to poll for stdin, or something like that.
    stdin_file <- tempfile()
    stdin <- enc2utf8(stdin)
    writeLines(stdin, stdin_file)
    defer(unlink(stdin_file))
  } else {
    stdin_file <- NULL
  }

  processx::run(
    command = command,
    args = args,
    stdin = stdin_file,
    stdout = "|",
    stderr = "|",
    encoding = "UTF-8",
    error_on_status = FALSE
  )
}
