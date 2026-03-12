cli_success <- function(
  verbose,
  message,
  ...,
  bullets = NULL,
  env = caller_env()
) {
  check_dots_empty0(...)
  cli_message(
    verbose = verbose,
    message = message,
    type = "v",
    bullets = bullets,
    env = env
  )
}

cli_failure <- function(
  verbose,
  message,
  ...,
  bullets = NULL,
  env = caller_env()
) {
  check_dots_empty0(...)
  cli_message(
    verbose = verbose,
    message = message,
    type = "x",
    bullets = bullets,
    env = env
  )
}

cli_info <- function(
  verbose,
  message,
  ...,
  bullets = NULL,
  env = caller_env()
) {
  check_dots_empty0(...)
  cli_message(
    verbose = verbose,
    message = message,
    type = "i",
    bullets = bullets,
    env = env
  )
}

cli_message <- function(
  verbose,
  message,
  type,
  bullets,
  env
) {
  if (!verbose) {
    return(invisible())
  }

  message <- set_names(message, type)

  if (!is.null(bullets)) {
    bullets <- set_names(bullets, " ")
    message <- c(message, bullets)
  }

  cli::cli_inform(message, .envir = env)
}

string_is_empty <- function(x) {
  as.logical(!nzchar(x))
}
