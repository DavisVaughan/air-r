air_run <- function(command, args, ..., stdin = NULL) {
  check_dots_empty0(...)

  has_stdin <- !is.null(stdin)

  args <- c(args, "--no-color")

  p <- processx::process$new(
    command = command,
    args = args,
    stdin = if (has_stdin) "|" else NULL,
    stdout = "|",
    stderr = "|",
    encoding = "UTF-8"
  )

  defer(p$kill())

  if (has_stdin) {
    stdin <- enc2utf8(stdin)

    # callr:::rs__write_for_sure
    while (TRUE) {
      stdin <- p$write_input(stdin)
      if (length(stdin) == 0L) {
        break
      }
      Sys.sleep(.1)
    }

    # Close connection, allowing Air to advance and begin formatting
    close(p$get_input_connection())
  }

  p$wait()

  list(
    status = p$get_exit_status(),
    stdout = p$read_all_output(),
    stderr = p$read_all_error()
  )
}
