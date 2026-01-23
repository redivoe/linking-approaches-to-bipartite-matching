timed_fun <- function(foo){
  function(...){
    start_time <- Sys.time()
    out <- foo(...)
    end_time <- Sys.time()
    elapsed_time <- difftime(end_time, start_time, units = "secs") |>
      as.numeric()
    return(list("out" = out,
                "time" = elapsed_time))
  }
}

warn_as_error <- function(f) {
  function(...) {
    withCallingHandlers(f(...), warning = function(w) { stop(w) })
  }
}

graph_retry_timed <- insistently(
  f = timed_fun(warn_as_error(graph_brl_cem)),
  rate = rate_delay(pause = 0, max_times = 5)
)

fs_retry_timed <- insistently(
  f = timed_fun(warn_as_error(fs_brl_cem)),
  rate = rate_delay(pause = 0, max_times = 5)
)
