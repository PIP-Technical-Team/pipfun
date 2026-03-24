# Summarize log contents

Provides a summary of the number of log entries by event type and
function.

## Usage

``` r
log_summary(name = getOption("pipfun.log.default"), by = "event")
```

## Arguments

- name:

  Name of the log (default: pipfun.log.default)

- by:

  Character vector of grouping variables (default: c("event"))

## Value

A data.table with summary counts.
