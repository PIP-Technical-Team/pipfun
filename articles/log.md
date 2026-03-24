# Structured Logging with pipfun

## Overview

{pipfun} provides a structured, in-memory logging system designed for
package development and data pipelines.

Unlike [`message()`](https://rdrr.io/r/base/message.html) or
[`cat()`](https://rdrr.io/r/base/cat.html), pipfun logs:

- store entries as structured rows

- capture function arguments automatically

- record return values

- attach metadata

- persist to disk with versioning (via {stamp})

Each log is a named piplog object stored in memory and behaves like a
data.table.

### The Mental Model

A log is:

- A named table

- Stored in an internal environment

- Appended row-by-row

- Queried later

- Optionally saved to disk

Each log entry records:

| Column  | Meaning                                       |
|---------|-----------------------------------------------|
| time    | Timestamp                                     |
| event   | Event type (“info”, “warning”, “error”, etc.) |
| message | Human-readable description                    |
| fun     | Calling function                              |
| package | Calling environment                           |
| args    | Captured function arguments                   |
| logmeta | Custom metadata                               |
| output  | Return value or summary                       |
| trace   | Call trace                                    |

### 1. Basic Usage

Initialize a Log:

``` r
log_init("analysis", overwrite = TRUE)

#This creates an empty piplog.

# Add entries
log_add("info", "Pipeline started", name = "analysis")
log_add("warning", "Missing values detected", name = "analysis")
log_add("error", "Model failed to converge", name = "analysis")

# Inspect
log_get("analysis")
```

### 2. Logging Inside Functions

The real power comes from automatic argument capture.

``` r
compute_mean <- function(x, na_rm = TRUE) {
  
  log_add("info", "Starting computation", name = "analysis")
  
  result <- mean(x, na.rm = na_rm)
  
  log_add("info", "Computation finished",
          name = "analysis",
          output = result)
  
  result
}

compute_mean(c(1, 2, 3, NA))
#> [1] 2
```

The log now contains:

- the arguments (x, na_rm)
- the return value
- the function call
- the timestamp

Inspect captured arguments:

``` r
log_get("analysis")$args[[1]]
#> $expr
#> NULL
#> 
#> $envir
#> NULL
#> 
#> $enclos
#> NULL
```

3.  Adding Structured Metadata

Use logmeta for domain-specific context.

``` r
log_add(
  "validation",
  "Invalid country code",
  name = "analysis",
  logmeta = list(
    field = "country",
    value = "XX",
    stage = "input_check"
  )
)
```

### 4. Filtering logs

Logs are queryable

``` r
# By event type
log_filter("analysis", event = "error")
#> 
#> ── Log entries: ──
#> 
#> → [2026-03-24 18:40:47.550221] ERROR - `Model failed to converge`
#> Function: `eval(expr, envir)` (from global)
#> Trace: eval(expr, envir)
#> 
# By multiple event types
log_filter("analysis", event = c("warning", "error"))
#> 
#> ── Log entries: ──
#> 
#> → [2026-03-24 18:40:47.549189] WARNING - `Missing values detected`
#> Function: `eval(expr, envir)` (from global)
#> Trace: eval(expr, envir)
#> 
#> → [2026-03-24 18:40:47.550221] ERROR - `Model failed to converge`
#> Function: `eval(expr, envir)` (from global)
#> Trace: eval(expr, envir)
#> 
# By function name
log_filter("analysis", fun = "compute_mean(c(1, 2, 3, NA))")
#> 
#> ── Log entries: ──
#> 
#> → [2026-03-24 18:40:47.617342] INFO - `Starting computation`
#> Function: `compute_mean(c(1, 2, 3, NA))` (from )
#> Trace: compute_mean(c(1, 2, 3, NA))
#> 
#> → [2026-03-24 18:40:47.618288] INFO - `Computation finished`
#> Function: `compute_mean(c(1, 2, 3, NA))` (from )
#> Trace: compute_mean(c(1, 2, 3, NA))
#> Output: 2
#> 
# By time window
log_filter("analysis", after = Sys.time() - 60)
#> 
#> ── Log entries: ──
#> 
#> → [2026-03-24 18:40:47.543822] INFO - `Pipeline started`
#> Function: `eval(expr, envir)` (from global)
#> Trace: eval(expr, envir)
#> 
#> → [2026-03-24 18:40:47.549189] WARNING - `Missing values detected`
#> Function: `eval(expr, envir)` (from global)
#> Trace: eval(expr, envir)
#> 
#> → [2026-03-24 18:40:47.550221] ERROR - `Model failed to converge`
#> Function: `eval(expr, envir)` (from global)
#> Trace: eval(expr, envir)
#> 
#> → [2026-03-24 18:40:47.617342] INFO - `Starting computation`
#> Function: `compute_mean(c(1, 2, 3, NA))` (from )
#> Trace: compute_mean(c(1, 2, 3, NA))
#> 
#> → [2026-03-24 18:40:47.618288] INFO - `Computation finished`
#> Function: `compute_mean(c(1, 2, 3, NA))` (from )
#> Trace: compute_mean(c(1, 2, 3, NA))
#> Output: 2
#> 
#> → [2026-03-24 18:40:47.74527] VALIDATION - `Invalid country code`
#> Function: `eval(expr, envir)` (from global)
#> Trace: eval(expr, envir)
#> Metadata: list(field = "country", value = "XX", stage = "input_check")
#> 
```

Filtering does not modify the original log.

### 5. Robust Error Logging

A common pattern is structured logging inside tryCatch():

``` r
log_init("pipeline", overwrite = TRUE)

read_data <- function(path) {
  tryCatch({
    
    log_add("io", "Reading file",
            name = "pipeline",
            logmeta = list(path = path))
    
    data <- read.csv(path)
    
    log_add("io", "File loaded",
            name = "pipeline",
            output = list(rows = nrow(data)))
    
    data
    
  }, error = function(e) {
    
    log_add("error", "File read failed",
            name = "pipeline",
            logmeta = list(
              path = path,
              message = e$message
            ))
    
    NULL
  })
}
```

### 6. Saving and Loading Logs

Logs are stored in memory by default.

To persist them, use
[`log_save()`](https://pip-technical-team.github.io/pipfun/reference/log_save.md)
and
[`log_load()`](https://pip-technical-team.github.io/pipfun/reference/log_load.md)
(powered by {stamp}).

##### Save

``` r
tmp <- tempdir()
stamp::st_init(tmp, alias = "demo")
#> ✔ stamp initialized
#>   alias: demo
#>   root: /tmp/RtmpEMGNhp
#>   state: /tmp/RtmpEMGNhp/.stamp

log_save(
  name  = "analysis",
  id    = file.path(tmp, "analysis_log"),
  alias = "demo"
)
#> ✔ Saved [qs2] → /tmp/RtmpEMGNhp/analysis_log.qs2
#> @ version 72a0e23f04b47a50
```

##### Load

``` r
log_reset("analysis")
#> ✔ Log analysis has been reset.

log_load(
  id    = file.path(tmp, "analysis_log.qs2"),
  name  = "analysis",
  alias = "demo"
)
#> ℹ Loading /tmp/RtmpEMGNhp/analysis_log.qs2 (version = latest, alias = "demo")
#> Warning: No primary key recorded for /tmp/RtmpEMGNhp/analysis_log.qs2.
#> ℹ You can add one with `st_add_pk()`.
#> ✔ Loaded [qs2] ←
#> /tmp/RtmpEMGNhp/analysis_log.qs2
#> 
#> ── Log entries: ──
#> 
#> → [2026-03-24 18:40:47.543822] INFO - `Pipeline started`
#> Function: `eval(expr, envir)` (from global)
#> Trace: eval(expr, envir)
#> 
#> → [2026-03-24 18:40:47.549189] WARNING - `Missing values detected`
#> Function: `eval(expr, envir)` (from global)
#> Trace: eval(expr, envir)
#> 
#> → [2026-03-24 18:40:47.550221] ERROR - `Model failed to converge`
#> Function: `eval(expr, envir)` (from global)
#> Trace: eval(expr, envir)
#> 
#> → [2026-03-24 18:40:47.617342] INFO - `Starting computation`
#> Function: `compute_mean(c(1, 2, 3, NA))` (from )
#> Trace: compute_mean(c(1, 2, 3, NA))
#> 
#> → [2026-03-24 18:40:47.618288] INFO - `Computation finished`
#> Function: `compute_mean(c(1, 2, 3, NA))` (from )
#> Trace: compute_mean(c(1, 2, 3, NA))
#> Output: 2
#> 
#> → [2026-03-24 18:40:47.74527] VALIDATION - `Invalid country code`
#> Function: `eval(expr, envir)` (from global)
#> Trace: eval(expr, envir)
#> Metadata: list(field = "country", value = "XX", stage = "input_check")
#> 
```

You can also list available versions:

``` r
log_load(
  id = file.path(tmp, "analysis_log.qs2"),
  version = "available",
  alias = "demo"
)
#>          version_id      artifact_id     content_hash code_hash size_bytes
#>              <char>           <char>           <char>    <char>      <num>
#> 1: 72a0e23f04b47a50 95825cc0d1408db7 24b8becf2f98c462      <NA>        823
#>                     created_at sidecar_format vintage
#>                         <char>         <char>   <num>
#> 1: 2026-03-24T18:40:48.423706Z           json       0
```

### 7. Recommended Usage Patterns

**Initialize once**

Create logs at:

- package load (.onLoad)

- pipeline start

- test setup

### Log entry and exit

``` r
log_add("start", "Beginning step", name = "analysis")
# ...
log_add("end", "Step completed", name = "analysis")
```

### Use consistent event types

Common choices:

- “info”

- “warning”

- “error”

- “debug”

- domain-specific tags like “validation”, “io”, “model”

### Keep messages human-readable

Use logmeta for structured data.

### When Should You Use pipfun Logging?

This system is especially useful when:

- Building R packages

- Writing reproducible pipelines

- Running long batch jobs

- Needing audit trails

- Collaborating across teams

- Saving logs with version control

It is less necessary for:

- Simple scripts

- One-off analyses

- Interactive exploration
