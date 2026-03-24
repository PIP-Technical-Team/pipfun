# Get all arguments of calling function

Calling function is
[parent.frame](https://rdrr.io/r/base/sys.parent.html)

## Usage

``` r
all_args()
```

## Value

arguments of calling function as a list

## Examples

``` r
foo <- function(x, y = 2, ...) {
  all_args()
}
foo(x = 1, y = 3)
#> $x
#> [1] 1
#> 
#> $y
#> [1] 3
#> 
foo(x = 1, z = 123)
#> $x
#> [1] 1
#> 
#> $z
#> [1] 123
#> 
foo(x = 1, z = "valid")
#> $x
#> [1] 1
#> 
#> $z
#> [1] "valid"
#> 
foo(x = 1, a = TRUE)
#> $x
#> [1] 1
#> 
#> $a
#> [1] TRUE
#> 
```
