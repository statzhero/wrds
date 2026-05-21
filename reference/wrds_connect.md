# Connect to WRDS

Establishes a connection to the WRDS PostgreSQL server using credentials
stored securely in the system keyring.

## Usage

``` r
wrds_connect(user_key = "wrds_user", password_key = "wrds_pw", keyring = NULL)
```

## Arguments

- user_key:

  Name of the keyring entry storing the WRDS username. Defaults to
  `"wrds_user"`.

- password_key:

  Name of the keyring entry storing the WRDS password. Defaults to
  `"wrds_pw"`.

- keyring:

  Optional keyring name. If `NULL` (default), uses the default keyring.

## Value

A `DBIConnection` object for the WRDS PostgreSQL database.

## Details

Credentials must be set up before first use with
[`wrds_set_credentials()`](https://statzhero.github.io/wrds/reference/wrds_set_credentials.md).
The connection uses `bigint = "numeric"` so that 64-bit integers from
PostgreSQL are returned as doubles, which avoids overflow and works well
with tidyverse functions.

## See also

[`wrds_disconnect()`](https://statzhero.github.io/wrds/reference/wrds_disconnect.md),
[`wrds_set_credentials()`](https://statzhero.github.io/wrds/reference/wrds_set_credentials.md)

## Examples

``` r
if (FALSE) { # \dontrun{
wrds <- wrds_connect()
list_subscriptions(wrds)
wrds_disconnect(wrds)
} # }
```
