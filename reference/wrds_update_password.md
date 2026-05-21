# Update WRDS password

Interactively updates the WRDS password stored in the system keyring
without changing the username.

## Usage

``` r
wrds_update_password(password_key = "wrds_pw", keyring = NULL)
```

## Arguments

- password_key:

  Name for the password keyring entry. Defaults to `"wrds_pw"`.

- keyring:

  Optional keyring name. If `NULL` (default), uses the default keyring.

## Value

Invisibly returns `TRUE` on success.

## See also

[`wrds_set_credentials()`](https://statzhero.github.io/wrds/reference/wrds_set_credentials.md),
[`wrds_connect()`](https://statzhero.github.io/wrds/reference/wrds_connect.md)

## Examples

``` r
if (FALSE) { # \dontrun{
wrds_update_password()
} # }
```
