# Set WRDS credentials

Interactively stores WRDS username and password in the system keyring
for secure, persistent storage.

## Usage

``` r
wrds_set_credentials(
  user_key = "wrds_user",
  password_key = "wrds_pw",
  keyring = NULL
)
```

## Arguments

- user_key:

  Name for the username keyring entry. Defaults to `"wrds_user"`.

- password_key:

  Name for the password keyring entry. Defaults to `"wrds_pw"`.

- keyring:

  Optional keyring name. If `NULL` (default), uses the default keyring.

## Value

Invisibly returns `TRUE` on success.

## Details

This function prompts for username and password interactively.
Credentials are stored securely using the operating system's keyring
(Keychain on macOS, Credential Manager on Windows, Secret Service on
Linux).

## Examples

``` r
if (FALSE) { # \dontrun{
wrds_set_credentials()
} # }
```
