# Changelog

## wrds 0.1.1

CRAN release: 2026-05-11

- [`list_subscriptions()`](https://statzhero.github.io/wrds/reference/list_subscriptions.md)
  lists subscribed data products.

## wrds 0.1.0

- [`wrds_connect()`](https://statzhero.github.io/wrds/reference/wrds_connect.md)
  now gives an informative error when authentication fails, with
  guidance on password updates and Duo 2FA enrollment.
- [`wrds_connect()`](https://statzhero.github.io/wrds/reference/wrds_connect.md)
  now uses `bigint = "numeric"`.
- [`wrds_update_password()`](https://statzhero.github.io/wrds/reference/wrds_update_password.md)
  updates the WRDS password without changing the username.

## wrds 0.0.2

- [`wrds_set_credentials()`](https://statzhero.github.io/wrds/reference/wrds_set_credentials.md)
  now only uses
  [`keyring::key_set()`](https://keyring.r-lib.org/reference/key_get.html)
  for password input ([@iangow](https://github.com/iangow)).

## wrds 0.0.1

CRAN release: 2026-01-19

- Initial release for WRDS access.
- Fixed startup message to use
  [`packageStartupMessage()`](https://rdrr.io/r/base/message.html) so it
  can be suppressed.
- [`get_table()`](https://statzhero.github.io/wrds/reference/get_table.md)
  provides generic access to any WRDS table with lazy query support.
