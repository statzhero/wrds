# wrds 0.1.1

* `list_subscriptions()` lists subscribed data products.

# wrds 0.1.0

* `wrds_connect()` now gives an informative error when authentication fails, with guidance on password updates and Duo 2FA enrollment.
* `wrds_connect()` now uses `bigint = "numeric"`.
* `wrds_update_password()` updates the WRDS password without changing the username.

# wrds 0.0.2

* `wrds_set_credentials()` now only uses `keyring::key_set()` for password input (@iangow).

# wrds 0.0.1

* Initial release for WRDS access.
* Fixed startup message to use `packageStartupMessage()` so it can be suppressed.
* `get_table()` provides generic access to any WRDS table with lazy query support.
