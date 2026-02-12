# wrds 0.0.2

* `wrds_set_credentials()` now only uses `keyring::key_set()` for password input (@iangow).

# wrds 0.0.1

* Initial release for WRDS access.
* Fixed startup message to use `packageStartupMessage()` so it can be suppressed.
* `get_table()` provides generic access to any WRDS table with lazy query support.
