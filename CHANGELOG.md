# Version 1.0.13

* Remove p1_utils dependency
* Parametrize the rebar binary (processone/fast_tls#51)
* Add Github Action to release in hex.pm when tagging
* Switch from Travis to Github Actions

# Version 1.0.12

* Update p1_utils dependency
* Changing Travis.yml

# Version 1.0.11

* Updating p1_utils to version 1.0.19.
* Fix to compile with Rebar2 and Erlang 23, added to Travis and Coveralls
* Document usage with Elixir mix
* Add more explicit usage example
* Add CoC and contribution guide

# Version 1.0.10

* Updating p1_utils to version 1.0.13.

# Version 1.0.9

* Updating p1_utils to version 6ff85e8.

# Version 1.0.8

* Updating p1_utils to version 1.0.12.

# Version 1.0.7

* Updating p1_utils to version 1.0.11.
* Fix compilation with rebar3

# Version 1.0.6

* Updating p1_utils to version 1.0.10.
* Fix possible overflow of inleft

# Version 1.0.5

* depends on p1_utils-1.0.9

# Version 1.0.4

* Update rebar.config.script (Paweł Chmielowski)
* Use p1_utils 1.0.7 (Christophe Romain)

# Version 1.0.3

* Use p1_utils 1.0.6 (Christophe Romain)
* Make sure iconv isn't compiled to native code (Holger Weiss)

# Version 1.0.2

* Update p1_utils (Mickaël Rémond)

# Version 1.0.1

* Use nif loading code from p1_utils (Paweł Chmielowski)
* Fix compilation on rebar3 (Paweł Chmielowski)

# Version 1.0.0

* Application is now called iconv for consistency (Mickaël Rémond)
* Initial release on Hex.pm (Mickaël Rémond)
* Standard ProcessOne build chain (Mickaël Rémond)
* Support for Travis-CI and test coverage (Mickaël Rémond)
