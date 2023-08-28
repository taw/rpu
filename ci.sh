#!/bin/bash

# This project can't be run on any modern system.
# There's probably some way to dockerize really ancient Linux repo where it runs but I never really tried.
#
# It shouldn't be too much work to port this to newer Ruby and OCaml, but this project is of historical interest only anyway.

# This would be the way to CI this project, unfortunately it can't even install anymore:
# * ocaml-ioxml is gone from Ubuntu repos (optional for some steps)
# * ruby 1.8.7 doesn't install (pretty much required)

### Install dependencies
sudo apt-get install -y ocaml camlp4 ocaml-ioxml
gem install bundler -v 1.17.3 # not even sure what's the right version for ruby 1.8.7
bundle install

### Build
bundle exec rake build
# These commands depend on ocaml-ioxml which doesn't seem to be in ubuntu repo any more:
(cd kdsaveload; make)
(cd ioxml; make)

### Test
bundle exec rake test
