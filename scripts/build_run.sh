#!/bin/sh
set -e

(
  cd "$(dirname "$0")"
  stack build
)
stackInstallRoot=$(cd $(dirname "$0") && stack path --local-install-root) 
exec "$stackInstallRoot/bin/hastor-exe" "$@"
