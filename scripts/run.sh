#!/bin/bash
set -e
exec stack run --verbose -- "$@"
