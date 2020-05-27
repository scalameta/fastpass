#!/usr/bin/env bash
set -eux

version=$1

coursier resolve \
  org.scalameta:fastpass_2.12:$version \
  -r sonatype:public
