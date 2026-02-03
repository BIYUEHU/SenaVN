set windows-shell := ["powershell.exe"]

default:
  @just --list

dev:
  pnpx nodemon --exec 'cabal run' --ext .hs

build:
