#!/bin/bash
set -euxo pipefail

export PATH=/opt/homebrew/bin:$PATH


curl -o '/Users/nicolehill/Poisson/Clients - Transfer/New Graph/fish-passage-22/Data/Discharge/pcic/baseflow.nc' 'https://data.pacificclimate.org/data/hydro_model_out/allwsbc.ACCESS1-0_rcp85_r1i1p1.1945to2099.BASEFLOW.nc.nc?BASEFLOW'$(echo '[27028:28123][183:248][192:281]'|jq -sRr @uri)

curl -o '/Users/nicolehill/Poisson/Clients - Transfer/New Graph/fish-passage-22/Data/Discharge/pcic/runoff.nc' 'https://data.pacificclimate.org/data/hydro_model_out/allwsbc.ACCESS1-0_rcp85_r1i1p1.1945to2099.RUNOFF.nc.nc?RUNOFF'$(echo '[27028:28123][183:248][192:281]'|jq -sRr @uri)
