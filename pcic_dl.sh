#!/bin/bash
set -euxo pipefail


curl -o '/Users/airvine/Dropbox/New Graph/fish-passage-22/Data/Discharge/pcic/baseflow.nc' 'https://data.pacificclimate.org/data/hydro_model_out/allwsbc.ACCESS1-0_rcp85_r1i1p1.1945to2099.BASEFLOW.nc.nc?BASEFLOW'$(echo '[27221:28060][196:222][200:274]'|jq -sRr @uri)

curl -o '/Users/airvine/Dropbox/New Graph/fish-passage-22/Data/Discharge/pcic/runoff.nc' 'https://data.pacificclimate.org/data/hydro_model_out/allwsbc.ACCESS1-0_rcp85_r1i1p1.1945to2099.RUNOFF.nc.nc?RUNOFF'$(echo '[27221:28060][196:222][200:274]'|jq -sRr @uri)
