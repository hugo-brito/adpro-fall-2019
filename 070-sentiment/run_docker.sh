#!/usr/bin/env bash
export DATA_PATH="/home/wasowski/work/2017-adpro-sentiment-analysis-data"
docker run -it --memory=6g -v "$(pwd)":/sentiment -v"${DATA_PATH}":/data mozilla/sbt:8u171_0.13.13
