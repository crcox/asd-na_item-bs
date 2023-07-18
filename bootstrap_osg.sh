#!/bin/bash

set -e

Rscript --vanilla --default-packages=methods,utils,stats bootstrap_osg.R $1 $2 $3
