#!/bin/bash

LOGFILE=output.log
BASEDIR=$(dirname $0)

cd $BASEDIR

r -f web_scraper_script.R > $LOGFILE && \
r -f web_scraper_script_nyt.R >> $LOGFILE 
