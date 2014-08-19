#!/bin/sh
cd ~/Remote
TAR_FILE="ECE4094-`date --iso`.tar.bz2"
tar acf "$TAR_FILE" ECE4094 --exclude=ECE4094/scratch/nios-spike
xdg-open "http://moodle.vle.monash.edu/mod/assignment/view.php?id=1784826"
kmail --subject "FYP Progress Report" --attach "$TAR_FILE" david.boland@monash.edu

