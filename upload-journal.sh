#!/bin/sh
cd ~/Remote
tar acf ECE4094-`date --iso`.tar.bz2 ECE4094 --exclude=ECE4094/scratch
xdg-open "http://moodle.vle.monash.edu/mod/assignment/view.php?id=1461360"
kmail --subject "FYP Progress Report" --attach ECE4094*.tar.bz2 lindsay.kleeman@monash.edu david.boland@monash.edu

