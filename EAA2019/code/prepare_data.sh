#!/bin/bash

qpdf --pages ../data/raw/EAA_2019_Abstract_Book_2_September_2019.pdf 11-612 -- ../data/raw/EAA_2019_Abstract_Book_2_September_2019.pdf - | pdfcrop --margins '-30 10 10 -20' - ../data/tmp/prep.pdf 

pdftotext -q -eol unix -nopgbrk ../data/tmp/prep.pdf ../data/tmp/temp.txt

grep -vw "^RACTS$" ../data/tmp/temp.txt > ../data/tmp/final.txt

awk -v RS= '{print > ("../data/prep/eaa_" NR ".txt")}' ../data/tmp/final.txt