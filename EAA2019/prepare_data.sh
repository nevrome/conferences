#!/bin/bash

qpdf --pages EAA_2019_Abstract_Book_2_September_2019.pdf 11-612 -- EAA_2019_Abstract_Book_2_September_2019.pdf - | pdfcrop --margins '-30 10 10 -20' - prep.pdf 

pdftotext -q -eol unix -nopgbrk prep.pdf temp.txt

grep -vw "^RACTS$" temp.txt > final.txt