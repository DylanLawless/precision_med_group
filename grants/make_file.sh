#!/bin/bash

gs -sDEVICE=pdfwrite \
	-dCompatibilityLevel=1.5 \
	-dNOPAUSE -dQUIET -dBATCH \
	-dPrinted=false \
	-dCompressFonts=true \
	-dPDFSETTINGS=/ebook -r72 \
	-sOutputFile=Dylan_Lawless_letter_of_intent.pdf \
	Labhart-Schwyzer/letter/letter.pdf \
	Dylan_Lawless_CV.pdf
