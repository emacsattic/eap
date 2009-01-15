texinfo-src := eap.texi

manuals : eap.html eap.info eap.txt

# single HTML file
eap.html : $(texinfo-src)
	makeinfo --css-include=eap.css --no-headers --no-split --html -o $@ $(texinfo-src)

# single Info file
eap.info : $(texinfo-src)
	makeinfo --no-split -o $@ $(texinfo-src)

# single plain text file 
eap.txt : $(texinfo-src)
	makeinfo --no-headers --plaintext -o $@ $(texinfo-src)

