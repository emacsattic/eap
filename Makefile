program := eap.el eap-autoloads.el eap-dired-keybindings.el 
texinfo-src := eap.texi
manuals := eap.html eap.info eap.txt

all : manuals packages

all-plus-upload : manuals packages upload-packages

manuals : $(manuals)

packages : README $(program) $(manuals)
	tar --verbose --create --auto-compress --totals --file eap.tgz \
	  README $(program) $(manuals)
	zip eap README $(program) $(manuals)

upload-packages :
	scp eap.tgz sebyte@download.gna.org:/upload/eap/
	scp eap.zip sebyte@download.gna.org:/upload/eap/

# single HTML file
eap.html : $(texinfo-src)
	makeinfo --css-include=eap.css --no-headers --no-split --html -o $@ $(texinfo-src)

# single Info file
eap.info : $(texinfo-src)
	makeinfo --no-split -o $@ $(texinfo-src)

# single plain text file 
eap.txt : $(texinfo-src)
	makeinfo --no-headers --plaintext -o $@ $(texinfo-src)

