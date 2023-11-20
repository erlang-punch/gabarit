######################################################################
#
######################################################################
SOURCE ?= https://raw.githubusercontent.com/w3c/xml-entities/gh-pages/unicode.xml

unicode.xml:
	curl -so $@ $(SOURCE)

clean:
	rm unicode.xml
