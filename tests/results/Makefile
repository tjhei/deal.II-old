# $Id$

# Generate links to identical output files

all: .links

.links:
	ln -s sparc-sun-solaris2.7+gcc2.95 sparc-sun-solaris2.8+gcc2.95
	ln -s sparc-sun-solaris2.7+gcc2.95 sparc-sun-solaris2.9+gcc2.95
	(cd i686-pc-linux-gnu+gcc2.95; sh .link)
	touch .links

clean:
	(cd i686-pc-linux-gnu+gcc2.95; sh .unlink)
	rm sparc-sun-solaris2.8+gcc2.95
	rm sparc-sun-solaris2.9+gcc2.95
	rm .links