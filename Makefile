gen-src: install
	cask exec emacs --debug --script script/build.el -- gen-src

install:
	cask install
