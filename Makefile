.POSIX:
SHELL = /bin/sh
.SUFFIXES:

PKG = frosthaven-manager
COLLECT = frosthaven-manager
LANGS = frosthaven-manager/bestiary

EXES = FrosthavenManager.app/Contents/MacOS/FrosthavenManager \
       FrosthavenManager \
       FrosthavenManager.exe

RACO = raco

install:
	$(RACO) pkg install --name $(PKG) --auto $(RACO_INSTALL_ARGS)

uninstall:
	$(RACO) pkg remove $(PKG)

setup:
	$(RACO) setup --pkgs $(PKG) $(RACO_SETUP_ARGS)

test:
	$(RACO) test $(RACO_TEST_ARGS) --package $(PKG)

check-deps:
	$(RACO) setup --check-pkg-deps --pkgs $(PKG) $(RACO_SETUP_ARGS)

fix-deps:
	$(RACO) setup --fix-pkg-deps --pkgs $(PKG) $(RACO_SETUP_ARGS)

fix-doc-index:
	$(RACO) setup --doc-index --pkgs $(PKG) $(RACO_SETUP_ARGS)

docs/frosthaven-manager/index.html:
	scribble +m --redirect-main http://pkg-build.racket-lang.org/doc/ --htmls --dest ./docs ./scribblings/frosthaven-manager.scrbl

$(EXES): gui/manager.rkt
	$(RACO) exe --gui -o FrosthavenManager gui/manager.rkt

# POSIX leaves $< unspecified in these target rules
macOS-FrosthavenManager: FrosthavenManager.app/Contents/MacOS/FrosthavenManager
	$(RACO) distribute $@ FrosthavenManager.app
linux-FrosthavenManager: FrosthavenManager
	$(RACO) distribute $@ FrosthavenManager
windows-FrosthavenManager: FrosthavenManager.exe
	$(RACO) distribute $@ FrosthavenManager.exe

linux-FrosthavenManager.tar.gz: linux-FrosthavenManager
	tar cvf - linux-FrosthavenManager | gzip >$@
macOS-FrosthavenManager.tar.gz: macOS-FrosthavenManager
	tar cvf - macOS-FrosthavenManager | gzip >$@
windows-FrosthavenManager.zip: windows-FrosthavenManager
	7z a $@ windows-FrosthavenManager
