CABAL?=cabal

PREFIX?=/usr

build:
	$(CABAL) build
	ln -sf dist/build/bckspc-bot/bckspc-bot bckspc-bot

install:
	install -d $(DESTDIR)$(PREFIX)/bin
	install bckspc-bot $(DESTDIR)$(PREFIX)/bin
