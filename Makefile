main:
	@if [ -z "$(GHCJS)" ]; then \
		echo "Error: GHCJS compiler not found. Please install it first."; \
		exit 1; \
	fi
	cabal build Playground --with-compiler="$(GHCJS)" --with-ghc-pkg="$(GHCPKGJS)" --allow-newer=ghc-bignum && \
	(mv dist-newstyle/build/javascript-ghcjs/ghc-9.10.2/P4DDG-0.0.0.1/x/Playground/build/Playground/Playground.jsexe/index.html dist-newstyle/build/javascript-ghcjs/ghc-9.10.2/P4DDG-0.0.0.1/x/Playground/build/Playground/Playground.jsexe/index-generated.html 2>/dev/null || true) && \
	cp -r dist-newstyle/build/javascript-ghcjs/ghc-9.10.2/P4DDG-0.0.0.1/x/Playground/build/Playground/Playground.jsexe/* appWeb/src/gen/