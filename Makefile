TMP = .tmp
VERSION ?= $(shell grep '^version:' oden.cabal | sed -Ee 's/^version\:(.+)$$/\1/' | xargs)
OS=$(shell tools/get_os.sh)
DIST_NAME=oden-$(VERSION)-$(OS)
DIST_ARCHIVE=dist/$(DIST_NAME).tar.gz

TEST_FLAG=
ifneq ($(strip $(TEST_PATTERN)),)
	TEST_FLAG=-m "$(TEST_PATTERN)"
endif

IMPORTER_SRC=$(shell find go/src/oden -name '*.go')

NODEMON=node_modules/.bin/nodemon

.PHONY: build
build: dist/go-lib/importer.a
	cabal build oden

dist/go-lib/importer.a: $(IMPORTER_SRC)
	mkdir -p dist/go-lib
	(cd dist/go-lib \
		&& GOPATH=$(PWD)/go go build -buildmode=c-archive oden/cmd/importer)

.PHONY: libs
libs: dist/go-lib/importer.a

.PHONY: clean
clean:
	rm -rf dist

$(TMP):
	mkdir -p $(TMP)

.PHONY: test
test:
	cabal exec runhaskell -- -isrc -itest test/Spec.hs $(TEST_FLAG)

.PHONY: regression-test
regression-test:
	regression-test/run-regression-tests.sh validate

.PHONY: lint
lint:
	hlint src cli

.PHONY: ci-test
ci-test: test regression-test lint

.PHONY: watch-test
watch-test: $(NODEMON)
	$(NODEMON) --watch src --watch test -e hs --exec 'make test || exit 1'

$(NODEMON):
	npm install nodemon

init-dev:
	cabal sandbox init
	cabal install --enable-tests --only-dependencies
	cabal configure --enable-tests

dist/build-oden-cli/bin/oden: build
	rm -rf dist/build-oden-cli
	cp -r dist/build dist/build-oden-cli
	mkdir -p dist/build-oden-cli/bin
	ghc \
		--make \
		-package-db .cabal-sandbox/*-packages.conf.d \
		-odir dist/build-oden-cli \
		-hidir dist/build-oden-cli \
		-ioden \
		-isrc \
		-fPIC \
		-static \
		-threaded \
		-o dist/build-oden-cli/bin/oden \
		dist/build-oden-cli/libHSoden*.a \
		dist/build-oden-cli/autogen/Paths_oden.hs \
		dist/go-lib/importer.a \
		$(shell find cli -name '*.hs')

.PHONY: cli
cli: dist/build-oden-cli/bin/oden

dist/oden: dist/build-oden-cli/bin/oden
	@mkdir -p dist/oden/bin
	cp README.md dist/oden/README.txt
	cp LICENSE.md dist/oden/LICENSE.txt
	cp CREDITS.md dist/oden/CREDITS.txt
	cp CHANGELOG.md dist/oden/CHANGELOG.txt
	cp dist/build-oden-cli/bin/oden dist/oden/bin/oden

$(DIST_ARCHIVE): dist/oden
	(cd dist && tar -czf $(DIST_NAME).tar.gz oden)

dist: $(DIST_ARCHIVE)

docker-dist:
	@(docker kill oden-builder &> /dev/null ; true)
	@(docker rm oden-builder &> /dev/null ; true)
	docker build \
		-t oden-build:$(VERSION) \
		.
	docker run \
		--env VERSION=$(VERSION) \
		--name oden-builder \
		oden-build:$(VERSION)
	docker cp oden-builder:/src/dist/oden-$(VERSION)-linux.tar.gz dist

release: $(DIST_ARCHIVE)
	@echo "\n\nDon't forget to set env variable GITHUB_TOKEN first!\n\n"
	go get github.com/aktau/github-release
	-git tag -a -m "Release $(VERSION)" $(VERSION) && git push origin +$(VERSION)
	-github-release release \
		--user oden-lang \
		--repo oden \
		--tag $(VERSION) \
		--name $(VERSION) \
		--pre-release
	find dist -name 'oden-$(VERSION)-*.tar.gz' -execdir \
		github-release upload \
			--user oden-lang \
			--repo oden \
			--tag $(VERSION) \
			--name {} \
			--file {} \;
