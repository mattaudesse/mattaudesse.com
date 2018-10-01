.default_goal := build
.phony: build-hs test-hs repl-hs watch-hs
.phony: build-ps test-ps repl-ps watch-ps deps-ps
.phony: build-static build clean deployable
.phony: serve dump-contacts todo backup-db


### Haskell ####################################################################
build-hs:
	@stack build --pedantic --test --no-run-tests --jobs 6

test-hs: build-hs
	@stack test

repl-hs:
	@stack ghci --test --main-is mattaudesse-com

watch-hs:
	@ghcid -T "Spec.main"


### PureScript #################################################################
PS_BUILD_DIR   = .purs-output
PS_SRC_DIR     = ps/src
PS_TEST_DIR    = ps/test
PS_BUNDLE_DIR  = dist/assets/js
PS_BUNDLE_DIST = $(PS_BUNDLE_DIR)/mattaudesse.com.js

deps-ps:
	@psc-package install

$(PS_BUNDLE_DIR):
	@mkdir -p $(PS_BUNDLE_DIR)

build-ps: $(PS_BUNDLE_DIR) deps-ps
	@pulp \
		--psc-package \
		build \
		--build-path $(PS_BUILD_DIR) \
		--src-path   $(PS_SRC_DIR) \
		--test-path  $(PS_TEST_DIR) \
		--jobs 6 \
		--optimise \
		| closure-compiler \
		--compilation_level SIMPLE \
		--js_output_file $(PS_BUNDLE_DIST)

test-ps: deps-ps
	@pulp \
		--psc-package \
		test \
		--main       Spec \
		--build-path $(PS_BUILD_DIR) \
		--src-path   $(PS_SRC_DIR) \
		--test-path  $(PS_TEST_DIR)

repl-ps: deps-ps
	@pulp \
		--psc-package \
		repl \
		--src-path  $(PS_SRC_DIR) \
		--test-path $(PS_TEST_DIR)

watch-ps: deps-ps
	@pulp \
		--psc-package \
		--before clear \
		--watch \
		test \
		--main       Spec \
		--build-path $(PS_BUILD_DIR) \
		--src-path   $(PS_SRC_DIR) \
		--test-path  $(PS_TEST_DIR)


### Common #####################################################################
build-static: build-hs
	@stack exec -- mattaudesse-com-static site

build: build-static build-ps

clean:
	@stack clean
	@[ -d $(PS_BUILD_DIR) ] && rm -r $(PS_BUILD_DIR) || true
	@[ -d ./dist          ] && rm -r ./dist          || true

DOCKER_CONTAINER_ID = `docker container ls --all --latest --quiet`
DOCKER_WORK_DIR     = `docker run mattaudesse-com-centos pwd`
DOCKER_EXE_RELPATH  = `docker run mattaudesse-com-centos find .stack-work/dist -name mattaudesse-com -type f`
LINUX_BINARY_PATH   = .mattaudesse-com-centos

deployable: build
	@docker build -t mattaudesse-com-centos .
	@docker container create mattaudesse-com-centos >/dev/null
	@docker cp \
		$(DOCKER_CONTAINER_ID):$(DOCKER_WORK_DIR)/$(DOCKER_EXE_RELPATH) \
		$(LINUX_BINARY_PATH)
	@echo A new centos binary has been stashed to $(LINUX_BINARY_PATH)

serve: build
	@stack exec -- mattaudesse-com serve

dump-contacts: build-hs
	@stack exec -- mattaudesse-com dump-contacts

todo:
	@ag -i --ignore Makefile todo . || echo "No TODOs left!"

backup-db:
	@mkdir -p .db-backup
	@rsync -vzhr --progress \
		mattaudesse@mattaudesse.com:/home/mattaudesse/webapps/mattaudesse_com/mattaudesse.com.db \
		.db-backup/`date -u +"%Y-%m-%d-%H-%M"`-mattaudesse.com.db
