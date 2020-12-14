.DEFAULT_GOAL := build
.PHONY: build-hs test-hs repl-hs watch-hs
.PHONY: build-ps test-ps repl-ps watch-ps deps-ps
.PHONY: build-static build clean cert-renew
.PHONY: deployable do-rsync-deploy deploy-stage deploy-cloud
.PHONY: serve dump-contacts todo backup-db


### Haskell ####################################################################
build-hs:
	@stack build --pedantic --test --no-run-tests

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

$(PS_BUNDLE_DIST): $(PS_BUNDLE_DIR) $(shell find ./ps -type f) ./psc-package.json ./Makefile
	@$(MAKE) -s deps-ps
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

build-ps: $(PS_BUNDLE_DIST)

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

SITE_FQDN  = mattaudesse.com
TLS_DIR    = ~/.config/letsencrypt/conf/live/$(SITE_FQDN)
SITE_ROOT  = /usr/local/jail/nginx/site/$(SITE_FQDN)
RSYNC      = rsync -vzhr --progress --perms
CLOUD      = matt@$(SITE_FQDN)
CLOUD_ROOT = $(CLOUD):$(SITE_ROOT)
BINARY     = $(shell stack path --local-install-root)/bin/mattaudesse-com

cert-renew:
	@certbot certonly \
	  --manual \
	  --preferred-challenges dns \
	  -d $(SITE_FQDN)

deployable: build test-hs test-ps
	@sudo BINARY=$(BINARY) sh/jail-new.sh

./etc/dhparam:
	@openssl dhparam -out ./etc/dhparam 4096

do-rsync-deploy: ./etc/dhparam
	@$(RSYNC) --mkpath --chmod=F0644 ./dist/                  $(ROOT)/static/
	@$(RSYNC) --mkpath --chmod=F0600 ./etc/nginx.conf         $(ROOT)/conf/
	@$(RSYNC) --mkpath --chmod=F0600 ./etc/dhparam            $(ROOT)/conf/
	@$(RSYNC) -L       --chmod=F0600 $(TLS_DIR)/chain.pem     $(ROOT)/conf/trusted
	@$(RSYNC) -L       --chmod=F0600 $(TLS_DIR)/fullchain.pem $(ROOT)/conf/rsa.crt
	@$(RSYNC) -L       --chmod=F0600 $(TLS_DIR)/privkey.pem   $(ROOT)/conf/rsa.key

deploy-stage:
	@$(MAKE) -s ROOT=$(SITE_ROOT) do-rsync-deploy
	@chmod 0700 $(SITE_ROOT)/conf
	@sudo bastille restart mattaudesse-com
	@sudo service -j nginx nginx reload

deploy-cloud:
	@sh/jail-deploy-cloud.sh
	@$(MAKE) -s ROOT=$(CLOUD_ROOT) do-rsync-deploy
	@ssh    $(CLOUD) 'chmod 0700 $(SITE_ROOT)/conf'
	@ssh -t $(CLOUD) 'sudo service -j nginx nginx reload'

serve: build
	@stack exec -- mattaudesse-com serve \
	  --db   ./mattaudesse.com.db \
	  --conf ./mattaudesse.com.yaml

dump-contacts: build-hs
	@stack exec -- mattaudesse-com dump-contacts --db ./mattaudesse.com.db

todo:
	@ag -i --ignore Makefile todo . || echo "No TODOs left!"

backup-db: BACKUP-DB-DIR=.db-backup/`date -u +"%Y"`/`date -u +"%m"`
backup-db:
	@mkdir -p $(BACKUP-DB-DIR)
	@$(RSYNC) \
	  $(CLOUD):/usr/local/jail/mattaudesse-com/db/mattaudesse.com.db \
	  $(BACKUP-DB-DIR)/`date -u +"%Y-%m-%d-%H-%M"`-mattaudesse.com.db
