#############################################
# "Alias" makefile - All targets are .PHONY #
#############################################

ifeq "$(shell command -v docker-compose 2> /dev/null)" ''
    $(error docker-compose not available, go install it)
endif

DOCKER_COMPOSE_LOCATION_OPTIONS := docker-compose --project-directory .
DOCKER_COMPOSE_RUN := $(DOCKER_COMPOSE_LOCATION_OPTIONS) run --use-aliases --rm --user="$(shell id -u):$(shell id -g)" -e DEBUG
DOCKER_COMPOSE_REBAR3 := $(DOCKER_COMPOSE_RUN) builder rebar3

ifeq (rebar3,$(firstword $(MAKECMDGOALS)))
.PHONY: rebar3
rebar3:
	@$(DOCKER_COMPOSE_REBAR3) $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
%: #Catch all
	@:
else

# Targets to run things:
.PHONY: all compile test
.PHONY: clean stop purge
.PHONY: dialyzer upgrade dev-release dev-shell
.PHONY: release dev-docker-release-console
all: | dev-docker-release-console ;

compile:
	$(DOCKER_COMPOSE_REBAR3) compile
test:
	@$(DOCKER_COMPOSE_REBAR3) ct
clean:
	$(DOCKER_COMPOSE_REBAR3) clean
stop:
	$(DOCKER_COMPOSE_LOCATION_OPTIONS) down
purge:
	$(DOCKER_COMPOSE_LOCATION_OPTIONS) run builder rm -rf _build/* _build
	$(DOCKER_COMPOSE_LOCATION_OPTIONS) down -v
dialyzer:
	$(DOCKER_COMPOSE_REBAR3) dialyzer
upgrade:
	$(DOCKER_COMPOSE_REBAR3) upgrade
dev-release:
	$(DOCKER_COMPOSE_REBAR3) release
dev-shell: | dev-release
	$(DOCKER_COMPOSE_REBAR3) shell --start-clean
dev-docker-release-console: | dev-release
	$(DOCKER_COMPOSE_RUN) builder _build/default/last_release_launcher console
dev-docker-release-console-clean: | dev-release
	$(DOCKER_COMPOSE_RUN) builder _build/default/last_release_launcher console_clean
release:
	$(DOCKER_COMPOSE_REBAR3) as prod release
endif

