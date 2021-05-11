SHELL := /bin/bash
COOKIE=magic
help:
	@echo "usage:"
	@echo "    make build        docker-compose build"
	@echo "    make clean build  Build after cleaning wasted images."
	@echo "    make up           docker-compose up --remove-orphans"
	@echo "    make build up     docker-compose up --build --remove-orphans"
	@echo "    make clean build up"
	@echo "                      Run all-new containers after cleaning"
	@echo "                      wasted images."
	@echo "    make describe-api"
	@echo "                      Show API routes."
	@echo "shell:"
	@echo "    make remote-node-web-api"
	@echo "                      Remote shell to node \`web_api@w.p\`; in it,"
	@echo "                      use 'Elixir.IEx':start() to use \`iex\` shell."
	@echo "    make remote-node-users"
	@echo "                      Remote shell to node \`users@u.p\`; in it,"
	@echo "                      to hot reload, remote shell it and execute"
	@echo "                      \`os:command(\"rebar3 compile\"), lm().\`"
	@echo "    make remote-node-orders"
	@echo "                      Remote shell to node \`orders@o.p\`."
	@echo "    make remote-node-trades"
	@echo "                      Remote shell to node \`trades@t.p\`."
	@echo "    make remote-node-data-persistency"
	@echo "                      Remote shell to node \`data_persistency@d.p\`."
build:
	@(pushd users; rebar3 compile && popd) \
    && (pushd trades; rebar3 compile && popd) \
    && (pushd data_persistency; rebar3 compile && popd) \
  && docker-compose build
clean:
	docker image prune --force
describe-api:
	(cd web_api; mix phx.routes)
up:
	docker-compose up --remove-orphans
remote-node-web-api:
	@docker exec -it w.p \
    erl -name wc@c.p \
        -setcookie $(COOKIE) \
        -remsh webapi@w.p
remote-node-users:
	@docker exec -it u.p \
    erl -name uc@c.p \
        -setcookie $(COOKIE) \
        -remsh users@u.p
remote-node-trades:
	@docker exec -it t.p \
    erl -name tc@c.p \
        -setcookie $(COOKIE) \
        -remsh trades@t.p
remote-node-data-persistency:
	@docker exec -it d.p \
    erl -name dc@c.p \
        -setcookie $(COOKIE) \
        -remsh data_persistency@d.p
