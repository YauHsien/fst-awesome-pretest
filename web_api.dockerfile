FROM elixir:1.12

RUN mix local.hex --force \
  && mix archive.install --force hex phx_new 1.5.8 \
  && apt-get update \
  && curl -sL https://deb.nodesource.com/setup_12.x | bash \
  && apt-get install -y apt-utils \
  && apt-get install -y nodejs \
  && apt-get install -y build-essential \
  && apt-get install -y inotify-tools \
  && mix local.rebar --force

ENV APP_HOME /parrent/app
RUN mkdir -p $APP_HOME
WORKDIR $APP_HOME
COPY web_api $APP_HOME
RUN rm -rf $APP_HOME/_build
RUN mix local.hex --force
RUN mix compile

EXPOSE 4000