FROM erlang:24

COPY include /parrent/include
COPY data_persistency /parrent/app

WORKDIR /parrent/app
RUN rebar3 get-deps
RUN rebar3 compile