FROM erlang:24

COPY include /parrent/include
COPY orders /parrent/app

WORKDIR /parrent/app