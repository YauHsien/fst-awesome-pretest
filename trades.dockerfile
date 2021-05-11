FROM erlang:24

COPY include /parrent/include
COPY trades /parrent/app

WORKDIR /parrent/app