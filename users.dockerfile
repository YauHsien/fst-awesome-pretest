FROM erlang:24

COPY include /parrent/include
COPY users /parrent/app

WORKDIR /parrent/app