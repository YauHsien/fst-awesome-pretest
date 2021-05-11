FROM postgres:latest
COPY ./persistent.sql /docker-entrypoint-initdb.d/