FROM debian:bookworm-slim

WORKDIR /app

COPY . /app

RUN apt-get update && \
  apt-get install -y \
  gcc \
  g++ \
  make \
  libboost-program-options-dev

RUN make release