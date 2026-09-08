FROM alpine:3.24 AS builder

RUN apk add --no-cache curl gcc g++ git gmp-dev libc-dev libffi-dev make musl-dev ncurses-dev perl tar xz gmp-static
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh
COPY . .
RUN source ~/.ghcup/env; \
    cabal install --enable-executable-static --installdir=.


FROM scratch

ARG RECOPY=0
COPY --from=builder trout-exe trout-exe
