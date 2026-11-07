ARG BUILD_FROM=ghcr.io/home-assistant/amd64-base-debian:bookworm

# The add-on's build context is this directory, not the repository root, so the
# source is fetched rather than copied. Bump VIRITIN_REF in build.yaml to move
# the add-on to a different revision.
FROM ${BUILD_FROM} AS builder

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      ca-certificates git zip \
      golang-go \
      libc6-dev libsqlite3-dev \
 && rm -rf /var/lib/apt/lists/*

ARG VIRITIN_REPO=https://github.com/ollisulopuisto/viritin.git
ARG VIRITIN_REF=master
RUN git clone --depth 1 --branch "${VIRITIN_REF}" "${VIRITIN_REPO}" /src

WORKDIR /src
RUN bash ./script/build.sh

FROM ${BUILD_FROM}

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      ca-certificates libsqlite3-0 libssl3 \
 && rm -rf /var/lib/apt/lists/*

COPY --from=builder /src/bin/*/viritin /opt/viritin/viritin
COPY --from=builder /src/cfg/ /opt/viritin/cfg/
COPY rootfs /

LABEL \
    org.opencontainers.image.title="Home Assistant Add-on: Viritin" \
    org.opencontainers.image.description="Internet radio for AV receivers whose vTuner service was shut down" \
    org.opencontainers.image.source="https://github.com/ollisulopuisto/viritin" \
    org.opencontainers.image.licenses="MIT"
