FROM ubuntu:focal
ENV PATH="/root/.local/bin:${PATH}"
RUN apt-get update
RUN apt-get -y install g++ gcc libc6-dev libffi-dev libgmp-dev make xz-utils zlib1g-dev git gnupg netbase libtinfo-dev
COPY stack /root/.local/bin/
COPY stack.yaml /root/.stack/global-project/stack.yaml
RUN stack install hlint
