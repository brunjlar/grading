FROM archlinux:20191006
RUN chown root root
RUN pacman -Syu --noconfirm stack tar make gcc gawk
ENV PATH="/root/.local/bin:${PATH}"
COPY stack.yaml /root/.stack/global-project/stack.yaml
RUN stack install hlint
