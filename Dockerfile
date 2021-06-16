FROM alpine:3.12

RUN sed -i 's/dl-cdn.alpinelinux.org/mirrors.aliyun.com/g' /etc/apk/repositories \
    && apk add tzdata \
    && cp /usr/share/zoneinfo/Asia/Shanghai /etc/localtime \
    && echo "Asia/Shanghai" > /etc/timezone \
    && apk del tzdata \
    && apk add dumb-init libc6-compat libtool openjdk8-jre curl mariadb-connector-c-dev sqlite-dev \
    && apk add --update --no-cache ttf-dejavu fontconfig \
    && rm -rf /var/cache/apk/*

ARG app=ygo-service
ARG path=/home/${app}
ENV LD_LIBRARY_PATH=${path}:/usr/lib/jvm/java-1.8-openjdk/jre/lib/amd64:/usr/lib/jvm/java-1.8-openjdk/jre/lib/amd64/server
RUN mkdir -p ${path}
COPY files ${path}/files
COPY ygosvr ${path}
COPY dbconv ${path}
COPY ygosvr.cfg ${path}
COPY application.yml ${path}
COPY favicon.ico ${path}

WORKDIR ${path}

ENTRYPOINT ["/usr/bin/dumb-init", "--"]
CMD ["./ygosvr"]

