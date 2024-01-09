FROM adoptopenjdk/openjdk16:alpine-jre

WORKDIR /app

RUN \
  apk add --no-cache \
    ca-certificates \
    tini \
    tzdata

COPY . /app
ENTRYPOINT ["/sbin/tini", "--"]
CMD ["java","-jar","/app/teledoplarr.jar"]

LABEL "maintainer"="Joseph Edmiston <joseph@edmiston.email>"
LABEL "org.opencontainers.image.source"="https://github.com/jcedmiston/TeleDoplarr"
