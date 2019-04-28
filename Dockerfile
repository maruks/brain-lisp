FROM ubuntu:14.10

EXPOSE 8080

ADD . /opt/
WORKDIR /opt/
ENTRYPOINT /opt/brain
