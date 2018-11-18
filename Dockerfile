FROM ubuntu:14.10

# docker build -t brain .
# docker run --name brain --tty --network net -d brain
# docker stop brain
# docker start brain


EXPOSE 8080

ADD . /opt/
WORKDIR /opt/
ENTRYPOINT /opt/brain
