FROM ubuntu:14.10

# docker build -t brain .
# docker run --name brain -p 9292:8080 -d brain
# docker stop brain
# docker start brain


EXPOSE 8080

ADD ./brain /opt/

CMD ["/opt/brain", "foreground"]
