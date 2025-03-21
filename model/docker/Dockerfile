FROM gafiatulin/alpine-sbt

RUN apk update && apk add bash git sudo
RUN git clone -b paper1 https://github.com/ElCep/DSCATT/ && \
    cd DSCATT/model/scala/ && \
    sbt ";project dscatt; run"

#RUN cd target && \
#    rm -f /my-apache-app && \
#    -dit --name my-apache-app -p 8080:80 -v "$PWD":/usr/local/apache2/htdocs/ httpd:2.4

