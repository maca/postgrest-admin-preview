FROM 'docker.io/alpine:3.15'


RUN apk add --no-cache build-base tzdata ruby ruby-dev ruby-json ruby-irb \
    postgresql-libs postgresql-dev && \
    gem install sequel pg faker
