FROM 'docker.io/postgres:14.1-alpine'


RUN apk add --no-cache make git && \
    cd /tmp && \
    git clone https://github.com/michelp/pgjwt.git && \
    cd pgjwt && \
    make install
