FROM ddrt
MAINTAINER erlang tristan.t.jiang@newegg.com
RUN mkdir ddrt
COPY . /ddrt/
CMD ./start.sh

