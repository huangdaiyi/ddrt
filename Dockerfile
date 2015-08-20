FROM fcf8250c4e9d
MAINTAINER erlang tristan.t.jiang@newegg.com
RUN rm -rf ddrt
RUN mkdir ddrt
COPY . /ddrt/
CMD ./start.sh

