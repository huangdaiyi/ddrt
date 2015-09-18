FROM fcf8250c4e9d
MAINTAINER erlang tristan.t.jiang@newegg.com
WORKDIR /ddrt/
RUN rm -rf ddrt
RUN mkdir ddrt
COPY . /ddrt/
RUN cp /usr/share/zoneinfo/Asia/Shanghai /etc/localtime
ADD . /ddrt/
CMD ./start.sh

