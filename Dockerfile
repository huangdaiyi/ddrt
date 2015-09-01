FROM fcf8250c4e9d
MAINTAINER erlang tristan.t.jiang@newegg.com
WORKDIR /ddrt/
#RUN rm -rf ddrt
#RUN mkdir ddrt
#COPY . /ddrt/
RUN echo "Asia/Shanghai" > /etc/timezone
RUN dpkg-reconfigure -f noninteractive tzdata
ADD . /ddrt/
CMD ./start.sh

