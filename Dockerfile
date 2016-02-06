FROM fcf8250c4e9d
MAINTAINER erlang hardy.d.huang@newegg.com
WORKDIR /ddrt/
#RUN rm -rf ddrt
#RUN mkdir ddrt
#COPY . /ddrt/
RUN cp /usr/share/zoneinfo/Asia/Shanghai /etc/localtime
ADD . /ddrt/
RUN chmod +x /ddrt/start.sh
CMD /ddrt/start.sh

