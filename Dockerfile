# The original file created by Anders  Fischer-Nielsen
# Additions by Andrzej Wąsowski

FROM ubuntu:18.04

RUN apt-get update 
RUN apt-get -y install wget git openjdk-8-jdk-headless vim emacs nano mcedit jed joe
RUN wget https://dl.bintray.com/sbt/debian/sbt-0.13.7.deb 
RUN dpkg -i sbt-0.13.7.deb
RUN git clone https://bitbucket.org/itu-square/2019-adpro-fall.git

RUN echo 'echo \* ' >> ~/.bashrc
RUN echo 'echo \* Welcome to stable testing environment for adpro' >> ~/.bashrc
RUN echo 'echo \* Installed text editors: vim emacs nano mcedit jed joe' >> ~/.bashrc
RUN echo 'echo \* Type "sbt" to start working with sbt, then use test, compile, run' >> ~/.bashrc
RUN echo 'echo \* Type "sbt console" if you want to experiment quickly with Scala REPL' >> ~/.bashrc
RUN echo 'echo \* ' >> ~/.bashrc

ENTRYPOINT ["/bin/bash"]
