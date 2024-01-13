FROM ubuntu:latest

ENV TZ=Europe/Moscow
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

RUN apt update -y
RUN apt install -y gcc
RUN apt install -y g++
RUN apt install -y cmake

WORKDIR /project

COPY . .