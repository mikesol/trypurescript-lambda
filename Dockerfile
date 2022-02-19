ARG OUTPUT_DIR=/root/output
ARG EXECUTABLE_NAME=bootstrap

FROM lambci/lambda:build-provided as build

COPY . .

SHELL ["/bin/bash", "--rcfile", "~/.profile", "-c"]

USER root

RUN yum -y install wget tar ncurses-compat-libs ncurses-devel

RUN ls /lib64/libncurs*

RUN ls /usr/lib/libncurs*

# fixes version issue
# https://stackoverflow.com/questions/63730439/lib64-libtinfo-so-5-no-version-information-available
RUN ln -sf /lib64/libncursesw.so.6 /lib64/libtinfo.so.5

# ugh, the certificate has been problematic for months
# holds nose and does...

RUN wget --no-check-certificate https://get.haskellstack.org/stable/linux-x86_64.tar.gz

RUN tar zxf linux-x86_64.tar.gz

RUN cp stack-2.7.3-linux-x86_64/stack /usr/bin

RUN yum -y install perl make automake gcc gmp-devel libffi zlib zlib-devel xz tar git gnupg python3

# Installing Haskell Stack
RUN curl -sSL https://get.haskellstack.org/ | sh

# Build the lambda
COPY . /root/lambda-function/

RUN pwd

RUN cd /root/lambda-function
WORKDIR /root/lambda-function/

RUN ls

RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.34.0/install.sh | bash

RUN . ~/.nvm/nvm.sh && nvm install node && cd staging && npm install && npx spago build --purs-args "-g corefn" && cd ..

RUN python3 gen_externs_array.py
RUN stack clean --full
RUN stack build

ARG OUTPUT_DIR

RUN mkdir -p ${OUTPUT_DIR} && \
    mkdir -p ${OUTPUT_DIR}/lib

ARG EXECUTABLE_NAME

RUN cp $(stack path --local-install-root)/bin/${EXECUTABLE_NAME} ${OUTPUT_DIR}/${EXECUTABLE_NAME}

ENTRYPOINT sh

FROM public.ecr.aws/lambda/provided:al2 as deploy

ARG EXECUTABLE_NAME

WORKDIR ${LAMBDA_RUNTIME_DIR}

ARG OUTPUT_DIR

COPY --from=build ${OUTPUT_DIR} .
COPY --from=build /root/lambda-function/staging/output/ output/

RUN ls
RUN mv ${EXECUTABLE_NAME} bootstrap || true
RUN ls

CMD [ "handler" ]
