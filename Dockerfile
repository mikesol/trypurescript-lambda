ARG OUTPUT_DIR=/root/output
ARG EXECUTABLE_NAME=bootstrap

FROM lambci/lambda:build-provided as build

COPY . .

SHELL ["/bin/bash", "--rcfile", "~/.profile", "-c"]

USER root

RUN yum -y install wget tar

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

RUN cd staging && npm install && npx spago build --purs-args "-g corefn" && cd ..

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
