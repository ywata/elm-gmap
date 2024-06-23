# Use the official Node.js base image
FROM node:latest

RUN apt update && apt install -y \
    python3 \
    python3-pip \
    python3.11-venv \
    curl

ENV HOME=/home/webapp
RUN useradd -d ${HOME} -s /bin/bash webapp
USER webapp
WORKDIR ${HOME}

RUN npm install elm elm-test


COPY --chown=webapp:webapp ./requirements.txt .
RUN python3 -m venv venv 
RUN . venv/bin/activate && pip3 install -r requirements.txt


COPY --chown=webapp:webapp ./tests ./tests
COPY --chown=webapp:webapp ./pyserv ./pyserv
COPY --chown=webapp:webapp index.html .
COPY --chown=webapp:webapp ./elm.json .
COPY --chown=webapp:webapp ./src ./src
COPY --chown=webapp:webapp  ./docker/run_app.sh ./bin/run_app.sh
RUN npx elm make src/Main.elm --output=main.js

# Command to run the application
EXPOSE 8082
# run command to start the server
CMD ./bin/run_app.sh ${API_KEY} 8082

