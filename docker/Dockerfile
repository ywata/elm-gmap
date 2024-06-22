# Use the official Node.js base image
FROM node:latest

ENV API_KEY="GoogleMapAPIKey"


# Set the working directory in the container
WORKDIR /app

# Copy the application's files into the container
#COPY . .

# Install any dependencies
RUN npm install elm elm-test

RUN apt update && apt install -y python3 
RUN apt-get install -y python3-pip python3.11-venv


COPY ./requirements.txt .
RUN python3 -m venv venv 
RUN . venv/bin/activate && pip3 install -r requirements.txt



COPY ./tests ./tests
COPY ./pyserv ./pyserv
COPY index.html .
COPY ./elm.json .
COPY ./src ./src
COPY ./bin ./bin
RUN npx elm make src/Main.elm --output=main.js

# Command to run the application

# run command to start the server
CMD ./bin/run.sh ${API_KEY} 8080

