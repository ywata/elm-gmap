#!/bin/bash

. venv/bin/activate
python3 -m pyserv.server --api-key $1 --port $2

