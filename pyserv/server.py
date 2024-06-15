
import argparse
import http.server
import socketserver
import json


import argparse
import sys


import requests



def parse_args(args):
    # Define the program description
    parser = argparse.ArgumentParser(description='Simple Application')

    # Add the arguments
    parser.add_argument('--api-key', type=str, required=True, help='API key')
    parser.add_argument('--port', type=int, help='Port', default=8080)
    parser.add_argument('--js-cache-path', type=str, help='js cache path', default=None)
    # Parse the arguments
    args = parser.parse_args(args)

    return args

class DummyResponse:
    status_code = None
    content = None
    def __init__(self, code, content):
        self.status_code = code
        self.content = content

def read_cache_or_request(cache_path, get_url):
    import os
    if cache_path is not None and os.path.isfile(cache_path):
        with open(cache_path, "rb") as f:
            print("return from cache")
            # mimic requests but only part of api are returned
            content = f.read()
            return DummyResponse(200, content)
    response = requests.get(get_url)
    if response.status_code == 200:
        with open(cache_path, "wb") as f:
            f.write(response.content)
    return response



class MyHandler(http.server.SimpleHTTPRequestHandler):
    api_key = ""
    js_cache_path = None
    def do_GET(self):
        if self.path == '/api':
            res = {'lat':10, 'lng':20}

            self.send_response(200)
            self.send_header('Content-type', 'application/json')
            self.end_headers()
            self.wfile.write(json.dumps(res).encode())
        elif self.path.startswith("/google_map_api"):
            #https://maps.googleapis.com/maps/api/js?key=


            # Request Google Map's JavaScript API code
            api_url = f'https://maps.googleapis.com/maps/api/js?key={self.api_key}'
            response = read_cache_or_request(self.js_cache_path, api_url)

            if response.status_code == 200:
                # Respond back to the client with the API code
                self.send_response(200)
                self.send_header('Content-type', 'application/javascript')
                self.end_headers()
                self.wfile.write(response.content)
            else:
                # Respond with an error status
                self.send_error(500)
        elif self.path == "/test-data.json":
            test_data = [{'key':"1",
                          'vec':[[
                              ["35.2", "139.0", "35.2", "139.1", "1", "1", "1"],
                              ["35.201", "139.0", "35.201", "139.1", "1", "0", "1"],
                              ["35.202", "139.0", "35.202", "139.1", "1", "1", "0"],
                          ]]}]
            self.send_response(200)
            self.send_header('Content-type', 'application/javascript')
            self.end_headers()
            self.wfile.write(json.dumps(test_data).encode())

        else:
            super().do_GET()




def main(args):
    MyHandler.api_key = args.api_key
    MyHandler.js_cache_path = args.js_cache_path
    with socketserver.TCPServer(("", args.port), MyHandler) as httpd:
        print("Server running at localhost:" + str(args.port))
        httpd.serve_forever()


if __name__ == "__main__":
    args = parse_args(sys.argv[1:])  # Omits the filename from the argument list
    main(args)