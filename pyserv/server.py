
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

    # Parse the arguments
    args = parser.parse_args(args)

    return args




class MyHandler(http.server.SimpleHTTPRequestHandler):
    api_key = ""
    def do_GET(self):
        if self.path == '/api':
            self.send_response(200)
            self.send_header('Content-type', 'application/json')
            self.end_headers()
            self.wfile.write(json.dumps({"message": "Hello, API!"}).encode('utf-8'))
        elif self.path.startswith("/google_map_api"):
            #https://maps.googleapis.com/maps/api/js?key=

            # Request Google Map's JavaScript API code
            api_url = f'https://maps.googleapis.com/maps/api/js?key={self.api_key}'
            response = requests.get(api_url)

            if response.status_code == 200:
                # Respond back to the client with the API code
                self.send_response(200)
                self.send_header('Content-type', 'application/javascript')
                self.end_headers()
                self.wfile.write(response.content)
            else:
                # Respond with an error status
                self.send_error(500)

        else:
            super().do_GET()




def main(args):
    MyHandler.api_key = args.api_key
    with socketserver.TCPServer(("", args.port), MyHandler) as httpd:
        print("Server running at localhost:" + str(args.port))
        httpd.serve_forever()


if __name__ == "__main__":
    args = parse_args(sys.argv[1:])  # Omits the filename from the argument list
    main(args)