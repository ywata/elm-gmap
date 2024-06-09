
import http.server
import socketserver
import json

PORT = 8080


class MyHandler(http.server.SimpleHTTPRequestHandler):
    def do_GET(self):
        if self.path == '/api':
            self.send_response(200)
            self.send_header('Content-type', 'application/json')
            self.end_headers()
            self.wfile.write(json.dumps({"message": "Hello, API!"}).encode('utf-8'))
        else:
            super().do_GET()


def main():
    with socketserver.TCPServer(("", PORT), MyHandler) as httpd:
        print("Server running at localhost:" + str(PORT))
        httpd.serve_forever()


if __name__ == "__main__":
    main()