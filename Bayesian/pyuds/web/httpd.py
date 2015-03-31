"""
Author: Sari Haj Hussein
"""
import http.server

server_address=('',8000)
httpd = http.server.HTTPServer(server_address, http.server.CGIHTTPRequestHandler)
httpd.serve_forever()