#!/usr/bin/env python3
"""Serve Penny ancestry/CSP test pages on two local HTTP origins."""
import argparse,json,threading,time
from http.server import BaseHTTPRequestHandler,ThreadingHTTPServer
from pathlib import Path
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--parent-bind',default='127.0.0.1')
p.add_argument('--child-bind',default='127.0.0.1')
p.add_argument('--parent-origin',default='http://10.0.2.2:18471')
p.add_argument('--child-origin',default='http://10.0.2.2:18472')
p.add_argument('--requests',type=Path,required=True)
a=p.parse_args()
parent=Path(__file__).with_name('parent.html').read_text().replace('__PARENT_ORIGIN__',json.dumps(a.parent_origin)).replace('__CHILD_ORIGIN__',json.dumps(a.child_origin))
class Page(BaseHTTPRequestHandler):
 def log_message(self,*args):pass
 def do_GET(self):
  kind=self.path.lstrip('/')
  if kind not in ('parent','control','allow','deny'):
   self.send_error(404);return
  with a.requests.open('a') as f:f.write(str(self.server.server_port)+' '+self.path+'\n')
  if kind=='parent':body=parent
  else:body="<!doctype html><script>parent.postMessage({kind:"+json.dumps(kind)+",ancestors:Array.from(location.ancestorOrigins)},"+json.dumps(a.parent_origin)+")</script>"
  data=body.encode();self.send_response(200);self.send_header('Content-Type','text/html')
  if kind=='allow':self.send_header('Content-Security-Policy','frame-ancestors *')
  if kind=='deny':self.send_header('Content-Security-Policy',"frame-ancestors 'none'")
  self.send_header('Content-Length',str(len(data)));self.end_headers()
  try:self.wfile.write(data)
  except (BrokenPipeError,ConnectionResetError):pass
servers=[ThreadingHTTPServer((a.parent_bind,18471),Page),ThreadingHTTPServer((a.child_bind,18472),Page)]
try:
 for s in servers:threading.Thread(target=s.serve_forever,daemon=True).start()
 print(a.parent_origin+'/parent',flush=True)
 while True:time.sleep(1)
except KeyboardInterrupt:pass
finally:
 for s in servers:s.shutdown();s.server_close()
