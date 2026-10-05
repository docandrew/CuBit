"""Loopback-only, paced HTTP range fixture for Penny media tests."""
from pathlib import Path
from http.server import BaseHTTPRequestHandler,ThreadingHTTPServer
import argparse,json,threading,time,re,ssl
parser=argparse.ArgumentParser(description=__doc__)
parser.add_argument('--media',required=True,type=Path)
parser.add_argument('--log',required=True,type=Path)
parser.add_argument('--port',type=int,default=0)
parser.add_argument('--recovery',action='store_true')
parser.add_argument('--cert',type=Path)
parser.add_argument('--key',type=Path)
parser.add_argument('--server-name',default='tls-test.cubit.internal')
args=parser.parse_args()
if bool(args.cert)!=bool(args.key):parser.error('--cert and --key must be supplied together')
clip=args.media.read_bytes()
page=Path(__file__).with_name('seek.html').read_text().replace('__MEDIA_URL__',json.dumps('/clip.webm'))
if args.recovery:
 page=page.replace('v.src="/clip.webm";', 'v.src="/broken.webm";').replace('(async()=>{', '(async()=>{ const failed=event("error"); await v.play().catch(()=>{}); await failed; if(!v.error)throw new Error("missing network error"); mark("NetworkErrorHandled"); v.src="/clip.webm";')
page=page.encode()
lock=threading.Lock();events=[]
def record(event):
 with lock:
  events.append(event);args.log.write_text(json.dumps(events,indent=2)+'\n')
class Handler(BaseHTTPRequestHandler):
 protocol_version='HTTP/1.1'
 def log_message(self,*args):pass
 def do_GET(self):
  if self.path=='/':
   self.send_response(200);self.send_header('Content-Type','text/html');self.send_header('Content-Length',str(len(page)));self.end_headers();self.wfile.write(page);record({'path':'/','status':200});return
  if self.path not in ('/clip.webm','/broken.webm'):self.send_error(404);return
  range_header=self.headers.get('Range');start=0;end=len(clip)-1
  if range_header:
   match=re.fullmatch(r'bytes=(\d+)-(\d*)',range_header)
   if not match:self.send_error(416);return
   start=int(match[1]);end=min(end,int(match[2])) if match[2] else end
   if start>end:self.send_error(416);return
  status=206 if range_header else 200
  self.send_response(status);self.send_header('Content-Type','video/webm');self.send_header('Accept-Ranges','bytes');self.send_header('Cache-Control','no-store');self.send_header('Content-Length',str(end-start+1))
  if range_header:self.send_header('Content-Range',f'bytes {start}-{end}/{len(clip)}')
  self.end_headers();sent=0;cancelled=False
  record({'tls':self.connection.version() if isinstance(self.connection,ssl.SSLSocket) else None,'path':self.path,'status':status,'range':range_header,'start':start,'end':end,'event':'request'})
  if self.path=='/broken.webm':
   partial=clip[start:start+min(16384,max(1,(end-start+1)//2))]
   self.wfile.write(partial);self.wfile.flush();self.close_connection=True
   record({'path':self.path,'event':'truncated','sent':len(partial),'declared':end-start+1});return
  try:
   for offset in range(start,end+1,4096):
    chunk=clip[offset:min(offset+4096,end+1)];self.wfile.write(chunk);self.wfile.flush();sent+=len(chunk);time.sleep(1/30)
  except (BrokenPipeError,ConnectionResetError):cancelled=True
  record({'path':self.path,'start':start,'event':'finished','sent':sent,'cancelled':cancelled})
server=ThreadingHTTPServer(('127.0.0.1',args.port),Handler)
server.daemon_threads=True
scheme='http';guest_host='10.0.2.2'
if args.cert:
 context=ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER);context.minimum_version=ssl.TLSVersion.TLSv1_2
 context.load_cert_chain(args.cert,args.key);server.socket=context.wrap_socket(server.socket,server_side=True)
 scheme='https';guest_host=args.server_name
print(json.dumps({'host_url':f'{scheme}://127.0.0.1:{server.server_port}/','qemu_url':f'{scheme}://{guest_host}:{server.server_port}/'}),flush=True)
try:server.serve_forever()
except KeyboardInterrupt:pass
finally:server.server_close()
