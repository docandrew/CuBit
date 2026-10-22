"""Bounded streamed subprocess capture without restricting compiler objects."""
import os, selectors, signal, subprocess, time

def capture(command, cwd, log, timeout, limit=1024*1024):
 child=subprocess.Popen(command,cwd=cwd,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,
   start_new_session=True,bufsize=0)
 selector=selectors.DefaultSelector();selector.register(child.stdout,selectors.EVENT_READ)
 deadline=time.monotonic()+timeout;written=0;failure=None
 def stop():
  try:os.killpg(child.pid,signal.SIGKILL)
  except ProcessLookupError:pass
  child.wait()
 try:
  with log.open('wb') as output:
   while selector.get_map():
    remaining=deadline-time.monotonic()
    if remaining<=0:failure='timeout';break
    for key,_ in selector.select(min(remaining,0.25)):
     chunk=os.read(key.fileobj.fileno(),min(4096,limit-written+1))
     if not chunk:selector.unregister(key.fileobj);continue
     available=limit-written
     output.write(chunk[:available]);written+=min(len(chunk),available)
     if len(chunk)>available:failure='log_limit';break
    if failure:break
  if failure:stop();return failure
  try:return child.wait(timeout=max(0,deadline-time.monotonic()))
  except subprocess.TimeoutExpired:stop();return 'timeout'
 except BaseException:
  stop();raise
 finally:
  selector.close();child.stdout.close()
