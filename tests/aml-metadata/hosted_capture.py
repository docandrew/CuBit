"""Bounded streamed subprocess capture without restricting compiler objects."""
import os, selectors, signal, subprocess, time

def capture(command, cwd, log, timeout, limit=1024*1024):
 child=subprocess.Popen(command,cwd=cwd,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,
   start_new_session=True,bufsize=0)
 selector=selectors.DefaultSelector();selector.register(child.stdout,selectors.EVENT_READ)
 deadline=None if timeout is None else time.monotonic()+timeout;written=0;failure=None
 def stop():
  # Let the replay supervisor reap its own separately grouped child first.
  try:os.killpg(child.pid,signal.SIGTERM)
  except ProcessLookupError:pass
  try:child.wait(timeout=3)
  except subprocess.TimeoutExpired:
   try:os.killpg(child.pid,signal.SIGKILL)
   except ProcessLookupError:pass
   child.wait()
  # Compiler descendants share this group; reap any remaining descendants.
  try:os.killpg(child.pid,signal.SIGKILL)
  except ProcessLookupError:pass
 try:
  with log.open('wb') as output:
   while selector.get_map():
    remaining=None if deadline is None else deadline-time.monotonic()
    if remaining is not None and remaining<=0:failure='timeout';break
    for key,_ in selector.select(0.25 if remaining is None else min(remaining,0.25)):
     chunk=os.read(key.fileobj.fileno(),min(4096,limit-written+1))
     if not chunk:selector.unregister(key.fileobj);continue
     available=limit-written
     output.write(chunk[:available]);written+=min(len(chunk),available)
     if len(chunk)>available:failure='log_limit';break
    if failure:break
  if failure:stop();return failure
  try:return child.wait(timeout=None if deadline is None else max(0,deadline-time.monotonic()))
  except subprocess.TimeoutExpired:stop();return 'timeout'
 except BaseException:
  stop();raise
 finally:
  selector.close();child.stdout.close()
