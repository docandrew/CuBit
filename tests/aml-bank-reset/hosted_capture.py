"""Bounded subprocess capture with cleanup ownership established before unmasking."""
import os,selectors,signal,subprocess,time

class CaptureInterrupted(RuntimeError):
 """Cancellation must not be InterruptedError, which selectors may swallow."""

def capture(command,cwd,log,timeout,limit=1024*1024):
 signals={signal.SIGTERM,signal.SIGINT}
 original_mask=signal.pthread_sigmask(signal.SIG_BLOCK,signals)
 child=None;selector=None
 def child_mask():signal.pthread_sigmask(signal.SIG_SETMASK,original_mask)
 def stop():
  if child is None:return
  # Caller handlers may raise. Keep repeated interrupts deferred until reaping.
  cleanup_mask=signal.pthread_sigmask(signal.SIG_BLOCK,signals)
  try:
   try:os.killpg(child.pid,signal.SIGTERM)
   except ProcessLookupError:pass
   try:child.wait(timeout=3)
   except subprocess.TimeoutExpired:
    try:os.killpg(child.pid,signal.SIGKILL)
    except ProcessLookupError:pass
    child.wait()
   try:os.killpg(child.pid,signal.SIGKILL)
   except ProcessLookupError:pass
  finally:signal.pthread_sigmask(signal.SIG_SETMASK,cleanup_mask)
 try:
  child=subprocess.Popen(command,cwd=cwd,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,
    start_new_session=True,bufsize=0,preexec_fn=child_mask)
  selector=selectors.DefaultSelector();selector.register(child.stdout,selectors.EVENT_READ)
  # Any pending startup signal now raises inside our cleanup-protected region.
  signal.pthread_sigmask(signal.SIG_SETMASK,original_mask)
  deadline=None if timeout is None else time.monotonic()+timeout;written=0;failure=None
  with log.open('wb') as output:
   while selector.get_map():
    remaining=None if deadline is None else deadline-time.monotonic()
    if remaining is not None and remaining<=0:failure='timeout';break
    for key,_ in selector.select(0.25 if remaining is None else min(remaining,0.25)):
     chunk=os.read(key.fileobj.fileno(),min(4096,limit-written+1))
     if not chunk:selector.unregister(key.fileobj);continue
     available=limit-written;output.write(chunk[:available]);written+=min(len(chunk),available)
     if len(chunk)>available:failure='log_limit';break
    if failure:break
  if failure:stop();return failure
  try:return child.wait(timeout=None if deadline is None else max(0,deadline-time.monotonic()))
  except subprocess.TimeoutExpired:stop();return 'timeout'
 except BaseException:
  stop();raise
 finally:
  try:
   if selector is not None:selector.close()
   if child is not None and child.stdout is not None:child.stdout.close()
  finally:signal.pthread_sigmask(signal.SIG_SETMASK,original_mask)
