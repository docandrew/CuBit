"""Generate the browser seek fixture using a supplied FFmpeg executable."""
from pathlib import Path
import argparse,base64,hashlib,json,subprocess
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--ffmpeg',required=True)
p.add_argument('--output',required=True,type=Path)
a=p.parse_args();a.output.mkdir(parents=True,exist_ok=True)
video=a.output/'seek.webm'
cmd=[a.ffmpeg,'-v','error','-y','-f','lavfi','-i','testsrc2=size=640x360:rate=30:duration=12','-f','lavfi','-i','aevalsrc=0.3*sin(2*PI*(300+100*floor(t))*t)|0.2*sin(2*PI*(300+100*floor(t))*t):s=48000:d=12','-map','0:v','-map','1:a','-c:v','libvpx','-threads','1','-g','30','-b:v','600k','-c:a','libopus','-b:a','96k','-vbr','off','-t','12',str(video)]
subprocess.run(cmd,check=True)
url='data:video/webm;base64,'+base64.b64encode(video.read_bytes()).decode()
html=Path(__file__).with_name('seek.html').read_text().replace('__MEDIA_URL__',json.dumps(url))
(a.output/'seek.html').write_text(html)
(a.output/'pages').write_text('data:text/html;base64,'+base64.b64encode(html.encode()).decode()+'\n')
(a.output/'provenance.json').write_text(json.dumps({'command':cmd,'sha256':hashlib.sha256(video.read_bytes()).hexdigest(),'audio_hz':'300 + 100 * floor(media_seconds)'},indent=2)+'\n')
