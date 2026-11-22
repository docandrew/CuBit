from pathlib import Path
import argparse, hashlib, json, subprocess, tempfile
parser=argparse.ArgumentParser(description="Exercise the actual pinned Servo input coalescer with a removed-barrier negative control")
parser.add_argument('--servo',type=Path,required=True)
parser.add_argument('--output',type=Path,required=True)
args=parser.parse_args()
source=args.servo/'components/script/dom/document/document_event_handler.rs'
src=source.read_text()
a=src.index('    pub(crate) fn note_pending_input_event(');start=src.index('{',a);level=1;i=start+1
while level:
 level+=(src[i]=='{')-(src[i]=='}');i+=1
method=src[a:i]
barrier='''        // Coalescing cannot move motion or scrolling across a button, cancel,
        // focus, or other input boundary.
        if !matches!(event.event.event, InputEvent::MouseMove(..)) {
            *self.mouse_move_event_index.borrow_mut() = None;
        }
        if !matches!(event.event.event, InputEvent::Wheel(..)) {
            *self.wheel_event_index.borrow_mut() = None;
        }
'''
assert method.count(barrier)==1, 'expected exactly one production coalescing barrier'
fixed=method
method=method.replace(barrier,'',1)
prefix='''use std::cell::RefCell;
#[derive(Clone,Debug,PartialEq)] enum InputEvent { MouseMove(i32), Wheel(Wheel), Down, Up, MouseCancel }
#[derive(Clone,Debug,PartialEq)] struct Wheel {delta: Delta,point:i32}
#[derive(Clone,Debug,PartialEq)] struct Delta {mode:u8,x:i32,y:i32,z:i32}
#[derive(Clone,Debug)] struct Event {event:InputEvent,id:u64}
#[derive(Clone,Debug)] struct ConstellationInputEvent {event:Event}
#[derive(Default)] struct Handler {pending_input_events:RefCell<Vec<ConstellationInputEvent>>,mouse_move_event_index:RefCell<Option<usize>>,wheel_event_index:RefCell<Option<usize>>,coalesced_mouse_move_event_ids:RefCell<Vec<u64>>,coalesced_wheel_event_ids:RefCell<Vec<u64>>}
impl Handler {
'''
suffix='''}
fn run(events: Vec<InputEvent>, expected: Vec<InputEvent>) {
 let h=Handler::default();let total=events.len();
 for (i,event) in events.into_iter().enumerate(){h.note_pending_input_event(ConstellationInputEvent{event:Event{event,id:i as u64}});}
 assert_eq!(h.pending_input_events.borrow().iter().map(|e|e.event.event.clone()).collect::<Vec<_>>(),expected);
 let mut ids=h.pending_input_events.borrow().iter().map(|e|e.event.id).collect::<Vec<_>>();
 ids.extend(h.coalesced_mouse_move_event_ids.borrow().iter());ids.extend(h.coalesced_wheel_event_ids.borrow().iter());ids.sort();
 assert_eq!(ids,(0..total as u64).collect::<Vec<_>>());
}
fn wheel(x:i32)->InputEvent{InputEvent::Wheel(Wheel{delta:Delta{mode:0,x,y:0,z:0},point:x})}
fn main(){
 use InputEvent::{MouseMove,Down,Up,MouseCancel};
 for barrier in [Down,Up,MouseCancel,wheel(2)] {
  let events=vec![MouseMove(1),barrier,MouseMove(3)];run(events.clone(),events);
 }
 for barrier in [Down,Up,MouseCancel,MouseMove(2)] {
  let events=vec![wheel(1),barrier,wheel(3)];run(events.clone(),events);
 }
 run(vec![MouseMove(1),MouseMove(2),Down,MouseMove(3),MouseMove(4),Up],vec![MouseMove(2),Down,MouseMove(4),Up]);
 run(vec![wheel(1),wheel(2),MouseCancel,wheel(4),wheel(8)],vec![wheel(3),MouseCancel,InputEvent::Wheel(Wheel{delta:Delta{mode:0,x:12,y:0,z:0},point:8})]);
 println!("PASS 10 actual-handler coalescing/order/ack cases");
}
'''
# Summed wheel keeps newest point, not sum.
suffix=suffix.replace('vec![wheel(3),MouseCancel,','vec![InputEvent::Wheel(Wheel{delta:Delta{mode:0,x:3,y:0,z:0},point:2}),MouseCancel,')
args.output.mkdir(parents=True,exist_ok=True)
out=Path(tempfile.mkdtemp(prefix='pointer-coalescing-',dir=args.output))
for name,body in [('candidate',fixed),('baseline',method)]:
 p=out/(name+'.rs');p.write_text(prefix+body+suffix)
 subprocess.run(['rustc','--edition=2024',str(p),'-o',str(out/name)],check=True)
 result=subprocess.run([str(out/name)],capture_output=True,text=True);(out/(name+'.log')).write_text(result.stdout+result.stderr)
 assert (result.returncode==0)==(name=='candidate'),(name,result.stdout,result.stderr)
 print(name,result.returncode,result.stdout.strip())
(out/'result.json').write_text(json.dumps({'result':'PASS','source_sha256':hashlib.sha256(source.read_bytes()).hexdigest(),'cases':10,'removed_barrier_rejected':True},indent=2)+'\n')
print(out)
