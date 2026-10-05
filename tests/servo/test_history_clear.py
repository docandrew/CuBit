"""Compile the exact history-cleanup patch with a controlled lifecycle model.

Native script/paint retirement and memory convergence require separate tests.
"""
from pathlib import Path
import subprocess
import tempfile

root=Path(__file__).resolve().parents[2]
method=(root/'userspace/servo/history_clear.rs').read_text()
preamble=r'''
use std::collections::{HashMap, HashSet};
type WebViewId=u32;
#[derive(Clone)]
struct Diff { old:Option<u32>, new:Option<u32> }
impl Diff {
    fn alive_old_pipeline(&self)->Option<u32>{self.old}
    fn alive_new_pipeline(&self)->Option<u32>{self.new}
}
struct History { past:Vec<Diff>, future:Vec<Diff> }
struct View { session_history:History }
struct Context { pipeline_id:u32, pipelines:HashSet<u32>, active:bool, view:u32 }
struct Pipeline { webview_id:u32, browsing_context_id:u32,
    history_state_id:Option<u32>, history_states:HashSet<u32>, children:Vec<u32> }
#[derive(Debug, PartialEq)]enum DiscardBrowsingContext { No }
#[derive(Debug, PartialEq)]enum ExitPipelineMode { Normal }
enum ScriptThreadMessage { RemoveHistoryStates(u32,Vec<u32>) }
struct Model {
    webviews:HashMap<u32,View>, pipelines:HashMap<u32,Pipeline>,
    browsing_contexts:HashMap<u32,Context>, closed:Vec<u32>,
    states:HashMap<u32,HashSet<u32>>, notifications:Vec<u32>
}
impl Model {
    fn fully_active_browsing_contexts_iter(&self,v:u32)->impl Iterator<Item=&Context> {
        self.browsing_contexts.values().filter(move |c|c.active && c.view==v)
    }
    fn notify_history_changed(&mut self,v:u32){self.notifications.push(v);}
    fn send_message_to_pipeline(&mut self,id:u32,msg:ScriptThreadMessage,_why:&str)->bool{
        let ScriptThreadMessage::RemoveHistoryStates(target,states)=msg;
        assert_eq!(id,target);self.states.entry(id).or_default().extend(states);true
    }
    fn close_pipeline(&mut self,id:u32,discard:DiscardBrowsingContext,exit:ExitPipelineMode){
        assert_eq!(discard,DiscardBrowsingContext::No);assert_eq!(exit,ExitPipelineMode::Normal);
        assert!(!self.closed.contains(&id),"duplicate close {id}");self.closed.push(id);
        let children=self.pipelines[&id].children.clone();
        for child in children {
            if let Some(context)=self.browsing_contexts.remove(&child) {
                for p in context.pipelines {self.close_pipeline(p,DiscardBrowsingContext::No,ExitPipelineMode::Normal);}
            }
        }
        if let Some(context)=self.browsing_contexts.get_mut(&id){context.pipelines.remove(&id);}
        // The map entry stays until an asynchronous exit acknowledgment.
    }
'''
trailer=r'''
}
fn fixture()->Model {
    let mut m=Model{webviews:HashMap::new(),pipelines:HashMap::new(),browsing_contexts:HashMap::new(),
        closed:vec![],states:HashMap::new(),notifications:vec![]};
    m.webviews.insert(10,View{session_history:History{
        past:[1,1,2,4,5,6,7].iter().map(|id|Diff{old:Some(*id),new:None}).collect(),
        future:vec![Diff{old:None,new:Some(3)},Diff{old:None,new:Some(2)},Diff{old:None,new:None}]
    }});
    for id in 1..=9 {
        let view=if id==4{20}else{10};
        m.browsing_contexts.insert(id,Context{pipeline_id:id,
            pipelines:if id==5{HashSet::new()}else{HashSet::from([id])},active:id==2||id==8,view});
        m.pipelines.insert(id,Pipeline{webview_id:view,browsing_context_id:id,
            history_state_id:Some(11),history_states:HashSet::from([10,11,12]),
            children:if id==6{vec![7]}else{vec![]}});
    }
    m.pipelines.get_mut(&8).unwrap().history_state_id=None;
    m
}
fn main(){
    for _ in 0..1000 {
        let mut m=fixture();m.handle_clear_session_history(10);
        assert_eq!(m.closed.iter().copied().collect::<HashSet<_>>(),HashSet::from([1,3,6,7]));
        assert_eq!(m.notifications,vec![10]);
        assert!(m.webviews[&10].session_history.past.is_empty());
        assert!(m.webviews[&10].session_history.future.is_empty());
        assert_eq!(m.pipelines[&2].history_states,HashSet::from([11]));
        assert!(m.pipelines[&8].history_states.is_empty());
        assert_eq!(m.states[&2],HashSet::from([10,12]));
        assert_eq!(m.states[&8],HashSet::from([10,11,12]));
        assert_eq!(m.pipelines[&4].history_states,HashSet::from([10,11,12]));
        assert!(m.browsing_contexts[&9].pipelines.contains(&9)); // pending navigation intact
        m.handle_clear_session_history(10);assert_eq!(m.closed.len(),4);
        m.handle_clear_session_history(999);assert_eq!(m.notifications,vec![10,10]);
    }
    println!("PASS history cleanup: 1000 orders; past/future, duplicate/recursive retirement, active state, foreign/closing/pending pipelines, repeated clear");
}
'''
with tempfile.TemporaryDirectory(prefix='penny-history-') as tmp:
    d=Path(tmp);(d/'test.rs').write_text(preamble+method+trailer)
    subprocess.run(['rustc','--edition=2024',str(d/'test.rs'),'-o',str(d/'test')],check=True)
    subprocess.run([str(d/'test')],check=True)
