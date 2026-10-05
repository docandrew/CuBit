"""Test patched DOM Rc sizing with an instrumented allocator/trait adapter.
Requires the patched Servo checkout; native tests cover the full trait path.
"""
from pathlib import Path
import subprocess,tempfile
root=Path(__file__).resolve().parents[2];servo=root/'userspace/rust/build/servo-work/servo'
mem=(servo/'components/script_bindings/mem.rs').read_text()
a=mem.index('pub(crate) unsafe fn malloc_size_of_including_raw_rc_self');b=mem.index('\n}',a)+2
helper=mem[a:b]
source=r'''
use std::alloc::{GlobalAlloc,Layout,System};
use std::sync::atomic::{AtomicUsize,Ordering};
static BASE:AtomicUsize=AtomicUsize::new(0);static SIZE:AtomicUsize=AtomicUsize::new(0);static DROPS:AtomicUsize=AtomicUsize::new(0);
struct Alloc;
unsafe impl GlobalAlloc for Alloc {
 unsafe fn alloc(&self,l:Layout)->*mut u8 {let p=unsafe{System.alloc(l)}; BASE.store(p as usize,Ordering::SeqCst);SIZE.store(l.size(),Ordering::SeqCst);p}
 unsafe fn dealloc(&self,p:*mut u8,l:Layout){unsafe{System.dealloc(p,l)}}
}
#[global_allocator] static ALLOC:Alloc=Alloc;
mod malloc_size_of {
 use std::rc::Rc;
 pub struct MallocSizeOfOps;
 impl MallocSizeOfOps {pub unsafe fn malloc_size_of<T>(&mut self,p:*const T)->usize{assert_eq!(p as usize,crate::BASE.load(crate::Ordering::SeqCst),"interior pointer sent to allocator");crate::SIZE.load(crate::Ordering::SeqCst)}}
 pub trait MallocSizeOf {fn size_of(&self,ops:&mut MallocSizeOfOps)->usize;}
 pub trait MallocUnconditionalSizeOf {fn unconditional_size_of(&self,ops:&mut MallocSizeOfOps)->usize;}
 impl<T:MallocSizeOf> MallocUnconditionalSizeOf for Rc<T> {
  fn unconditional_size_of(&self,ops:&mut MallocSizeOfOps)->usize {
   let offset=std::mem::align_of::<T>().max(2*std::mem::size_of::<usize>());
   (unsafe{ops.malloc_size_of(Rc::as_ptr(self).wrapping_byte_sub(offset))})+ (**self).size_of(ops)
  }
 }
}
use malloc_size_of::{MallocSizeOf,MallocSizeOfOps};use std::os::raw::c_void;
'''+helper+r'''
#[repr(align(64))] struct Aligned(u64);
impl Drop for Aligned {fn drop(&mut self){DROPS.fetch_add(1,Ordering::SeqCst);}}
impl MallocSizeOf for Aligned {fn size_of(&self,_:&mut MallocSizeOfOps)->usize{self.0 as usize}}
fn main(){
 let value=std::rc::Rc::new(Aligned(37));let bytes=SIZE.load(Ordering::SeqCst);let weak=std::rc::Rc::downgrade(&value);
 let raw=std::rc::Rc::as_ptr(&value).cast::<c_void>();
 for _ in 0..100 {assert_eq!(unsafe{malloc_size_of_including_raw_rc_self::<Aligned>(&mut MallocSizeOfOps,raw)},bytes+37);assert_eq!(std::rc::Rc::strong_count(&value),1);assert_eq!(std::rc::Rc::weak_count(&value),1);}
 assert_eq!(DROPS.load(Ordering::SeqCst),0);drop(value);assert_eq!(DROPS.load(Ordering::SeqCst),1);assert!(weak.upgrade().is_none());drop(weak);
 println!("PASS Rc allocation base, alignment64, repeated sizing, unchanged reference counts and single destruction");
}
'''
d=Path(tempfile.mkdtemp(prefix='penny-rc-size-'));(d/'test.rs').write_text(source)
try:
 subprocess.run(['rustc','--edition=2024',str(d/'test.rs'),'-o',str(d/'test')],check=True)
 subprocess.run([str(d/'test')],check=True)
 bad=source.replace('object.unconditional_size_of(ops)', 'unsafe { ops.malloc_size_of(obj) + (*(obj as *const T)).size_of(ops) }')
 (d/'bad.rs').write_text(bad)
 subprocess.run(['rustc','--edition=2024',str(d/'bad.rs'),'-o',str(d/'bad')],check=True)
 result=subprocess.run([str(d/'bad')],capture_output=True,text=True)
 assert result.returncode!=0 and 'interior pointer sent to allocator' in result.stderr
 (d/'negative.log').write_text(result.stderr)
 (d/'bad').unlink()
 print('PASS old interior-pointer negative control rejected')
 print('ARTIFACTS',d)
finally:(d/'test').unlink(missing_ok=True)
