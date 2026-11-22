--  Hosted builds only: a pthread key whose destructor calls the C entry
--  point cualloc_thread_exit, so a Linux thread's CuAlloc heap is left for
--  adoption when the thread ends (glibc runs key destructors after the
--  thread's C++/Rust thread-local destructors, and again for up to four
--  rounds when a destructor sets the key anew, as Arm does when a late
--  free takes a heap again).
package Thread_Exit_Key is
   --  Set the calling thread's key (made once, on first use).
   procedure Arm;
end Thread_Exit_Key;
