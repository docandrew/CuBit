--  A small block parked by its heap (ALLOC-001 step 13: the heap's spare,
--  still live in its core) that another thread frees again: the owner
--  un-parks it when it releases the remote free, so it is handed out once.
package Parked_Twice is
   function Run return Boolean;
end Parked_Twice;
