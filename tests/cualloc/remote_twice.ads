--  Regression (found by the thread stress): a remote free that filled its
--  batch (the batch is published at once, full) was added a second time to
--  a fresh batch, so the owner later released the block twice; the second
--  release could free it after it had been handed out again. This drives
--  the exact sequence with one helper task: returns True when no block is
--  handed out while still live.
package Remote_Twice is
   function Run return Boolean;
end Remote_Twice;
