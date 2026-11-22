with Directory_Blocks;

generic
   with procedure Write_Block
     (Data : Directory_Blocks.Block_Data;
      Size : Directory_Blocks.Block_Length; Success : out Boolean);
package Directory_Commit is
   type Commit_Result is (Committed, Original_Restored, Recovery_Required);
   procedure Commit
     (Original, Replacement : Directory_Blocks.Block_Data;
      Size : Directory_Blocks.Block_Length; Result : out Commit_Result);
end Directory_Commit;
