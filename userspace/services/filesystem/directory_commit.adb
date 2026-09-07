package body Directory_Commit is
   procedure Commit
     (Original, Replacement : Directory_Blocks.Block_Data;
      Size : Directory_Blocks.Block_Length; Result : out Commit_Result)
   is
      Success : Boolean;
   begin
      Write_Block (Replacement, Size, Success);
      if Success then
         Result := Committed;
         return;
      end if;
      --  A failed I/O may have changed part or all of the block. Never assume
      --  "failure" means "nothing written", nor report success after rollback.
      Write_Block (Original, Size, Success);
      Result := (if Success then Original_Restored else Recovery_Required);
   end Commit;
end Directory_Commit;
