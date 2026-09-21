package body Heap_Growth with SPARK_Mode is
   procedure Apply (Count : Natural; Success : out Boolean) is
      Added : Natural := 0;
      OK : Boolean;
   begin
      Success := False;
      while Added < Count loop
         Add (Added, OK);
         if not OK then
            for P in reverse 1 .. Added loop Unmap (P - 1); end loop;
            if Added > 0 then Synchronize; end if;
            for P in 1 .. Added loop Release_Latest; end loop;
            return;
         end if;
         Added := Added + 1;
         pragma Loop_Invariant (Added <= Count);
      end loop;
      Success := True;
   end Apply;
end Heap_Growth;
