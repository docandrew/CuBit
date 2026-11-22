package body Intel_GPU_Initial_Ring_Publish is
   -- LRC register state starts at page1; CTX_RING_HEAD=5, TAIL=7.
   Head : constant Unsigned_32 := 4096 + 5 * 4;
   Tail : constant Unsigned_32 := 4096 + 7 * 4;
   Ring : constant Unsigned_32 := 65536;
   Bytes : constant Unsigned_32 :=
     Intel_GPU_ADLN_Context_Init.Command_Words'Length * 4;
   procedure Publish (Object : in out Attempt;
                      Segment : Intel_GPU_ADLN_Context_Init.Segment;
                      Status : out Result)
   is
      Data : Unsigned_32;
      OK : Boolean;
      function Owned return Boolean is
      begin
         if Exclusive_Ready then return True; end if;
         Status := Ownership_Lost; return False;
      end Owned;
      function Check (Offset, Expected : Unsigned_32) return Boolean is
      begin
         if not Owned then return False; end if;
         Read_32 (Offset, Data, OK);
         if not Owned then return False; end if;
         if not OK then Status := Read_Failed; return False; end if;
         if Data /= Expected then Status := Verify_Failed; return False; end if;
         return True;
      end Check;
      function Store (Offset, Value : Unsigned_32) return Boolean is
      begin
         if not Owned then return False; end if;
         Write_32 (Offset, Value, OK);
         if not Owned then return False; end if;
         if not OK then Status := Write_Failed; return False; end if;
         return True;
      end Store;
      function Visible (Offset, Count : Unsigned_32) return Boolean is
      begin
         if not Owned then return False; end if;
         OK := Flush (Offset, Count);
         if not Owned then return False; end if;
         if not OK then Status := Flush_Failed; return False; end if;
         return True;
      end Visible;
   begin
      Status := Rejected;
      if Object.Used or else not Segment.Valid then return; end if;
      Object.Used := True;
      if not Check (Head, 0) or else not Check (Tail, 0) then return; end if;
      for I in Segment.Words'Range loop
         if not Store (Ring + Unsigned_32 (I) * 4, Segment.Words (I)) then
            return;
         end if;
      end loop;
      for I in Segment.Words'Range loop
         if not Check (Ring + Unsigned_32 (I) * 4, Segment.Words (I)) then
            return;
         end if;
      end loop;
      if not Visible (Ring, Bytes) or else not Store (Tail, Bytes) or else
        not Check (Tail, Bytes) or else not Visible (Tail, 4)
      then return; end if;
      Status := Published;
   end Publish;
end Intel_GPU_Initial_Ring_Publish;
