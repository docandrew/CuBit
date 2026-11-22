package body Intel_GPU_Pipe_IRQ is
   use Interfaces;
   Value : Phase := Fresh;
   function State return Phase is (Value);
   Read_Offset, Read_Value, Expected : Unsigned_32 := 0;
   function Last_Read_Offset return Unsigned_32 is (Read_Offset);
   function Last_Read_Value return Unsigned_32 is (Read_Value);
   function Expected_Value return Unsigned_32 is (Expected);
   Base : constant Unsigned_32 :=
     16#44400# + 16 * Intel_GPU_Display_Topology.Pipe'Pos (Item);
   -- ADL-N display13 has planes1..5. IMR bits17/18 are the older
   -- plane6/7 flip-done masks, absent here (NUC reads FFF9FFFF after ~0).
   -- Keep the all-ones mask write, but do not demand reserved bits read one.
   IMR_Readback_Mask : constant Unsigned_32 := 16#FFF9FFFF#;
   procedure Quiesce
     (Owner_Ready, Power_Ready, Delivery_Blocked : Boolean; Status : out Result)
   is
      Data : Unsigned_32;
      OK : Boolean;
      function Store (Offset, Bits : Unsigned_32) return Boolean is
      begin
         Write_32 (Base + Offset, Bits, OK);
         if not OK then Status := Write_Failed; end if;
         return OK;
      end Store;
      function Load (Offset : Unsigned_32) return Boolean is
      begin
         Read_32 (Base + Offset, Data, OK);
         Read_Offset := Base + Offset; Read_Value := Data;
         if not OK then Status := Read_Failed; end if;
         return OK;
      end Load;
      function Verify (Offset, Wanted : Unsigned_32) return Boolean is
         Mask : constant Unsigned_32 :=
           (if Offset = 4 then IMR_Readback_Mask else Unsigned_32'Last);
      begin
         Expected := Wanted and Mask;
         if not Load (Offset) then return False; end if;
         if (Data and Mask) /= Expected then Status := Verify_Failed; return False; end if;
         return True;
      end Verify;
   begin
      Status := Rejected;
      if Value /= Fresh or else not Owner_Ready or else not Power_Ready or else
        not Delivery_Blocked
      then return; end if;
      Value := Uncertain;
      if not Store (4, Unsigned_32'Last) or else
        not Verify (4, Unsigned_32'Last) or else
        not Store (12, 0) or else not Verify (12, 0)
      then return; end if;
      -- W1C identity registers may queue two events. The first posting read
      -- need not be zero; the second clear must drain the remaining event.
      for Pass in 1 .. 2 loop
         if not Store (8, Unsigned_32'Last) or else not Load (8) then return; end if;
      end loop;
      if Data /= 0 then Status := Pending_Events; return; end if;
      if not Verify (4, Unsigned_32'Last) or else not Verify (12, 0) then return; end if;
      Value := Masked;
      Status := Complete;
   end Quiesce;
end Intel_GPU_Pipe_IRQ;
