with System.Address_To_Access_Conversions;
with CCL_Manifest_Bindings;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Logging;
with CuBit.Log_Records;
package body Mesa_Probe_Log is
   use type System.Address;
   type Log_Context is limited record
      Writer : CuBit.Logging.Publisher
        (CapabilitySlot (CCL_Manifest_Bindings.Slot_Logstore));
      Enabled : Boolean := True;
   end record;
   package Pointers is new System.Address_To_Access_Conversions (Log_Context);

   --  A copy into the publisher's ring (CuBit.Logging): no IPC, no pacing,
   --  no completion. A shed record is counted (Dropped, in the summary).
   procedure Emit (Context, Text : System.Address; Length : Unsigned_32) is
      Item : constant Pointers.Object_Pointer := Pointers.To_Pointer (Context);
      Ignore_Submitted : Boolean;
   begin
      if Context = System.Null_Address or Text = System.Null_Address or
        Length not in 1 .. 191 then return; end if;
      if not Item.Enabled then return; end if;
      declare
         View : String (1 .. Natural (Length)) with Import, Address => Text;
         Record_Value : constant CuBit.Log_Records.Decoded :=
           CuBit.Log_Records.Make (View);
      begin
         if Record_Value.Success then
            CuBit.Logging.Emit (Item.Writer, Record_Value.Value, Ignore_Submitted);
         end if;
      end;
   end Emit;

   function Run (Callback : Probe_Callback) return Unsigned_32 is
      Item : aliased Log_Context;
      Result : Unsigned_32;
      Done, Drained : Boolean;
      Ignore : Unsigned_64;
   begin
      if Callback = null then return 1; end if;
      Result := Callback (Item'Address);
      declare
         Hex_Digits : constant String := "0123456789ABCDEF";
         Hex : String (1 .. 16);
         Loss : Unsigned_64 := CuBit.Logging.Dropped (Item.Writer);
      begin
         for I in reverse Hex'Range loop
            Hex (I) := Hex_Digits (Natural (Loss and 15) + 1);
            Loss := Shift_Right (Loss, 4);
         end loop;
         declare
            Summary : constant String := "MESA-LOG bridge dropped(hex)=" & Hex;
         begin
            debugPrint (Summary & ASCII.LF);
            Emit (Item'Address, Summary'Address, Summary'Length);
         end;
      end;
      CuBit.Logging.Flush (Item.Writer, Drained);
      CuBit.Logging.Disconnect (Item.Writer, Done);
      if not Done then
         debugPrint ("MESA-DISCOVERY logger retiring; storage retained" & ASCII.LF);
      end if;
      while not Done loop
         CuBit.Logging.Disconnect (Item.Writer, Done);
         if not Done then Ignore := syscall (SYSCALL_SLEEP, 100); end if;
      end loop;
      return Result;
   end Run;
end Mesa_Probe_Log;
