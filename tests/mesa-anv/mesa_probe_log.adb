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
      Token : Unsigned_64 := 16#4D45_0000#;
      Enabled : Boolean := True;
      Published : Boolean := False;
   end record;
   package Pointers is new System.Address_To_Access_Conversions (Log_Context);

   procedure Emit (Context, Text : System.Address; Length : Unsigned_32) is
      Item : constant Pointers.Object_Pointer := Pointers.To_Pointer (Context);
      Receipt : CompletionEntry;
      Submitted, Handled : Boolean;
      Ignore : Unsigned_64;
   begin
      if Context = System.Null_Address or Text = System.Null_Address or
        Length not in 1 .. 191 then return; end if;
      if not Item.Enabled or else Item.Token = Unsigned_64'Last then return; end if;
      declare
         View : String (1 .. Natural (Length)) with Import, Address => Text;
         Record_Value : constant CuBit.Log_Records.Decoded :=
           CuBit.Log_Records.Make (View);
      begin
         if not Record_Value.Success then return; end if;
         declare
            Valid_Record : constant CuBit.Log_Records.Log_Record := Record_Value.Value;
         begin
         -- Diagnostic fixture only: the collector admits a bounded burst and
         -- replenishes one credit per 100 ms. Do not let initialization trace
         -- volume hide its final result. Pace NEW records, never replay a
         -- rejected record or weaken the production collector's budget.
         if Item.Published then
            Ignore := syscall (SYSCALL_SLEEP, 125);
         end if;
         Item.Published := True;
         Item.Token := Item.Token + 1;
         CuBit.Logging.Emit (Item.Writer, Valid_Record, Item.Token, Submitted);
         end;
      end;
      if not Submitted then Item.Enabled := False; return; end if;
      -- This fixture has no other async IPC; GPU transport uses capCall.
      -- Never replay a lost log or reuse storage with an outstanding receipt.
      for Attempt in 1 .. 200 loop
         if Poll_Completion (Receipt'Address) = 1 then
            CuBit.Logging.Complete (Item.Writer, Receipt, Handled);
         end if;
         exit when not CuBit.Logging.Pending (Item.Writer);
         Ignore := syscall (SYSCALL_SLEEP, 1);
      end loop;
      if CuBit.Logging.Pending (Item.Writer) then Item.Enabled := False; end if;
   end Emit;

   function Run (Callback : Probe_Callback) return Unsigned_32 is
      Item : aliased Log_Context;
      Result : Unsigned_32;
      Done, Handled : Boolean;
      Receipt : CompletionEntry;
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
      CuBit.Logging.Disconnect (Item.Writer, Done);
      if not Done then
         debugPrint ("MESA-DISCOVERY logger retiring; storage retained" & ASCII.LF);
      end if;
      while not Done loop
         if Poll_Completion (Receipt'Address) = 1 then
            CuBit.Logging.Complete (Item.Writer, Receipt, Handled);
         end if;
         CuBit.Logging.Disconnect (Item.Writer, Done);
         if not Done then Ignore := syscall (SYSCALL_SLEEP, 100); end if;
      end loop;
      return Result;
   end Run;
end Mesa_Probe_Log;
