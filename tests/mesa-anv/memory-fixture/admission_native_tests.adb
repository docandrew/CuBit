with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Capability_Grants; use CuBit.Capability_Grants;
with Intel_Render_Admission;
with Intel_Render_Admission_Native; use Intel_Render_Admission_Native;
procedure Admission_Native_Tests is
   package Core renames Intel_Render_Admission;
   use type Core.Phase;
   Item : Broker_Request;
   Target : Recipient;
   Receipt : CompletionEntry;
   Used : Boolean;
   Session : constant Unsigned_64 := 16#4750_0000_0000_0001#;
begin
   Inspection := [1, 1, 0, 42, 0, 7];
   Target := Capture (7);
   Inspection := [1, 11, 0, 77, 0, 9];
   Start (Item, Target, 31, 30, 4);
   Advance (Item, 100);
   pragma Assert (State (Item) = Core.Reserve_Pending);
   pragma Assert (Last_Submit.tag = (16#0A21#, 4, 0, 0));
   pragma Assert (Last_Submit.words = [1, 7 * 2 ** 32 + 42, 0, 0]);
   pragma Assert (Last_Slot = 31 and Last_Token = 100);
   Receipt := (token => 99, from => 77, status => 0,
               msg => ((16#0A21#, 4, 0, 0), [0, 1, Session, 0]), valid => True);
   Complete (Item, Receipt, Used); pragma Assert (not Used);
   Receipt.token := 100;
   Complete (Item, Receipt, Used);
   pragma Assert (Used and State (Item) = Core.Delegate_Ready);
   Inspection := [1, 9, 0, 42, 0, 7];
   Grant_Result := 0;
   Advance (Item, 101);
   pragma Assert (Last_Arguments = [9 * 2 ** 32 + 77, 30, 40, 1, Session, 0]);
   pragma Assert (State (Item) = Core.Delegate_Ready);
   -- Later inspection changes do not replace either captured recipient.
   Inspection := [1, 1, 0, 42, 0, 8];
   Advance (Item, 101);
   pragma Assert (Last_Arguments = [7 * 2 ** 32 + 42, 31, 4, 3, Session, 0]);
   pragma Assert (State (Item) = Core.Activate_Ready);
   Advance (Item, 101);
   pragma Assert (Last_Submit.words = [1, 7 * 2 ** 32 + 42, Session, 1]);
   Cancel (Item);
   Receipt.token := 101;
   Complete (Item, Receipt, Used);
   pragma Assert (Used and State (Item) = Core.Abort_Ready);
   Advance (Item, 102);
   pragma Assert (Last_Submit.words = [1, 7 * 2 ** 32 + 42, Session, 2]);
   Receipt.token := 102;
   Complete (Item, Receipt, Used);
   pragma Assert (Used and State (Item) = Core.Failed);
   for Fault in 0 .. 8 loop
      declare
         Broken : Broker_Request;
         Bad : CompletionEntry :=
           (token => 200, from => 77, status => 0,
            msg => ((16#0A21#, 4, 0, 0), [0, 1, 123, 0]), valid => True);
      begin
         Inspection := [1, 11, 0, 77, 0, 9];
         Start (Broken, Target, 31, 30, 4);
         Advance (Broken, 200);
         case Fault is
            when 0 => Bad.from := 78;
            when 1 => Bad.status := 1;
            when 2 => Bad.msg.tag.label := 0;
            when 3 => Bad.msg.tag.length := 3;
            when 4 => Bad.msg.tag.flags := 1;
            when 5 => Bad.msg.tag.reserved := 1;
            when 6 => Bad.msg.words (1) := 2;
            when 7 => Bad.msg.words (3) := 1;
            when others => Bad.valid := False;
         end case;
         Complete (Broken, Bad, Used);
         if Fault = 8 then
            pragma Assert (not Used and State (Broken) = Core.Reserve_Pending);
            Bad.valid := True;
            Cancel (Broken);
            Complete (Broken, Bad, Used);
            pragma Assert (Used and State (Broken) = Core.Abort_Ready);
         else
            pragma Assert (Used and State (Broken) = Core.Quarantined);
            Complete (Broken, Bad, Used); pragma Assert (not Used);
         end if;
      end;
   end loop;
   declare
      Rejected : Broker_Request;
   begin
      Inspection := [1, 11, 0, 77, 0, 9];
      Start (Rejected, Target, 31, 30, 4);
      Submit_Result := False;
      Advance (Rejected, 300);
      pragma Assert (State (Rejected) = Core.Failed);
      Submit_Result := True;
      Advance (Rejected, 301);
      pragma Assert (State (Rejected) = Core.Failed and Last_Token = 300);
   end;
   for Index in 1 .. 16 loop
      declare
         Mapped : Broker_Request;
         Tag : constant Unsigned_64 := Session - 1 + Unsigned_64 (Index);
         R : constant CompletionEntry :=
           (token => 350, from => 77, status => 0,
            msg => ((16#0A21#, 4, 0, 0), [0, 1, Tag, 0]), valid => True);
      begin
         Inspection := [1, 11, 0, 77, 0, 9];
         Start (Mapped, Target, 31, 30, 4);
         Advance (Mapped, 350);
         Complete (Mapped, R, Used);
         Inspection := [1, 9, 0, 42, 0, 7];
         Grant_Result := 0;
         Advance (Mapped, 351);
         pragma Assert (Last_Arguments =
           [9 * 2 ** 32 + 77, 30, 39 + Unsigned_64 (Index), 1, Tag, 0]);
         pragma Assert (State (Mapped) = Core.Delegate_Ready and Last_Token = 350);
         Advance (Mapped, 351);
         pragma Assert (Last_Arguments = [7 * 2 ** 32 + 42, 31, 4, 3, Tag, 0]);
         pragma Assert (State (Mapped) = Core.Activate_Ready and Last_Token = 350);
      end;
   end loop;
   -- Each partial-install failure must abort, never activate. No destination
   -- slot is revoked/recycled by this adapter, even after successful abort.
   for Fault in 0 .. 8 loop
      declare
         Partial : Broker_Request;
         R : CompletionEntry :=
           (token => 400, from => 77, status => 0,
            msg => ((16#0A21#, 4, 0, 0), [0, 1, Session, 0]), valid => True);
      begin
         Inspection := [1, 11, 0, 77, 0, 9];
         Start (Partial, Target, 31, 30, 4);
         Advance (Partial, 400);
         if Fault = 6 then R.msg.words (2) := Session - 1;
         elsif Fault = 7 then R.msg.words (2) := Session + 16;
         end if;
         Complete (Partial, R, Used);
         Inspection := [1, 9, 0, 42, 0, 7];
         if Fault = 0 then Inspection (5) := 8; end if;
         if Fault = 1 then Inspection (0) := 6; end if;
         if Fault = 8 then Inspection (1) := 8; end if;
         Grant_Result := (if Fault = 2 then Unsigned_64'Last else 0);
         Last_Arguments := [others => 0];
         Advance (Partial, 401);
         if Fault in 0 | 1 | 6 | 7 | 8 then
            pragma Assert (Last_Arguments = [0, 0, 0, 0, 0, 0]);
         elsif Fault in 3 .. 5 then
            pragma Assert (State (Partial) = Core.Delegate_Ready);
            pragma Assert (Last_Arguments (2) = 40);
            if Fault = 3 then
               Cancel (Partial);
            else
               Grant_Result := (if Fault = 4 then Unsigned_64'Last else 0);
               Advance (Partial, 401);
               if Fault = 5 then
                  pragma Assert (State (Partial) = Core.Activate_Ready);
                  Cancel (Partial);
               end if;
            end if;
         end if;
         pragma Assert (State (Partial) = Core.Abort_Ready);
         Advance (Partial, 402);
         pragma Assert (Last_Submit.words (3) = 2);
      end;
   end loop;
   Put_Line ("NATIVE-ADMISSION-ADAPTER: PASS mocked IPC and delegation");
end Admission_Native_Tests;
