with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Capability_Grants; use CuBit.Capability_Grants;
with Intel_GPU_Broker_Request;
with Intel_Render_Launch_Client;
procedure Launch_Client_Tests is
   package R renames Intel_GPU_Broker_Request;
   package C is new Intel_Render_Launch_Client (31, 1000, 1015);
   use type C.Phase;
   Object : C.Launcher;
   Child : Recipient;
   ID, Bad : C.Ticket;
   Used : Boolean;
   Reply : CompletionEntry := (1000, 77, 0,
     ((R.Label, 4, 0, 0), [1, 0, 1000, 16#7_0000002A#]), True);
begin
   Inspection := [1, 9, 0, 42, 0, 7];
   Child := Capture (7);
   Inspection := [1, 3, 0, 77, 0, 9];
   C.Start (Object, False, Child, 40, 4, Bad);
   pragma Assert (Bad = 0 and Last_Operation = 0);
   Application_Inspection (5) := 8;
   C.Start (Object, True, Child, 40, 4, Bad);
   pragma Assert (Bad = 0 and Last_Operation = 0);
   Application_Inspection (5) := 7;
   C.Start (Object, True, Child, 40, 4, ID);
   pragma Assert (ID = 1 and C.State (Object, ID) = C.Pending);
   pragma Assert (Last_Arguments = [16#9_0000004D#, 40, 40, 9, 0, 0]);
   pragma Assert (Last_Token = 1000 and Last_Submit.words = [1, 40, 4, 1000]);
   C.Start (Object, True, Child, 40, 4, Bad);
   pragma Assert (Bad = 0);
   C.Complete (Object, Reply, Used);
   pragma Assert (Used and C.State (Object, ID) = C.Admitted);
   C.Complete (Object, Reply, Used);
   pragma Assert (not Used);
   declare
      Fresh : C.Launcher;
      Receipt : CompletionEntry := Reply;
   begin
      Submit_Result := False;
      C.Start (Fresh, True, Child, 40, 4, ID);
      pragma Assert (ID = 1 and C.State (Fresh, ID) = C.Rejected);
      Submit_Result := True;
      C.Start (Fresh, True, Child, 40, 5, ID);
      pragma Assert (ID = 2 and Last_Token = 1001 and
        Last_Submit.words (1) = 41);
      -- Invalid receipt and unrelated token leave the real request pending.
      Receipt.valid := False;
      Receipt.token := 1001;
      C.Complete (Fresh, Receipt, Used);
      pragma Assert (not Used and C.State (Fresh, ID) = C.Pending);
      Receipt.valid := True;
      Receipt.token := 999;
      C.Complete (Fresh, Receipt, Used);
      pragma Assert (not Used and C.State (Fresh, ID) = C.Pending);
   end;
   for Fault in 0 .. 8 loop
      declare
         Fresh : C.Launcher;
         Receipt : CompletionEntry := Reply;
      begin
         C.Start (Fresh, True, Child, 40, 4, ID);
         case Fault is
            when 0 => Receipt.status := 1;
            when 1 => Receipt.from := 78;
            when 2 => Receipt.msg.tag.label := 0;
            when 3 => Receipt.msg.tag.length := 3;
            when 4 => Receipt.msg.words (0) := 2;
            when 5 => Receipt.msg.words (2) := 1001;
            when 6 => Receipt.msg.words (3) := 16#8_0000002A#;
            when 7 => Receipt.msg.words (1) := 2;
            when others => Receipt.msg.words (1) := 1;
         end case;
         C.Complete (Fresh, Receipt, Used);
         pragma Assert (Used and C.State (Fresh, ID) =
           (if Fault = 8 then C.Rejected else C.Uncertain));
         C.Complete (Fresh, Reply, Used);
         pragma Assert (not Used); -- no late success after uncertain result
      end;
   end loop;
   Grant_Result := Unsigned_64'Last;
   for I in 2 .. C.Capacity loop
      C.Start (Object, True, Child, 40, CapabilitySlot (I + 4), ID);
      pragma Assert (ID = I and C.State (Object, ID) = C.Rejected);
   end loop;
   C.Start (Object, True, Child, 40, 30, Bad);
   pragma Assert (Bad = 0); -- failed installations do not recycle source slots
   Put_Line ("launch client PASS");
end Launch_Client_Tests;
