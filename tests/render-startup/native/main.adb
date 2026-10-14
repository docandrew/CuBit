--  The software-only child of the optional-render fixture (CAP-002): it
--  runs only if it holds no render capability (its optional render entry is
--  absent from its startup table) and its own identity can be captured from
--  its self endpoint. It prints that identity for check.py.
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Capability_Grants;
with CuBit.Handles.Startup;
with CuBit.Process_IDs;
with CuBit.Startup_State;
with CuBit.Startup_Tables;
procedure Main is
   package ST renames CuBit.Startup_Tables;
   use type ST.Capability_Kind;
   use type ST.Entry_State;
   Table : constant ST.Table := CuBit.Startup_State.Current;
   Render_Held : Boolean := False;
   Self : CuBit.Capability_Grants.Recipient;
begin
   for Index in 1 .. Table.Count loop
      Render_Held := Render_Held
        or else (Table.Entries (Index).Kind = ST.Render
                 and then Table.Entries (Index).State = ST.Granted);
   end loop;
   Self := CuBit.Capability_Grants.Capture
     (CuBit.Handles.Startup.Endpoint_Named (ST.Self_Text));
   if not CuBit.Startup_State.Is_Loaded or else Render_Held
     or else not CuBit.Capability_Grants.Valid (Self)
   then
      debugPrint ("TEST: FAIL render-startup software capability" & ASCII.LF);
      return;
   end if;
   debugPrint ("RENDER-STARTUP: software child incarnation=" &
     Unsigned_64'Image (CuBit.Process_IDs.To_Word
       (CuBit.Capability_Grants.Incarnation (Self))) & ASCII.LF);
end Main;
