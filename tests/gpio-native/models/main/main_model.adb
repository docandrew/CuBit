pragma Ada_2022;
with Ada.Command_Line;
with GPIO_Channel_Badges;
package body Main_Model is
   use type P.Process_ID, W.Word, I.Word;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Why; end if; end Check;
   procedure Initialize is
   begin
      Current := Scenario'Value (Ada.Command_Line.Argument (1));
      for Pin in G.ACPI_Pin range 0 .. 359 loop
         declare L : constant G.Location := G.Locate (Pin); begin
            if L.Found then
               Memory (L.Resource) (Natural (L.Enable_Register) / 4) := 16#A500_0000#;
               Memory (L.Resource) (Natural (L.Status_Register) / 4) := 16#A500_0000#;
            end if;
         end;
      end loop;
      declare L : constant G.Location := G.Locate (160); begin
         Memory (L.Resource) (Natural (L.Host_Register) / 4) := 7;
         Memory (L.Resource) (Natural (L.Enable_Register) / 4) := 16#A500_0007#;
         Memory (L.Resource) (Natural (L.Status_Register) / 4) := 16#A500_0007#;
      end;
      Initial_Memory := Memory;
      Stop_Queued := Current = Early_Stop;
      Check (Badge = GPIO_Channel_Badges.Canonical ((160, GPIO_Channel_Badges.Level_Triggered, GPIO_Channel_Badges.Active_Low)),
         "fixture has canonical full-width GPv1 badge");
   end Initialize;
   function Authorized (Caller : P.Process_ID) return Boolean is (Caller = Launcher_ID);
   procedure Take_Offer (Caller : P.Process_ID; Receipt : out Natural; Owned : out Boolean) is
   begin
      Check (Authorized (Caller) and then not Offered, "one launcher offer accepted");
      Offered := True; Receipt := 17; Owned := True;
   end Take_Offer;
   procedure Inspect (Receipt : Natural; Sink : out Natural; Owner : out P.Process_ID; Value : out W.Word; Valid : out Boolean) is
   begin Check (Receipt = 17 and then Offered, "inspect owned symbolic sink"); Sink := 17; Owner := Client_ID; Value := Badge; Valid := True; end Inspect;
   procedure Discard (Receipt : Natural; Closed : out Boolean) is
   begin Check (Receipt = 17 and then Offered and then not Sink_Closed, "discard owned unused sink once"); Sink_Discards := Sink_Discards + 1; Sink_Closed := True; Closed := True; end Discard;
   procedure Save (Reply : out CuBit.Handles.Reply; Accepted : out Boolean) is
   begin Check (not Saved, "one saved admin reply"); Saved := True; Reply := 1; Accepted := True; end Save;
   procedure Answer_Saved (Reply : CuBit.Handles.Reply; Label : W.Operation; Status : W.Completion_Status; Delivered : out Boolean) is
      use type CuBit.Handles.Reply;
   begin
      Check (Reply = 1 and then Saved and then Label /= 0, "saved reply consumed exactly once");
      Saved := False; Saved_Answers := Saved_Answers + 1; Last_Admin := Status; Delivered := True;
   end Answer_Saved;
   procedure Answer_Current (Caller : P.Process_ID; Label : W.Operation; Status : W.Completion_Status; Delivered : out Boolean) is
   begin Check (Authorized (Caller) and then Label /= 0, "typed current reply"); Last_Admin := Status; Delivered := True; end Answer_Current;
   procedure Answer_Control (Reply : CuBit.Handles.Reply; Value : W.Completion; Delivered : out Boolean) is
      use type CuBit.Handles.Reply;
   begin Check (Reply /= 0 and then W.Valid (Value), "valid modeled control completion"); Control_Answers := Control_Answers + 1; Last_Control := Value; Delivered := True; end Answer_Control;
   procedure Try_Event (Sink : Natural; Value : W.Event; Accepted : out Boolean) is
   begin Check (Sink = 17 and then not Sink_Closed, "event goes to current sink"); Event_Deliveries := Event_Deliveries + 1; Last_Event := Value; Accepted := True; end Try_Event;
   procedure Close_Sink (Sink : Natural; Closed : out Boolean) is
      L : constant G.Location := G.Locate (160);
   begin
      Check (Sink = 17 and then not Sink_Closed and then (Memory (L.Resource) (Natural (L.Enable_Register) / 4) and 1) = 0,
         "close once only after confirmed mask"); Sink_Closes := Sink_Closes + 1; Sink_Closed := True; Closed := True;
   end Close_Sink;
end Main_Model;
