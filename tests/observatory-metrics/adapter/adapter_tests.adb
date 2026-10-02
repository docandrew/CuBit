with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Metric_Protocol;
with CuBit.Metric_Records;
with Observatory_Metric_Observer;
with Observatory_Metric_Queries;
procedure Adapter_Tests is
   package G renames CuBit.Memory_Grants;
   package P renames CuBit.Metric_Protocol;
   package R renames CuBit.Metric_Records;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "adapter check" & Checks'Image; end if;
   end Check;
   procedure Scenario (Mode : Natural) is
      package O is new Observatory_Metric_Observer (17);
      Sequence : Unsigned_64 := 0;
      Submitted, Success : Boolean;
      Rows, Result : P.Summary_Page := [others => [others => 0]];
      Written : P.Row_Count;
      Next : Observatory_Metric_Queries.Cursor;
      C : CompletionEntry;
      Name : constant R.Slot_Words := R.Encode
        ((Kind => R.Describe, Key => 1, Declared => R.Counter,
          Measure => R.Count, Name => R.To_Name ("test")));
   begin
      G.Allow_Create := Mode /= 1; Allow_Submit := Mode /= 2;
      G.Allow_Revoke := True; G.Allow_Retirement := False;
      G.Creates := 0; G.Revokes := 0; G.Checks := 0; Submits := 0;
      O.Begin_Query (0, Sequence, 10, Submitted);
      if Mode = 1 then
         Check (not Submitted and O.Disabled and Submits = 0); return;
      elsif Mode = 2 then
         Check (not Submitted and O.Disabled);
         O.Tick (11); Check (G.Revokes = 1 and G.Checks = 1); return;
      end if;
      Check (Submitted and Sequence = 1 and Sent_Token = 1 and G.Creates = 1);
      Check (Sent.tag.label = P.Operation'Enum_Rep (P.Query_Summaries));
      Check (Sent.words (0) = 0 and Sent.words (3) = R.Page_Bytes);
      O.Begin_Query (0, Sequence, 12, Submitted);
      Check (not Submitted and Sequence = 1 and G.Creates = 1);
      Rows (0) (P.Row_Source) := Unsigned_64'Last;
      Rows (0) (P.Row_Publisher_Tag) := P.Publisher_Tag (1);
      Rows (0) (P.Row_Key) := 1;
      Rows (0) (P.Row_Kind) := R.Record_Kind'Enum_Rep (R.Counter);
      Rows (0) (P.Row_Unit) := R.Unit'Enum_Rep (R.Count);
      for W in 0 .. 3 loop Rows (0) (P.Row_First_Name + W) := Name (4 + W); end loop;
      if Mode = 4 then Rows (0) (P.Row_Flags) := 4; end if;
      G.Write_Page (Rows);
      C := (token => 2, valid => True, status => 0,
            msg => (tag => (P.Status'Enum_Rep (P.OK), 4, 0, 0), words => [1, 512, 512, 0]));
      O.Collect (C); O.Tick (20);
      Check (not O.Ready and not O.Disabled and G.Revokes = 0);
      C.token := 1;
      if Mode = 3 then C.valid := False; end if;
      if Mode = 5 then O.Tick (250_010); end if;
      O.Collect (C); O.Tick (21);
      Check (not O.Ready and G.Revokes = 1 and G.Checks in 1 .. 2);
      if Mode in 0 | 4 | 6 then Check (not O.Disabled); end if;
      O.Take (Result, Written, Next, Success);
      Check (not Success and Written = 0);
      if Mode = 6 then O.Tick (250_010); end if;
      -- Confirmed retirement is required even after a good reply.
      G.Allow_Retirement := True;
      O.Tick ((if Mode in 0 | 4 then 22 else 250_011));
      if Mode in 3 .. 6 then
         Check (O.Disabled and not O.Ready);
         O.Begin_Query (0, Sequence, 250_012, Submitted);
         Check (not Submitted and G.Creates = 1); return;
      end if;
      Check (O.Ready and not O.Disabled);
      O.Take (Result, Written, Next, Success);
      Check (Success and Written = 1 and Next = 512);
      Check (Result (0) (P.Row_Source) = Unsigned_64'Last);
      Check (not O.Ready);
      O.Begin_Query (0, Sequence, 30, Submitted); Check (Submitted);
      C.token := 2;
      -- Collector claims one row but writes nothing into the new grant.
      O.Collect (C); O.Tick (31);
      Check (O.Disabled and not O.Ready);
      O.Close; O.Collect (C); Check (O.Disabled and not O.Ready);
   end Scenario;
   procedure Happy is
      package O is new Observatory_Metric_Observer (17);
      Sequence : Unsigned_64 := 0;
      OK : Boolean;
      Rows : P.Summary_Page;
      Written : P.Row_Count;
      Next : Observatory_Metric_Queries.Cursor;
      C : CompletionEntry := (token => 1, valid => True, status => 0,
        msg => (tag => (P.Status'Enum_Rep (P.OK), 4, 0, 0), words => [0, 512, 512, 0]));
   begin
      G.Allow_Create := True; G.Allow_Revoke := True; G.Allow_Retirement := True; Allow_Submit := True;
      O.Begin_Query (0, Sequence, 0, OK); Check (OK);
      O.Collect (C); O.Tick (1); Check (O.Ready and not O.Disabled);
      O.Take (Rows, Written, Next, OK); Check (OK and Written = 0 and Next = 512);
      O.Begin_Query (0, Sequence, 2, OK); Check (OK and Sequence = 2);
      O.Collect (C); Check (not O.Ready); -- old completion cannot finish token2
      C.token := 2; O.Collect (C); O.Tick (3); Check (O.Ready);
      O.Close; Check (O.Disabled and not O.Ready);
   end Happy;
   procedure Revoke_And_Limits is
      package O is new Observatory_Metric_Observer (17);
      package Exhausted is new Observatory_Metric_Observer (17);
      package Clock_End is new Observatory_Metric_Observer (17);
      Sequence : Unsigned_64 := 0;
      End_Sequence : Unsigned_64 := Unsigned_64'Last - 1;
      OK : Boolean;
      C : constant CompletionEntry := (token => 1, valid => True, status => 0,
        msg => (tag => (P.Status'Enum_Rep (P.OK), 4, 0, 0), words => [0, 512, 512, 0]));
   begin
      G.Creates := 0; G.Revokes := 0; G.Checks := 0;
      G.Allow_Create := True; Allow_Submit := True;
      G.Allow_Revoke := False; G.Allow_Retirement := True;
      O.Begin_Query (0, Sequence, 0, OK); Check (OK); O.Collect (C);
      for I in 1 .. 1000 loop
         O.Tick (Unsigned_64 (I));
         Check (not O.Ready and not O.Disabled and G.Revokes = I and G.Checks = 0);
      end loop;
      G.Allow_Revoke := True; O.Tick (1001);
      Check (O.Ready and G.Revokes = 1001 and G.Checks = 1);
      Exhausted.Begin_Query (0, End_Sequence, 0, OK);
      Check (not OK and Exhausted.Disabled and G.Creates = 1);
      Sequence := 0;
      Clock_End.Begin_Query (0, Sequence, Unsigned_64'Last - 3, OK); Check (OK);
      Clock_End.Tick (Unsigned_64'Last - 1); Check (not Clock_End.Disabled);
      Clock_End.Tick (Unsigned_64'Last); Check (Clock_End.Disabled);
   end Revoke_And_Limits;
begin
   for Mode in 0 .. 6 loop Scenario (Mode); end loop;
   Happy;
   Revoke_And_Limits;
   Put_Line ("PASS observer adapter:" & Checks'Image & " checks");
end Adapter_Tests;
