with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with Observatory_Archive_Reader;
with Observatory_Trace_View;
with Observatory_Query_Lifetime;
procedure Reader_Tests is
   package G renames CuBit.Memory_Grants;
   package V renames Observatory_Trace_View;
   package IO renames Ada.Streams.Stream_IO;
   File : IO.File_Type;
   Data : Stream_Element_Array (1 .. 20480);
   Last : Stream_Element_Offset;
   Checks : Natural := 0;
   procedure Check (Value : Boolean) is
   begin Checks := Checks + 1; if not Value then raise Program_Error with Checks'Image; end if; end Check;
   procedure Scenario (Fault : Natural) is
      package R is new Observatory_Archive_Reader (17);
      use type R.Result_Kind;
      Sequence, Now, Token : Unsigned_64 := 0;
      Accepted, Success : Boolean;
      View : V.State;
      C : CompletionEntry;
      Page : G.Bytes;
      Offset, Bytes, Counter : Natural := 0;
      Before_Creates, Before_Submits : Natural;
      In_Read, In_Close : Boolean;
   begin
      G.Allow_Create := Fault /= 1; Allow_Submit := Fault /= 2;
      G.Allow_Revoke := True; G.Allow_Retirement := False;
      G.Creates := 0; G.Revokes := 0; G.Checks := 0; Submits := 0;
      R.Start (1, Accepted); Check (Accepted); R.Tick (Sequence, Now);
      if Fault in 1 | 2 then
         Check (R.Status = R.Unavailable);
      elsif Fault = 3 then
         R.Tick (Sequence, R.Timeout_Us); Check (R.Status = R.Unavailable);
      else
         while R.Status = R.Loading loop
            Counter := Counter + 1; Check (Counter < 512);
            Token := Sent_Token; In_Read := Sent.tag.label = 2; In_Close := Sent.tag.label = 3;
            C := (token => Token, status => 0, valid => True,
                  msg => (tag => (16#F000#, 1, 0, 0), words => [others => 0]));
            if Sent.tag.label = 1 then C.msg.words (0) := 77; C.msg.words (1) := 20480; C.msg.tag.length := 2;
            elsif In_Read then
               Check (Sent.words (0) = 77 and Sent.words (3) = Unsigned_64 (Offset));
               Bytes := Natural'Min (20480 - Offset, Natural (Sent.words (2)));
               Bytes := Natural'Min (Bytes, (case Counter mod 3 is when 0 => 17, when 1 => 255, when others => 4096));
               if Fault = 9 then Bytes := Natural'Min (Bytes, 255 - Offset); end if;
               Page := [others => 0];
               for I in 1 .. Bytes loop Page (I - 1) := Unsigned_8 (Data (Stream_Element_Offset (Offset + I))); end loop;
               G.Write_Bytes (Page); C.msg.words (0) := Unsigned_64 (Bytes); Offset := Offset + Bytes;
               if Fault = 8 then C.msg.words (0) := Sent.words (2) + 1; end if;
            else Check (In_Close and Sent.words (0) = 77); end if;
            if Fault = 4 then C.valid := False; end if;
            if Fault = 11 then C.msg.tag.length := 1; end if;
            if Fault = 5 then C.msg.tag.length := 4; end if;
            if Fault = 6 then C.msg.words (3) := 1; end if;
            if Fault = 10 and In_Close then C.msg.words (0) := 1; end if;
            Before_Creates := G.Creates; Before_Submits := Submits;
            C.token := Token + 1; R.Collect (C); R.Tick (Sequence, Now + 1);
            Check (R.Status = R.Loading and Submits = Before_Submits);
            C.token := Token; R.Collect (C);
            G.Allow_Retirement := False; R.Tick (Sequence, Now + 2);
            if not In_Close then
               Check (G.Creates = Before_Creates and Submits = Before_Submits);
               R.Take (View, Success); Check (not Success and not V.Ready (View));
               R.Start (0, Accepted); Check (not Accepted);
            end if;
            if Fault = 7 then R.Tick (Sequence, Now + R.Timeout_Us); end if;
            G.Allow_Retirement := True; Now := Now + 3; R.Tick (Sequence, Now);
            exit when R.Status /= R.Loading;
         end loop;
         if Fault = 0 then
            Check (R.Status = R.Complete and not R.Cleanup_Pending);
            R.Take (View, Success); Check (Success and V.Length (View) = 14 and V.Total (View) = 78);
            C.token := Token; C.valid := False; R.Collect (C); Check (R.Status = R.Complete);
            R.Start (0, Accepted); Check (Accepted); R.Close;
         elsif Fault = 9 then
            Check (R.Status = R.Incomplete and not R.Cleanup_Pending);
            R.Take (View, Success); Check (not Success);
            R.Start (0, Accepted); Check (Accepted); R.Close;
         else Check (R.Status = R.Unavailable); end if;
      end if;
      G.Allow_Retirement := True; R.Tick (Sequence, 500_000);
      Check (not R.Cleanup_Pending);
      R.Start (0, Accepted); Check (not Accepted);
   end Scenario;
begin
   IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1)); IO.Read (File, Data, Last); IO.Close (File);
   Check (Last = Data'Last);
   declare
      package L renames Observatory_Query_Lifetime;
      use type L.Phase;
      Default_Life, Archive_Life, Saturated : L.State;
      OK : Boolean;
   begin
      L.Start (Default_Life, 1, 0, OK); Check (OK and L.Deadline (Default_Life) = 250_000);
      L.Expire (Default_Life, 249_999); Check (L.Status (Default_Life) = L.Waiting);
      L.Expire (Default_Life, 250_000); Check (L.Status (Default_Life) = L.Failed);
      L.Start (Archive_Life, 1, 0, OK, Budget_Us => 2_000_000); Check (OK);
      L.Expire (Archive_Life, 616_000); Check (L.Status (Archive_Life) = L.Waiting);
      L.Expire (Archive_Life, 2_000_000); Check (L.Status (Archive_Life) = L.Failed);
      L.Start (Saturated, 1, Unsigned_64'Last - 10, OK, Budget_Us => 20);
      Check (OK and L.Deadline (Saturated) = Unsigned_64'Last);
      L.Expire (Saturated, Unsigned_64'Last - 1); Check (L.Status (Saturated) = L.Waiting);
      L.Expire (Saturated, Unsigned_64'Last); Check (L.Status (Saturated) = L.Failed);
   end;
   for Fault in 0 .. 11 loop Scenario (Fault); end loop;
   Put_Line ("PASS asynchronous archive reader checks=" & Checks'Image);
end Reader_Tests;
