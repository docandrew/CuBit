with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Command_Line;
with Interfaces; use Interfaces;
with Observatory_Archive_Stream;
with Observatory_Trace_Plot;
procedure Stream_Tests is
   package S renames Observatory_Archive_Stream;
   package V renames S.V;
   package A renames V.A;
   package F renames V.F;
   package P renames Observatory_Trace_Plot;
   package IO renames Ada.Streams.Stream_IO;
   use type A.S.Capture;
   File : IO.File_Type;
   Bytes : Stream_Element_Array (1 .. 20480);
   Last : Stream_Element_Offset;
   State : S.State;
   View : V.State;
   Checks : Natural := 0;
   procedure Check (Value : Boolean) is
   begin Checks := Checks + 1; if not Value then raise Program_Error with Checks'Image; end if; end Check;
   Window : P.Bounds;
   Point : P.Interval;
   Bar : P.Bar;
   procedure Append_Chunk (Data : A.Chunk) is
   begin
      for Word of Data loop
         for Shift in 0 .. 7 loop
            S.Append (State, S.Byte (Shift_Right (Word, Shift * 8) and 255));
         end loop;
      end loop;
   end Append_Chunk;
   Header, Data : A.Chunk;
   Archive : F.State;
   Accepted : Boolean;
   Capture : A.S.Capture :=
     (True, 1, 1, A.S.P.Publisher_Tag (1), 1, 1, 0, 0,
      (A.W.Frame_Event, 1, (0, 1, 1, 0, Unsigned_64'Last - 1)));
begin
   IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1)); IO.Read (File, Bytes, Last); IO.Close (File);
   Check (Last = Bytes'Last);
   for Page in 0 .. 1 loop
      S.Start (State, Page);
      for I in Bytes'Range loop
         S.Append (State, S.Byte (Bytes (I))); Check (not S.Ready (State));
      end loop;
      S.Finish (State); Check (S.Ready (State)); View := S.Context (State);
      Check (V.Length (View) = (if Page = 0 then 64 else 14));
      Window := P.Extent (View);
      for I in 1 .. V.Length (View) loop
         Point := P.Describe (V.Value_At (View, I));
         Bar := P.Project (Point, Window, 480);
         Check (Bar.Left + Bar.Pixels <= 480);
      end loop;
      S.Append (State, 0); Check (not S.Ready (State) and not V.Ready (S.Context (State)));
   end loop;
   for Tail in 0 .. 255 loop
      S.Start (State, 0);
      for I in 1 .. 20480 - 256 + Tail loop S.Append (State, S.Byte (Bytes (Stream_Element_Offset (I)))); end loop;
      S.Finish (State); Check (not S.Ready (State));
   end loop;
   Header := F.Header ((1, 1, 0, 4096)); F.Start (Archive, Header); S.Start (State, 63); Append_Chunk (Header);
   for I in 1 .. 4096 loop
      Capture.Value.Event_ID := Unsigned_64 (I);
      Data := F.Event_Chunk (Archive, Capture); F.Feed (Archive, Data, Accepted); Append_Chunk (Data);
   end loop;
   Append_Chunk (F.Footer (Archive, Unsigned_64'Last - 1, F.Budget_Reached, (Emitted_Events => 4096, others => 0)));
   Check (S.Length (State) = S.Maximum_Bytes); S.Finish (State); Check (S.Ready (State));
   View := S.Context (State); Check (V.Length (View) = 64 and V.Value_At (View, 1).Value.Event_ID = 4033);
   S.Append (State, 0); Check (not S.Ready (State));
   for Size in P.Width loop
      Bar := P.Project ((4, 0, Unsigned_64'Last), (0, Unsigned_64'Last), Size);
      Check (Bar.Left = 0 and Bar.Pixels = Size);
      Bar := P.Project ((4, Unsigned_64'Last, Unsigned_64'Last), (0, Unsigned_64'Last), Size);
      Check (Bar.Left = Size - 1 and Bar.Pixels = 1);
      Bar := P.Project ((0, 10, 10), (10, 10), Size);
      Check (Bar.Left = 0 and Bar.Pixels = 1);
   end loop;
   Put_Line ("PASS archive byte stream and plot checks=" & Checks'Image);
end Stream_Tests;
