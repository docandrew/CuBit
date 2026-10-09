with Ada.Command_Line;
with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Compositor_Trace_Framing;
procedure Archive_Reader is
   package F renames Compositor_Trace_Framing;
   package A renames F.A;
   package IO renames Ada.Streams.Stream_IO;
   use type F.Phase, A.W.Event_Kind;
   File : IO.File_Type;
   Bytes : Stream_Element_Array (1 .. 256);
   Last : Stream_Element_Offset;
   Data : A.Chunk;
   State : F.State;
   First : Boolean := True;
   Accepted : Boolean;
   Tail : Natural := 0;
   Kinds : array (A.W.Event_Kind) of Natural := [others => 0];
   Frames, Min_Us, Max_Us, Total_Us : Unsigned_64 := 0;
   procedure Fail (Why : String) is
   begin
      Put_Line ("ARCHIVE: REJECTED " & Why);
      raise Program_Error;
   end Fail;
begin
   if Ada.Command_Line.Argument_Count /= 1 then Fail ("file argument required"); end if;
   IO.Open (File, IO.In_File, Ada.Command_Line.Argument (1));
   while not IO.End_Of_File (File) loop
      IO.Read (File, Bytes, Last);
      if Last /= Bytes'Last then Tail := Natural (Last); exit; end if;
      for I in Data'Range loop
         Data (I) := 0;
         for J in 0 .. 7 loop
            Data (I) := Data (I) or Shift_Left
              (Unsigned_64 (Bytes (Stream_Element_Offset (I * 8 + J + 1))), J * 8);
         end loop;
      end loop;
      if First then
         F.Start (State, Data); First := False;
         if F.Status (State) /= F.Reading then Fail ("header"); end if;
         Put_Line ("ARCHIVE: capture=" & F.Info (State).Capture_ID'Image &
           " endpoint=" & F.Info (State).Endpoint'Image &
           " budget=" & F.Info (State).Budget'Image & " boot-identity=unknown");
      else
         F.Feed (State, Data, Accepted);
         if F.Status (State) = F.Rejected then Fail ("chunk order, checksum or identity"); end if;
         if Accepted then
            declare
               V : constant A.Decoded := A.Decode (Data);
               Packet : constant A.W.Packet := A.W.Encode (V.Value.Value);
            begin
               Kinds (V.Value.Value.Kind) := Kinds (V.Value.Value.Kind) + 1;
               Put ("EVENT: sequence=" & V.Sequence'Image &
                 " pid=" & V.Value.Pid'Image & " publisher=" & V.Value.Publisher'Image &
                 " batch=" & V.Value.Batch'Image & " history=" & V.Value.First_Sequence'Image &
                 " producer-dropped=" & V.Value.Producer_Dropped'Image & " words=");
               for Word of Packet loop Put (Word'Image); end loop;
               New_Line;
               if V.Value.Value.Kind = A.W.Frame_Event then
                  declare Span : constant Unsigned_64 :=
                    V.Value.Value.Frame.Completed - V.Value.Value.Frame.Submitted;
                  begin
                     if Frames = 0 or else Span < Min_Us then Min_Us := Span; end if;
                     if Span > Max_Us then Max_Us := Span; end if;
                     Frames := Frames + 1;
                     if Span > Unsigned_64'Last - Total_Us then Fail ("duration sum overflow"); end if;
                     Total_Us := Total_Us + Span;
                  end;
               end if;
            end;
         end if;
      end if;
   end loop;
   IO.Close (File);
   F.End_Of_File (State, Tail);
   if F.Status (State) /= F.Complete then
      Put_Line ("ARCHIVE: INCOMPLETE events=" & F.Events (State)'Image & " tail=" & Tail'Image);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure); return;
   end if;
   Put_Line ("ARCHIVE: COMPLETE events=" & F.Events (State)'Image &
     " input=" & Kinds (A.W.Input_Event)'Image & " source=" & Kinds (A.W.Source_Event)'Image &
     " render=" & Kinds (A.W.Render_Event)'Image & " frame=" & Kinds (A.W.Frame_Event)'Image);
   if Frames > 0 then
      Put_Line ("COMPLETION: frames=" & Frames'Image & " min_us=" & Min_Us'Image &
        " max_us=" & Max_Us'Image & " sum_us=" & Total_Us'Image &
        " scope=software-submit-to-completion-collection-not-photons");
   end if;
end Archive_Reader;
