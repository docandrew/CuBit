with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Interfaces;
with CCL.Resource_Sections;

--  Linux-hosted test tool: decode a .cubit.resources section file and print
--  the plan, or the refusal. Exit status 0 only for a decoded section.
procedure Resource_Decode is
   use Ada.Text_IO;
   use CCL.Resource_Sections;
   use type Ada.Streams.Stream_Element_Offset;
   File : Ada.Streams.Stream_IO.File_Type;
   Raw : Ada.Streams.Stream_Element_Array (1 .. MAX_SECTION_BYTES + 1);
   Last : Ada.Streams.Stream_Element_Offset;
   Plan : Section_Plan;
   Status : Decode_Status;

   function Image (N : Interfaces.Unsigned_64) return String is
      Text : constant String := N'Image;
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;
begin
   Ada.Streams.Stream_IO.Open
     (File, Ada.Streams.Stream_IO.In_File, Ada.Command_Line.Argument (1));
   Ada.Streams.Stream_IO.Read (File, Raw, Last);
   Ada.Streams.Stream_IO.Close (File);
   if Last > MAX_SECTION_BYTES then
      Put_Line ("TOO_LONG");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;
   declare
      Data : Byte_Array (1 .. Natural (Last));
   begin
      for I in Data'Range loop
         Data (I) := Interfaces.Unsigned_8 (Raw (Ada.Streams.Stream_Element_Offset (I)));
      end loop;
      Decode (Data, Plan, Status);
   end;
   if Status /= Decoded then
      Put_Line (Status'Image);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;
   Put_Line ("match " & Plan.Match.Kind'Image & " " &
             Image (Interfaces.Unsigned_64 (Plan.Match.Values (1))) & " " &
             Image (Interfaces.Unsigned_64 (Plan.Match.Values (2))) & " " &
             Image (Interfaces.Unsigned_64 (Plan.Match.Values (3))));
   for Item of Plan.Entries (1 .. Plan.Count) loop
      Put_Line (Item.Kind'Image & " " & Item.Rights'Image & " slot=" &
                Image (Interfaces.Unsigned_64 (Item.Slot)) & " index=" &
                Image (Interfaces.Unsigned_64 (Item.Index)) & " amount=" &
                Image (Item.Amount) & " extra=" & Image (Item.Extra));
   end loop;
end Resource_Decode;
