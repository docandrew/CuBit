with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with Intel_GPU_GGTT_Mapping;
procedure GGTT_Mapping_Tests is
   Owner, Reset : Boolean := True;
   BAR : Unsigned_64 := Base;
begin
   Mode := Natural'Value (Ada.Command_Line.Argument (1));
   if Mode = 15 then Owner := False;
   elsif Mode = 16 then Reset := False;
   elsif Mode = 17 then BAR := Unsigned_64'Last;
   elsif Mode = 18 then Expected_Bytes := 4096;
   elsif Mode = 19 then Expected_Bytes := 2_097_152;
   elsif Mode = 20 then Expected_Bytes := 4_194_304;
   end if;
   declare
      Outcome : constant String := Intel_GPU_GGTT_Mapping.Prepare
        (Owner, Reset, BAR, Expected_Bytes);
      Saved_Maps : constant Natural := Maps;
      Saved_Submissions : constant Natural := Submissions;
   begin
      if Mode in 0 | 4 | 19 | 20 then
         pragma Assert (Intel_GPU_GGTT_Mapping.Ready);
         pragma Assert (Intel_GPU_GGTT_Mapping.Bytes = Expected_Bytes);
         pragma Assert (Maps = Natural (Expected_Bytes / 2_097_152));
         pragma Assert (Submissions = (if Mode = 4 then 2 else 1));
      else
         pragma Assert (not Intel_GPU_GGTT_Mapping.Ready);
         pragma Assert (Intel_GPU_GGTT_Mapping.Bytes = 0);
         pragma Assert (Maps = (if Mode = 6 then 2 else 0));
      end if;
      pragma Assert (Intel_GPU_GGTT_Mapping.Prepare (True, True, Base, 8_388_608) = "already-attempted");
      pragma Assert (Maps = Saved_Maps and Submissions = Saved_Submissions);
      if Mode in 7 | 15 .. 18 then pragma Assert (Submissions = 0); end if;
      if Mode = 9 then pragma Assert (Polls = 30_000); end if;
      Ada.Text_IO.Put_Line ("GGTT mapping PASS" & Natural'Image (Mode) & ": " & Outcome);
   end;
end GGTT_Mapping_Tests;
