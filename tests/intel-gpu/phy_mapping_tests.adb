with Ada.Command_Line; with Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with Intel_GPU_PHY_Mapping;
procedure PHY_Mapping_Tests is
   BAR : Unsigned_64 := Base;
   Owner : Boolean := True;
begin
   Mode := Natural'Value (Ada.Command_Line.Argument (1));
   if Mode = 14 then Owner := False;
   elsif Mode = 15 then BAR := Unsigned_64'Last;
   elsif Mode = 16 then BAR := 0;
   end if;
   declare
      Result : constant String := Intel_GPU_PHY_Mapping.Prepare (Owner, BAR);
      Saved_Maps : constant Natural := Maps;
      Saved_Submissions : constant Natural := Submissions;
   begin
      if Mode in 0 | 4 then
         pragma Assert (Intel_GPU_PHY_Mapping.Ready and Maps = 3);
         pragma Assert (Submissions = (if Mode = 4 then 4 else 3));
      else
         pragma Assert (not Intel_GPU_PHY_Mapping.Ready);
         pragma Assert (Maps = (if Mode = 6 then 2 else 0));
      end if;
      if Mode in 7 | 14 .. 16 then pragma Assert (Submissions = 0); end if;
      if Mode = 9 then pragma Assert (Polls = 30_000); end if;
      pragma Assert (Intel_GPU_PHY_Mapping.Prepare (True, Base) = "already-attempted");
      pragma Assert (Maps = Saved_Maps and Submissions = Saved_Submissions);
      Ada.Text_IO.Put_Line ("PHY mapping PASS" & Natural'Image (Mode) & ": " & Result);
   end;
end PHY_Mapping_Tests;
