pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages;

package body CuBit.Boot_Milestones is
   package Codes renames CuBit.Boot_Milestone_Codes;
   Marked : array (Milestone) of Boolean := [others => False];

   procedure Mark (Item : Milestone) is
      Ignore : Unsigned_64;
   begin
      if Marked (Item) then
         return;
      end if;
      Marked (Item) := True;
      Ignore := CuBit.Messages.syscall
        (Codes.System_Call, Codes.Mark_Operation, Milestone'Enum_Rep (Item));
   end Mark;
end CuBit.Boot_Milestones;
