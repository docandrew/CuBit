pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Handles.Devices;

package body CuBit.Boot_Milestones is
   Marked : array (Milestone) of Boolean := [others => False];

   procedure Mark (Item : Milestone; Authority : CuBit.Handles.Notification) is
      Ignore : Boolean;
   begin
      if Marked (Item) then
         return;
      end if;
      Marked (Item) := True;
      Ignore := CuBit.Handles.Devices.Mark_Milestone
        (Milestone'Enum_Rep (Item), Authority);
   end Mark;
end CuBit.Boot_Milestones;
