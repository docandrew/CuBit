with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Display_Topology; use Intel_GPU_Display_Topology;
with Intel_GPU_Display_Lease;
procedure Display_Topology_Tests is
   procedure Check_Pipe (Item : Pipe) is
      Seen, Removed : Unsigned_64 := 0;
      procedure Hold (W : Well; Added, Success : out Boolean) is
      begin
         pragma Assert ((Seen and Ancestors (W)) = Ancestors (W));
         Seen := Seen or Bit (W);
         Added := True; Success := True;
      end Hold;
      procedure Drop (W : Well; Added : Boolean; Success : out Boolean) is
      begin
         pragma Assert (Added);
         for Other in Well loop
            if (Seen and Bit (Other)) /= 0 and then (Ancestors (Other) and Bit (W)) /= 0 then
               pragma Assert ((Removed and Bit (Other)) /= 0);
            end if;
         end loop;
         Removed := Removed or Bit (W);
         Success := True;
      end Drop;
      package Lease is new Intel_GPU_Display_Lease (Well, Valid, Hold, Drop);
      OK : Boolean;
   begin
      Lease.Acquire (Required (Item), OK);
      pragma Assert (OK and Seen = Required (Item));
      Lease.Release (OK);
      pragma Assert (OK and Removed = Seen);
   end Check_Pipe;
begin
   pragma Assert (Required (A) = 9 and Required (B) = 21 and
                  Required (C) = 39 and Required (D) = 71);
   -- Independent Boolean oracle for every representable low-byte selection,
   -- including the unsupported eighth bit.
   for Mask in Unsigned_64 range 0 .. 255 loop
      declare
         Has_PW1 : constant Boolean := (Mask and 1) /= 0;
         Has_DC : constant Boolean := (Mask and 2) /= 0;
         Has_PW2 : constant Boolean := (Mask and 4) /= 0;
         Expected : constant Boolean := Mask /= 0 and Mask < 128 and
           ((Mask and 126) = 0 or Has_PW1) and
           ((Mask and 112) = 0 or Has_PW2) and
           ((Mask and 96) = 0 or Has_DC);
      begin pragma Assert (Valid (Mask) = Expected); end;
   end loop;
   pragma Assert (not Valid (Unsigned_64'Last));
   for W in Well loop
      pragma Assert (Ancestors (W) < Bit (W));
      if W /= DC_Off then
         pragma Assert (Request_Mask (W) =
           Shift_Left (Unsigned_32'(2),
             (case W is when PW1 => 0, when PW2 => 2,
              when PWA => 10, when PWB => 12, when PWC => 14, when PWD => 16,
              when DC_Off => 0)));
         pragma Assert (State_Mask (W) * 2 = Request_Mask (W));
      end if;
   end loop;
   for Item in Pipe loop Check_Pipe (Item); end loop;
   Ada.Text_IO.Put_Line ("Display topology PASS: 256 selections and four composed pipe lifetimes");
end Display_Topology_Tests;
