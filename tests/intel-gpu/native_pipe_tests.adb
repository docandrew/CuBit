with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Display_Topology; use Intel_GPU_Display_Topology;
with Intel_GPU_Display_Mapping;
with Intel_GPU_Native_Pipe;
procedure Native_Pipe_Tests is
   Count : Natural := 0;
   procedure Check (Item : Pipe; Owner, Page, Blocked, Mapped : Boolean;
                    Parents : Unsigned_64) is
      package Driver is new Intel_GPU_Native_Pipe (Item);
   begin
      Intel_GPU_Display_Mapping.Mapping_Ready := Mapped;
      pragma Assert (not Driver.Held);
      pragma Assert (Driver.Acquire (Owner, Page, Blocked, Parents) =
                       "prerequisites-unavailable");
      pragma Assert (not Driver.Held);
      -- Even a rejected first attempt may not be replayed with stronger claims.
      Intel_GPU_Display_Mapping.Mapping_Ready := True;
      pragma Assert (Driver.Acquire (True, True, True, 127) = "already-attempted");
      pragma Assert (not Driver.Held);
      Count := Count + 1;
   end Check;
begin
   for Item in Pipe loop
      for Flags in Unsigned_64 range 0 .. 15 loop
         for Parents in Unsigned_64 range 0 .. 255 loop
            -- Intentionally never enter hardware MMIO on the host. Exercise
            -- every rejected combination, including C/D without DC-off.
            if Flags /= 15 or else not Valid (Parents) or else
              (Parents and Ancestors (Pipe_Well (Item))) /= Ancestors (Pipe_Well (Item))
            then
               Check (Item, (Flags and 1) /= 0, (Flags and 2) /= 0,
                      (Flags and 4) /= 0, (Flags and 8) /= 0, Parents);
            end if;
         end loop;
      end loop;
   end loop;
   Put_Line ("native pipe admission PASS cases=" & Count'Image);
end Native_Pipe_Tests;
