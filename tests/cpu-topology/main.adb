with Ada.Text_IO;
with CPU_Topology; use CPU_Topology;
with Interfaces; use Interfaces;
procedure Main is
   T : Topology;
   Expected : constant array (CPU_Index range 0 .. 3) of APIC_ID := [6, 0, 2, 4];
begin
   Initialize (T, 6);
   for ID of Expected loop
      Include_CPU (T, ID);
      Include_CPU (T, ID);
   end loop;
   pragma Assert (Count (T) = 4);
   for CPU in Expected'Range loop
      pragma Assert (Destination (T, CPU) = Expected (CPU));
   end loop;
   -- Every supported destination, every possible BSP, duplicate insertion.
   for BSP of Expected loop
      Initialize (T, BSP);
      for ID in APIC_ID loop
         Include_CPU (T, ID);
      end loop;
      pragma Assert (Count (T) = 255 and Unique (T));
      pragma Assert (Destination (T, 0) = BSP);
      for ID in APIC_ID loop
         Include_CPU (T, ID);
      end loop;
      pragma Assert (Count (T) = 255);
   end loop;
   Ada.Text_IO.Put_Line ("CPU topology: sparse IDs, nonzero BSP, full capacity, duplicates PASS");
end Main;
