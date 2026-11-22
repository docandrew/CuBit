with Ada.Text_IO;
with Interfaces; use Interfaces;
with Device_Memory_Admission; use Device_Memory_Admission;
procedure Main is
begin
   for R in Unsigned_64 range 0 .. 16 loop
      for S in Unsigned_64 range 0 .. 16 loop
         for B in Unsigned_64 range 0 .. 32 loop
            for N in Unsigned_64 range 0 .. 16 loop
               pragma Assert (Covers (R, S, B, N) =
                 (S > 0 and N > 0 and B >= R and B + N <= R + S));
            end loop;
         end loop;
      end loop;
   end loop;
   pragma Assert (Covers (Unsigned_64'Last - 4095, 4096, Unsigned_64'Last, 1));
   pragma Assert (not Covers (Unsigned_64'Last - 4095, 8192, Unsigned_64'Last, 1));
   pragma Assert (not Covers (0, 4096, Unsigned_64'Last - 4095, 8192));
   for Read_Right in Boolean loop
      for Write_Right in Boolean loop
         pragma Assert (Allows (Read_Right, Write_Right, Read_Only) = Read_Right);
         pragma Assert (Allows (Read_Right, Write_Right, Read_Write) = (Read_Right and Write_Right));
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("PASS: device memory containment and access admission");
end Main;
