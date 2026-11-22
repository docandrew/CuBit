with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Display_Topology; use Intel_GPU_Display_Topology;
with Intel_GPU_Plane_Registers; use Intel_GPU_Plane_Registers;
procedure Plane_Registers_Tests is
   -- Independent explicit plane-control addresses, no stride arithmetic.
   A_Control : constant array (Plane_Number) of Unsigned_32 :=
     [16#70180#, 16#70280#, 16#70380#, 16#70480#, 16#70580#];
   B_Control : constant array (Plane_Number) of Unsigned_32 :=
     [16#71180#, 16#71280#, 16#71380#, 16#71480#, 16#71580#];
   C_Control : constant array (Plane_Number) of Unsigned_32 :=
     [16#72180#, 16#72280#, 16#72380#, 16#72480#, 16#72580#];
   D_Control : constant array (Plane_Number) of Unsigned_32 :=
     [16#73180#, 16#73280#, 16#73380#, 16#73480#, 16#73580#];
   Field_Offset : constant array (Field) of Unsigned_32 := [0, 8, 16, 36, 28, 44];
   Count : Natural := 0;
begin
   for P in Pipe loop
      for N in Plane_Number loop
         for F in Field loop
            declare R : constant Selection := Select_Register (P, N, F); begin
                  pragma Assert (R.Valid);
                  pragma Assert (R.Register_Offset =
                    (case P is when A => A_Control (N), when B => B_Control (N),
                     when C => C_Control (N), when D => D_Control (N)) + Field_Offset (F));
                  pragma Assert ((R.Register_Offset mod 4) = 0);
               Count := Count + 1;
            end;
         end loop;
      end loop;
   end loop;
   pragma Assert (Count = 120);
   Ada.Text_IO.Put_Line ("ADL-N plane registers PASS: all 120 selections");
end Plane_Registers_Tests;
