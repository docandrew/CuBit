with Ada.Text_IO;
with Interfaces; use Interfaces;
with Servo_Input_Geometry;
with Client_Canvas_Geometry;
procedure Input_Geometry_Tests is
   package G renames Servo_Input_Geometry;
   package C renames Client_Canvas_Geometry;
   Cases : Natural := 0;
   procedure Check (X : Integer_32; O : C.Logical_Edge; N, D : C.Component) is
      Product : constant Integer_64 := Integer_64 (X) * Integer_64 (N);
      Expected : Integer_64 := Product / Integer_64 (D);
   begin
      if Product > 0 and then Product rem Integer_64 (D) /= 0 then Expected := Expected + 1; end if;
      Expected := Expected - Integer_64 ((O * N + D - 1) / D);
      Expected := Integer_64'Max (Integer_64 (Integer_32'First),
        Integer_64'Min (Integer_64 (Integer_32'Last), Expected));
      if Integer_64 (G.Relative (X, O, N, D)) /= Expected then
         raise Program_Error with "signed device point mismatch";
      end if;
      Cases := Cases + 1;
   end Check;
   type Origins is array (Positive range <>) of C.Logical_Edge;
begin
   for N in C.Component loop
      for D in C.Component loop
         for O of Origins'[0, 31, 40, 65_500] loop
            Check (Integer_32'First, O, N, D);
            Check (Integer_32'Last, O, N, D);
            for X in -100 .. 100 loop Check (Integer_32 (X), O, N, D); end loop;
            if G.Relative (Integer_32 (O), O, N, D) /= 0 then
               raise Program_Error with "origin phase mismatch";
            end if;
         end loop;
         for X in 0 .. 100 loop
            declare
               Left : constant Natural := C.Edge (40 + X, N, D);
               Right : constant Natural := C.Edge (41 + X, N, D);
               Pixel : constant Natural := Natural (G.Relative (Integer_32 (40 + X), 40, N, D));
            begin
               if Left < Right and then C.Sample (40, 102, Pixel, N, D) /= X then
                  raise Program_Error with "nonempty logical-cell round trip mismatch";
               end if;
               Cases := Cases + 1;
            end;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("SERVO-INPUT-GEOMETRY: PASS cases=" & Natural'Image (Cases));
end Input_Geometry_Tests;
