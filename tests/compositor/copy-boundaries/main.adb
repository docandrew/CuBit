with Ada.Text_IO;
with Interfaces; use Interfaces;
with System;
with Compositor_Row_Copy;
with Compositor_Upload;
with Compositor_Upload_Copy;
with Compositor_Readback_Copy;
procedure Main is
   procedure Reset_Copies with Import, Convention => C, External_Name => "reset_copies";
   function Copy_Calls return Unsigned_64 with Import, Convention => C, External_Name => "copy_calls";
   function Copy_Bytes return Unsigned_64 with Import, Convention => C, External_Name => "copy_bytes";
   type Bytes is array (Natural range <>) of aliased Unsigned_8;
   Source : aliased Bytes (0 .. 63);
   Target : aliased Bytes (0 .. 63) := (others => 16#EE#);
   Rows : Natural;
   Plan : Compositor_Upload.Plan;
   OK, Copied : Boolean;
   procedure Clean is begin Target := (others => 16#EE#); end Clean;
begin
   for I in Source'Range loop Source (I) := Unsigned_8 (I); end loop;
   -- Tight 2x3 source into 12-byte pitch; one-row payload budget.
   Compositor_Readback_Copy.Copy (Source (0)'Address, Target (4)'Address,
     2, 3, 1, 24, 36, 12, 8, Rows);
   pragma Assert (Rows = 1);
   for I in Target'Range loop
      pragma Assert (Target (I) = (if I in 16 .. 23 then Source (I - 8) else 16#EE#));
   end loop;
   Clean;
   Compositor_Readback_Copy.Copy (Source (0)'Address, Target (4)'Address,
     2, 3, 3, 24, 36, 12, 8, Rows);
   pragma Assert (Rows = 0 and (for all V of Target => V = 16#EE#));
   Compositor_Readback_Copy.Copy (Source (0)'Address, Target (4)'Address,
     2, 3, 0, 24, 36, 12, 7, Rows);
   pragma Assert (Rows = 0 and (for all V of Target => V = 16#EE#));
   Compositor_Readback_Copy.Copy (Target (0)'Address, Target (4)'Address,
     2, 3, 0, 24, 36, 12, 24, Rows);
   pragma Assert (Rows = 0 and (for all V of Target => V = 16#EE#));
   -- Tight batches coalesce without copying outside the admitted row span.
   Clean; Reset_Copies;
   Compositor_Readback_Copy.Copy (Source (0)'Address, Target (4)'Address,
     2, 3, 0, 24, 24, 8, 24, Rows);
   pragma Assert (Rows = 3 and Copy_Calls = 1);
   for I in Target'Range loop
      pragma Assert (Target (I) = (if I in 4 .. 27 then Source (I - 4) else 16#EE#));
   end loop;
   Clean; Reset_Copies;
   Compositor_Readback_Copy.Copy (Source (0)'Address, Target (4)'Address,
     2, 3, 1, 24, 24, 8, 16, Rows);
   pragma Assert (Rows = 2 and Copy_Calls = 1);
   for I in Target'Range loop
      pragma Assert (Target (I) = (if I in 12 .. 27 then Source (I - 4) else 16#EE#));
   end loop;
   Clean; Reset_Copies;
   Compositor_Readback_Copy.Copy (Source (0)'Address, Target (4)'Address,
     2, 3, 0, 24, 36, 12, 24, Rows);
   pragma Assert (Rows = 3 and Copy_Calls = 3);
   for I in Target'Range loop
      pragma Assert (Target (I) =
        (if I in 4 .. 35 and then (I - 4) mod 12 < 8
         then Source (((I - 4) / 12) * 8 + (I - 4) mod 12) else 16#EE#));
   end loop;
   Clean;
   -- Coalescing must preserve the per-call event-loop work cap.
   declare
      Large_Source : aliased Bytes (0 .. 524_287);
      Large_Target : aliased Bytes (0 .. 524_319) := (others => 16#EE#);
   begin
      for I in Large_Source'Range loop Large_Source (I) := Unsigned_8 (I mod 251); end loop;
      Reset_Copies;
      Compositor_Readback_Copy.Copy (Large_Source (0)'Address, Large_Target (16)'Address,
        128, 1024, 0, 524_288, 524_288, 512, 524_288, Rows);
      pragma Assert (Rows = 512 and Copy_Calls = 1);
      for I in Large_Target'Range loop
         pragma Assert (Large_Target (I) =
           (if I in 16 .. 262_159 then Large_Source (I - 16) else 16#EE#));
      end loop;
      Large_Target := (others => 16#EE#); Reset_Copies;
      Compositor_Readback_Copy.Copy_Region
        (Large_Source (0)'Address, Large_Target (16)'Address,
         128, 1024, 0, (1, 0, 128, 1024), 524_288, 524_288, 512, 524_288, Rows);
      pragma Assert (Rows = 516 and Copy_Bytes = 262_128 and Copy_Calls = 516);
      for I in Large_Target'Range loop
         pragma Assert (Large_Target (I) =
           (if I >= 16 and then (I - 16) / 512 < Rows and then (I - 16) mod 512 >= 4
            then Large_Source (I - 16) else 16#EE#));
      end loop;
      for I in Large_Source'Range loop
         pragma Assert (Large_Source (I) = Unsigned_8 (I mod 251));
      end loop;
   end;
   -- Exhaustive small rectangles, starts, and payload budgets. Compare every
   -- target byte, including unchanged pixels, row padding and outer canaries.
   declare
      use Compositor_Row_Copy.G;
      Expected_Rows, Row_Bytes : Natural;
      Expected : Unsigned_8;
   begin
      for L in Pixel_Edge range 0 .. 3 loop
         for R in Pixel_Edge range L + 1 .. 4 loop
            for T in Pixel_Edge range 0 .. 2 loop
               for B in Pixel_Edge range T + 1 .. 3 loop
                  for First in Pixel_Edge range 0 .. 3 loop
                     for Budget in Natural range 0 .. 49 loop
                        Clean; Reset_Copies;
                        Row_Bytes := Natural (R - L) * 4;
                        Expected_Rows := (if First >= B - T then 0 else
                          Natural'Min (Natural (B - T - First), Budget / Row_Bytes));
                        Compositor_Readback_Copy.Copy_Region
                          (Source (0)'Address, Target (4)'Address, 4, 3, First,
                           (L, T, R, B), 48, 60, 20, Budget, Rows);
                        pragma Assert (Rows = Expected_Rows);
                        pragma Assert (Copy_Bytes = Unsigned_64 (Rows * Row_Bytes));
                        for I in Target'Range loop
                           Expected := 16#EE#;
                           if I >= 4 and then (I - 4) / 20 >= Natural (T + First) and then
                             (I - 4) / 20 < Natural (T + First) + Rows and then
                             (I - 4) mod 20 >= Natural (L) * 4 and then
                             (I - 4) mod 20 < Natural (R) * 4
                           then Expected := Source (((I - 4) / 20) * 16 + (I - 4) mod 20); end if;
                           pragma Assert (Target (I) = Expected);
                        end loop;
                     end loop;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
      Clean; Reset_Copies;
      Compositor_Readback_Copy.Copy_Region (Source (0)'Address, Target (4)'Address,
        4, 3, 0, (1, 1, 2, 3), 48, 60, 20, 48, Rows);
      pragma Assert (Rows = 2 and Copy_Bytes = 8 and Copy_Calls = 2);
      -- One-column repair copies 8 bytes instead of the 48-byte image.
      Clean; Reset_Copies;
      Compositor_Readback_Copy.Copy_Region (Source (0)'Address, Target (4)'Address,
        4, 3, 1, (0, 0, 4, 3), 48, 48, 16, 48, Rows);
      pragma Assert (Rows = 2 and Copy_Bytes = 32 and Copy_Calls = 1);
      for I in Target'Range loop
         pragma Assert (Target (I) = (if I in 20 .. 51 then Source (I - 4) else 16#EE#));
      end loop;
      for Case_ID in 1 .. 6 loop
         Clean; Reset_Copies;
         Compositor_Readback_Copy.Copy_Region
           ((if Case_ID = 1 then System.Null_Address elsif Case_ID = 2 then Target (0)'Address else Source (0)'Address),
            Target (4)'Address, 4, 3, 0,
            (1, 1, (if Case_ID = 3 then 5 else 2), 3),
            (if Case_ID = 4 then 47 else 48), (if Case_ID = 5 then 55 else 60),
            (if Case_ID = 6 then 15 else 20), 48, Rows);
         pragma Assert (Rows = 0 and Copy_Bytes = 0 and (for all V of Target => V = 16#EE#));
      end loop;
   end;
   -- Upload source rectangle, honoring both source and staging row padding.
   Compositor_Upload.Make (3, 3, 32, (1, 1, 2, 2), 4, 3,
     Compositor_Upload.BGRA8, Plan, OK);
   pragma Assert (OK);
   Compositor_Upload_Copy.Copy (Source (0)'Address, Target (4)'Address,
     48, 16, 3, 3, Compositor_Upload.BGRA8, Plan, Copied);
   pragma Assert (Copied);
   for I in Target'Range loop
      pragma Assert (Target (I) =
        (if I in 8 .. 15 then Source (I + 12)
         elsif I in 20 .. 27 then Source (I + 16) else 16#EE#));
   end loop;
   Clean;
   Compositor_Upload_Copy.Copy (Target (0)'Address, Target (4)'Address,
     48, 16, 3, 3, Compositor_Upload.BGRA8, Plan, Copied);
   pragma Assert (not Copied and (for all V of Target => V = 16#EE#));
   Compositor_Upload_Copy.Copy (System.Null_Address, Target (4)'Address,
     48, 16, 3, 3, Compositor_Upload.BGRA8, Plan, Copied);
   pragma Assert (not Copied and (for all V of Target => V = 16#EE#));
   for I in Source'Range loop pragma Assert (Source (I) = Unsigned_8 (I)); end loop;
   Ada.Text_IO.Put_Line ("PASS actual memcpy wrappers: bounded rows, padding/canaries, overlap/null rejection, immutable source");
end Main;
