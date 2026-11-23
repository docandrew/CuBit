pragma Ada_2022;
with Ada.Text_IO;
with ACPI_CMOS_Indices;
with ACPI_CMOS_Profiles;

procedure CMOS_Indices_Tests is
   package I renames ACPI_CMOS_Indices;
   package P renames ACPI_CMOS_Profiles;
   use type I.Byte, I.Word, I.Decision_Kind, P.Route_Kind, P.Port_Pair, P.Device_Kind;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin
      if not Good then raise Program_Error with "CMOS policy regression"; end if;
      Checks := Checks + 1;
   end Check;
   type Limits is array (Positive range <>) of I.Byte;
   Bounds : constant Limits := [63, 127, 255];
begin
   -- Each low-bit pattern round-trips through either possible retained NMI
   -- state. Test the complete selector value as well as the separate bits.
   for Index in I.Hardware_Index loop
      for NMI in Boolean loop
         declare
            Value : constant I.Byte := I.Selector (Index, NMI);
         begin
            Check (Natural (Value) = Natural (Index) + (if NMI then 128 else 0));
            Check ((Value and 16#7F#) = Index);
            Check (((Value and 16#80#) /= 0) = NMI);
         end;
      end loop;
   end loop;

   -- Every address of every identified/unknown profile. Inverse mapping
   -- reconstructs the original logical address from the selected bank/index.
   for Kind in P.Device_Kind loop
      for Extended in Boolean loop
         declare
            Profile : constant P.Profile := (Kind, Extended);
            Maximum : constant Natural :=
              (case Kind is
                 when P.PC_AT_64 => 63,
                 when P.Intel_Compatible => (if Extended then 255 else 127),
                 when others => 0);
         begin
            for Cell in I.Byte loop
               declare
                  Route : constant P.Route := P.Locate (Profile, Cell);
               begin
                  if Kind in P.Unknown_Device | P.Dallas then
                     Check (Route.Kind = P.Unsupported_Device);
                  elsif Natural (Cell) > Maximum then
                     Check (Route.Kind = P.Outside_Profile);
                  else
                     Check (Route.Kind = P.Ready);
                     Check (Natural (Route.Index) +
                       (if Route.Pair = P.Extended_Pair then 128 else 0) = Natural (Cell));
                     if Route.Pair = P.Extended_Pair then
                        Check (Kind = P.Intel_Compatible and Extended);
                     end if;
                  end if;
               end;
            end loop;
         end;
      end loop;
   end loop;

   -- Exhaustive protected-byte behavior with every possible FADT century
   -- byte. A zero century identifies no additional register.
   for Century in I.Byte loop
      for Cell in I.Byte loop
         for Bound of Bounds loop
            for Writing in Boolean loop
               declare
                  Result : constant I.Decision := I.Resolve
                    (I.Word (Cell), 1, 0, Bound, Century, Writing);
                  Is_Read_Only : constant Boolean :=
                    Natural (Cell) in 0 .. 13 or (Century /= 0 and Cell = Century);
               begin
                  if Cell > Bound then
                     Check (Result.Kind = I.Outside_Device);
                  elsif Writing and Is_Read_Only then
                     Check (Result.Kind = I.Read_Only_Cell);
                  else
                     Check (Result.Kind = I.Accepted);
                     Check (Result.Cell = Cell);
                  end if;
               end;
            end loop;
         end loop;
      end loop;
   end loop;

   -- Extents around each profile boundary: every short base/length, first
   -- and last byte, the byte just past the region, and maximum integer offset.
   -- Integer arithmetic here is an independent bounded reference model.
   for Bound of Bounds loop
      for First in 0 .. 260 loop
         for Count in 0 .. 260 loop
            declare
               type Offsets is array (Positive range <>) of I.Word;
               Points : constant Offsets :=
                 [0, (if Count = 0 then 0 else I.Word (Count - 1)), I.Word (Count), I.Word'Last];
               Fits : constant Boolean := Count > 0 and then First + Count - 1 <= Natural (Bound);
            begin
               for Offset of Points loop
                  declare
                     Result : constant I.Decision :=
                       I.Resolve (I.Word (First), I.Word (Count), Offset, Bound, 0, False);
                     Expected : constant Boolean := Fits and then Offset < I.Word (Count);
                  begin
                     Check ((Result.Kind = I.Accepted) = Expected);
                     if Expected then
                        Check (Natural (Result.Cell) = First + Natural (Offset));
                     end if;
                  end;
               end loop;
            end;
         end loop;
      end loop;
   end loop;
   -- Explicit modular-overflow attempts cannot wrap into the valid device.
   for Bound of Bounds loop
      Check (I.Resolve (I.Word'Last, 2, 1, Bound, 0, False).Kind = I.Outside_Device);
      Check (I.Resolve (1, I.Word'Last, 0, Bound, 0, False).Kind = I.Outside_Device);
      Check (I.Resolve (0, I.Word'Last, I.Word'Last - 1, Bound, 0, False).Kind = I.Outside_Device);
   end loop;
   Ada.Text_IO.Put_Line ("CMOS indices/profile checks " & Checks'Image & " PASS");
end CMOS_Indices_Tests;
