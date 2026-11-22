pragma Ada_2022;
with Ada.Text_IO;
with Firmware_Tables.Catalog;
with Firmware_Tables.Copies;
procedure Catalog_Tests is
   use Firmware_Tables;
   use Firmware_Tables.Catalog;
   use type Address_Value;
   use type Byte;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with Checks'Image; end if;
   end Check;
   D : constant Descriptor :=
     (Physical => 16#1000#, Extent => 116, Name => "DSDT", Revision => 2);
   function Extra (I : Positive) return Descriptor is
     (Physical => Address_Value (I) * 4096 + 16#1000#,
      Extent => 36 + I, Name => (if I mod 2 = 0 then "SSDT" else "FACP"),
      Revision => Byte (I mod 256));
   S : State;
   type Table_Length_Array is array (Positive range <>) of Table_Length;
begin
   for N in 0 .. Max_Tables - 1 loop
      Reset (S);
      Check (Count (S) = 0 and Current (S) = Receiving);
      for I in 1 .. N loop
         Include (S, Extra (I));
         Check (Count (S) = 0 and Current (S) = Receiving);
      end loop;
      Include (S, D);
      Include (S, D);
      Check (Count (S) = 0);
      Seal (S);
      Check (Current (S) = Ready and Count (S) = N + 1);
      Check (Item (S, 1) = D);
      for I in 1 .. N loop Check (Item (S, I + 1) = Extra (I)); end loop;
      Seal (S);
      Check (Count (S) = N + 1);
   end loop;
   Include (S, D); -- mutation after publication invalidates the inventory
   Check (Current (S) = Failed and Count (S) = 0);
   for Failure in 1 .. 5 loop
      Reset (S);
      Include (S, D);
      case Failure is
         when 1 =>
            Include (S, (D with delta Physical => D.Physical + 1));
         when 2 =>
            Include (S, (D with delta Name => "FACS"));
         when 3 =>
            Include (S, (D with delta Physical => Address_Value'Last - 34));
         when 4 =>
            for I in 1 .. Max_Tables loop Include (S, Extra (I)); end loop;
         when 5 => Reject (S);
      end case;
      Check (Current (S) = Failed and Count (S) = 0);
      Include (S, D);
      Seal (S);
      Check (Current (S) = Failed and Count (S) = 0);
   end loop;
   Reset (S);
   Include (S, Extra (1));
   Seal (S);
   Check (Current (S) = Failed and Count (S) = 0);
   Reset (S);
   Include (S, (D with delta Physical => Address_Value'Last - 115));
   Seal (S);
   Check (Current (S) = Ready and Count (S) = 1);
   Check (Item (S, 1).Physical = Address_Value'Last - 115);
   -- Discovery metadata has no 1 MiB allocation policy. Large descriptors
   -- remain subject to physical interval overflow checks, even at the host
   -- byte-index representation limit; no large backing allocation is needed.
   for Extent of Table_Length_Array'[1_048_577, Table_Length'Last] loop
      Reset (S);
      Include (S, (D with delta Extent => Extent));
      Seal (S);
      Check (Current (S) = Ready and Count (S) = 1);
      Check (Item (S, 1).Extent = Extent);
      Reset (S);
      Include (S, (D with delta Extent => Extent,
                  Physical => Address_Value'Last - Address_Value (Extent) + 1));
      Seal (S);
      Check (Current (S) = Ready);
      Reset (S);
      Include (S, (D with delta Extent => Extent,
                  Physical => Address_Value'Last - Address_Value (Extent) + 2));
      Seal (S);
      Check (Current (S) = Failed and Count (S) = 0);
   end loop;
   declare
      Raw : Bytes (101 .. 136) := [others => 0];
      Page : Bytes (201 .. 4296);
      Tiny : Bytes (1 .. 35);
      Empty : Bytes (1 .. 0);
      Success : Boolean;
      Expected : constant Descriptor :=
        (Physical => 4096, Extent => 36, Name => "DSDT", Revision => 2);
      procedure Check_Failure (D : Descriptor; Source : Bytes) is
      begin
         Page := [others => 255];
         Copies.Copy_Validated (D, Source, Page, Success);
         Check (not Success and then (for all B of Page => B = 0));
      end Check_Failure;
   begin
      Raw (101 .. 104) := [16#44#, 16#53#, 16#44#, 16#54#];
      Raw (105) := 36; Raw (109) := 2;
      declare
         Sum : Byte := 0;
      begin
         for B of Raw loop Sum := Sum + B; end loop;
         Raw (110) := 0 - Sum;
      end;
      Check (Copies.Matches (Expected, Raw));
      Copies.Copy_Validated (Expected, Raw, Page, Success);
      Check (Success and Page (201 .. 236) = Raw);
      Check ((for all I in 237 .. Page'Last => Page (I) = 0));
      Copies.Copy_Validated (Expected, Raw, Tiny, Success);
      Check (not Success and then (for all B of Tiny => B = 0));
      Copies.Copy_Validated (Expected, Raw, Empty, Success);
      Check (not Success);
      Check_Failure (Expected, Raw (101 .. 135));
      Check_Failure (Expected, Raw & Byte'(0));
      Check_Failure ((Expected with delta Name => "SSDT"), Raw);
      Check_Failure ((Expected with delta Name => "FACS"), Raw);
      Check_Failure ((Expected with delta Revision => 1), Raw);
      Check_Failure ((Expected with delta Physical => Address_Value'Last), Raw);
      -- Every single-byte corruption, including header, length and checksum.
      for I in Raw'Range loop
         declare
            Original : constant Byte := Raw (I);
         begin
            for Delta_Byte in Byte range 1 .. 255 loop
               Raw (I) := Original + Delta_Byte;
               Check_Failure (Expected, Raw);
            end loop;
            Raw (I) := Original;
         end;
      end loop;
      Copies.Copy_Validated (Expected, Raw, Page, Success);
      Check (Success);
      Raw := [others => 255];
      Check (Copies.Matches (Expected, Page (201 .. 236)));
   end;
   Ada.Text_IO.Put_Line ("ACPI-CATALOG: PASS" & Checks'Image);
end Catalog_Tests;
