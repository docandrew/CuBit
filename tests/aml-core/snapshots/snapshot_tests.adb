pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Firmware_Tables.Catalog;
with Firmware_Tables.Snapshots;
procedure Snapshot_Tests is
   use Firmware_Tables;
   use Firmware_Tables.Snapshots;
   use type Catalog.Descriptor;
   Checks : Natural := 0;
   procedure Check (B : Boolean) is
   begin
      Checks := Checks + 1;
      if not B then raise Program_Error with Checks'Image; end if;
   end Check;
   function Raw (D : Catalog.Descriptor) return Bytes is
      B : Bytes (1 .. D.Extent) := [others => 0];
      Sum : Byte := 0;
   begin
      for I in 1 .. 4 loop B (I) := Character'Pos (D.Name (I)); end loop;
      for I in 0 .. 3 loop B (5 + I) := Byte (Shift_Right (Unsigned_32 (D.Extent), 8 * I) and 255); end loop;
      B (9) := D.Revision;
      for V of B loop Sum := Sum + V; end loop;
      B (10) := 0 - Sum;
      return B;
   end Raw;
   Base : constant Catalog.Descriptor := (16#1000#, 36, "DSDT", 2);
   D : Catalog.Descriptor;
   Good : Boolean;
   Output : Bytes (17 .. 17 + Max_Table_Bytes + 4095);
   Tiny : Bytes (1 .. 35);
   None : Bytes (1 .. 0);
begin
   for N in 0 .. Max_Tables + 1 loop
      declare
         S : State;
      begin
         Copy (S, 1, Output, Good);
         Check (not Good and (for all V of Output => V = 0));
         Begin_Snapshot (S, N);
         for I in 1 .. N loop
            D := (Base with delta Name => (if I = 1 then "DSDT" else "SSDT"));
            Append (S, D, Raw (D), Good);
            Check (Good = (N <= Max_Tables));
            Check (Count (S) = 0);
         end loop;
         Seal (S);
         Check (Count (S) = (if N in 1 .. Max_Tables then N else 0));
         if Current (S) = Ready then
            Copy (S, 1, Tiny, Good); Check (not Good and (for all V of Tiny => V = 0));
            Copy (S, 1, None, Good); Check (not Good);
            Copy (S, N + 1, Output, Good); Check (not Good and (for all V of Output => V = 0));
            for I in 1 .. N loop
               Copy (S, I, Output, Good);
               Check (Good and Output (17 .. 52) = Raw (Item (S, I)));
               Check ((for all J in 53 .. Output'Last => Output (J) = 0));
            end loop;
            -- All mutation entrypoints are inert after publication.
            Begin_Snapshot (S, 0); Append (S, Base, Raw (Base), Good);
            Check (not Good); Reject (S); Seal (S);
            Check (Count (S) = N and Current (S) = Ready);
            Copy (S, 1, Output, Good);
            Check (Good and Output (17 .. 52) = Raw (Base));
         end if;
      end;
   end loop;
   for Failure in 1 .. 9 loop
      declare
         S : State;
         B : Bytes (1 .. 36) := Raw (Base);
      begin
         Begin_Snapshot (S, 2);
         if Failure = 1 then
            Seal (S);
         elsif Failure = 2 then
            Reject (S);
         elsif Failure = 3 then
            D := (Base with delta Name => "SSDT");
            Append (S, D, Raw (D), Good);
         elsif Failure = 4 then
            B (11) := 1; Append (S, Base, B, Good);
         elsif Failure = 5 then
            Append (S, Base, Tiny, Good);
         else
            Append (S, Base, B, Good); Check (Good);
            D := (Base with delta Name => "SSDT");
            case Failure is
               when 6 => D.Name := "DSDT";
               when 7 => D.Name := "FACS";
               when 8 => D.Extent := Max_Table_Bytes + 1;
               when 9 => D.Physical := Unsigned_64'Last;
               when others => null;
            end case;
            Append (S, D, Raw (D), Good);
         end if;
         Seal (S); Check (Current (S) = Failed and Count (S) = 0);
         Begin_Snapshot (S, 1); Append (S, Base, Raw (Base), Good);
         Check (not Good); Seal (S); Copy (S, 1, Output, Good);
         Check (not Good and (for all V of Output => V = 0));
      end;
   end loop;
   -- Original buffer is no longer authoritative after capture. Maximum-size
   -- tables fill the total budget exactly; the next append fails the inventory.
   for N in 16 .. 17 loop
      declare
         S : State;
         B : Bytes (1 .. Max_Table_Bytes);
      begin
         Begin_Snapshot (S, N);
         for I in 1 .. N loop
            D := (Base with delta Extent => Max_Table_Bytes,
                  Name => (if I = 1 then "DSDT" else "SSDT"));
            B := Raw (D); Append (S, D, B, Good); B := [others => 255];
            Check (Good = (I <= 16));
         end loop;
         Seal (S); Check (Count (S) = (if N = 16 then 16 else 0));
         Copy (S, 1, Output, Good); Check (Good = (N = 16));
         if Good then
            D.Name := "DSDT";
            Check (Output (17 .. 16 + Max_Table_Bytes) = Raw (D));
            Check ((for all J in 17 + Max_Table_Bytes .. Output'Last => Output (J) = 0));
         end if;
      end;
   end loop;
   -- Runtime capacities exceed all three prototype limits. Unequal lengths
   -- and payloads catch overlap or accidental fixed-stride indexing. Mutating
   -- caller storage after every append must not affect the published snapshot.
   declare
      Sizes : constant array (Positive range <>) of Positive :=
        [36, 65_537, 145_770, 1_048_577];
      Total : Positive := 36 * 35;
   begin
      for N of Sizes loop Total := Total + N; end loop;
      declare
         S : State (39, Total, Sizes (4));
      begin
         Begin_Snapshot (S, 39);
         for I in 1 .. 39 loop
            D := (Base with delta
              Extent => (if I <= Sizes'Length then Sizes (I) else 36),
              Name => (if I = 1 then "DSDT" else "SSDT"),
              Revision => Byte (I));
            declare
               B : Bytes := Raw (D);
            begin
               Append (S, D, B, Good);
               B := [others => 255];
               Check (Good and Count (S) = 0);
            end;
         end loop;
         Seal (S);
         Check (Current (S) = Ready and Count (S) = 39);
         for I in 1 .. 39 loop
            D := Item (S, I);
            declare
               -- Exercise a non-one-based destination ending at Positive'Last.
               B : Bytes (Positive'Last - D.Extent - 63 .. Positive'Last);
            begin
               Copy (S, I, B, Good);
               Check (Good);
               Check (B (B'First .. B'First + D.Extent - 1) = Raw (D));
               Check ((for all J in B'First + D.Extent .. B'Last => B (J) = 0));
            end;
         end loop;
      end;
   end;
   -- The allocation and per-table quota are independent. Exact fit succeeds;
   -- insufficient allocation, count or per-table quota fails without publishing.
   for Failure in 0 .. 3 loop
      declare
         S : State
           ((if Failure = 1 then 1 else 2),
            (if Failure = 2 then 71 else 72),
            (if Failure = 3 then 35 else 36));
      begin
         Begin_Snapshot (S, 2);
         Append (S, Base, Raw (Base), Good);
         Check (Good = (Failure not in 1 | 3));
         D := (Base with delta Name => "SSDT");
         Append (S, D, Raw (D), Good);
         Check (Good = (Failure = 0));
         Seal (S);
         Check (Count (S) = (if Failure = 0 then 2 else 0));
         if Failure /= 0 then
            Copy (S, 1, Output, Good);
            Check (not Good and (for all B of Output => B = 0));
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("ACPI-SNAPSHOT: PASS" & Checks'Image);
end Snapshot_Tests;
