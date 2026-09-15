pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Boot_Modules; use Boot_Modules;
with Multiboot_Memory_Map;
procedure Main is
   Map : Multiboot_Memory_Map.Entries (1 .. 3) :=
     [1 => (4096, Window_End - 1, Multiboot_Memory_Map.Usable, False), others => <>];
   State : Catalog;
   Name : Module_Name;
   Result : Status;
   Cases : Natural := 0;
   Seed : Unsigned_32 := 17;
   function Random (Limit : Positive) return Natural is
   begin
      Seed := Seed * 1664525 + 1013904223;
      return Natural (Seed mod Unsigned_32 (Limit));
   end Random;
   function Named (Value : String) return Module_Name is
      N : Module_Name;
   begin
      N.Text (1 .. Value'Length) := Value;
      N.Length := Value'Length;
      return N;
   end Named;
   procedure Reject (First, Limit : Unsigned_64; Expected : Status;
                     N : Module_Name := Named ("other")) is
      Before : constant Catalog := State;
   begin
      Append (State, Map, First, Limit, 4096, N, Result);
      pragma Assert (Result = Expected and then State = Before);
      Cases := Cases + 1;
   end Reject;
begin
   for Length in Natural range 0 .. 128 loop
      for Payload in Natural range 0 .. Length loop
         declare
            Data : Multiboot_Memory_Map.Bytes (7 .. 6 + Length) := [others => 16#A5#];
         begin
            Clear_Padding (Data, Payload);
            for I in Data'Range loop
               pragma Assert (Data (I) = (if I - Data'First < Payload then 16#A5# else 0));
            end loop;
            Cases := Cases + 1;
         end;
      end loop;
   end loop;
   Name.Text (1 .. 8) := "init.img";
   Name.Length := 8;
   Append (State, Map, 4096, 5000, 4096, Name, Result);
   pragma Assert (Result = Success and then Count (State) = 0);
   Reject (4096, 5000, Overlapping_Payload);
   Reject (8192, 9000, Duplicate_Name, Name);
   Reject (0, 5000, Invalid_Range);
   Reject (4097, 5000, Invalid_Range);
   Reject (4096, 4096, Invalid_Range);
   Reject (8192, 4096, Invalid_Range);
   Reject (8192, Unsigned_64'Last, Invalid_Range);
   Reject (8192, 9000, Invalid_Name, Named (""));
   Map (2) := (8192, 8192, Multiboot_Memory_Map.Reserved, False);
   Reject (8192, 9000, Not_RAM);
   Map (2) := (others => <>);
   Seal (State);
   pragma Assert (Count (State) = 1 and then Get (State, 1).Limit = 5000);
   pragma Assert (Reserved_End (State) = 8192);
   Append (State, Map, 8192, 9000, 4096, Name, Result);
   pragma Assert (Result = Already_Sealed);
   Reject (8192, 9000, Already_Sealed);
   declare
      Frozen : Catalog;
      External_Name : Module_Name := Named ("init.img");
   begin
      Append (Frozen, Map, 4096, 5000, 4096, External_Name, Result);
      pragma Assert (Result = Success);
      External_Name.Text (1) := 'X';
      Seal (Frozen);
      pragma Assert (Get (Frozen, 1).Name.Text (1 .. 8) = "init.img");
      Cases := Cases + 1;
   end;
   declare
      Full : Catalog;
      N : Module_Name;
   begin
      for I in 1 .. Maximum_Modules loop
         N.Text (1) := Character'Val (32 + I);
         N.Length := 1;
         Append (Full, Map, Unsigned_64 (I * Page_Bytes),
                 Unsigned_64 (I * Page_Bytes + 1), 4096, N, Result);
         pragma Assert (Result = Success and then Count (Full) = 0);
      end loop;
      declare
         Before : constant Catalog := Full;
      begin
         Append (Full, Map, 999424, 999425, 4096, N, Result);
         pragma Assert (Result = Capacity_Exceeded and then Full = Before);
      end;
      Seal (Full);
      pragma Assert (Count (Full) = Maximum_Modules);
      for I in 1 .. Count (Full) loop
         pragma Assert (Valid (Get (Full, I)));
         pragma Assert (Get (Full, I).Page_Limit <= Reserved_End (Full));
         for J in 1 .. I - 1 loop
            pragma Assert (not Overlaps (Get (Full, I).First, Get (Full, I).Page_Limit,
                                        Get (Full, J).First, Get (Full, J).Page_Limit));
         end loop;
      end loop;
      Cases := Cases + Maximum_Modules;
   end;
   -- A byte ledger independently checks coverage and reserved precedence,
   -- including adjoining regions, holes, duplicates and reversed map order.
   for Trial in 1 .. 2000 loop
      for R of Map loop
         declare
            Low : constant Natural := Random (33);
            High : constant Natural := Low + Random (33 - Low);
         begin
            R := (Unsigned_64 (Low), Unsigned_64 (High),
              (if Random (3) = 0 then Multiboot_Memory_Map.Reserved
               else Multiboot_Memory_Map.Usable), Random (5) = 0);
         end;
      end loop;
      for Low in Address range 1 .. 31 loop
         for High in Address range Low + 1 .. 32 loop
            declare
               Expected : Boolean := True;
               use type Multiboot_Memory_Map.Region_Kind;
            begin
               for Byte in Low .. High - 1 loop
                  declare
                     Usable, Blocked : Boolean := False;
                  begin
                     for R of Map loop
                        if not R.Empty and then R.First <= Unsigned_64 (Byte) and then
                          Unsigned_64 (Byte) <= R.Last
                        then
                           if R.Kind = Multiboot_Memory_Map.Usable then Usable := True;
                           else Blocked := True; end if;
                        end if;
                     end loop;
                     Expected := Expected and Usable and not Blocked;
                  end;
               end loop;
               pragma Assert (In_RAM (Map, Low, High) = Expected);
               Cases := Cases + 1;
            end;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("PASS boot modules:" & Cases'Image & " catalog/RAM cases");
end Main;
