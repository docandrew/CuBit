with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Multiboot2_Info; use Multiboot2_Info;
with Multiboot_Memory_Map;
with Firmware_Tables;
procedure Main is
   use type Firmware_Tables.Admission;
   use type Firmware_Tables.Root_Kind;
   Data : Bytes (0 .. 199) := [others => 0];
   Value : Snapshot;
   Map : Multiboot_Memory_Map.Entries (1 .. 8);
   Count : Natural;
   Result : Status;
   Checks : Natural := 0;
   procedure Check (B : Boolean) is
   begin
      Checks := Checks + 1;
      if not B then raise Program_Error with "check" & Checks'Image; end if;
   end Check;
   procedure Word (D : in out Bytes; At_Byte : Natural; V : Unsigned_32) is
   begin
      for I in 0 .. 3 loop
         D (At_Byte + I) := Unsigned_8 (Shift_Right (V, I * 8) and 255);
      end loop;
   end Word;
   procedure Run (D : Bytes; Expected : Status) is
   begin
      Parse (D, Unsigned_64'Last, Value, Map, Count, Result);
      Check (Result = Expected);
      if Result /= Success then
         Check (Count = 0 and Value.Module_Count = 0 and
                Value.Root.Status /= Firmware_Tables.Accepted);
      end if;
   end Run;
begin
   Word (Data, 0, Data'Length);
   -- memory map: one 24-byte record
   Word (Data, 8, 6); Word (Data, 12, 40); Word (Data, 16, 24);
   Word (Data, 32, 16#2000_0000#); Word (Data, 40, 1);
   -- framebuffer
   Word (Data, 48, 8); Word (Data, 52, 38);
   Word (Data, 56, 16#E000_0000#); Word (Data, 64, 4096);
   Word (Data, 68, 1024); Word (Data, 72, 768);
   Data (76) := 32; Data (77) := 1;
   Data (80 .. 85) := [16, 8, 8, 8, 0, 8];
   -- module
   Word (Data, 88, 3); Word (Data, 92, 25);
   Word (Data, 96, 16#200_0000#); Word (Data, 100, 16#210_0000#);
   declare
      Name : constant String := "init.img";
      Magic : constant String := "RSD PTR ";
      Sum : Unsigned_8 := 0;
   begin
      for I in Name'Range loop Data (103 + I) := Character'Pos (Name (I)); end loop;
      Word (Data, 120, 15); Word (Data, 124, 44);
      for I in Magic'Range loop Data (127 + I) := Character'Pos (Magic (I)); end loop;
      Data (143) := 2;
      Word (Data, 148, 36); Word (Data, 152, 16#100_0000#);
      for I in 128 .. 147 loop Sum := Sum + Data (I); end loop;
      Data (136) := -Sum;
      Sum := 0;
      for I in 128 .. 163 loop Sum := Sum + Data (I); end loop;
      Data (160) := -Sum;
   end;
   Word (Data, 168, 16#FAFA#); Word (Data, 172, 24); -- unknown tag
   Word (Data, 196, 8);
   Run (Data, Success);
   Check (Count = 1 and then Map (1).Last = 16#1FFF_FFFF#);
   Check (Value.Module_Count = 1 and then Value.Modules (1).Name.Text (1 .. 8) = "init.img");
   Check (Value.Frame.Width = 1024 and Value.Frame.Blue_Size = 8);
   Check (Value.Root.Status = Firmware_Tables.Accepted and then
          Value.Root.Kind = Firmware_Tables.XSDT and then Value.Root.Address = 16#100_0000#);
   for N in 0 .. Data'Length - 1 loop Run (Data (0 .. N - 1), Bad_Header); end loop;
   for Size in Unsigned_32 range 0 .. 7 loop
      declare D : Bytes := Data; begin
         Word (D, 12, Size); Run (D, Bad_Tag);
      end;
   end loop;
   declare D : Bytes := Data; begin
      Word (D, 12, Unsigned_32'Last); Run (D, Bad_Tag);
      D := Data; Word (D, 16, 23); Run (D, Invalid_Map);
      D := Data; Word (D, 20, 1); Run (D, Invalid_Map);
      D := Data; Word (D, 168, 6); Run (D, Duplicate_Tag);
      D := Data; Word (D, 168, 8); Run (D, Duplicate_Tag);
      D := Data; Word (D, 168, 15); Run (D, Duplicate_Tag);
      D := Data; Word (D, 168, 18); Run (D, Boot_Services_Active);
      D := Data; Word (D, 8, 99); Run (D, Missing_Map);
      D := Data; Word (D, 48, 99); Run (D, Missing_Framebuffer);
      D := Data; Word (D, 192, 99); Run (D, Missing_End);
      D := Data; D (136) := D (136) + 1; Run (D, Invalid_ACPI);
      D := Data; D (112) := 1; Run (D, Invalid_Module);
      D := Data; Word (D, 52, 32); Run (D, Invalid_Framebuffer);
      D := Data; Word (D, 12, 39); Run (D, Invalid_Map);
      D := Data; Word (D, 24, 16#FFFF_FFFF#); Word (D, 28, 16#FFFF_FFFF#);
      Run (D, Invalid_Map);
   end;
   declare Shifted : constant Bytes (100 .. 299) := Data; begin
      Run (Shifted, Success);
   end;
   declare
      Extended : Bytes (0 .. 207) := [others => 16#CC#];
      Empty_Map : Multiboot_Memory_Map.Entries (1 .. 0);
   begin
      Extended (0 .. 47) := Data (0 .. 47);
      Extended (56 .. 207) := Data (48 .. 199);
      Word (Extended, 0, Extended'Length);
      Word (Extended, 12, 48);
      Word (Extended, 16, 32);
      Run (Extended, Success); -- version-zero records with opaque extensions
      Check (Count = 1 and then Map (1).Last = 16#1FFF_FFFF#);
      Parse (Data, Unsigned_64'Last, Value, Empty_Map, Count, Result);
      Check (Result = Capacity_Exceeded and Count = 0 and Value.Module_Count = 0);
      Parse (Data, 16#FFFF#, Value, Map, Count, Result);
      Check (Result = Invalid_Map and Count = 0);
   end;
   -- Full-byte mutation corpus: not every mutation is invalid (unknown tags
   -- and addresses may be legitimate), but none may escape admission by
   -- exception or publish partial counts on failure.
   for I in Data'Range loop
      for Delta_Byte in Unsigned_8 range 1 .. 255 loop
         declare D : Bytes := Data; begin
            D (I) := D (I) + Delta_Byte;
            Parse (D, Unsigned_64'Last, Value, Map, Count, Result);
            Check (Count <= Map'Length);
            if Result /= Success then
               Check (Count = 0 and Value.Module_Count = 0 and
                      Value.Root.Status /= Firmware_Tables.Accepted);
            else
               Check (Count > 0 and Value.Frame.Kind in 1 .. 2);
               for J in 1 .. Count loop
                  Check (Multiboot_Memory_Map.Valid (Map (J), Unsigned_64'Last));
               end loop;
            end if;
         end;
      end loop;
   end loop;
   Put_Line ("Multiboot2:" & Checks'Image & " checks PASS");
end Main;
