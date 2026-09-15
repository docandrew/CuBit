pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Multiboot_Entry; use Multiboot_Entry;

procedure Main is
   Count : Natural := 0;
   Result : Status;
   Data : Header;
   Output : Snapshot_Header;

   function Assembly_Gate (Magic, Base : Unsigned_32) return Unsigned_32
     with Import, Convention => C, External_Name => "test_boot_entry_gate";

   procedure Check_Address (Base, Limit : Unsigned_64; Magic : Unsigned_32) is
      Expected : constant Boolean := Magic = Loader_Magic and then
        Base /= 0 and then Limit >= Header_Bytes and then
        Base <= Limit - Header_Bytes;
   begin
      Admit_Address (Magic, Base, Limit, Result);
      pragma Assert ((Result = Success) = Expected);
      if Limit = Bootstrap_Limit and then Base <= Unsigned_64 (Unsigned_32'Last) then
         pragma Assert ((Assembly_Gate (Magic, Unsigned_32 (Base)) = 1) = Expected);
      end if;
      Count := Count + 1;
   end Check_Address;
begin
   for Limit in Unsigned_64 range 0 .. 256 loop
      for Base in Unsigned_64 range 0 .. 300 loop
         Check_Address (Base, Limit, Loader_Magic);
         Check_Address (Base, Limit, 0);
      end loop;
   end loop;
   for Offset in Unsigned_64 range 0 .. 120 loop
      Check_Address (Bootstrap_Limit - Offset, Bootstrap_Limit, Loader_Magic);
      Check_Address (Bootstrap_Limit - Offset, Bootstrap_Limit, 0);
      Check_Address (Unsigned_64'Last - Offset, Unsigned_64'Last, Loader_Magic);
   end loop;
   for Base in Unsigned_64 range 0 .. 4096 loop
      Check_Address (Base, Bootstrap_Limit, Loader_Magic);
      Check_Address (Base, Bootstrap_Limit, 0);
      Check_Address (Unsigned_64 (Unsigned_32'Last) - Base, Bootstrap_Limit, Loader_Magic);
   end loop;
   -- Exhaust all relevant flag combinations and framebuffer tags. Poison
   -- unadvertised fields, then verify every output byte independently.
   for Flags in Natural range 0 .. 7 loop
      for Tag in Unsigned_8 range 0 .. 255 loop
         Data := [others => 16#A5#];
         -- Distinct mask bytes detect incorrect packed/GRUB union offsets.
         Data (110 .. 117) := [16#DA#, 16#DB#, 16, 8, 8, 8, 0, 8];
         Data (0) := 16#B7#;
         Data (1) := 16#EF#;
         if Flags mod 2 = 1 then Data (0) := Data (0) or 8; end if;
         if (Flags / 2) mod 2 = 1 then Data (0) := Data (0) or 64; end if;
         if Flags / 4 = 1 then Data (1) := Data (1) or 16; end if;
         Data (109) := Tag;
         Snapshot (Data, Output, Result);
         if Flags / 2 mod 2 = 0 then
            pragma Assert (Result = Missing_Memory_Map);
         elsif Flags / 4 = 0 then
            pragma Assert (Result = Missing_Framebuffer);
         elsif Tag not in 1 .. 2 then
            pragma Assert (Result = Unsupported_Framebuffer);
         else
            pragma Assert (Result = Success);
         end if;
         for I in Output'Range loop
            if Result /= Success then
               pragma Assert (Output (I) = 0);
            elsif I = 0 then
               pragma Assert (Output (I) = (if Flags mod 2 = 1 then 72 else 64));
            elsif I = 1 then
               pragma Assert (Output (I) = 16);
            elsif I in 110 .. 115 and then Tag = 1 then
               pragma Assert (Output (I) = Data (I + 2));
            elsif I in 44 .. 51 or else I in 88 .. 109 or else
              (I in 20 .. 27 and then Flags mod 2 = 1)
            then
               pragma Assert (Output (I) = Data (I));
            else
               pragma Assert (Output (I) = 0);
            end if;
         end loop;
         Count := Count + 1;
      end loop;
   end loop;
   Put_Line ("PASS boot entry:" & Count'Image & " address/header cases");
end Main;
