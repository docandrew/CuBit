pragma Ada_2022;
package body Multiboot_Entry with SPARK_Mode is
   procedure Admit_Address
     (Magic : Unsigned_32; Base, Limit : Unsigned_64; Result : out Status)
   is
   begin
      if Magic /= Loader_Magic then
         Result := Wrong_Loader;
      elsif Base = 0 or else Base >= Limit or else
        Header_Bytes > Limit - Base
      then
         Result := Header_Outside_Mapping;
      else
         Result := Success;
      end if;
   end Admit_Address;

   procedure Snapshot (Data : Header; Output : out Snapshot_Header;
                       Result : out Status)
   is
   begin
      Output := [others => 0];
      if (Data (0) and 16#40#) = 0 then
         Result := Missing_Memory_Map;
      elsif (Data (1) and 16#10#) = 0 then
         Result := Missing_Framebuffer;
      elsif Data (109) not in 1 .. 2 then
         Result := Unsupported_Framebuffer;
      else
         -- No failure branches after publication starts.
         Output (0) := Data (0) and 16#48#;
         Output (1) := 16#10#;
         if (Data (0) and 8) /= 0 then
            Output (20 .. 27) := Data (20 .. 27);
         end if;
         Output (44 .. 51) := Data (44 .. 51);
         Output (88 .. 109) := Data (88 .. 109);
         if Data (109) = 1 then
            -- Normalize the GRUB wire ABI into our packed internal snapshot.
            Output (110 .. 115) := Data (112 .. 117);
         end if;
         Result := Success;
      end if;
   end Snapshot;
end Multiboot_Entry;
