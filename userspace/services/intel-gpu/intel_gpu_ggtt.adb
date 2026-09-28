package body Intel_GPU_GGTT with SPARK_Mode is
   function Encode_System_Page (DMA_Address : Unsigned_64) return Unsigned_64 is
   begin
      if DMA_Address = 0 or else DMA_Address >= 2 ** 32 or else
        DMA_Address mod 4096 /= 0
      then return 0; end if;
      -- Gen8+ GGTT system memory: address and present bit. Do not apply
      -- PPGTT writable/cache bits or the Gen12 local-memory selector here.
      return DMA_Address + 1;
   end Encode_System_Page;
   function Table_Size (GGC : Unsigned_16) return Unsigned_64 is
   begin
      if GGC = Unsigned_16'Last then return 0; end if;
      case Shift_Right (GGC, 6) and 3 is
         when 1 => return 2 * 1024 * 1024;
         when 2 => return 4 * 1024 * 1024;
         when 3 => return 8 * 1024 * 1024;
         when others => return 0;
      end case;
   end Table_Size;
   function Plan_Window
     (Table_Bytes, GPU_Start, Buffer_Bytes : Unsigned_64) return Window
   is
      First, Count, Begin_Byte, End_Byte, Map_Begin, Map_End : Unsigned_64;
   begin
      if Table_Bytes = 0 or else Table_Bytes > Maximum_Table_Bytes or else
        Table_Bytes mod 4096 /= 0 or else GPU_Start mod 4096 /= 0 or else
        Buffer_Bytes = 0
      then return (others => <>); end if;
      First := GPU_Start / 4096;
      Count := (Buffer_Bytes - 1) / 4096 + 1;
      if First >= Table_Bytes / 8 or else
        Count > Table_Bytes / 8 - First
      then return (others => <>); end if;
      Begin_Byte := First * 8;
      End_Byte := (First + Count) * 8;
      Map_Begin := Begin_Byte / 4096 * 4096;
      Map_End := (End_Byte + 4095) / 4096 * 4096;
      return (True, First, Count, Table_BAR_Offset + Map_Begin,
              Map_End - Map_Begin);
   end Plan_Window;
end Intel_GPU_GGTT;
