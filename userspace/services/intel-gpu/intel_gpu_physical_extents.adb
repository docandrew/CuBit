package body Intel_GPU_Physical_Extents with SPARK_Mode is
   function Ready (Object : Map) return Boolean is (Object.Accepted);

   procedure Admit (Bases : Addresses; Object : out Map; Success : out Boolean) is
   begin
      Object := (Accepted => False, Bases => [others => 0]);
      Success := False;
      for I in Block_Index loop
         -- Preserve the current below4GiB DMA policy. Alignment means two
         -- admitted equal-sized blocks overlap iff their bases are equal.
         if Bases (I) = 0 or else Bases (I) mod Block_Bytes /= 0 or else
           Bases (I) > 2 ** 32 - Block_Bytes
         then return; end if;
         for J in Block_Index loop
            if J < I and then Bases (J) = Bases (I) then return; end if;
         end loop;
      end loop;
      Object := (Accepted => True, Bases => Bases);
      Success := True;
   end Admit;

   function Resolve (Object : Map; Offset, Bytes : Unsigned_64) return Span is
      Index : Block_Index;
      Within_Block, Length : Unsigned_64;
   begin
      if not Object.Accepted or else Bytes = 0 or else Offset >= Capacity
        or else Bytes > Capacity - Offset
      then return (others => <>); end if;
      Index := Block_Index (Offset / Block_Bytes);
      Within_Block := Offset mod Block_Bytes;
      Length := Unsigned_64'Min (Bytes, Block_Bytes - Within_Block);
      return (True, Object.Bases (Index) + Within_Block, Length);
   end Resolve;
end Intel_GPU_Physical_Extents;
