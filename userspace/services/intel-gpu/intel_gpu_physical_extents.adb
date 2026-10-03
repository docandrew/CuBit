package body Intel_GPU_Physical_Extents with SPARK_Mode is
   function Ready (Object : Map) return Boolean is (Object.Accepted);
   function Committed_Bytes (Object : Map) return Unsigned_64 is
     (if Object.Accepted then Unsigned_64 (Object.Count) * Block_Bytes else 0);
   function Compatible (Left, Right : Map) return Boolean is
   begin
      if not Ready (Left) or else not Ready (Right) then return False; end if;
      for I in Block_Index loop
         if I < Natural'Min (Left.Count, Right.Count) and then
           Left.Bases (I) /= Right.Bases (I) then return False; end if;
      end loop;
      return True;
   end Compatible;

   procedure Admit (Bases : Addresses; Object : out Map; Success : out Boolean;
                    Committed_Blocks : Natural := Addresses'Length) is
   begin
      Object := (Accepted => False, Count => 0, Bases => [others => 0]);
      Success := False;
      if Committed_Blocks not in 1 .. Addresses'Length then return; end if;
      for I in Block_Index loop
         if I >= Committed_Blocks then
            -- Uncommitted entries must not smuggle device addresses.
            if Bases (I) /= 0 then return; end if;
         else
         -- Preserve the current below4GiB DMA policy. Alignment means two
         -- admitted equal-sized blocks overlap iff their bases are equal.
         if Bases (I) = 0 or else Bases (I) mod Block_Bytes /= 0 or else
           Bases (I) > 2 ** 32 - Block_Bytes
         then return; end if;
         for J in Block_Index loop
            if J < I and then Bases (J) = Bases (I) then return; end if;
         end loop;
         end if;
      end loop;
      Object := (Accepted => True, Count => Committed_Blocks, Bases => Bases);
      Success := True;
   end Admit;

   function Resolve (Object : Map; Offset, Bytes : Unsigned_64) return Span is
      Index : Block_Index;
      Within_Block, Length : Unsigned_64;
   begin
      if not Object.Accepted or else Bytes = 0 or else Offset >= Committed_Bytes (Object)
        or else Bytes > Committed_Bytes (Object) - Offset
      then return (others => <>); end if;
      Index := Block_Index (Offset / Block_Bytes);
      Within_Block := Offset mod Block_Bytes;
      Length := Unsigned_64'Min (Bytes, Block_Bytes - Within_Block);
      return (True, Object.Bases (Index) + Within_Block, Length);
   end Resolve;
end Intel_GPU_Physical_Extents;
