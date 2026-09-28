package body Intel_GPU_PCI_Power with SPARK_Mode is
   function Decode (Data : Configuration) return Power_Status is
      Pointer : Natural range 0 .. 255 := Natural (Data (16#34#));
      Seen : array (Natural range 0 .. 255) of Boolean := [others => False];
      Found : Boolean := False;
      PM_Offset : Natural range 0 .. 248 := 0;
      State : Power_Status := Unavailable;
   begin
      if Data (0) = 16#FF# and Data (1) = 16#FF# then
         return Unavailable;
      end if;
      if (Data (16#0E#) and 16#7F#) /= 0 then
         return Malformed;
      end if;
      if (Data (6) and 16#10#) = 0 then
         return Unavailable;
      end if;
      --  At most 48 distinct DWORD-aligned capability headers fit 0x40..0xFC.
      --  Inspect the entire chain so an earlier PM entry cannot hide a cycle
      --  or duplicate PM capability later in the list.
      for Step in 1 .. 48 loop
         if Pointer = 0 then
            if Found and then Seen (PM_Offset + 4) then
               return Malformed;
            end if;
            return State;
         end if;
         if Pointer < 16#40# or else Pointer mod 4 /= 0 or else Seen (Pointer)
         then
            return Malformed;
         end if;
         Seen (Pointer) := True;
         if Data (Pointer) = 1 then
            if Found or else Pointer > 16#F8# then
               return Malformed;
            end if;
            if (Data (Pointer + 2) and 7) not in 1 .. 3 then
               return Malformed;
            end if;
            Found := True;
            PM_Offset := Pointer;
            case Data (Pointer + 4) and 3 is
               when 0 => State := D0;
               when 1 => State := D1;
               when 2 => State := D2;
               when others => State := D3_Hot;
            end case;
         end if;
         Pointer := Natural (Data (Pointer + 1));
      end loop;
      if Pointer /= 0 then
         return Malformed;
      end if;
      --  A capability header cannot alias the second DWORD of the PM record.
      if Found and then Seen (PM_Offset + 4) then
         return Malformed;
      end if;
      return State;
   end Decode;
end Intel_GPU_PCI_Power;
