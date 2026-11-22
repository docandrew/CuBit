pragma Ada_2022;
package body PCI_Power_Control with SPARK_Mode is
   function Locate (Data : Configuration) return Location is
      Pointer : Natural range 0 .. 255 := Natural (Data (16#34#));
      Seen : array (Natural range 0 .. 255) of Boolean := [others => False];
      Result : Location := (Status => Absent, Offset => PM_Offset'First);
   begin
      --  No function, not a type-0 header, or no capability list.
      if Data (0) = 16#FF# and then Data (1) = 16#FF# then
         return (Status => Absent, Offset => PM_Offset'First);
      end if;
      if (Data (16#0E#) and 16#7F#) /= 0 then
         return (Status => Malformed, Offset => PM_Offset'First);
      end if;
      if (Data (6) and 16#10#) = 0 then
         return (Status => Absent, Offset => PM_Offset'First);
      end if;
      --  At most 48 distinct dword-aligned headers fit 0x40 .. 0xFC; walk the
      --  whole chain so a later duplicate or cycle is still refused.
      for Step in 1 .. 48 loop
         pragma Loop_Invariant
           (if Result.Status = Found then
              Result.Offset mod 4 = 0 and then Data (Result.Offset) = 1);
         if Pointer = 0 then
            return Result;
         end if;
         if Pointer < 16#40# or else Pointer mod 4 /= 0 or else Seen (Pointer) then
            return (Status => Malformed, Offset => PM_Offset'First);
         end if;
         Seen (Pointer) := True;
         if Data (Pointer) = 1 then
            if Result.Status = Found or else Pointer > PM_Offset'Last then
               return (Status => Malformed, Offset => PM_Offset'First);
            end if;
            Result := (Status => Found, Offset => Pointer);
         end if;
         Pointer := Natural (Data (Pointer + 1));
      end loop;
      if Pointer /= 0 then
         return (Status => Malformed, Offset => PM_Offset'First);
      end if;
      return Result;
   end Locate;
end PCI_Power_Control;
