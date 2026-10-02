pragma Ada_2022;
package body ACPI_FADT.Transactions with SPARK_Mode is
   procedure Byte_Difference (Low, High : Natural) with
     Ghost, Pre => Low <= High and then High <= 63,
     Post => Unsigned_64 (High) - Unsigned_64 (Low) = Unsigned_64 (High - Low);
   procedure Byte_Difference (Low, High : Natural) is
   begin
      pragma Assert (Unsigned_64 (High) = Unsigned_64 (Low) + Unsigned_64 (High - Low));
   end Byte_Difference;
   function Describe
     (Item : Generic_Address; Width : Access_Width;
      Allowed_First, Allowed_Last : Unsigned_64) return Plan is
      Unit_Bytes : constant Positive range 1 .. 8 := Octets (Width);
      Unit_Bits : constant Positive := Unit_Bytes * 8;
      Start_Unit : constant Natural := Natural (Item.Bit_Offset) / Unit_Bits;
      End_Unit : constant Natural :=
        (Natural (Item.Bit_Offset) + Natural (Item.Width) + Unit_Bits - 1) / Unit_Bits;
      Skip, Tail : Unsigned_64 range 0 .. 63;
      First, Last : Unsigned_64;
   begin
      if Item.Address = 0 then return (Code => Absent); end if;
      if Item.Space not in 0 | 1 then return (Code => Unsupported_Space); end if;
      if Item.Width = 0 then return (Code => Empty_Field); end if;
      if Item.Space = 1 and Width = Qword_Access then
         return (Code => Unsupported_IO_Width);
      end if;
      if not Aligned (Item.Address, Width) then
         return (Code => Misaligned);
      end if;
      pragma Assert (End_Unit > Start_Unit);
      pragma Assert (End_Unit * Unit_Bytes - 1 - Start_Unit * Unit_Bytes =
        (End_Unit - Start_Unit) * Unit_Bytes - 1);
      Skip := Unsigned_64 (Start_Unit * Unit_Bytes);
      Tail := Unsigned_64 (End_Unit * Unit_Bytes - 1);
      pragma Assert (Natural (Tail) - Natural (Skip) =
        (End_Unit - Start_Unit) * Unit_Bytes - 1);
      pragma Assert (Skip <= Tail);
      Byte_Difference (Start_Unit * Unit_Bytes, End_Unit * Unit_Bytes - 1);
      pragma Assert (Tail - Skip =
        Unsigned_64 ((End_Unit - Start_Unit) * Unit_Bytes - 1));
      if Tail > Unsigned_64'Last - Item.Address then
         return (Code => Outside_Range);
      end if;
      First := Item.Address + Skip;
      Last := Item.Address + Tail;
      pragma Assert (Last - First = Tail - Skip);
      -- Expose each supported modulus to the prover; all additions above are
      -- bounded before this alignment composition.
      case Width is
         when Byte_Access => null;
         when Word_Access => pragma Assert (First mod 2 = 0);
         when Dword_Access => pragma Assert (First mod 4 = 0);
         when Qword_Access => pragma Assert (First mod 8 = 0);
      end case;
      if First < Allowed_First or else Last > Allowed_Last or else
        (Item.Space = 1 and then Last > 65535)
      then return (Code => Outside_Range); end if;
      return (Code => Ready, First => First, Last => Last,
              Width => Width, Count => End_Unit - Start_Unit);
   end Describe;
   function Address_At (Item : Plan; Index : Positive) return Unsigned_64 is
      Offset : constant Unsigned_64 := Unsigned_64 ((Index - 1) * Octets (Item.Width));
   begin
      pragma Assert (Offset <= Item.Last - Item.First);
      pragma Assert (Offset <= Unsigned_64'Last - Item.First);
      return Item.First + Offset;
   end Address_At;
end ACPI_FADT.Transactions;
