pragma Ada_2022;
-- Geometry after register-specific access-width selection. A GAS Access_Size
-- field is not universally authoritative (legacy FADT registers differ).
-- This unit does not resolve that policy, grant authority, or perform accesses.
package ACPI_FADT.Transactions with SPARK_Mode, Pure is
   type Access_Width is (Byte_Access, Word_Access, Dword_Access, Qword_Access);
   function Octets (Width : Access_Width) return Positive is
     (case Width is when Byte_Access => 1, when Word_Access => 2,
       when Dword_Access => 4, when Qword_Access => 8);
   function Aligned (Address : Unsigned_64; Width : Access_Width) return Boolean is
     (case Width is when Byte_Access => True,
       when Word_Access => Address mod 2 = 0,
       when Dword_Access => Address mod 4 = 0,
       when Qword_Access => Address mod 8 = 0);
   type Status is (Ready, Absent, Unsupported_Space, Empty_Field,
                   Unsupported_IO_Width, Misaligned, Outside_Range);
   type Plan (Code : Status := Absent) is record
      case Code is
         when Ready =>
            First, Last : Unsigned_64;
            Width : Access_Width;
            Count : Positive range 1 .. 64;
         when others => null;
      end case;
   end record;
   -- Bounds come from an already selected region, never from AML as authority.
   -- Require naturally aligned transactions for the intended backend. A field
   -- can span several transactions; edge accesses include neighboring bits.
   function Describe
     (Item : Generic_Address; Width : Access_Width;
      Allowed_First, Allowed_Last : Unsigned_64) return Plan with
     Post => (if Describe'Result.Code = Ready then
       Item.Space in 0 | 1 and then Item.Width /= 0
       and then Describe'Result.Width = Width
       and then Describe'Result.First >= Allowed_First
       and then Describe'Result.Last <= Allowed_Last
       and then Describe'Result.First <= Describe'Result.Last
       and then Describe'Result.First >= Item.Address
       and then Describe'Result.First - Item.Address =
         Unsigned_64 ((Natural (Item.Bit_Offset) / (8 * Octets (Width))) * Octets (Width))
       and then Describe'Result.Last - Item.Address =
         Unsigned_64 (((Natural (Item.Bit_Offset) + Natural (Item.Width) +
           8 * Octets (Width) - 1) / (8 * Octets (Width))) * Octets (Width) - 1)
       and then Aligned (Describe'Result.First, Width)
       and then Describe'Result.Last - Describe'Result.First =
         Unsigned_64 (Describe'Result.Count * Octets (Width) - 1)
       and then (if Item.Space = 1 then Width /= Qword_Access
         and then Describe'Result.Last <= 65535));
   function Address_At (Item : Plan; Index : Positive) return Unsigned_64 with
     Pre => Item.Code = Ready and then Index <= Item.Count
       and then Item.First <= Item.Last
       and then Item.Last - Item.First = Unsigned_64 (Item.Count * Octets (Item.Width) - 1),
     Post => Address_At'Result >= Item.First
       and then Address_At'Result <= Item.Last
       and then Item.Last - Address_At'Result >= Unsigned_64 (Octets (Item.Width) - 1);
end ACPI_FADT.Transactions;
