pragma Ada_2022;
with Interfaces;
-- Trusted x86-64 backend adapter. Cell must be the aligned live FACS DWORD
-- in independently admitted retained write-back RAM. No IPC accepts this
-- access value; physical geometry, lifetime and mapping are caller obligations.
package ACPI_Backend_Lock_Atomic with SPARK_Mode => Off is
   type Word is mod 2 ** 32 with Atomic, Size => 32, Alignment => 4;
   function Read_Word (Cell : not null access Word) return Interfaces.Unsigned_32;
   -- One nonblocking atomic attempt. Returns the value observed by cmpxchg;
   -- equality with Expected means Replacement was installed. No hidden retry.
   function Compare_Exchange
     (Cell : not null access Word; Expected, Replacement : Interfaces.Unsigned_32)
      return Interfaces.Unsigned_32;
end ACPI_Backend_Lock_Atomic;
