with Memory_Identity_Map;
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Ada.Text_IO;
procedure Memory_Identity_Map_Test is
   type Page is array (1 .. 512) of Unsigned_64;
   RAM : array (1 .. 64) of aliased Page with Alignment => 4096;
   Live : array (RAM'Range) of Boolean := [others => False];
   Attempts, Fail_At, Outstanding : Natural := 0;
   function Allocate return Address is
   begin
      Attempts := Attempts + 1;
      if Attempts = Fail_At then return Null_Address; end if;
      for I in RAM'Range loop
         if not Live (I) then
            Live (I) := True;
            Outstanding := Outstanding + 1;
            return RAM (I)'Address;
         end if;
      end loop;
      return Null_Address;
   end Allocate;
   procedure Release (Address : System.Address) is
      I : constant Natural := Natural
        ((To_Integer (Address) - To_Integer (RAM'Address)) / 4096) + 1;
   begin
      pragma Assert (I in RAM'Range and then Live (I));
      pragma Assert (Address = RAM (I)'Address);
      Live (I) := False;
      Outstanding := Outstanding - 1;
   end Release;
   package M is new Memory_Identity_Map (Allocate, Release);
   use type M.Insert_Result;
   Object : M.Map;
   Status : M.Insert_Result;
   Removed : Boolean;
   Value : constant Address := To_Address (16#1234#);
   Other : constant Address := To_Address (16#5678#);
begin
   for Failure in 1 .. 7 loop
      Attempts := 0; Fail_At := Failure;
      M.Insert (Object, 1, Value, 65536, Status);
      pragma Assert (Status = M.No_Memory and M.Bytes (Object) = 0);
      pragma Assert (Outstanding = 0 and M.Find (Object, 1) = Null_Address);
   end loop;
   Fail_At := 0;
   M.Insert (Object, 1, Value, 6 * 4096, Status);
   pragma Assert (Status = M.Metadata_Limit and Outstanding = 0);
   M.Insert (Object, 0, Value, 65536, Status);
   pragma Assert (Status = M.Invalid_Argument);
   M.Insert (Object, 1, Null_Address, 65536, Status);
   pragma Assert (Status = M.Invalid_Argument);
   for I in 1 .. 600 loop
      M.Insert (Object, Unsigned_64 (I), Value, 65536, Status);
      pragma Assert (Status = M.Inserted);
      pragma Assert (M.Find (Object, 1) = Value);
   end loop;
   M.Insert (Object, Unsigned_64'Last, Other, 65536, Status);
   pragma Assert (Status = M.Inserted and M.Find (Object, Unsigned_64'Last) = Other);
   M.Insert (Object, 1, Other, 65536, Status);
   pragma Assert (Status = M.Already_Present and M.Find (Object, 1) = Value);
   M.Remove (Object, 1, Other, Removed);
   pragma Assert (not Removed and M.Find (Object, 1) = Value);
   for I in reverse 1 .. 600 loop
      M.Remove (Object, Unsigned_64 (I), Value, Removed);
      pragma Assert (Removed and M.Find (Object, Unsigned_64'Last) = Other);
   end loop;
   M.Remove (Object, Unsigned_64'Last, Other, Removed);
   pragma Assert (Removed and Outstanding = 0 and M.Bytes (Object) = 0);
   -- Distinct identities over time must not permanently consume index pages.
   for I in 1 .. 2000 loop
      M.Insert (Object, Shift_Left (Unsigned_64 (I), 32), Value, 7 * 4096, Status);
      pragma Assert (Status = M.Inserted and Outstanding = 7);
      M.Remove (Object, Shift_Left (Unsigned_64 (I), 32), Value, Removed);
      pragma Assert (Removed and Outstanding = 0 and M.Bytes (Object) = 0);
   end loop;
   Ada.Text_IO.Put_Line
     ("PASS identity map: 600 live values, 64-bit keys, every growth failure, exact removal, 2000 churn cycles with full branch reclamation");
end Memory_Identity_Map_Test;
