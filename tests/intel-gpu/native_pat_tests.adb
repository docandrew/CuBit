with Ada.Text_IO; with Interfaces; with Interfaces.C;
with System; with System.Storage_Elements;
with Intel_GPU_Native_PAT;
procedure Native_PAT_Tests is
   use Interfaces; use System; use System.Storage_Elements;
   use type Interfaces.C.int;
   Owner : Boolean := False;
   Checks, Fail_Check : Natural := 0;
   function Ready return Boolean is
   begin Checks := Checks + 1; return Owner and Checks /= Fail_Check; end Ready;
   package IO is new Intel_GPU_Native_PAT (Ready);
   function Mmap (Address : System.Address; Length : Interfaces.C.size_t;
     Protection, Flags, FD : Interfaces.C.int; Offset : Interfaces.C.long)
     return System.Address with Import, Convention => C, External_Name => "mmap";
   function Munmap (Address : System.Address; Length : Interfaces.C.size_t)
     return Interfaces.C.int with Import, Convention => C, External_Name => "munmap";
   Mapping : System.Address;
   OK : Boolean;
   Count : Natural := 0;
   Entry_Zero : Unsigned_32 with Import, Volatile_Full_Access,
     Address => To_Address (16#61209800#);
begin
   pragma Assert (IO.Read32 (16#4800#) = Unsigned_32'Last);
   IO.Write32 (16#4800#, 3, OK); pragma Assert (not OK);
   for Offset in Unsigned_32 range 0 .. 16#1FFFFF# loop
      pragma Assert ((IO.Address_For (Offset) /= 0) =
        (Offset in 16#4800# .. 16#481C# and Offset mod 4 = 0));
      if IO.Address_For (Offset) /= 0 then
         Count := Count + 1;
         pragma Assert (IO.Address_For (Offset) = 16#61209000# + Unsigned_64 (Offset mod 4096));
      end if;
   end loop;
   pragma Assert (Count = 8 and IO.Address_For (Unsigned_32'Last) = 0);
   Mapping := Mmap (To_Address (16#61209000#), 4096, 3, 16#100022#, -1, 0);
   if Mapping /= To_Address (16#61209000#) then raise Program_Error; end if;
   Owner := True;
   for I in Unsigned_32 range 0 .. 7 loop
      IO.Write32 (16#4800# + I * 4, I mod 4, OK); pragma Assert (OK);
      pragma Assert (IO.Read32 (16#4800# + I * 4) = I mod 4);
   end loop;
   IO.Write32 (16#4800#, 4, OK); pragma Assert (not OK and Entry_Zero = 0);
   IO.Write32 (16#4800#, Unsigned_32'Last, OK); pragma Assert (not OK and Entry_Zero = 0);
   Checks := 0; Fail_Check := 1;
   IO.Write32 (16#4800#, 3, OK); pragma Assert (not OK and Entry_Zero = 0);
   Checks := 0; Fail_Check := 2;
   IO.Write32 (16#4800#, 3, OK); pragma Assert (not OK and Entry_Zero = 3);
   Checks := 0;
   pragma Assert (IO.Read32 (16#4800#) = Unsigned_32'Last);
   pragma Assert (Munmap (Mapping, 4096) = 0);
   Ada.Text_IO.Put_Line ("native PAT PASS: exact offsets, host stores, invalid values, ownership loss (NOT GPU)");
end Native_PAT_Tests;
