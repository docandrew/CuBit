with Ada.Text_IO; with Interfaces; with Interfaces.C;
with System; with System.Storage_Elements;
with Intel_GPU_Native_GuC_IO;
procedure Native_GuC_IO_Tests is
   use Interfaces; use System; use System.Storage_Elements;
   use type Interfaces.C.int;
   Owner : Boolean := False;
   Checks, Fail_Check : Natural := 0;
   function Ready return Boolean is
   begin Checks := Checks + 1; return Owner and Checks /= Fail_Check; end Ready;
   package IO is new Intel_GPU_Native_GuC_IO (Ready);
   function Mmap (Address : System.Address; Length : Interfaces.C.size_t;
     Protection, Flags, FD : Interfaces.C.int; Offset : Interfaces.C.long)
     return System.Address with Import, Convention => C, External_Name => "mmap";
   function Munmap (Address : System.Address; Length : Interfaces.C.size_t)
     return Interfaces.C.int with Import, Convention => C, External_Name => "munmap";
   Mapping : System.Address;
   OK, Readable, Writable : Boolean;
   R_Count, W_Count : Natural := 0;
   Shim : Unsigned_32 with Import, Volatile_Full_Access, Address => To_Address (16#6120716C#);
begin
   -- Denied ownership must be safe even before any CPU mapping exists.
   pragma Assert (IO.Read32 (16#C000#) = Unsigned_32'Last);
   IO.Write32 (16#13816C#, 1, OK); pragma Assert (not OK);
   for Offset in Unsigned_32 range 0 .. 16#1FFFFF# loop
      Readable := Offset in 16#C000# | 16#C050# | 16#C064# | 16#C314# | 16#C340# | 16#13816C#;
      Writable := Offset mod 4 = 0 and then
        (Offset in 16#C050# | 16#C064# | 16#C340# | 16#13816C# or else
         (Offset >= 16#C180# and Offset < 16#C1BC#) or else
         (Offset >= 16#C200# and Offset < 16#C300#) or else
         (Offset >= 16#C300# and Offset < 16#C318#));
      pragma Assert ((IO.Address_For (Offset, False) /= 0) = Readable);
      pragma Assert ((IO.Address_For (Offset, True) /= 0) = Writable);
      if Readable then R_Count := R_Count + 1; end if;
      if Writable then
         W_Count := W_Count + 1;
         pragma Assert (IO.Address_For (Offset, True) =
           (if Offset = 16#13816C# then 16#6120716C#
            else 16#61206000# + Unsigned_64 (Offset mod 4096)));
      end if;
   end loop;
   pragma Assert (R_Count = 6 and W_Count = 89);
   pragma Assert (IO.Address_For (Unsigned_32'Last, True) = 0);
   Mapping := Mmap (To_Address (16#61200000#), 32768, 3, 16#100022#, -1, 0);
   if Mapping /= To_Address (16#61200000#) then
      raise Program_Error with "native GuC fixture mapping unavailable";
   end if;
   Owner := True;
   IO.Write32 (16#13816C#, 1, OK); pragma Assert (OK and Shim = 1);
   pragma Assert (IO.Read32 (16#13816C#) = 1);
   IO.Write32 (16#138168#, 99, OK); pragma Assert (not OK and Shim = 1);
   IO.Write32 (16#C000#, 99, OK); pragma Assert (not OK);
   Checks := 0; Fail_Check := 1;
   IO.Write32 (16#13816C#, 2, OK); pragma Assert (not OK and Shim = 1);
   Checks := 0; Fail_Check := 2;
   IO.Write32 (16#13816C#, 3, OK); pragma Assert (not OK and Shim = 3);
   Checks := 0;
   pragma Assert (IO.Read32 (16#13816C#) = Unsigned_32'Last);
   pragma Assert (Munmap (Mapping, 32768) = 0);
   Ada.Text_IO.Put_Line ("native GuC IO PASS: exhaustive offsets,89 writes,6 reads,actual host stores,owner loss (NOT GPU)");
end Native_GuC_IO_Tests;
