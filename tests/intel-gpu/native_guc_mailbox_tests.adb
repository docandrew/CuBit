with Ada.Text_IO; with Interfaces; with Interfaces.C;
with System; with System.Storage_Elements;
with Intel_GPU_Native_GuC_Mailbox;
procedure Native_GuC_Mailbox_Tests is
   use Interfaces; use System; use System.Storage_Elements;
   use type Interfaces.C.int;
   Owner : Boolean := False;
   Checks, Fail_Check : Natural := 0;
   function Ready return Boolean is
   begin Checks := Checks + 1; return Owner and Checks /= Fail_Check; end Ready;
   package IO is new Intel_GPU_Native_GuC_Mailbox (Ready);
   function Mmap (Address : System.Address; Length : Interfaces.C.size_t;
     Protection, Flags, FD : Interfaces.C.int; Offset : Interfaces.C.long)
     return System.Address with Import, Convention => C, External_Name => "mmap";
   function Munmap (Address : System.Address; Length : Interfaces.C.size_t)
     return Interfaces.C.int with Import, Convention => C, External_Name => "munmap";
   Mapping : System.Address;
   OK, Readable, Writable : Boolean;
   R_Count, W_Count : Natural := 0;
   Doorbell : Unsigned_32 with Import, Volatile_Full_Access,
     Address => To_Address (16#612081F0#);
begin
   pragma Assert (IO.Read_Word (0) = Unsigned_32'Last);
   IO.Write_Word (0, 1, OK); pragma Assert (not OK);
   IO.Notify (OK); pragma Assert (not OK);
   for Offset in Unsigned_32 range 0 .. 16#1FFFFF# loop
      Readable := Offset in 16#190240# | 16#190244# | 16#190248# | 16#19024C#;
      Writable := Readable or Offset = 16#1901F0#;
      pragma Assert ((IO.Address_For (Offset, False) /= 0) = Readable);
      pragma Assert ((IO.Address_For (Offset, True) /= 0) = Writable);
      if Readable then R_Count := R_Count + 1; end if;
      if Writable then
         W_Count := W_Count + 1;
         pragma Assert (IO.Address_For (Offset, True) =
           16#61208000# + Unsigned_64 (Offset mod 4096));
      end if;
   end loop;
   pragma Assert (R_Count = 4 and W_Count = 5);
   pragma Assert (IO.Address_For (Unsigned_32'Last, True) = 0);
   Mapping := Mmap (To_Address (16#61208000#), 4096, 3, 16#100022#, -1, 0);
   if Mapping /= To_Address (16#61208000#) then raise Program_Error; end if;
   Owner := True;
   for I in 0 .. 3 loop
      IO.Write_Word (I, Unsigned_32 (I + 42), OK); pragma Assert (OK);
   end loop;
   for I in 0 .. 3 loop
      pragma Assert (IO.Read_Word (I) = Unsigned_32 (I + 42));
   end loop;
   IO.Write_Word (Natural'Last, 99, OK); pragma Assert (not OK);
   pragma Assert (IO.Read_Word (Natural'Last) = Unsigned_32'Last);
   IO.Notify (OK); pragma Assert (OK and Doorbell = 1);
   Doorbell := 0; Checks := 0; Fail_Check := 1;
   IO.Notify (OK); pragma Assert (not OK and Doorbell = 0);
   Checks := 0; Fail_Check := 2;
   IO.Notify (OK); pragma Assert (not OK and Doorbell = 1);
   Checks := 0;
   IO.Write_Word (0, 123, OK); pragma Assert (not OK);
   Checks := 0; Fail_Check := 0;
   pragma Assert (IO.Read_Word (0) = 123);
   Checks := 0; Fail_Check := 2;
   pragma Assert (IO.Read_Word (0) = Unsigned_32'Last);
   pragma Assert (Munmap (Mapping, 4096) = 0);
   Ada.Text_IO.Put_Line ("native GuC mailbox PASS: exact selectors, host stores, owner loss (NOT GPU)");
end Native_GuC_Mailbox_Tests;
