with Interfaces; with Interfaces.C; with Ada.Text_IO;
with System; with System.Storage_Elements;
with Intel_GPU_Native_CT_Send; with Intel_GPU_GuC_CT_Send;
procedure Native_CT_Send_Tests is
   use Interfaces; use System; use System.Storage_Elements;
   use type Interfaces.C.int;
   Owner : Boolean := False;
   Checks, Fail_Check, Notifications : Natural := 0;
   function Ready return Boolean is
   begin Checks := Checks + 1; return Owner and Checks /= Fail_Check; end Ready;
   package IO is new Intel_GPU_Native_CT_Send (16#61084000#, Ready);
   function Mmap (Address : System.Address; Length : Interfaces.C.size_t;
     Protection, Flags, FD : Interfaces.C.int; Offset : Interfaces.C.long)
     return System.Address with Import, Convention => C, External_Name => "mmap";
   function Munmap (Address : System.Address; Length : Interfaces.C.size_t)
     return Interfaces.C.int with Import, Convention => C, External_Name => "munmap";
   type Memory is array (Natural range 0 .. 8191) of Unsigned_32
     with Volatile_Components, Component_Size => 32;
   Data : Memory with Import, Address => To_Address (16#61084000#);
   procedure Notify (Success : out Boolean) is
   begin
      pragma Assert (Data (1) = 1 and Data (2048 + 1023) = 16#12340001#
                     and Data (2048) = 16#00000508#);
      Notifications := Notifications + 1; Success := True;
   end Notify;
   package TX is new Intel_GPU_GuC_CT_Send
     (IO.Read_Descriptor, IO.Write_Word, IO.Make_Visible, IO.Write_Tail, Notify);
   use type TX.Result;
   use type TX.Phase;
   Mapping : System.Address;
   Head, Tail, Status : Unsigned_32;
   OK : Boolean;
begin
   IO.Read_Descriptor (Head, Tail, Status, OK); pragma Assert (not OK);
   IO.Write_Tail (1, OK); pragma Assert (not OK);
   IO.Write_Word (0, 1, OK); pragma Assert (not OK);
   Mapping := Mmap (To_Address (16#61084000#), 32768, 3, 16#100022#, -1, 0);
   if Mapping /= To_Address (16#61084000#) then raise Program_Error; end if;
   Owner := True;
   for I in Unsigned_32 range 0 .. 1023 loop
      IO.Write_Word (I, I xor 16#ABCD0000#, OK);
      pragma Assert (OK and Data (2048 + Natural (I)) = (I xor 16#ABCD0000#));
   end loop;
   IO.Write_Word (1024, 1, OK); pragma Assert (not OK and Data (3072) = 0);
   IO.Write_Word (Unsigned_32'Last, 1, OK); pragma Assert (not OK);
   for I in 3 .. 15 loop
      for Bit in 0 .. 31 loop
         Data (I) := Shift_Left (1, Bit);
         IO.Read_Descriptor (Head, Tail, Status, OK); pragma Assert (not OK);
         Data (I) := 0;
      end loop;
   end loop;
   Data (0) := 1023; Data (1) := 1023;
   declare
      Channel : TX.Channel; Result : TX.Result;
   begin
      TX.Initialize (Channel, True, 1024, 1023);
      TX.Send (Channel, [1 => 16#00000508#], 16#1234#, Result);
      pragma Assert (Result = TX.Queued and Notifications = 1);
      pragma Assert (Data (0) = 1023 and Data (2) = 0 and Data (1024) = 0);
      Owner := False;
      TX.Send (Channel, [1 => 16#00000508#], 16#1235#, Result);
      pragma Assert (TX.State (Channel) = TX.Broken and Notifications = 1);
      Owner := True;
   end;
   IO.Write_Tail (1024, OK); pragma Assert (not OK and Data (1) = 1);
   Checks := 0; Fail_Check := 1;
   IO.Write_Tail (3, OK); pragma Assert (not OK and Data (1) = 1);
   Checks := 0; Fail_Check := 2;
   IO.Write_Tail (3, OK); pragma Assert (not OK and Data (1) = 3);
   pragma Assert (Munmap (Mapping, 32768) = 0);
   Ada.Text_IO.Put_Line ("native CT send PASS: host memory, wrap, bounds, ownership, notification (NOT GPU coherence)");
end Native_CT_Send_Tests;
