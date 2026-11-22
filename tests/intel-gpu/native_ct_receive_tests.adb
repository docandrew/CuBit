with Interfaces; with Interfaces.C; with Ada.Text_IO;
with System; with System.Storage_Elements;
with Intel_GPU_Native_CT_Receive; with Intel_GPU_GuC_CT_Receive;
procedure Native_CT_Receive_Tests is
   use Interfaces; use System; use System.Storage_Elements;
   use type Interfaces.C.int;
   Owner : Boolean := False;
   Checks, Fail_Check : Natural := 0;
   function Ready return Boolean is
   begin Checks := Checks + 1; return Owner and Checks /= Fail_Check; end Ready;
   package IO is new Intel_GPU_Native_CT_Receive (16#61084000#, Ready);
   package RX is new Intel_GPU_GuC_CT_Receive
     (IO.Read_Descriptor, IO.Read_Word, IO.Finish_Reads, IO.Write_Head, IO.Make_Visible);
   use type RX.Result;
   function Mmap (Address : System.Address; Length : Interfaces.C.size_t;
     Protection, Flags, FD : Interfaces.C.int; Offset : Interfaces.C.long)
     return System.Address with Import, Convention => C, External_Name => "mmap";
   function Munmap (Address : System.Address; Length : Interfaces.C.size_t)
     return Interfaces.C.int with Import, Convention => C, External_Name => "munmap";
   type Memory is array (Natural range 0 .. 8191) of Unsigned_32
     with Volatile_Components, Component_Size => 32;
   Data : Memory with Import, Address => To_Address (16#61084000#);
   Mapping : System.Address;
   Head, Tail, Status, Value : Unsigned_32;
   OK : Boolean;
begin
   IO.Read_Descriptor (Head, Tail, Status, OK); pragma Assert (not OK);
   IO.Write_Head (1, OK); pragma Assert (not OK);
   Mapping := Mmap (To_Address (16#61084000#), 32768, 3, 16#100022#, -1, 0);
   if Mapping /= To_Address (16#61084000#) then raise Program_Error; end if;
   Owner := True;
   for I in Unsigned_32 range 0 .. 4095 loop
      Data (3072 + Natural (I)) := I xor 16#ABCD0000#;
      IO.Read_Word (I, Value, OK); pragma Assert (OK and Value = (I xor 16#ABCD0000#));
   end loop;
   IO.Read_Word (4096, Value, OK); pragma Assert (not OK);
   IO.Read_Word (Unsigned_32'Last, Value, OK); pragma Assert (not OK);
   for I in 3 .. 15 loop
      for Bit in 0 .. 31 loop
         Data (1024 + I) := Shift_Left (1, Bit);
         IO.Read_Descriptor (Head, Tail, Status, OK); pragma Assert (not OK);
         Data (1024 + I) := 0;
      end loop;
   end loop;
   Data (1025) := 2; Data (3072) := 16#12340001#; Data (3073) := 16#F0000000#;
   declare
      Channel : RX.Channel; Message : RX.Message; Result : RX.Result;
   begin
      RX.Initialize (Channel, True, 4096, 0);
      RX.Poll (Channel, Message, Result);
      pragma Assert (Result = RX.Received and Message.Length = 1 and
        Message.Fence = 16#1234# and Message.Payload (1) = 16#F0000000#);
      pragma Assert (Data (1024) = 2 and Data (1025) = 2 and Data (1026) = 0);
      RX.Poll (Channel, Message, Result); pragma Assert (Result = RX.Empty);
   end;
   IO.Write_Head (4096, OK); pragma Assert (not OK and Data (1024) = 2);
   Checks := 0; Fail_Check := 1;
   IO.Write_Head (3, OK); pragma Assert (not OK and Data (1024) = 2);
   Checks := 0; Fail_Check := 2;
   IO.Write_Head (3, OK); pragma Assert (not OK and Data (1024) = 3);
   Checks := 0;
   IO.Read_Word (0, Value, OK); pragma Assert (not OK and Value = 0);
   Checks := 0; Fail_Check := 0;
   Data (1024) := 4095; Data (1025) := 1;
   Data (3072 + 4095) := 16#FFFF0001#; Data (3072) := 16#90000001#;
   declare
      Channel : RX.Channel; Message : RX.Message; Result : RX.Result;
   begin
      RX.Initialize (Channel, True, 4096, 4095);
      RX.Poll (Channel, Message, Result);
      pragma Assert (Result = RX.Received and Message.Fence = 65535 and
        Message.Length = 1 and Message.Payload (1) = 16#90000001#);
      pragma Assert (Data (1024) = 1 and Data (1025) = 1);
   end;
   pragma Assert (Munmap (Mapping, 32768) = 0);
   Ada.Text_IO.Put_Line ("native CT receive PASS: host memory, framing, reserved bits, bounds, ownership (NOT GPU coherence)");
end Native_CT_Receive_Tests;
