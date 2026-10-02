with Interfaces; with Interfaces.C; with Ada.Text_IO;
with System; with System.Storage_Elements;
with Intel_GPU_Native_CT_Send; with Intel_GPU_GuC_CT_Send;
with Intel_GPU_Native_CT_Receive; with Intel_GPU_GuC_CT_Receive;
with Intel_GPU_GuC_CT_Roundtrip;
procedure Native_CT_Roundtrip_Tests is
   use Interfaces; use System; use System.Storage_Elements;
   use type Interfaces.C.int;
   function Ready return Boolean is (True);
   function Mmap (Address : System.Address; Length : Interfaces.C.size_t;
     Protection, Flags, FD : Interfaces.C.int; Offset : Interfaces.C.long)
     return System.Address with Import, Convention => C, External_Name => "mmap";
   function Munmap (Address : System.Address; Length : Interfaces.C.size_t)
     return Interfaces.C.int with Import, Convention => C, External_Name => "munmap";
   type Memory is array (Natural range 0 .. 8191) of Unsigned_32
     with Volatile_Components, Component_Size => 32;
   Data : Memory with Import, Address => To_Address (16#61084000#);
   Mapping : System.Address;
   package Send_IO is new Intel_GPU_Native_CT_Send (16#61084000#, Ready);
   package Receive_IO is new Intel_GPU_Native_CT_Receive (16#61084000#, Ready);
   package RX is new Intel_GPU_GuC_CT_Receive
     (Receive_IO.Read_Descriptor, Receive_IO.Read_Word, Receive_IO.Finish_Reads,
      Receive_IO.Write_Head, Receive_IO.Make_Visible);
   Scenario, Notifications, Events : Natural := 0;
   procedure Notify (Success : out Boolean) is
      Cursor : Natural := Natural (Data (1025));
      procedure Emit (Value : Unsigned_32) is
      begin
         Data (3072 + Cursor) := Value;
         Cursor := (Cursor + 1) mod 4096;
      end Emit;
   begin
      -- Independently check literal wire words at the publication boundary.
      pragma Assert (Data (1) = 1 and Data (2048 + 1022) = 16#002A0002#
        and Data (2048 + 1023) = 16#40# and Data (2048) = 0);
      Notifications := Notifications + 1;
      Data (0) := Data (1); -- simulated firmware consumes H2G request
      Emit (16#00000002#); Emit (16#90008003#); Emit (16#DEADBEEF#);
      Emit ((if Scenario = 1 then 16#002B0001# else 16#002A0001#));
      Emit ((if Scenario = 2 then 16#E0000001# else 16#F0000000#));
      Data (1025) := Unsigned_32 (Cursor); -- publish all G2H words last
      Success := True;
   end Notify;
   package TX is new Intel_GPU_GuC_CT_Send
     (Send_IO.Read_Descriptor, Send_IO.Write_Word, Send_IO.Make_Visible,
      Send_IO.Write_Tail, Notify);
   use type TX.Result;
begin
   Mapping := Mmap (To_Address (16#61084000#), 32768, 3, 16#100022#, -1, 0);
   if Mapping /= To_Address (16#61084000#) then raise Program_Error; end if;
   for Test_Case in 0 .. 2 loop
      Scenario := Test_Case; Notifications := 0; Events := 0;
      for I in Data'Range loop Data (I) := 0; end loop;
      Data (0) := 1022; Data (1) := 1022;
      Data (1024) := 4094; Data (1025) := 4094;
      declare
         Sender : TX.Channel;
         Receiver : RX.Channel;
         Saved_Event : RX.Message;
         procedure Queue (Fence : Unsigned_16; Success : out Boolean) is
            Status : TX.Result;
         begin
            TX.Send (Sender, [16#40#, 0], Fence, Status);
            Success := Status = TX.Queued;
         end Queue;
         procedure Poll (Item : out RX.Message; Status : out RX.Result) is
         begin RX.Poll (Receiver, Item, Status); end Poll;
         procedure Retain (Item : RX.Message; Success : out Boolean) is
         begin Saved_Event := Item; Events := Events + 1; Success := True; end Retain;
         function Now return Unsigned_64 is (10);
         procedure Pause is null;
         package Exchange is new Intel_GPU_GuC_CT_Roundtrip
           (RX, Ready, Queue, Poll, Retain, Now, Pause);
         use type Exchange.Result;
         Attempt : Exchange.Attempt;
         Result : Exchange.Result;
         Reply : Unsigned_32;
      begin
         TX.Initialize (Sender, True, 1024, 1022);
         RX.Initialize (Receiver, True, 4096, 4094);
         Exchange.Execute (Attempt, 42, 10, Reply, Result);
         pragma Assert (Result = (case Scenario is
           when 0 => Exchange.Complete, when 1 => Exchange.Invalid_Reply,
           when others => Exchange.Firmware_Failed));
         pragma Assert (Notifications = 1 and Events = 1);
         pragma Assert (Saved_Event.Length = 2 and Saved_Event.Fence = 0 and
           Saved_Event.Payload (1) = 16#90008003# and
           Saved_Event.Payload (2) = 16#DEADBEEF#);
         pragma Assert (Data (0) = 1 and Data (1) = 1 and
           Data (1024) = 3 and Data (1025) = 3);
         -- Reusing released ring storage must not alter the retained copy.
         Data (3072) := 0;
         pragma Assert (Saved_Event.Payload (2) = 16#DEADBEEF#);
         Exchange.Execute (Attempt, 42, 10, Reply, Result);
         pragma Assert (Result = Exchange.Rejected and Notifications = 1);
      end;
   end loop;
   pragma Assert (Munmap (Mapping, 32768) = 0);
   Ada.Text_IO.Put_Line ("native CT integrated PASS: both rings wrap, retained events, matching/wrong fence, failure, no replay (NOT hardware)");
end Native_CT_Roundtrip_Tests;
