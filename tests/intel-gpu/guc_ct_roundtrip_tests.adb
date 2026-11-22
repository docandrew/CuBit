with Interfaces; use Interfaces;
with Ada.Text_IO;
with Intel_GPU_GuC_CT_Receive;
with Intel_GPU_GuC_CT_Roundtrip;
procedure GuC_CT_Roundtrip_Tests is
   procedure Descriptor (H, T, S : out Unsigned_32; OK : out Boolean) is
   begin H := 0; T := 0; S := 0; OK := False; end Descriptor;
   procedure Read (I : Unsigned_32; V : out Unsigned_32; OK : out Boolean) is
   begin V := I; OK := False; end Read;
   procedure Barrier (OK : out Boolean) is
   begin OK := False; end Barrier;
   procedure Write (V : Unsigned_32; OK : out Boolean) is
   begin OK := V = 0; end Write;
   package RX is new Intel_GPU_GuC_CT_Receive (Descriptor, Read, Barrier, Write, Barrier);
   Scenario, Calls, Sends, Events : Natural := 0;
   Clock : Unsigned_64 := 10;
   Owner : Boolean := True;
   function Ready return Boolean is (Owner);
   procedure Queue (Fence : Unsigned_16; Success : out Boolean) is
   begin
      pragma Assert (Fence = 42); Sends := Sends + 1;
      Success := Scenario /= 7;
   end Queue;
   procedure Poll (Item : out RX.Message; Status : out RX.Result) is
   begin
      Calls := Calls + 1;
      Item := (Length => 1, Fence => 42, Payload => [1 => 16#F0000000#, others => 0]);
      Status := RX.Received;
      case Scenario is
         when 1 => Item.Fence := 43;
         when 2 => Item.Payload (1) := 16#70000000#;
         when 3 => Item.Length := 0;
         when 4 => Item.Payload (1) := 16#E0000001#;
         when 5 => Item.Payload (1) := 16#D0000000#;
         when 6 => Status := RX.Empty;
         when 8 => Owner := False;
         when 9 => Clock := 0;
         when 10 => Clock := 1_000_010;
         when 11 | 12 =>
            if Calls <= (if Scenario = 11 then 8 else 9) then
               Item.Payload (1) := 16#90008003#; Item.Fence := 0;
            end if;
         when 13 => if Calls = 1 then Item.Payload (1) := 16#B0000001#; end if;
         when 14 => Item.Length := 2;
         when 15 => Status := RX.Corrupt;
         when 16 => Item.Payload (1) := 16#F0000001#;
         when others => null;
      end case;
   end Poll;
   procedure Retain (Item : RX.Message; OK : out Boolean) is
   begin
      pragma Assert (Item.Payload (1) = 16#90008003#);
      Events := Events + 1; OK := True;
   end Retain;
   function Now return Unsigned_64 is (Clock);
   procedure Pause is null;
   package Test is new Intel_GPU_GuC_CT_Roundtrip
     (RX, Ready, Queue, Poll, Retain, Now, Pause);
   use type Test.Result;
   Expected : constant array (Natural range 0 .. 16) of Test.Result :=
     [Test.Complete, Test.Invalid_Reply, Test.Invalid_Reply, Test.Invalid_Reply,
      Test.Firmware_Failed, Test.Retry_Requested, Test.Timed_Out, Test.Queue_Failed,
      Test.Ownership_Lost, Test.Invalid_Clock, Test.Timed_Out, Test.Complete,
      Test.Event_Overflow, Test.Complete, Test.Invalid_Reply, Test.Receive_Failed,
      Test.Invalid_Reply];
begin
   for I in Expected'Range loop
      Scenario := I; Calls := 0; Sends := 0; Events := 0; Clock := 10; Owner := True;
      declare
         Object : Test.Attempt; Result : Test.Result; Reply : Unsigned_32;
      begin
         Test.Execute (Object, 42, 20, Reply, Result);
         pragma Assert (Result = Expected (I) and Sends = 1 and Calls <= 20);
         pragma Assert (Events <= 8);
         Owner := True;
         Test.Execute (Object, 42, 20, Reply, Result);
         pragma Assert (Result = Test.Rejected and Sends = 1);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("CT roundtrip PASS: correlation, deadlines, bounded events, failures and no replay (simulated firmware)");
end GuC_CT_Roundtrip_Tests;
