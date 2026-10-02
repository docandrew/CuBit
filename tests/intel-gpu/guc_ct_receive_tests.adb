with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_GuC_CT_Receive;
procedure GuC_CT_Receive_Tests is
   Memory : array (Unsigned_32 range 0 .. 7) of Unsigned_32;
   Head, Tail, Flags : Unsigned_32;
   Calls, Failure, Reads, Releases, Barriers : Natural;
   procedure Step (OK : out Boolean) is
   begin Calls := Calls + 1; OK := Calls /= Failure; end Step;
   procedure Descriptor (H, T, S : out Unsigned_32; OK : out Boolean) is
   begin H := Head; T := Tail; S := Flags; Step (OK); end Descriptor;
   procedure Read_Word (Index : Unsigned_32; Value : out Unsigned_32; OK : out Boolean) is
   begin
      pragma Assert (Releases = 0 and Barriers = 0);
      pragma Assert (Index = (6 + Unsigned_32 (Reads)) mod 8);
      Value := Memory (Index); Reads := Reads + 1; Step (OK);
   end Read_Word;
   procedure Finish_Reads (OK : out Boolean) is
   begin pragma Assert (Reads = 4 and Releases = 0); Barriers := 1; Step (OK); end Finish_Reads;
   procedure Write_Head (Value : Unsigned_32; OK : out Boolean) is
   begin
      pragma Assert (Barriers = 1 and Value = 2 and Reads = 4);
      Head := Value; Releases := Releases + 1;
      -- Firmware can overwrite released space immediately. Output must be
      -- the previously copied message, not a reference to this memory.
      Memory := [others => 16#DEADBEEF#]; Step (OK);
   end Write_Head;
   procedure Make_Visible (OK : out Boolean) is
   begin pragma Assert (Releases = 1); Barriers := 2; Step (OK); end Make_Visible;
   package Transport is new Intel_GPU_GuC_CT_Receive
     (Descriptor, Read_Word, Finish_Reads, Write_Head, Make_Visible);
   use Transport;
   Output : Message;
   Status : Result;
   procedure Reset is
   begin
      Head := 6; Tail := 2; Flags := 0;
      Calls := 0; Failure := 0; Reads := 0; Releases := 0; Barriers := 0;
      Memory := [6 => 16#12340003#, 7 => 11, 0 => 22, 1 => 33, others => 0];
   end Reset;
begin
   for Fail in 0 .. 8 loop
      declare Object : Channel; Saved : Natural; begin
         Reset; Failure := Fail;
         Initialize (Object, True, 8, 6);
         Poll (Object, Output, Status);
         if Fail = 0 then
            pragma Assert (Status = Received and State (Object) = Active and Calls = 8);
            pragma Assert (Output.Length = 3 and Output.Fence = 16#1234#);
            pragma Assert (Output.Payload (1) = 11 and Output.Payload (2) = 22
                           and Output.Payload (3) = 33);
            pragma Assert (Output.Payload (255) = 0);
            Poll (Object, Output, Status);
            pragma Assert (Status = Empty and Calls = 9 and Reads = 4);
            pragma Assert (Output = Message'(others => <>));
         else
            pragma Assert (State (Object) = Broken and Calls = Fail);
            pragma Assert (Status = (if Fail <= 5 then Corrupt else Quarantined));
            pragma Assert (Output = Message'(others => <>));
            Saved := Calls;
            Initialize (Object, True, 8, Head);
            Poll (Object, Output, Status);
            pragma Assert (Status = Rejected and Calls = Saved);
         end if;
      end;
   end loop;
   for Fault in 1 .. 8 loop
      declare Object : Channel; begin
         Reset;
         case Fault is
            when 1 => Head := 8;
            when 2 => Tail := 8;
            when 3 => Head := 5;
            when 4 => Flags := 1;
            when 5 => Memory (6) := 0;
            when 6 => Memory (6) := 16#103#;
            when 7 => Memory (6) := 16#1003#;
            when 8 => Tail := 0; -- Header promises more than published tail.
         end case;
         Initialize (Object, True, 8, 6);
         Poll (Object, Output, Status);
         pragma Assert (Status = Corrupt and State (Object) = Broken);
         pragma Assert (Releases = 0 and Barriers = 0 and Reads <= 1);
         pragma Assert (Output = Message'(others => <>));
      end;
   end loop;
   declare Object : Channel; begin
      Reset; Tail := Head;
      Poll (Object, Output, Status);
      pragma Assert (Status = Rejected and Calls = 0);
      Initialize (Object, True, 8, 6);
      Poll (Object, Output, Status);
      pragma Assert (Status = Empty and Calls = 1 and Reads = 0 and State (Object) = Active);
      Tail := 2;
      Poll (Object, Output, Status);
      pragma Assert (Status = Received and Calls = 9);
   end;
   Ada.Text_IO.Put_Line ("GuC CT receive PASS: copy before release, wrapping, failures, empty retry and corruption");
end GuC_CT_Receive_Tests;
