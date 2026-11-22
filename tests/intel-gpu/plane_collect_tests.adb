with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Plane_Decode; use Intel_GPU_Plane_Decode;
with Intel_GPU_Plane_Collect;
procedure Plane_Collect_Tests is
   Begin_OK, Finish_OK, Held, Sentinel : Boolean := True;
   Starts, Ends, Reads, Fail_At, Change_At : Natural := 0;
   Fields : constant array (0 .. 5) of Unsigned_32 :=
     [16#84000000#, 120, 16#0437077F#, 0, 16#200000#, 16#200000#];
   procedure Begin_Access (Success : out Boolean) is
   begin
      pragma Assert (not Held);
      Starts := Starts + 1;
      Success := Begin_OK;
      Held := Success;
   end Begin_Access;
   procedure End_Access (Success : out Boolean) is
   begin
      pragma Assert (Held);
      Ends := Ends + 1;
      Held := False;
      Success := Finish_OK;
   end End_Access;
   procedure Read_Field (Index : Natural; Value : out Unsigned_32;
                         Success : out Boolean) is
   begin
      pragma Assert (Held and Index = Reads mod 6 and Reads < 12);
      Reads := Reads + 1;
      Value := Fields (Index);
      Success := Reads /= Fail_At or Sentinel;
      if Reads = Fail_At and Sentinel then Value := Unsigned_32'Last; end if;
      if Reads = Change_At then Value := Value xor 1; end if;
   end Read_Field;
   package Probe is new Intel_GPU_Plane_Collect (Begin_Access, End_Access, Read_Field);
   use Probe;
   O : Observation;
   procedure Reset is
   begin
      Held := False; Begin_OK := True; Finish_OK := True; Sentinel := False;
      Starts := 0; Ends := 0; Reads := 0; Fail_At := 0; Change_At := 0;
   end Reset;
begin
   Reset;
   Begin_OK := False;
   Inspect (8 * 1024 * 1024, O);
   pragma Assert (O.State = Access_Unavailable and Reads = 0 and Ends = 0);
   pragma Assert (not O.Decoded.Memory.Valid);
   -- Every read failure/sentinel, in either pass, with successful or failed
   -- power cleanup. Never decode partial data or read after the first fault.
   for At_Read in 1 .. 12 loop
      for All_Ones in Boolean loop
         for Cleanup in Boolean loop
            Reset; Fail_At := At_Read; Sentinel := All_Ones; Finish_OK := Cleanup;
            Inspect (8 * 1024 * 1024, O);
            pragma Assert (O.State = (if Cleanup then Read_Failed else Access_End_Failed));
            pragma Assert (Starts = 1 and Ends = 1 and Reads = At_Read and not Held);
            pragma Assert (O.Reads = At_Read - 1 and not O.Decoded.Memory.Valid);
         end loop;
      end loop;
   end loop;
   Reset; Finish_OK := False;
   Inspect (8 * 1024 * 1024, O);
   pragma Assert (O.State = Access_End_Failed and Reads = 12 and Ends = 1);
   pragma Assert (not O.Decoded.Memory.Valid);
   for Field in 7 .. 12 loop
      Reset; Change_At := Field;
      Inspect (8 * 1024 * 1024, O);
      pragma Assert (O.State = Collected and O.Decoded.State = Changing);
      pragma Assert (Reads = 12 and Ends = 1 and not O.Decoded.Memory.Valid);
   end loop;
   Reset;
   Inspect (8 * 1024 * 1024, O);
   pragma Assert (O.State = Collected and O.Decoded.State = Linear_Ready);
   pragma Assert (O.Reads = 12 and Reads = 12 and Starts = 1 and Ends = 1 and not Held);
   pragma Assert (O.Decoded.Memory.First = 16#200000# and O.Decoded.Memory.Bytes = 8_294_400);
   -- Reusing the destination after success must not expose stale valid data
   -- when the next acquisition fails before any register can be read.
   Begin_OK := False;
   Inspect (8 * 1024 * 1024, O);
   pragma Assert (O.State = Access_Unavailable and O.Reads = 0);
   pragma Assert (not O.Decoded.Memory.Valid and O.Decoded.Memory.Bytes = 0);
   pragma Assert (O.Before = (others => 0) and O.After = (others => 0));
   pragma Assert (Reads = 12 and Starts = 2 and Ends = 1);
   Ada.Text_IO.Put_Line ("Plane collector PASS: 48 read/sentinel/cleanup failures, six transitions and power lifetime");
end Plane_Collect_Tests;
