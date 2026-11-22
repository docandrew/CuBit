with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Timestamp_Clock; use Intel_GPU_Timestamp_Clock;
with Intel_GPU_Timestamp_Observe;
procedure Timestamp_Clock_Tests is
   Frequencies : constant array (Natural range 0 .. 3) of Unsigned_32 :=
     [24_000_000, 19_200_000, 38_400_000, 25_000_000];
   Unrelated : constant array (Natural range 0 .. 3) of Unsigned_32 :=
     [0, 1, 16#8000_0000#, 16#FFFF_FFC1#];
   Raw, Expected : Unsigned_32;
   Reads, Fail_At : Natural := 0;
   Words : array (1 .. 4) of Unsigned_32 := [0, 4, 0, 4];
   procedure Read (Offset : Unsigned_32; Value : out Unsigned_32;
                   Success : out Boolean) is
   begin
      Reads := Reads + 1;
      pragma Assert (Reads <= 4);
      pragma Assert (Offset = (if Reads mod 2 = 1 then CTC_Mode_Offset
        elsif (Words (Reads - 1) and 1) = 0 then CONFIG0_Offset else Override_Offset));
      Value := Words (Reads);
      Success := Reads /= Fail_At;
   end Read;
   package Probe is new Intel_GPU_Timestamp_Observe (Read);
   use type Probe.Outcome;
   Hz : Unsigned_32;
   Status : Probe.Outcome;
begin
   for Selector in 0 .. 7 loop
      for Divider in 0 .. 3 loop
         for Noise of Unrelated loop
            Raw := Shift_Left (Unsigned_32 (Selector), 3) or
              Shift_Left (Unsigned_32 (Divider), 1) or Noise;
            pragma Assert (Encode (Decode (Raw)) = Raw);
            pragma Assert (Decode (Raw).Crystal_Selector = Bits_3 (Selector));
            pragma Assert (Decode (Raw).CTC_Shift = Bits_2 (Divider));
            Expected := (if Selector < 4 then
              Frequencies (Selector) / (2 ** (3 - Divider)) else 0);
            pragma Assert (Crystal_Timestamp_Hz (Decode (Raw)) = Expected);
         end loop;
      end loop;
   end loop;
   for D in 0 .. 1023 loop
      for N in 0 .. 15 loop
         Raw := Unsigned_32 (D) or Shift_Left (Unsigned_32 (N), 12);
         pragma Assert (Timestamp_Hz (Decode_Mode (1), Decode (0), Decode_Override (Raw)) =
           Unsigned_32 ((D + 1) * 1_000_000 + 1_000_000 / (N + 1)));
      end loop;
   end loop;
   Probe.Sample (Hz, Status);
   pragma Assert (Status = Probe.Ready and Hz = 12_000_000 and Reads = 4);
   for F in 1 .. 4 loop
      Reads := 0; Fail_At := F;
      Probe.Sample (Hz, Status);
      pragma Assert (Status = Probe.Read_Failed and Hz = 0 and Reads = F);
   end loop;
   Fail_At := 0; Reads := 0; Words := [0, 4, 16#FFFF_FFFE#, 16#FFFF_FFC5#];
   Probe.Sample (Hz, Status);
   pragma Assert (Status = Probe.Ready and Hz = 12_000_000);
   Reads := 0; Words := [0, 4, 0, 6];
   Probe.Sample (Hz, Status);
   pragma Assert (Status = Probe.Changing and Hz = 0);
   Reads := 0; Words := [0, 4, 1, 4];
   Probe.Sample (Hz, Status);
   pragma Assert (Status = Probe.Changing and Hz = 0);
   Reads := 0; Words := [0, 32, 0, 32];
   Probe.Sample (Hz, Status);
   pragma Assert (Status = Probe.Reserved_Selector and Hz = 0);
   Reads := 0; Words := [1, 0, 1, 0];
   Probe.Sample (Hz, Status);
   pragma Assert (Status = Probe.Ready and Hz = 2_000_000);
   Reads := 0; Words := [1, 0, 16#FFFF_FFFF#, 16#FFFF_0C00#];
   Probe.Sample (Hz, Status);
   pragma Assert (Status = Probe.Ready and Hz = 2_000_000);
   Reads := 0; Words := [1, 0, 1, 1];
   Probe.Sample (Hz, Status);
   pragma Assert (Status = Probe.Changing and Hz = 0);
   Reads := 0; Words := [1, 0, 1, 16#1000#];
   Probe.Sample (Hz, Status);
   pragma Assert (Status = Probe.Changing and Hz = 0);
   Put_Line ("Timestamp decode/observation PASS: 128 crystal + 16384 divider cases and read/change faults");
end Timestamp_Clock_Tests;
