with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_GT_Frequency; use Intel_GPU_GT_Frequency;
-- Field placement and units of the GT frequency decoder (i915's encodings):
-- capability fields in 50 MHz, request and CAGF ratios in 50/3 MHz.
procedure GT_Frequency_Tests is
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then raise Program_Error with What; end if;
   end Check;
   -- RP0 1450 MHz (29), RP1 700 MHz (14), RPn 100 MHz (2).
   Cap : constant Unsigned_32 := 16#0002_0E1D#;
begin
   Check (RP0_MHz (Cap) = 1_450 and RP1_MHz (Cap) = 700 and RPn_MHz (Cap) = 100, "capabilities");
   -- 1450 MHz is ratio 87 (87 * 50 / 3 = 1450) in bits 31:23.
   Check (Requested_MHz (Shift_Left (87, Request_Shift)) = 1_450, "request");
   Check (Requested_MHz (Shift_Left (87, Request_Shift) or 16#7F_FFFF#) = 1_450, "request ignores low bits");
   -- CAGF bits 19:11: ratio 6 is 100 MHz; ratio 20 is 333 MHz (rounded).
   Check (Actual_MHz (Shift_Left (6, CAGF_Shift)) = 100, "cagf");
   Check (Actual_MHz (Shift_Left (20, CAGF_Shift) or 16#7FF#) = 333, "cagf rounds, ignores low bits");
   Check (Actual_MHz (Shift_Left (16#1FF#, CAGF_Shift)) = 8_517, "cagf saturates within the field");
   Check (Host_Turbo (16#80#) and not Host_Turbo (16#7F#), "turbo bit");
   --  GT0_PERF_LIMIT_REASONS decoding (i915_reg.h bits).
   Check (not Any_Limit (0, False) and not Any_Limit (0, True), "no limits");
   Check (Limited_By (16#0002#, Thermal, False) and not Limited_By (16#0002#, Thermal, True),
          "thermal now");
   Check (Limited_By (16#0C00#, Power_Limit_1, False) and Limited_By (16#0C00#, Power_Limit_2, False),
          "pl1 and pl2");
   Check (Limited_By (16#0400_0000#, Power_Limit_1, True) and not Any_Limit (16#0400_0000#, False),
          "pl1 latched only");
   Check (not Any_Limit (16#021C#, False), "bits outside the mask are no reason");
   declare
      Total : Interfaces.Unsigned_32 := 0;
   begin
      for R in Limit_Reason loop Total := Total or Reason_Bit (R); end loop;
      Check (Total = Reasons_Mask, "mask is exactly the reasons");
   end;
   Ada.Text_IO.Put_Line ("GT frequency decoder PASS:" & Checks'Image & " checks");
end GT_Frequency_Tests;
