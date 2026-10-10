with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Media_Engines; use Intel_GPU_Media_Engines;
with Intel_GPU_ADLN_Inventory;
with Intel_GPU_Native_Media_Fuse;
-- Hosted bit-pattern tests for the Gen12.0 media fuse decoder, including
-- the ADL-N ceiling, the SFC rule, stability, and equivalence with the
-- existing Intel_GPU_ADLN_Inventory decode. Not hardware evidence: the NUC
-- fuse value is still unknown (gpu-async-submission.md H8, Low).
procedure Media_Engines_Tests is
   V : constant Unsigned_16 := Intel_Vendor;
   D : constant Unsigned_16 := ADLN_N95_Device;
   E : Engines;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; Label : String) is
   begin
      if not Condition then
         Put_Line ("FAIL: " & Label);
         raise Program_Error;
      end if;
      Checks := Checks + 1;
   end Check;
   -- Disable-bit fuse words (media IP 12.0).
   VCS0_Off : constant Fuse_Word := 2 ** 0;
   VCS1_Off : constant Fuse_Word := 2 ** 1;
   VCS2_Off : constant Fuse_Word := 2 ** 2;
   VECS0_Off : constant Fuse_Word := 2 ** 16;
   Outside_Ceiling : constant Fuse_Word :=
     VCS1_Off or 2 ** 3 or 2 ** 4 or 2 ** 5 or 2 ** 6 or 2 ** 7 or
     2 ** 17 or 2 ** 18 or 2 ** 19;
   Unrelated_Noise : constant Fuse_Word := 16#7FF0_FF00#;
   type Noise_List is array (1 .. 2) of Fuse_Word;
   Noises : constant Noise_List := [0, Unrelated_Noise];
begin
   -- ADL-N with every ceiling engine present.
   E := Decode (V, D, 0);
   Check (E.Valid and E.Render and E.Copy, "full: valid");
   Check (E.Video = VDBOX_Set'[0 | 2 => True, others => False], "full: VCS0+VCS2");
   Check (E.Enhance = VEBOX_Set'[0 => True, others => False], "full: VECS0");
   Check (E.SFC = VDBOX_Set'[0 | 2 => True, others => False], "full: SFC on evens");
   Check (E.Video_Count = 2 and E.Enhance_Count = 1, "full: counts");
   Check (Logical (E, 0) = 0 and Logical (E, 2) = 1, "full: logical");
   Check (Has_Video (E), "full: has video");

   -- Single-VDBOX ADL-N part (VCS2 fused off).
   E := Decode (V, D, VCS2_Off);
   Check (E.Video = VDBOX_Set'[0 => True, others => False] and E.Video_Count = 1,
          "vcs2 off");
   Check (E.SFC = VDBOX_Set'[0 => True, others => False], "vcs2 off: SFC");
   -- VCS0 fused off: VCS2 remains, is logical 0 and keeps its SFC.
   E := Decode (V, D, VCS0_Off);
   Check (E.Video = VDBOX_Set'[2 => True, others => False], "vcs0 off");
   Check (Logical (E, 2) = 0 and E.SFC (2), "vcs0 off: logical/SFC");
   -- No media at all.
   E := Decode (V, D, VCS0_Off or VCS2_Off or VECS0_Off);
   Check (E.Valid and not Has_Video (E) and E.Enhance_Count = 0, "no media");
   Check (E.Render and E.Copy, "no media: render/copy remain");

   -- Fuse bits outside the platform ceiling and unrelated bits are ignored.
   E := Decode (V, D, Outside_Ceiling or Unrelated_Noise);
   Check (E = Decode (V, D, 0), "outside ceiling ignored");

   -- Rejections: unreadable register, other devices, unstable reads.
   Check (Decode (V, D, Unreadable) = (Engines'(others => <>)), "unreadable");
   Check (not Decode (V, 16#46A6#, 0).Valid, "other ADL-P id not admitted");
   Check (not Decode (16#1002#, D, 0).Valid, "other vendor");
   Check (Decode_Stable (V, D, 0, 0) = Decode (V, D, 0), "stable");
   Check (not Decode_Stable (V, D, 0, VCS2_Off).Valid, "unstable");

   -- Gen12 SFC rule on synthetic sets (not reachable on ADL-N).
   Check (Has_SFC (VDBOX_Set'[1 => True, others => False], 1), "odd without even: SFC");
   Check (not Has_SFC (VDBOX_Set'[0 | 1 => True, others => False], 1),
          "odd with even: no SFC");
   Check (not Has_SFC (VDBOX_Set'[others => False], 4), "disabled: no SFC");

   -- Every combination of all twelve field bits, with and without noise,
   -- agrees with the existing ADL-N inventory decoder.
   for Pattern in Unsigned_32 range 0 .. 2 ** 12 - 1 loop
      for Noise of Noises loop
         declare
            Fuse : constant Fuse_Word :=
              (Pattern and 16#FF#) or Shift_Left (Pattern and 16#F00#, 8) or Noise;
            Old : constant Intel_GPU_ADLN_Inventory.Inventory :=
              Intel_GPU_ADLN_Inventory.Decode (V, D, Fuse);
            use Intel_GPU_ADLN_Inventory;
         begin
            E := Decode (V, D, Fuse);
            Check (E.Valid = Old.Valid, "equivalence: valid");
            Check (E.Video (0) = Old.Engines (Video_0) and
                   E.Video (2) = Old.Engines (Video_2) and
                   E.Enhance (0) = Old.Engines (Enhance_0) and
                   E.Render = Old.Engines (Render) and
                   E.Copy = Old.Engines (Copy), "equivalence: engines");
            Check (E.Video_Count + E.Enhance_Count <= 3, "equivalence: ceiling");
         end;
      end loop;
   end loop;

   -- Native wrapper over an aligned in-memory page standing in for MMIO.
   declare
      type Page is array (0 .. 1023) of Unsigned_32
        with Alignment => 4096, Volatile_Components;
      Fake : aliased Page := [others => 0];
      Owner : Boolean := True;
      function Ready return Boolean is (Owner);
      function Base return Unsigned_64 is
        (Unsigned_64 (To_Integer (Fake'Address)));
      package Native is new Intel_GPU_Native_Media_Fuse (Ready, Base);
      Index : constant Natural := Natural (Fuse_Register mod 4096) / 4;
   begin
      Fake (Index) := VCS2_Off;
      E := Native.Sample (V, D);
      Check (E = Decode (V, D, VCS2_Off) and Native.Last_Raw = VCS2_Off,
             "native: sample");
      Owner := False;
      E := Native.Sample (V, D);
      Check (not E.Valid and Native.Last_Raw = Unreadable, "native: owner lost");
   end;
   Put_Line ("media_engines_tests: PASS (" & Checks'Image & " checks)");
end Media_Engines_Tests;
