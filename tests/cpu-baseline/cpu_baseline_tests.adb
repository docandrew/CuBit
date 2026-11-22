--  CPU_Baseline over the CPUID words of CPU models: those CuBit targets
--  (N150/Gracemont, Ryzen, QEMU's Broadwell) meet x86-64-v3; older ones,
--  and an OS that left AVX state off, are refused naming what they lack.
with Ada.Text_IO;
with Interfaces; use Interfaces;
with CPU_Baseline; use CPU_Baseline;
procedure CPU_Baseline_Tests is
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      if not Condition then
         raise Program_Error with "FAIL: " & What;
      end if;
      Checks := Checks + 1;
   end Check;
   function B (Position : Natural) return Unsigned_32 is (Shift_Left (Unsigned_32'(1), Position));
   --  Leaf 1 ECX bits: SSE3 0, SSSE3 9, FMA 12, SSE4.1 19, SSE4.2 20,
   --  MOVBE 22, POPCNT 23, XSAVE 26, OSXSAVE 27, AVX 28, F16C 29.
   Haswell_Leaf1 : constant Unsigned_32 :=
     B (0) or B (9) or B (12) or B (19) or B (20) or B (22) or B (23) or B (26) or B (27) or B (28) or B (29);
   --  Leaf 7 EBX: BMI1 3, AVX2 5, BMI2 8.
   Haswell_Leaf7 : constant Unsigned_32 := B (3) or B (5) or B (8);
   LZCNT_Bit : constant Unsigned_32 := B (5);
   AVX_State : constant Unsigned_64 := 2#111#;   --  x87, SSE, AVX
   V3 : constant Words :=
     (Leaf1_ECX => Haswell_Leaf1, Leaf7_EBX => Haswell_Leaf7, Extended_ECX => LZCNT_Bit,
      XCR0 => AVX_State, Leaf7_Present => True, Extended_Present => True);
   --  Westmere: SSE4.2 and POPCNT, no XSAVE, no AVX.
   Westmere : constant Words :=
     (Leaf1_ECX => B (0) or B (9) or B (19) or B (20) or B (23), Leaf7_EBX => 0, Extended_ECX => 0,
      XCR0 => 0, Leaf7_Present => False, Extended_Present => True);
   --  Sandy Bridge: AVX with XSAVE, but no FMA, F16C, MOVBE, AVX2, BMI, LZCNT.
   Sandy_Bridge : constant Words :=
     (Leaf1_ECX => B (0) or B (9) or B (19) or B (20) or B (23) or B (26) or B (27) or B (28),
      Leaf7_EBX => 0, Extended_ECX => 0, XCR0 => AVX_State, Leaf7_Present => True, Extended_Present => True);
   W : Words;
begin
   Check (Meets (V3) and Names (Missing (V3)) = "", "x86-64-v3 (Haswell, Broadwell, Gracemont, Zen) meets it");
   Check (not Meets (Westmere), "Westmere is refused");
   Check (Names (Missing (Westmere)) = "AVX2 FMA BMI1 BMI2 MOVBE F16C LZCNT OSXSAVE AVX XCR0.AVX",
          "Westmere lacks: " & Names (Missing (Westmere)));
   Check (Names (Missing (Sandy_Bridge)) = "AVX2 FMA BMI1 BMI2 MOVBE F16C LZCNT",
          "Sandy Bridge lacks: " & Names (Missing (Sandy_Bridge)));
   --  One feature off at a time: each is named alone.
   for F in Feature loop
      W := V3;
      case F is
         when FMA => W.Leaf1_ECX := W.Leaf1_ECX and not B (12);
         when MOVBE => W.Leaf1_ECX := W.Leaf1_ECX and not B (22);
         when OSXSAVE => W.Leaf1_ECX := W.Leaf1_ECX and not B (27);
         when AVX => W.Leaf1_ECX := W.Leaf1_ECX and not B (28);
         when F16C => W.Leaf1_ECX := W.Leaf1_ECX and not B (29);
         when BMI1 => W.Leaf7_EBX := W.Leaf7_EBX and not B (3);
         when AVX2 => W.Leaf7_EBX := W.Leaf7_EBX and not B (5);
         when BMI2 => W.Leaf7_EBX := W.Leaf7_EBX and not B (8);
         when LZCNT => W.Extended_ECX := 0;
         when XCR0_AVX => W.XCR0 := 2#011#;   --  the OS left AVX state off
      end case;
      Check (not Meets (W) and then (for all G in Feature => Missing (W) (G) = (G = F)),
             "only " & F'Image & " missing is refused, naming it alone");
   end loop;
   --  Leaves the CPU does not have count as zero, whatever their words say.
   W := V3; W.Leaf7_Present := False;
   Check (Names (Missing (W)) = "AVX2 BMI1 BMI2", "no leaf 7: " & Names (Missing (W)));
   W := V3; W.Extended_Present := False;
   Check (Names (Missing (W)) = "LZCNT", "no extended leaf: " & Names (Missing (W)));
   Ada.Text_IO.Put_Line ("CPU-BASELINE: PASS" & Checks'Image & " checks");
end CPU_Baseline_Tests;
