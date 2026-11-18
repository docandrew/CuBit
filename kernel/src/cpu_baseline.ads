-------------------------------------------------------------------------------
--  CuBit OS
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The x86-64-v3 baseline CuBit's userspace is built for (the font engine's
--  target, x86_64-unknown-cubit): what CPUID and XCR0 must report before
--  any program starts, so a CPU without it is refused at boot with one clear
--  line instead of faulting on an instruction mid-run.
--
--  The kernel reads the words (CPUID, XGETBV); this package only judges
--  them. Each feature is one named bit of one named word.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package CPU_Baseline with SPARK_Mode, Pure is

   type Feature is (AVX2, FMA, BMI1, BMI2, MOVBE, F16C, LZCNT, OSXSAVE, AVX, XCR0_AVX);

   --  The words read: CPUID leaf 1's ECX, leaf 7 subleaf 0's EBX, leaf
   --  8000_0001h's ECX, and XCR0 (zero when OSXSAVE is off). Leaf7_Present
   --  and Extended_Present: the CPU's highest leaves include them (a word
   --  of an absent leaf counts as all zero).
   type Words is record
      Leaf1_ECX     : Unsigned_32 := 0;
      Leaf7_EBX     : Unsigned_32 := 0;
      Extended_ECX  : Unsigned_32 := 0;
      XCR0          : Unsigned_64 := 0;
      Leaf7_Present, Extended_Present : Boolean := False;
   end record;

   function Has (W : Words; F : Feature) return Boolean;

   type Feature_Set is array (Feature) of Boolean;
   --  The features the CPU lacks (all False: it meets the baseline).
   function Missing (W : Words) return Feature_Set
     with Post => (for all F in Feature => Missing'Result (F) = not Has (W, F));
   function Meets (W : Words) return Boolean is
     (for all F in Feature => Has (W, F));

   --  The missing features' names, space-separated, in Text (1 .. Length)
   --  (Length 0: none). A fixed buffer: the kernel reports before its
   --  secondary stack exists.
   Names_Capacity : constant := 9 * (Feature'Pos (Feature'Last) + 1);
   subtype Names_Text is String (1 .. Names_Capacity);
   procedure Describe (Set : Feature_Set; Text : out Names_Text; Length : out Natural)
     with Post => Length <= Names_Capacity;
   function Names (Set : Feature_Set) return String;

private
   --  CPUID leaf 1 ECX.
   Leaf1_FMA     : constant := 12;
   Leaf1_MOVBE   : constant := 22;
   Leaf1_OSXSAVE : constant := 27;
   Leaf1_AVX     : constant := 28;
   Leaf1_F16C    : constant := 29;
   --  CPUID leaf 7.0 EBX.
   Leaf7_BMI1 : constant := 3;
   Leaf7_AVX2 : constant := 5;
   Leaf7_BMI2 : constant := 8;
   --  CPUID leaf 8000_0001h ECX: LZCNT (ABM).
   Extended_LZCNT : constant := 5;
   --  XCR0: SSE (bit 1) and AVX (bit 2) state enabled.
   XCR0_SSE_AVX : constant Unsigned_64 := 2#110#;

   function Bit (Word : Unsigned_32; Position : Natural) return Boolean is
     ((Shift_Right (Word, Position) and 1) = 1)
     with Pre => Position < 32;
end CPU_Baseline;
