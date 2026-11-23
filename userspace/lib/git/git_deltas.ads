------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  git's delta format (SELF-003 step 2): rebuilding an object from its
--  base and an untrusted delta, streaming in and out.
--
--  @description
--  git's documentation, gitformat-pack ("Deltified representation"): the
--  delta starts with the base's size and the result's size, each a
--  little-endian base-128 number; then instructions. An instruction byte
--  with the top bit set copies from the base: its low four bits say which
--  of four offset bytes follow, the next three which of three size bytes
--  (least significant first; a size of 0 means 65536). Without the top
--  bit, the byte (1 .. 127) is the count of literal bytes that follow; 0
--  is reserved.
--
--  Proved at level 2 (tests/git-codecs): no run-time error; a copy never
--  reads outside the base; the result never passes its declared size nor
--  the caller's ceiling; output goes only to Output (Out_Next'Old ..
--  Out_Next - 1); Finished only with exactly the declared size written;
--  a refusal is final.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces;
with Inflate_Streams; use Inflate_Streams;

package Git_Deltas with SPARK_Mode, Pure is
   use type Interfaces.Unsigned_8;
   use type Interfaces.Unsigned_32;

   type Refusal is
     (No_Refusal,
      Bad_Size,              --  a size too large to represent
      Base_Size_Mismatch,    --  the declared base size is not the base's
      Result_Too_Large,      --  the declared result size passes the caller's ceiling
      Copy_Outside_Base,     --  a copy reaches past the base's end
      Result_Overrun,        --  instructions make more than the declared size
      Reserved_Instruction,  --  instruction byte 0
      Truncated,             --  the delta ended before the result was whole
      Extra_Data);           --  bytes after the result was whole

   function Name (Why : Refusal) return String is
     (case Why is
         when No_Refusal => "No_Refusal",
         when Bad_Size => "Bad_Size",
         when Base_Size_Mismatch => "Base_Size_Mismatch",
         when Result_Too_Large => "Result_Too_Large",
         when Copy_Outside_Base => "Copy_Outside_Base",
         when Result_Overrun => "Result_Overrun",
         when Reserved_Instruction => "Reserved_Instruction",
         when Truncated => "Truncated",
         when Extra_Data => "Extra_Data");

   type Applier is private;

   function Valid (A : Applier) return Boolean;
   function Failed (A : Applier) return Boolean;
   function Failure (A : Applier) return Refusal;
   function Base_Length (A : Applier) return Byte_Count;
   function Maximum_Result (A : Applier) return Byte_Count;
   --  The declared result size, once read, and the bytes written so far.
   function Sizes_Read (A : Applier) return Boolean;
   function Result_Size (A : Applier) return Byte_Count;
   function Produced (A : Applier) return Byte_Count;
   function Complete (A : Applier) return Boolean;

   procedure Start (A : out Applier; Base_Bytes : Byte_Count; Ceiling : Byte_Count)
   with Post => Valid (A) and then not Failed (A) and then Base_Length (A) = Base_Bytes
                  and then Maximum_Result (A) = Ceiling and then Produced (A) = 0 and then not Sizes_Read (A);

   --  Apply delta bytes Input (In_Next ..) to Base, writing Output
   --  (Out_Next ..). Stops when the input is used up (Need_Input) or the
   --  output is full (Output_Full), or on a refusal.
   procedure Apply
     (A : in out Applier;
      Base : Byte_Array;
      Input : Byte_Array;
      In_Next : in out Positive;
      Output : in out Byte_Array;
      Out_Next : in out Positive;
      Result : out Progress)
   with
     Pre =>
       Valid (A) and then Byte_Count (Base'Length) = Base_Length (A)
       and then Base'Last < Positive'Last and then Input'Last < Positive'Last and then Output'Last < Positive'Last
       and then In_Next in Input'First .. Input'Last + 1
       and then Out_Next in Output'First .. Output'Last + 1,
     Post =>
       Valid (A)
       and then Base_Length (A) = Base_Length (A'Old) and then Maximum_Result (A) = Maximum_Result (A'Old)
       and then In_Next in In_Next'Old .. Input'Last + 1
       and then Out_Next in Out_Next'Old .. Output'Last + 1
       and then Produced (A) = Produced (A'Old) + Byte_Count (Out_Next - Out_Next'Old)
       and then (for all I in Output'Range =>
                   (if I < Out_Next'Old or else I >= Out_Next then Output (I) = Output'Old (I)))
       and then (if Sizes_Read (A) then Produced (A) <= Result_Size (A)
                   and then Result_Size (A) <= Maximum_Result (A))
       and then Result in Need_Input | Output_Full | Refused
       and then (Result = Refused) = Failed (A)
       and then (if Failed (A'Old) then Result = Refused)
       and then (if Result = Refused then Failure (A) /= No_Refusal)
       and then (if Result = Need_Input then In_Next = Input'Last + 1)
       and then (if Result = Output_Full then Out_Next = Output'Last + 1);

   --  The delta ended: Finished only if the result is whole.
   procedure Finish (A : in out Applier; Result : out Progress)
   with Pre => Valid (A),
        Post => Valid (A) and then Result in Finished | Refused and then (Result = Refused) = Failed (A)
          and then (if Result = Finished then Sizes_Read (A) and then Produced (A) = Result_Size (A));
private
   type Phase_Kind is
     (Base_Size, Target_Size, Instruction, Copy_Arguments, Copying, Inserting, Failed_Phase);

   --  Base-128 sizes: the last group starts at bit 49 at most, and a
   --  size must fit Byte_Count.
   MAXIMUM_SHIFT : constant := 49;
   COPY_ARGUMENT_BYTES : constant := 7;

   type Applier is record
      Phase : Phase_Kind := Base_Size;
      Why : Refusal := No_Refusal;
      Base_Bytes : Byte_Count := 0;
      Ceiling : Byte_Count := 0;
      Value : Interfaces.Unsigned_64 := 0;
      Shift : Natural range 0 .. MAXIMUM_SHIFT + 7 := 0;
      Sizes : Boolean := False;
      Target : Byte_Count := 0;
      Done : Byte_Count := 0;
      Op : Byte := 0;
      Argument : Natural range 0 .. COPY_ARGUMENT_BYTES := 0;
      Copy_Offset : Interfaces.Unsigned_32 := 0;
      Copy_Size : Interfaces.Unsigned_32 := 0;
      Source : Byte_Count := 0;
      Remaining : Byte_Count := 0;
   end record;

   function Valid (A : Applier) return Boolean is
     ((A.Phase = Failed_Phase) = (A.Why /= No_Refusal)
      and then (if A.Sizes then A.Done <= A.Target and then A.Target <= A.Ceiling else A.Done = 0)
      and then (if A.Phase in Instruction .. Inserting then A.Sizes)
      and then (if A.Phase in Base_Size | Target_Size then not A.Sizes)
      and then (if A.Phase = Copying then
                  A.Remaining >= 1 and then A.Source <= A.Base_Bytes
                  and then A.Remaining <= A.Base_Bytes - A.Source
                  and then A.Remaining <= A.Target - A.Done)
      and then (if A.Phase = Inserting then A.Remaining >= 1 and then A.Remaining <= A.Target - A.Done));

   function Failed (A : Applier) return Boolean is (A.Phase = Failed_Phase);
   function Failure (A : Applier) return Refusal is (A.Why);
   function Base_Length (A : Applier) return Byte_Count is (A.Base_Bytes);
   function Maximum_Result (A : Applier) return Byte_Count is (A.Ceiling);
   function Sizes_Read (A : Applier) return Boolean is (A.Sizes);
   function Result_Size (A : Applier) return Byte_Count is (A.Target);
   function Produced (A : Applier) return Byte_Count is (A.Done);
   function Complete (A : Applier) return Boolean is
     (A.Phase = Instruction and then A.Done = A.Target);
end Git_Deltas;
