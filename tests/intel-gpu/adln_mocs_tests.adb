with Interfaces; use Interfaces;
with Ada.Command_Line;
with Ada.Text_IO;
with Intel_GPU_ADLN_MOCS;
with Intel_GPU_L3_MOCS_Registers;
with Intel_GPU_MOCS_Control_Registers;
procedure ADLN_MOCS_Tests is
   package MOCS renames Intel_GPU_ADLN_MOCS;
   -- Independent raw-value oracle retained from the pre-record ADLN table.
   function Golden_Control (I : MOCS.Entry_Index) return Unsigned_32 is
     (case I is
        when 3 | 4 | 49 | 51 | 61 => 16#5#,
        when 6 | 7 => 16#17#, when 8 | 9 => 16#27#,
        when 10 | 11 => 16#77#, when 12 | 13 => 16#57#,
        when 14 | 15 => 16#67#, when 16 | 17 => 16#4005#,
        when 18 => 16#60037#, when 19 => 16#737#,
        when 20 => 16#337#, when 21 => 16#137#,
        when 22 => 16#3B7#, when 23 => 16#7B7#, when others => 16#37#);
begin
   -- Exercise fields unused by today's policy table too. Roundtrips alone
   -- cannot detect a consistently misplaced representation field.
   declare
      use Intel_GPU_MOCS_Control_Registers;
      R : Control_Register;
      procedure Expect (Mask : Unsigned_32) is
      begin
         pragma Assert (Encode (R) = Mask);
         R := Decode (Unsigned_32'(0));
      end Expect;
   begin
      R := Decode (Unsigned_32'(0));
      R.Cacheability := Bits_2'Last; Expect (16#00000003#);
      R.Target_Cache := Bits_2'Last; Expect (16#0000000C#);
      R.LRU_Management := Bits_2'Last; Expect (16#00000030#);
      R.Do_Not_Allocate_On_Miss := 1; Expect (16#00000040#);
      R.Reverse_Skip_Caching := 1; Expect (16#00000080#);
      R.Skip_Caching_Control := Bits_3'Last; Expect (16#00000700#);
      R.Page_Fault_Mode := Bits_3'Last; Expect (16#00003800#);
      R.Snoop_Control := 1; Expect (16#00004000#);
      R.Class_Of_Service := Bits_2'Last; Expect (16#00018000#);
      R.Self_Snoop := Bits_2'Last; Expect (16#00060000#);
      R.Reserved_High := Bits_13'Last; Expect (16#FFF80000#);
      for Bit in 0 .. 31 loop
         declare
            V : constant Unsigned_32 := Shift_Left (Unsigned_32'(1), Bit);
         begin
            pragma Assert (Encode (Decode (V)) = V);
         end;
      end loop;
   end;
   for I in MOCS.Entry_Index loop
      pragma Assert (MOCS.Control (I) = Golden_Control (I));
      pragma Assert (Intel_GPU_MOCS_Control_Registers.Matches (MOCS.Control (I), Golden_Control (I)));
      for Bit in 0 .. 31 loop
         pragma Assert (not Intel_GPU_MOCS_Control_Registers.Matches
           (MOCS.Control (I) xor Shift_Left (Unsigned_32'(1), Bit), Golden_Control (I)));
      end loop;
   end loop;
   declare
      use Intel_GPU_L3_MOCS_Registers;
      R : Pair_Register;
      V : constant Unsigned_32 := 16#00300010#;
   begin
      pragma Assert (Pack (Uncached, Write_Back) = V);
      for Bit in 0 .. 31 loop
         R := Decode (Shift_Left (Unsigned_32'(1), Bit));
         pragma Assert (Encode (R) = Shift_Left (Unsigned_32'(1), Bit));
         pragma Assert (Matches (V xor Shift_Left (Unsigned_32'(1), Bit), V) =
           (Bit = 15 or Bit = 31));
      end loop;
      pragma Assert (not Matches (Unsigned_32'Last, V));
   end;
   pragma Assert (MOCS.Uncached_Index = 3);
   pragma Assert (MOCS.Control (0) = 16#37# and MOCS.L3 (0) = 16#30#);
   pragma Assert (MOCS.Control (3) = 5 and MOCS.L3 (3) = 16#10#);
   pragma Assert (MOCS.Control (18) = 16#60037#);
   -- Intel TGL Vol6-5.23 p17: SCF=0 is coherent access; skip caching
   -- must be disabled for coherent surfaces. These are field invariants,
   -- not a claim that the complete CPU/GPU mapping is coherent.
   for I in MOCS.Entry_Index loop
      declare
         use Intel_GPU_MOCS_Control_Registers;
         R : constant Control_Register := Decode (MOCS.Control (I));
      begin
         -- Mesa26.2.3 isl.c's Gen12 integrated branch selects 2 for
         -- internal surfaces, 3 for uncached/blitter access, 48 for HDC
         -- L1+L3+LLC, and 61 for external surfaces. Cover the actual Mesa
         -- command-selected indices, not just our diagnostic batches.
         if I in 0 | 2 | 3 | 18 | 48 | 61 then
            pragma Assert (R.Snoop_Control = 0);
            pragma Assert (R.Skip_Caching_Control = 0);
            pragma Assert (R.Reverse_Skip_Caching = 0);
         elsif I = 16 or I = 17 then
            pragma Assert (R.Snoop_Control = 1);
         end if;
      end;
   end loop;
   -- Intel TGL Vol6-5.23 p16: hardware selects entry63 for L3
   -- evictions, independently of the command's surface MOCS. Preserve LLC
   -- caching while forcing L3 uncached. These field checks complement the
   -- raw table oracle; they do not prove end-to-end host coherence.
   declare
      use Intel_GPU_MOCS_Control_Registers;
      Eviction : constant Control_Register := Decode (MOCS.Control (63));
      Displayable : constant Control_Register := Decode (MOCS.Control (61));
   begin
      pragma Assert (Eviction.Cacheability = 3);
      pragma Assert (Eviction.Target_Cache = 1);
      pragma Assert (Eviction.Snoop_Control = 0);
      pragma Assert (Eviction.Skip_Caching_Control = 0);
      pragma Assert (Eviction.Reverse_Skip_Caching = 0);
      pragma Assert (MOCS.L3 (63) = Intel_GPU_L3_MOCS_Registers.Pack
        (Intel_GPU_L3_MOCS_Registers.Uncached, 0));
      -- Same page requires displayable entry61 to disallow LLC caching.
      -- Do not accidentally apply a blanket WB requirement to all entries.
      pragma Assert (Displayable.Cacheability = 1);
   end;
   pragma Assert (MOCS.Value (65) = 16#00100030#);
   pragma Assert (MOCS.Value (95) = 16#00100010#);
   for I in MOCS.Register_Index loop
      pragma Assert (MOCS.Offset (I) =
        (if I < 64 then 16#4000# + Unsigned_32 (I) * 4
         else 16#B020# + Unsigned_32 (I - 64) * 4));
      if I >= 64 then
         pragma Assert ((MOCS.Value (I) and 16#FFFF#) = MOCS.L3 ((I - 64) * 2));
         pragma Assert (Shift_Right (MOCS.Value (I), 16) = MOCS.L3 ((I - 64) * 2 + 1));
      end if;
   end loop;
   -- Optional decimal dump for independent pinned-source comparison.
   if Ada.Command_Line.Argument_Count > 0 then
      for I in MOCS.Entry_Index loop
         Ada.Text_IO.Put_Line (Unsigned_32'Image (MOCS.Control (I)) &
                               Unsigned_32'Image (MOCS.L3 (I)));
      end loop;
   end if;
end ADLN_MOCS_Tests;
