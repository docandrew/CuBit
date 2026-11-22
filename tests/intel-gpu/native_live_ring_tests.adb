with Ada.Text_IO;
with Interfaces; use Interfaces;
with Interfaces.C;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_ADLN_Context_Init;
with Intel_GPU_ADLN_Barrier;
with Intel_GPU_Native_Live_Ring;
procedure Native_Live_Ring_Tests is
   Owner, Coherent : Boolean := False;
   Marker : Unsigned_64 := 1;
   function Owned return Boolean is (Owner);
   function Coherent_Memory return Boolean is (Coherent);
   procedure Read_Marker (Value : out Unsigned_64; OK : out Boolean) is
   begin Value := Marker; OK := Owner; end;
   Selected_Base : Unsigned_64 := 16#72000000#;
   function Mapping_Base return Unsigned_64 is (Selected_Base);
   function Mapping_Bytes return Unsigned_64 is (81920);
   function Invalid_Base return Unsigned_64 is (Unsigned_64'Last - 4095);
   package Native is new Intel_GPU_Native_Live_Ring
     (Mapping_Base, Mapping_Bytes, Owned, Coherent_Memory, Read_Marker);
   package Mixed is new Intel_GPU_Native_Live_Ring
     (Mapping_Base, Mapping_Bytes, Owned, Coherent_Memory, Read_Marker);
   -- Separate instances for the rejection tests: failed access is sticky.
   package Denied is new Intel_GPU_Native_Live_Ring
     (Mapping_Base, Mapping_Bytes, Owned, Coherent_Memory, Read_Marker);
   package Noncoherent is new Intel_GPU_Native_Live_Ring
     (Mapping_Base, Mapping_Bytes, Owned, Coherent_Memory, Read_Marker);
   package Overflowed is new Intel_GPU_Native_Live_Ring
     (Invalid_Base, Mapping_Bytes, Owned, Coherent_Memory, Read_Marker);
   Native_Channel : Native.Channel;
   Mixed_Channel : Mixed.Channel;
   Denied_Channel : Denied.Channel;
   Noncoherent_Channel : Noncoherent.Channel;
   Overflowed_Channel : Overflowed.Channel;
   function Mmap (Address : System.Address; Length : Interfaces.C.size_t;
                  Protection, Flags, FD : Interfaces.C.int;
                  Offset : Interfaces.C.long) return System.Address
     with Import, Convention => C, External_Name => "mmap";
   function Munmap (Address : System.Address; Length : Interfaces.C.size_t)
     return Interfaces.C.int with Import, Convention => C, External_Name => "munmap";
   use type Interfaces.C.int;
   Base : constant Integer_Address := 16#72000000#;
   type Words is array (Natural range 0 .. 20479) of Unsigned_32;
   RAM : Words with Import, Volatile, Address => To_Address (Base);
   Mapping : System.Address;
   OK : Boolean;
   Saved_Head, Saved_Tail : Unsigned_32;
   Segment : Intel_GPU_ADLN_Context_Init.Segment;
begin
   Segment := Intel_GPU_ADLN_Context_Init.Build (True, 0, 2);
   Denied.Append (Denied_Channel, Segment, OK); pragma Assert (not OK);
   Native.Read_Saved_Pointers (Native_Channel, Saved_Head, Saved_Tail, OK);
   pragma Assert (not OK and Saved_Head = 0 and Saved_Tail = 0);
   Owner := True;
   Noncoherent.Append (Noncoherent_Channel, Segment, OK); pragma Assert (not OK);
   Native.Read_Saved_Pointers (Native_Channel, Saved_Head, Saved_Tail, OK);
   pragma Assert (not OK);
   -- No mapping existed for either rejection above.
   Mapping := Mmap (To_Address (Base), 81920, 3, 16#100022#, -1, 0);
   if Mapping /= To_Address (Base) then
      if Mapping /= To_Address (Integer_Address'Last) then
         declare Ignored : constant Interfaces.C.int := Munmap (Mapping, 81920); begin null; end;
      end if;
      raise Program_Error with "cannot reserve live ring fixture";
   end if;
   Coherent := True;
   Overflowed.Read_Saved_Pointers (Overflowed_Channel, Saved_Head, Saved_Tail, OK);
   pragma Assert (not OK);
   Overflowed.Append (Overflowed_Channel, Segment, OK); pragma Assert (not OK);
   RAM := [others => 16#A5A5A5A5#]; RAM (1031) := 384;
   for Seq in Unsigned_32 range 2 .. 12 loop
      Segment := Intel_GPU_ADLN_Context_Init.Build (True, 0, Seq);
      Marker := Unsigned_64 (Seq - 1); -- host simulation, not GPU execution
      Native.Append (Native_Channel, Segment, OK);
      pragma Assert (OK and RAM (1031) = Seq * 384 and Native.Sequence (Native_Channel) = Seq);
      for I in Segment.Words'Range loop
         pragma Assert (RAM (16384 + Natural (Seq - 1) * 96 + I) = Segment.Words (I));
      end loop;
   end loop;
   -- Includes a ring page boundary. Saved head, HWSP, prior segment and all
   -- other context fields must remain byte-for-byte unchanged.
   for I in RAM'Range loop
      if I /= 1031 and not (I in 16480 .. 16384 + 12 * 96 - 1) then
         pragma Assert (RAM (I) = 16#A5A5A5A5#);
      end if;
   end loop;
   -- Readback cannot be mistaken for a live engine head. The sentinel in
   -- the saved head was never modified by this host-only fixture.
   Owner := True;
   Native.Read_Saved_Pointers (Native_Channel, Saved_Head, Saved_Tail, OK);
   pragma Assert (OK and Saved_Head = 16#A5A5A5A5# and Saved_Tail = 12 * 384);
   declare
      Barrier : constant Intel_GPU_ADLN_Barrier.Segment := Intel_GPU_ADLN_Barrier.Build (3);
   begin
      RAM := [others => 16#A5A5A5A5#]; RAM (1031) := 384; Marker := 1;
      Mixed.Append (Mixed_Channel, Intel_GPU_ADLN_Context_Init.Build (True, 0, 2), OK);
      pragma Assert (OK and Mixed.Tail (Mixed_Channel) = 768);
      Marker := 2; -- simulated GPU completion of the preceding initializer
      Mixed.Append (Mixed_Channel, Barrier, OK);
      pragma Assert (OK and Mixed.Tail (Mixed_Channel) = 888 and RAM (1031) = 888 and Mixed.Sequence (Mixed_Channel) = 3);
      for I in Barrier.Words'Range loop
         pragma Assert (RAM (16384 + 192 + I) = Barrier.Words (I));
      end loop;
      for I in RAM'Range loop
         if I /= 1031 and not (I in 16480 .. 16384 + 222 - 1) then
            pragma Assert (RAM (I) = 16#A5A5A5A5#);
         end if;
      end loop;
      -- Model the exclusion interval without calling Append: a deliberate
      -- pause must not be represented as permanent Writer.Fail. Hardware
      -- scheduling and TLB completion remain outside this RAM fixture.
      Owner := False;
      pragma Assert (Mixed.Tail (Mixed_Channel) = 888 and Mixed.Sequence (Mixed_Channel) = 3);
      pragma Assert (RAM (1031) = 888);
      Owner := True; Marker := 3;
      Segment := Intel_GPU_ADLN_Context_Init.Build_Batch (True, 0, 4, 16#208000#);
      Mixed.Append (Mixed_Channel, Segment, OK);
      pragma Assert (OK and Mixed.Tail (Mixed_Channel) = 1272 and Mixed.Sequence (Mixed_Channel) = 4);
      for I in Segment.Words'Range loop
         pragma Assert (RAM (16384 + 222 + I) = Segment.Words (I));
      end loop;
      Mixed.Fail (Mixed_Channel);
      Marker := 4;
      Mixed.Append (Mixed_Channel, Intel_GPU_ADLN_Barrier.Build (5), OK);
      pragma Assert (not OK and Mixed.Tail (Mixed_Channel) = 1272 and RAM (1031) = 1272);
   end;
   -- One native writer instance, independent retained channels and mappings.
   -- Completion/tail values are simulated by the host, not the GPU.
   declare
      Channels : array (1 .. 2) of Native.Channel;
      Second_Base : constant Integer_Address := 16#72100000#;
      Second_RAM : Words with Import, Volatile, Address => To_Address (Second_Base);
      Second_Map : System.Address;
   begin
      Second_Map := Mmap (To_Address (Second_Base), 81920, 3, 16#100022#, -1, 0);
      if Second_Map /= To_Address (Second_Base) then
         if Second_Map /= To_Address (Integer_Address'Last) then
            declare Ignored : constant Interfaces.C.int := Munmap (Second_Map, 81920);
            begin null; end;
         end if;
         raise Program_Error with "cannot reserve second context fixture";
      end if;
      RAM := [others => 0]; RAM (1031) := 384;
      Second_RAM := [others => 0]; Second_RAM (1031) := 384;
      Selected_Base := Unsigned_64 (Base); Marker := 1;
      Native.Append (Channels (1), Intel_GPU_ADLN_Context_Init.Build (True, 0, 2), OK);
      pragma Assert (OK and RAM (1031) = 768 and Second_RAM (1031) = 384);
      Selected_Base := Unsigned_64 (Second_Base);
      Native.Append (Channels (2), Intel_GPU_ADLN_Context_Init.Build (True, 0, 2), OK);
      pragma Assert (OK and Second_RAM (1031) = 768);
      Selected_Base := Unsigned_64 (Base); Marker := 2;
      Native.Append (Channels (1), Intel_GPU_ADLN_Barrier.Build (3), OK);
      pragma Assert (OK and RAM (1031) = 888 and Second_RAM (1031) = 768);
      pragma Assert (Native.Sequence (Channels (1)) = 3 and
                     Native.Sequence (Channels (2)) = 2);
      -- Wrong selected backing cannot redirect channel1 writes into channel2.
      Selected_Base := Unsigned_64 (Second_Base); Marker := 3;
      Native.Append (Channels (1), Intel_GPU_ADLN_Barrier.Build (4), OK);
      pragma Assert (not OK and RAM (1031) = 888 and Second_RAM (1031) = 768);
      Native.Read_Saved_Pointers (Channels (1), Saved_Head, Saved_Tail, OK);
      pragma Assert (not OK and Saved_Head = 0 and Saved_Tail = 0);
      Marker := 2;
      Native.Append (Channels (2), Intel_GPU_ADLN_Barrier.Build (3), OK);
      pragma Assert (OK and Second_RAM (1031) = 888);
      -- Failure quarantines only channel1 and remains sticky on correct backing.
      Selected_Base := Unsigned_64 (Base); Marker := 3;
      Native.Append (Channels (1), Intel_GPU_ADLN_Barrier.Build (4), OK);
      pragma Assert (not OK and RAM (1031) = 888);
      pragma Assert (Munmap (Second_Map, 81920) = 0);
   end;
   declare
      Wrapping : Native.Channel;
      Before : Words;
      Old_Tail, New_Tail, Length, Start : Unsigned_32;
      Wrapped : Boolean;
      Wrap_Count : Natural := 0;
   begin
      RAM := [others => 16#A5A5A5A5#]; RAM (1031) := 384;
      Selected_Base := Unsigned_64 (Base); Owner := True; Coherent := True;
      for Seq in Unsigned_32 range 2 .. 4097 loop
         Marker := Unsigned_64 (Seq - 1);
         Old_Tail := Native.Tail (Wrapping);
         Length := (if Seq mod 2 = 0 then 384 else 120);
         Wrapped := Old_Tail > 16320 - Length;
         Start := (if Wrapped then 0 else Old_Tail);
         Before := RAM;
         if Seq mod 2 = 0 then
            Native.Append (Wrapping, Intel_GPU_ADLN_Context_Init.Build_Batch
              (True, 0, Unsigned_64 (Seq), 16#208000#), OK);
         else
            Native.Append (Wrapping, Intel_GPU_ADLN_Barrier.Build (Unsigned_64 (Seq)), OK);
         end if;
         New_Tail := Native.Tail (Wrapping);
         pragma Assert (OK and New_Tail = Start + Length and RAM (1031) = New_Tail);
         pragma Assert (Native.Sequence (Wrapping) = Seq);
         if Wrapped then
            Wrap_Count := Wrap_Count + 1;
            for I in Natural (Old_Tail / 4) .. 4095 loop
               pragma Assert (RAM (16384 + I) = 0);
            end loop;
         end if;
         for I in RAM'Range loop
            if I /= 1031 and then not
              (I in 16384 + Natural (Start / 4) .. 16384 + Natural (New_Tail / 4) - 1) and then
              not (Wrapped and then I >= 16384 + Natural (Old_Tail / 4))
            then
               pragma Assert (RAM (I) = Before (I));
            end if;
         end loop;
      end loop;
      pragma Assert (Wrap_Count > 60);
      Ada.Text_IO.Put_Line ("Native mixed wrap PASS:4096 segments, padding, exact write bounds, unchanged head/context/HWSP; simulated completions");
   end;
   Owner := False;
   pragma Assert (Munmap (Mapping, 81920) = 0);
   Native.Append (Native_Channel, Intel_GPU_ADLN_Context_Init.Build (True, 0, 13), OK);
   pragma Assert (not OK);
   Native.Read_Saved_Pointers (Native_Channel, Saved_Head, Saved_Tail, OK);
   pragma Assert (not OK);
   Ada.Text_IO.Put_Line ("Native live ring PASS: coherent gate, exact mapped writes, page crossing, retained context (host fixture only)");
end Native_Live_Ring_Tests;
