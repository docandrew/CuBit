with Ada.Text_IO;
with Interfaces; use Interfaces;
with Interfaces.C;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_ADLN_Context_Init;
with Intel_GPU_Native_Initial_Ring;
with Intel_GPU_Native_Live_Ring;
with Intel_GPU_Submission_Backing;
with Intel_GPU_ADLN_L3_Commands;
with Intel_GPU_Submission_Image;
procedure Native_Initial_Ring_Tests is
   Owner, Exclusive : Boolean := False;
   function Owned return Boolean is (Owner);
   function Alone return Boolean is (Exclusive);
   Owner_Calls, Fail_On_Call : Natural := 0;
   function Fading_Owner return Boolean is
   begin
      Owner_Calls := Owner_Calls + 1;
      return Owner and then Owner_Calls /= Fail_On_Call;
   end Fading_Owner;
   package Fading is new Intel_GPU_Native_Initial_Ring
     (16#71000000#, Intel_GPU_Submission_Backing.After_Last -
       Intel_GPU_Submission_Backing.First, Fading_Owner, Alone);
   package Native is new Intel_GPU_Native_Initial_Ring (16#71000000#, Intel_GPU_Submission_Backing.After_Last -
       Intel_GPU_Submission_Backing.First, Owned, Alone);
   package Short is new Intel_GPU_Native_Initial_Ring
     (16#71000000#, 81920, Owned, Alone);
   package Application is new Intel_GPU_Native_Initial_Ring
     (16#71000000#, Intel_GPU_Submission_Backing.After_Last -
       Intel_GPU_Submission_Backing.First, Owned, Alone);
   function Live_Base return Unsigned_64 is (16#71000000#);
   function Live_Bytes return Unsigned_64 is
     (Intel_GPU_Submission_Backing.After_Last - Intel_GPU_Submission_Backing.First);
   function Live_Owner return Boolean is (Owner and not Exclusive);
   package Live is new Intel_GPU_Native_Live_Ring
     (Live_Base, Live_Bytes, Live_Owner, Live_Owner, Application.Read_Marker);
   Live_Channel : Live.Channel;
   Premature_Channel : Live.Channel;
   function Mmap (Address : System.Address; Length : Interfaces.C.size_t;
                  Protection, Flags, FD : Interfaces.C.int;
                  Offset : Interfaces.C.long) return System.Address
     with Import, Convention => C, External_Name => "mmap";
   function Munmap (Address : System.Address; Length : Interfaces.C.size_t)
     return Interfaces.C.int with Import, Convention => C, External_Name => "munmap";
   use type Interfaces.C.int;
   Base : constant Integer_Address := 16#71000000#;
   Bytes : constant Interfaces.C.size_t := Interfaces.C.size_t
     (Intel_GPU_Submission_Backing.After_Last - Intel_GPU_Submission_Backing.First);
   type Words is array (Natural range 0 .. Natural (Bytes) / 4 - 1) of Unsigned_32;
   RAM : Words with Import, Volatile, Address => To_Address (Base);
   Mapping : System.Address;
   Marker : Unsigned_64;
   L3_Value, L3_Parameters : Unsigned_32;
   Pixels : Native.Pixel_Samples;
   Image : Native.Target_Image;
   use type Native.Pixel_Samples;
   OK : Boolean;
   Segment : constant Intel_GPU_ADLN_Context_Init.Segment :=
     Intel_GPU_ADLN_Context_Init.Build (True, 16#12345678#);
begin
   Native.Read_Marker (Marker, OK);
   pragma Assert (not OK); -- no mapping: owner rejection must precede loads
   Native.Read_L3_Result (L3_Value, L3_Parameters, OK);
   pragma Assert (not OK and L3_Value = Unsigned_32'Last);
   Native.Read_Pixels (Pixels, OK);
   pragma Assert (not OK and Pixels = Native.Pixel_Samples'(others => Unsigned_32'Last));
   Native.Sample_Pixels_No_Flush (Pixels, OK);
   pragma Assert (not OK and Pixels = Native.Pixel_Samples'(others => Unsigned_32'Last));
   Mapping := Mmap (To_Address (Base), Bytes, 3, 16#100022#, -1, 0);
   if Mapping /= To_Address (Base) then
      if Mapping /= To_Address (Integer_Address'Last) then
         declare Ignored : constant Interfaces.C.int := Munmap (Mapping, Bytes); begin null; end;
      end if;
      raise Program_Error with "cannot reserve retained submission fixture";
   end if;
   Owner := True; Exclusive := True;
   Short.Read_Batch_Result (Marker, OK); pragma Assert (not OK);
   declare
      Samples : Short.Pixel_Samples;
   begin
      Short.Sample_Pixels_No_Flush (Samples, OK);
      pragma Assert (not OK and (for all P of Samples => P = Unsigned_32'Last));
   end;
   Short.Publish (Segment, OK); pragma Assert (not OK);
   Native.Read_Marker (Marker, OK);
   pragma Assert (OK and Marker = 0);
   Native.Publish (Segment, OK);
   pragma Assert (OK and RAM (1031) = 384 and RAM (1029) = 0);
   for I in RAM'Range loop
      if I in 16384 .. 16384 + Segment.Words'Length - 1 then
         pragma Assert (RAM (I) = Segment.Words (I - 16384));
      elsif I /= 1031 then pragma Assert (RAM (I) = 0);
      end if;
   end loop;
   Native.Publish (Segment, OK); pragma Assert (not OK);
   declare
      package B renames Intel_GPU_Submission_Backing;
      package I renames Intel_GPU_Submission_Image;
      First : constant Natural := Natural ((B.Offsets (B.Completion_Page) - B.First) / 4);
      Before : constant Words := RAM;
      Value : Unsigned_32;
   begin
      Native.Prepare_Copy_Source (OK); pragma Assert (OK);
      for W in RAM'Range loop
         pragma Assert (RAM (W) =
           (if W = First + I.Copy_Source_Offset / 4 then I.Copy_Probe_Value else Before (W)));
      end loop;
      Native.Prepare_Copy_Source (OK); pragma Assert (not OK);
      Native.Read_Copy_Result (Value, OK); pragma Assert (OK and Value = 0);
      -- CPU simulation only, not a GPU/cache coherence test.
      RAM (First + I.Copy_Result_Offset / 4) := I.Copy_Probe_Value;
      Native.Read_Copy_Result (Value, OK);
      pragma Assert (OK and Value = I.Copy_Probe_Value);
   end;
   Exclusive := False; -- hardware simulation only: GPU writes final marker
   RAM (52) := 1;
   Native.Read_Marker (Marker, OK);
   pragma Assert (OK and Marker = 1);
   declare
      package B renames Intel_GPU_Submission_Backing;
      Index : constant Natural := Natural
        ((B.Offsets (B.Completion_Page) - B.First +
          Intel_GPU_ADLN_L3_Commands.Readback_Offset) / 4);
   begin
      Native.Read_L3_Result (L3_Value, L3_Parameters, OK);
      pragma Assert (OK and L3_Value = 0 and L3_Parameters = 0);
      -- Simulated GPU write, not proof of device execution.
      RAM (Index) := 16#B0000040#;
      RAM (Index - 1) := 16#DEADBEEF#;
      RAM (Index + 1) := 16#2058#;
      RAM (Index + 2) := 16#FEEDFACE#;
      Native.Read_L3_Result (L3_Value, L3_Parameters, OK);
      pragma Assert (OK and L3_Value = 16#B0000040# and L3_Parameters = 16#2058#);
   end;
   declare
      package B renames Intel_GPU_Submission_Backing;
      First : constant Natural := Natural ((B.Offsets (B.Offscreen_Buffer) - B.First) / 4);
   begin
      RAM (First + 32 * 64 + 32) := 16#FFFF0000#;
      RAM (First) := 1;
      RAM (First + 63) := 2;
      RAM (First + 63 * 64) := 3;
      RAM (First + 4095) := 4;
      Native.Sample_Pixels_No_Flush (Pixels, OK);
      pragma Assert (OK and Pixels = [16#FFFF0000#,1,2,3,4]);
      Native.Read_Pixels (Pixels, OK);
      pragma Assert (OK and Pixels = [16#FFFF0000#,1,2,3,4]);
      -- Reject before the first read, at each pixel, and at the final owner
      -- check. No partial sample may escape with apparently valid pixels.
      for Failure in 1 .. 7 loop
         declare
            Samples : Fading.Pixel_Samples;
         begin
            Owner_Calls := 0; Fail_On_Call := Failure;
            Fading.Sample_Pixels_No_Flush (Samples, OK);
            pragma Assert (not OK and
              (for all P of Samples => P = Unsigned_32'Last));
         end;
      end loop;
      Native.Read_Image (Image, OK);
      pragma Assert (OK);
      for I in Image'Range loop pragma Assert (Image (I) = RAM (First + I)); end loop;
   end;
   -- A separately prepared application context has no bootstrap PPGTT leaf.
   -- Publish setup-only commands and verify the entire backing write footprint.
   -- Reset fixture RAM only after the preceding simulated work is finished.
   Exclusive := True;
   RAM := [others => 0];
   declare
      Setup : constant Intel_GPU_ADLN_Context_Init.Segment :=
        Intel_GPU_ADLN_Context_Init.Build_Setup (True, 16#12345678#);
   begin
      Application.Publish (Setup, OK);
      pragma Assert (OK and RAM (1031) = 384);
      for I in RAM'Range loop
         if I in 16384 .. 16384 + Setup.Words'Length - 1 then
            pragma Assert (RAM (I) = Setup.Words (I - 16384));
         elsif I /= 1031 then pragma Assert (RAM (I) = 0);
         end if;
      end loop;
      pragma Assert (for all I in 58 .. 63 => RAM (16384 + I) = 0);
      Application.Publish (Setup, OK); pragma Assert (not OK);
      Exclusive := False;
      declare
         Dispatch : Intel_GPU_ADLN_Context_Init.Segment :=
           Intel_GPU_ADLN_Context_Init.Build_Batch
             (True, 16#12345678#, 2, 16#208000#);
         Before : Words := RAM;
      begin
         -- No GPU setup completion yet: even a correctly encoded branch
         -- cannot be appended. This channel stays quarantined afterwards.
         Live.Append (Premature_Channel, Dispatch, OK);
         pragma Assert (not OK and RAM = Before);
         RAM (52) := 1; -- simulated GPU completion, not a native execution test
         Before := RAM;
         Live.Append (Premature_Channel, Dispatch, OK);
         pragma Assert (not OK and RAM = Before);
         Live.Append (Live_Channel, Dispatch, OK);
         pragma Assert (OK and Live.Sequence (Live_Channel) = 2 and
                        Live.Tail (Live_Channel) = 768 and RAM (1031) = 768);
         for I in RAM'Range loop
            if I in 16480 .. 16480 + Dispatch.Words'Length - 1 then
               pragma Assert (RAM (I) = Dispatch.Words (I - 16480));
            elsif I /= 1031 then
               pragma Assert (RAM (I) = Before (I));
            end if;
         end loop;
         -- The encoder selects nonprivileged PPGTT and the owned batch VA.
         pragma Assert (RAM (16480 + 59) = 16#18800101# and
                        RAM (16480 + 60) = 16#208000#);
         RAM (52) := 2; -- simulated completion of the first dispatch
         Before := RAM;
         Dispatch := Intel_GPU_ADLN_Context_Init.Build_Batch
           (True, 16#12345678#, 3, 16#20A000#);
         Live.Append (Live_Channel, Dispatch, OK);
         pragma Assert (OK and Live.Sequence (Live_Channel) = 3 and
                        Live.Tail (Live_Channel) = 1152 and RAM (1031) = 1152);
         for I in RAM'Range loop
            if I in 16576 .. 16576 + Dispatch.Words'Length - 1 then
               pragma Assert (RAM (I) = Dispatch.Words (I - 16576));
            elsif I /= 1031 then
               pragma Assert (RAM (I) = Before (I));
            end if;
         end loop;
      end;
   end;
   Owner := False;
   pragma Assert (Munmap (Mapping, Bytes) = 0);
   Native.Read_Marker (Marker, OK); pragma Assert (not OK);
   Native.Read_L3_Result (L3_Value, L3_Parameters, OK);
   pragma Assert (not OK and L3_Value = Unsigned_32'Last);
   Native.Read_Pixels (Pixels, OK);
   pragma Assert (not OK);
   Native.Sample_Pixels_No_Flush (Pixels, OK);
   pragma Assert (not OK and Pixels = Native.Pixel_Samples'(others => Unsigned_32'Last));
   Native.Read_Image (Image, OK);
   pragma Assert (not OK and (for all P of Image => P = Unsigned_32'Last));
   Ada.Text_IO.Put_Line ("Native initial ring PASS: fixed mapping, exact writes, cache flush, marker read, owner and retry rejection (host fixture only)");
end Native_Initial_Ring_Tests;
