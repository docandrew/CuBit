with Interfaces; use Interfaces;
with Intel_GPU_Submission_Image; use Intel_GPU_Submission_Image;
with Ada.Text_IO;
with Intel_GPU_ADLN_Probe_Shaders;
with Intel_GPU_ADLN_Offscreen_Batch;
procedure Submission_Image_Tests is
   package Shaders renames Intel_GPU_ADLN_Probe_Shaders;
   Result : Image;
   Draw : constant Intel_GPU_ADLN_Offscreen_Batch.Image :=
     Intel_GPU_ADLN_Offscreen_Batch.Build (6, Draw_URB_KiB, Draw_VS_Threads, Draw_PS_Threads);
   procedure Reject (DMA, GPU : Unsigned_64) is
      Bad : constant Image := Build (DMA, GPU);
   begin
      pragma Assert (not Bad.Valid);
      pragma Assert (for all Word of Bad.Words => Word = 0);
   end Reject;
begin
   declare
      Pages : Backing_Pages;
      GPUs : constant array (Positive range 1 .. 8) of Unsigned_64 :=
        [0, 1, 4095, 4096, 16#FED_EC000#, 16#FED_ED000#,
         16#FEE00000#, Unsigned_64'Last];
   begin
      for P in Pages'Range loop
         Pages (P) := 16#4000000# - Unsigned_64 (P) * 8192;
      end loop;
      Result := Build (Pages, 16#2000000#);
      pragma Assert (Result.Valid);
      pragma Assert (Result.Words (1075) = Unsigned_32 (Pages (20)));
      pragma Assert (Result.Words (20480) = (Unsigned_32 (Pages (21)) or 3));
      pragma Assert (Result.Words (21504) = (Unsigned_32 (Pages (22)) or 3));
      pragma Assert (Result.Words (22530) = (Unsigned_32 (Pages (23)) or 3));
      pragma Assert (Result.Words (23552) = (Unsigned_32 (Pages (24)) or 3));
      pragma Assert (Result.Words (23566) = (Unsigned_32 (Pages (32)) or 3));
      -- A gap between scattered pages is not part of this allocation.
      Result := Build_For_VM (Pages, 16#2000000#, Pages (0) - 4096);
      pragma Assert (Result.Valid and Result.Words (1075) = Unsigned_32 (Pages (0) - 4096));
      pragma Assert (Valid_For_VM (Pages, 16#2000000#, Pages (0) - 4096));
      for GPU of GPUs loop
         pragma Assert (Valid_For_VM (Pages, GPU, 4096) =
           Build_For_VM (Pages, GPU, 4096).Valid);
      end loop;
      for P in Pages'Range loop
         Result := Build_For_VM (Pages, 16#2000000#, Pages (P));
         pragma Assert (not Result.Valid and (for all Word of Result.Words => Word = 0));
         pragma Assert (not Valid_For_VM (Pages, 16#2000000#, Pages (P)));
      end loop;
      for P in Pages'Range loop
         declare
            Bad : Backing_Pages := Pages;
         begin
            Bad (P) := 0;
            pragma Assert (not Valid_For_VM (Bad, 16#2000000#, 4096));
            Result := Build (Bad, 16#2000000#);
            pragma Assert (not Result.Valid and
              (for all Word of Result.Words => Word = 0));
            pragma Assert (not Build_For_VM (Bad, 16#2000000#, 4096).Valid);
            Bad (P) := Pages ((P + 1) mod Pages'Length);
            pragma Assert (not Valid_For_VM (Bad, 16#2000000#, 4096));
            pragma Assert (not Build (Bad, 16#2000000#).Valid);
            pragma Assert (not Build_For_VM (Bad, 16#2000000#, 4096).Valid);
         end;
      end loop;
   end;
   Result := Build_For_VM (16#108C000#, 16#2000000#, 16#4000000#);
   pragma Assert (Result.Valid);
   pragma Assert (Result.Words (1024 + 51) = 16#4000000#);
   pragma Assert (Result.Words (1024 + 5) = 0 and Result.Words (1024 + 7) = 0);
   pragma Assert (for all I in 16384 .. Image_Words'Last => Result.Words (I) = 0);
   for P in 0 .. Byte_Count / 4096 - 1 loop
      Result := Build_For_VM (16#108C000#, 16#2000000#,
                             16#108C000# + Unsigned_64 (P) * 4096);
      pragma Assert (not Result.Valid);
      pragma Assert (for all Word of Result.Words => Word = 0);
   end loop;
   pragma Assert (not Build_For_VM (4096, 4096, 0).Valid);
   pragma Assert (not Build_For_VM (4096, 4096, 2 ** 32).Valid);
   pragma Assert (not Build_For_VM (4096, 4096, 16#4000001#).Valid);
   pragma Assert (Build_For_VM (4096, 4096, 4096 + Byte_Count).Valid);
   Result := Build (16#108C000#,16#2000000#);
   pragma Assert (Result.Valid);
   pragma Assert (Result.Words (1024 + 9) = 16#2010000#);
   pragma Assert (Result.Words (1024 + 51) = 16#10A0000#);
   for I in 16384 .. 20479 loop pragma Assert (Result.Words (I) = 0); end loop;
   for I in 20480 .. 24575 loop
      pragma Assert (Result.Words (I) =
        (case I is
          when 20480 => 16#10A1003#, when 21504 => 16#10A2003#,
          when 22530 => 16#10A3003#, when 23552 => 16#10A4003#,
          when 23554 => 16#10A5003#, when 23556 => 16#10A7003#,
          when 23558 => 16#10A8003#, when 23560 => 16#10A9003#,
          when 23562 => 16#10AA003#, when 23564 => 16#10AB003#,
          when 23566 => 16#10AC003#, when others => 0));
   end loop;
   pragma Assert (Result.Words (24576) = (Shift_Left (16#20#, 23) or 2));
   pragma Assert ((Result.Words (24576) and Shift_Left (1, 22)) = 0);
   pragma Assert (Result.Words (24577) = 16#201000# and Result.Words (24578) = 0);
   pragma Assert (Result.Words (24579) = 16#43554249#);
   pragma Assert (Result.Words (24580) = 16#17000003# and
     Result.Words (24581) = 16#201080# and Result.Words (24582) = 0 and
     Result.Words (24583) = 16#201100# and Result.Words (24584) = 0 and
     Result.Words (24585) = 16#05000000#);
   pragma Assert (Draw.Valid and Draw.Count = 270);
   pragma Assert (Draw_Batch_VA = 16#200400#);
   for I in 24586 .. 31743 loop
      if I in 24832 .. 24832 + Draw.Count - 1 then
         pragma Assert (Result.Words (I) = Draw.Data (I - 24832));
      else
         pragma Assert (Result.Words (I) = 0);
      end if;
   end loop;
   for I in 31744 .. 32767 loop
      if I in 31808 .. 31819 then
         pragma Assert (Result.Words (I) = Shaders.Vertices (I - 31808));
      else
      pragma Assert (Result.Words (I) =
        (case I is
          when 31744 => 64,
          when 31760 => 16#23014000#, when 31761 => 16#86000010#,
          when 31762 => 16#003F003F#, when 31763 => 255,
          when 31765 => 256, when 31767 => 16#09770000#,
          when 31768 => 16#00202000#,
          -- Independent Mesa viewport fixture at state-page offsets512/576.
          when 31872 | 31873 | 31875 | 31876 => 16#42000000#,
          when 31874 | 31881 | 31883 | 31889 => 16#3F800000#,
          when 31880 | 31882 => 16#BF800000#,
          when 31885 | 31887 => 16#427C0000#,
          -- Blend state at640: common0, RT0 writable, RT1..7 disabled.
          when 31907 | 31909 | 31911 | 31913 | 31915 | 31917 | 31919 => 15,
          when 31906 | 31908 | 31910 | 31912 | 31914 | 31916 | 31918 | 31920 => 11,
          when others => 0));
      end if;
   end loop;
   for I in 32768 .. Image_Words'Last loop
      if I in 32768 .. 32811 then
         pragma Assert (Result.Words (I) = Shaders.Vertex (I - 32768));
      elsif I in 32832 .. 32859 then
         pragma Assert (Result.Words (I) = Shaders.Fragment (I - 32832));
      else
         pragma Assert (Result.Words (I) = 0);
      end if;
   end loop;
   pragma Assert (Image_Words'Last = 33791 and Byte_Count = 135168);
   pragma Assert (Offscreen_VA = 16#202000# and GGTT_Bytes = 81920);
   pragma Assert (Build (2 ** 32 - Byte_Count,16#FEDEC000#).Valid);
   Reject (2 ** 32 - Byte_Count + 4096,16#2000000#);
   -- Independent low-address context extent; no firmware prefix required.
   Result := Build (4096, 16#3000000#);
   pragma Assert (Result.Valid);
   pragma Assert (Result.Words (1024 + 9) = 16#3010000#);
   pragma Assert (Result.Words (1024 + 51) = 16#15000#);
   pragma Assert (Result.Words (20480) = 16#16003#);
   pragma Assert (Result.Words (23552) = 16#19003#);
   pragma Assert (Result.Words (23554) = 16#1A003#);
   Reject (4096,16#FEDED000#);
   Reject (0,4096); Reject (4096,0);
   Reject (Unsigned_64'Last,4096); Reject (4096,Unsigned_64'Last);
   Reject (4097,4096); Reject (4096,4097);
   Ada.Text_IO.Put_Line ("Submission image PASS: immutable marker/draw batches, private pages; NOT submitted");
end Submission_Image_Tests;
