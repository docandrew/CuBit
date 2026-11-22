with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_DMA_Cache;
with Intel_GPU_Initial_Ring_Publish;
with Intel_GPU_Submission_Backing;
with Intel_GPU_ADLN_L3_Commands;
with Intel_GPU_Submission_Image;
with Intel_GPU_ADLN_PPHWSP;
with Intel_GPU_Timeline;
with System.Machine_Code;
package body Intel_GPU_Native_Initial_Ring is
   Base : constant Unsigned_64 := CPU_Base;
   Mapping_Valid : constant Boolean :=
     (Base /= 0 and then Base mod 4096 = 0 and then
      Backing_Bytes >= Intel_GPU_Submission_Backing.After_Last -
        Intel_GPU_Submission_Backing.First and then
      Base < 2 ** 47 and then Backing_Bytes <= 2 ** 47 - Base);
   Ring_Bytes : constant Unsigned_32 :=
     Intel_GPU_ADLN_Context_Init.Command_Words'Length * 4;
   Active, Attempted : Boolean := False;
   Copy_Attempted : Boolean := False;
   Published_Ready : Boolean := False;
   function Owned return Boolean is
     (Mapping_Valid and then Active and then Owner_Ready and then Exclusive_Ready);
   function Ring_Word (Offset : Unsigned_32) return Boolean is
     (Offset in 65536 .. 65536 + Ring_Bytes - 4 and then Offset mod 4 = 0);
   procedure Store (Offset, Value : Unsigned_32; OK : out Boolean) is
   begin
      OK := False;
      if not Owned or else not (Ring_Word (Offset) or else
        (Offset = 4124 and then Value = Ring_Bytes)) then return; end if;
      declare
         Word : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Base + Unsigned_64 (Offset)));
      begin Word := Value; end;
      OK := Owned;
   end Store;
   procedure Load (Offset : Unsigned_32; Value : out Unsigned_32;
                   OK : out Boolean) is
   begin
      Value := 0; OK := False;
      if not Owned or else not (Ring_Word (Offset) or Offset in 4116 | 4124)
      then return; end if;
      declare
         Word : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Base + Unsigned_64 (Offset)));
      begin Value := Word; end;
      OK := Owned;
   end Load;
   function Flush (Offset, Bytes : Unsigned_32) return Boolean is
      Page : Unsigned_64;
   begin
      if not Owned then return False; end if;
      if Offset = 65536 and Bytes = Ring_Bytes then Page := Base + 65536;
      elsif Offset = 4124 and Bytes = 4 then Page := Base + 4096;
      else return False;
      end if;
      -- Cache helper requires complete aligned lines. Both pages are wholly
      -- owned and the context has never run, so no concurrent GPU state write.
      return Intel_GPU_DMA_Cache.Flush_Range (Page, 4096) and then Owned;
   end Flush;
   package Writer is new Intel_GPU_Initial_Ring_Publish (Owned, Store, Load, Flush);
   Attempt : Writer.Attempt;
   -- A torn read (high half changed) is retried this many times before
   -- the read is reported failed.
   Timeline_Read_Attempts : constant := 3;
   procedure Read_Marker (Value : out Unsigned_64; OK : out Boolean) is
      package PPHWSP renames Intel_GPU_ADLN_PPHWSP;
      Sample : Intel_GPU_Timeline.Read_Result;
      High_First, Low, High_Second : Unsigned_32 := 0;
      -- CPU never writes the PPHWSP after backing initialization. Flush the
      -- retained page before each volatile DWORD read, so every half is
      -- fetched from memory rather than one possibly torn cached line.
      procedure Read_Half (Offset : Unsigned_32; Half : out Unsigned_32;
                           Read_OK : out Boolean) is
      begin
         Half := Unsigned_32'Last;
         Read_OK := Intel_GPU_DMA_Cache.Flush_Range (Base, PPHWSP.Page_Bytes)
           and then Owner_Ready;
         if not Read_OK then return; end if;
         declare
            Word : Unsigned_32 with Import, Volatile_Full_Access,
              Address => To_Address (Integer_Address (Base + Unsigned_64 (Offset)));
         begin Half := Word; end;
      end Read_Half;
   begin
      Value := Unsigned_64'Last; OK := False;
      if not Mapping_Valid or else not Owner_Ready then return; end if;
      for Attempt in 1 .. Timeline_Read_Attempts loop
         Read_Half (PPHWSP.Timeline_Offset + 4, High_First, OK);
         if OK then Read_Half (PPHWSP.Timeline_Offset, Low, OK); end if;
         if OK then Read_Half (PPHWSP.Timeline_Offset + 4, High_Second, OK); end if;
         if not OK then Value := Unsigned_64'Last; return; end if;
         Sample := Intel_GPU_Timeline.Combine
           (Intel_GPU_Timeline.Half (High_First), Intel_GPU_Timeline.Half (Low),
            Intel_GPU_Timeline.Half (High_Second));
         if Sample.Stable then
            Value := Unsigned_64 (Sample.Observed);
            OK := Owner_Ready;
            if not OK then Value := Unsigned_64'Last; end if;
            return;
         end if;
      end loop;
      OK := False;
   end Read_Marker;
   procedure Read_Batch_Result (Value : out Unsigned_64; OK : out Boolean) is
      package Backing renames Intel_GPU_Submission_Backing;
      Page : constant Unsigned_64 := Base +
        Backing.Offsets (Backing.Completion_Page) - Backing.First;
   begin
      Value := Unsigned_64'Last; OK := False;
      if not Mapping_Valid or else not Owner_Ready then return; end if;
      -- CPU does not modify this page after initial materialization. The
      -- probe writes one DWORD; read64 also checks the zero upper sentinel.
      if not Intel_GPU_DMA_Cache.Flush_Range (Page, 4096) or else not Owner_Ready
      then return; end if;
      declare
         Word : Unsigned_64 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Page));
      begin Value := Word; end;
      OK := Owner_Ready;
   end Read_Batch_Result;
   procedure Prepare_Copy_Source (OK : out Boolean) is
      package B renames Intel_GPU_Submission_Backing;
      package I renames Intel_GPU_Submission_Image;
      Page : constant Unsigned_64 := Base + B.Offsets (B.Completion_Page) - B.First;
   begin
      OK := False;
      if Copy_Attempted then return; end if;
      Copy_Attempted := True;
      if not Published_Ready or else not Mapping_Valid or else
        not Owner_Ready or else not Exclusive_Ready
      then return; end if;
      declare
         Source : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Page + I.Copy_Source_Offset));
      begin Source := I.Copy_Probe_Value; end;
      -- Orders the CPU store before device notification; does not write back
      -- the cache line. Later marker polling touches a different backing page.
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
      OK := Owner_Ready and then Exclusive_Ready;
   end Prepare_Copy_Source;
   procedure Read_Copy_Result (Value : out Unsigned_32; OK : out Boolean) is
      package B renames Intel_GPU_Submission_Backing;
      package I renames Intel_GPU_Submission_Image;
      Page : constant Unsigned_64 := Base + B.Offsets (B.Completion_Page) - B.First;
   begin
      Value := Unsigned_32'Last; OK := False;
      if not Mapping_Valid or else not Owner_Ready then return; end if;
      if not Intel_GPU_DMA_Cache.Flush_Range (Page, 4096) or else not Owner_Ready
      then return; end if;
      declare
         Result : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Page + I.Copy_Result_Offset));
      begin Value := Result; end;
      OK := Owner_Ready;
      if not OK then Value := Unsigned_32'Last; end if;
   end Read_Copy_Result;
   procedure Read_L3_Result (Value, Parameters : out Unsigned_32; OK : out Boolean) is
      package Backing renames Intel_GPU_Submission_Backing;
      Page : constant Unsigned_64 := Base +
        Backing.Offsets (Backing.Completion_Page) - Backing.First;
   begin
      Value := Unsigned_32'Last; Parameters := Unsigned_32'Last; OK := False;
      if not Mapping_Valid or else not Owner_Ready then return; end if;
      if not Intel_GPU_DMA_Cache.Flush_Range (Page, 4096) or else not Owner_Ready
      then return; end if;
      declare
         Word : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address
             (Page + Intel_GPU_ADLN_L3_Commands.Readback_Offset));
         Info : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address
             (Page + Intel_GPU_ADLN_L3_Commands.Readback_Offset + 4));
      begin Value := Word; Parameters := Info; end;
      OK := Owner_Ready;
   end Read_L3_Result;
   procedure Sample_Pixels_No_Flush (Values : out Pixel_Samples; OK : out Boolean) is
      package Backing renames Intel_GPU_Submission_Backing;
      Target : constant Unsigned_64 := Base +
        Backing.Offsets (Backing.Offscreen_Buffer) - Backing.First;
      type Offsets is array (Natural range 0 .. 4) of Unsigned_64;
      Pixel_Offsets : constant Offsets := [8320, 0, 252, 16128, 16380];
   begin
      Values := [others => Unsigned_32'Last]; OK := False;
      if not Mapping_Valid or else not Owner_Ready then return; end if;
      for I in Values'Range loop
         if not Mapping_Valid or else not Owner_Ready then
            Values := [others => Unsigned_32'Last]; return;
         end if;
         declare
            Word : Unsigned_32 with Import, Volatile_Full_Access,
              Address => To_Address (Integer_Address (Target + Pixel_Offsets (I)));
         begin Values (I) := Word; end;
      end loop;
      OK := Owner_Ready;
      if not OK then Values := [others => Unsigned_32'Last]; end if;
   end Sample_Pixels_No_Flush;
   procedure Read_Pixels (Values : out Pixel_Samples; OK : out Boolean) is
      package Backing renames Intel_GPU_Submission_Backing;
      Target : constant Unsigned_64 := Base +
        Backing.Offsets (Backing.Offscreen_Buffer) - Backing.First;
   begin
      Values := [others => Unsigned_32'Last]; OK := False;
      if not Mapping_Valid or else not Owner_Ready then return; end if;
      if not Intel_GPU_DMA_Cache.Flush_Range (Target, 16384) then return; end if;
      Sample_Pixels_No_Flush (Values, OK);
   end Read_Pixels;
   procedure Read_Image (Values : out Target_Image; OK : out Boolean) is
      package Backing renames Intel_GPU_Submission_Backing;
      Target : constant Unsigned_64 := Base +
        Backing.Offsets (Backing.Offscreen_Buffer) - Backing.First;
   begin
      Values := [others => Unsigned_32'Last]; OK := False;
      if not Mapping_Valid or else not Owner_Ready then return; end if;
      if not Intel_GPU_DMA_Cache.Flush_Range (Target, 16384) or else not Owner_Ready
      then return; end if;
      for Row in 0 .. 63 loop
         if not Mapping_Valid or else not Owner_Ready then
            Values := [others => Unsigned_32'Last]; return;
         end if;
         for Column in 0 .. 63 loop
            declare
               Index : constant Natural := Row * 64 + Column;
               Pixel : Unsigned_32 with Import, Volatile_Full_Access,
                 Address => To_Address (Integer_Address (Target + Unsigned_64 (Index) * 4));
            begin Values (Index) := Pixel; end;
         end loop;
      end loop;
      OK := Owner_Ready;
      if not OK then Values := [others => Unsigned_32'Last]; end if;
   end Read_Image;
   procedure Publish (Segment : Intel_GPU_ADLN_Context_Init.Segment;
                      Success : out Boolean) is
      Status : Writer.Result;
      Marker : Unsigned_64;
      OK : Boolean;
      use type Writer.Result;
   begin
      Success := False;
      if Attempted or else not Segment.Valid then return; end if;
      Attempted := True;
      if not Mapping_Valid or else not Owner_Ready or else not Exclusive_Ready then return; end if;
      Read_Marker (Marker, OK);
      if not OK or else Marker /= 0 then return; end if;
      Read_Batch_Result (Marker, OK);
      if not OK or else Marker /= 0 then return; end if;
      Active := True;
      Writer.Publish (Attempt, Segment, Status);
      Success := Status = Writer.Published;
      Published_Ready := Success;
      Active := False;
   end Publish;
end Intel_GPU_Native_Initial_Ring;
