with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_Buffer_Backing;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Buffer;
procedure VM_Buffer_Tests is
   package VM is new Intel_GPU_VM_Image (16);
   package Buffers is new Intel_GPU_VM_Buffer (VM);
   package Layout renames Intel_GPU_Buffer_Backing;
   package Replies renames Intel_GPU_Buffer_Reply;
   Tables : VM.Backing_Pages;
   Object : VM.Image;
   OK : Boolean;
   Backing : constant Replies.Backing :=
     Intel_GPU_Buffer_Reply.From_Linear (16#0200_0000#, Layout.CPU_Base, 16#100_0000#, 16#0200_0000#);
   type Table_Copy is array (VM.Page_Number, Table_Index) of Unsigned_64;
   procedure Reject (B : Replies.Backing; GPU, Offset, Bytes : Unsigned_64;
                     Mode : Page_Access := Read_Write) is
      Saved : Table_Copy;
      Count : constant Natural := VM.Used (Object);
   begin
      for P in VM.Page_Number loop
         for I in Table_Index loop Saved (P, I) := VM.Entry_Value (Object, P, I); end loop;
      end loop;
      for Removing in Boolean loop
         if Removing then
            Buffers.Unbind_Range (Object, B, GPU, Offset, Bytes, OK);
         else
            Buffers.Bind_Range (Object, B, GPU, Offset, Bytes, Write_Back, Mode, OK);
         end if;
         pragma Assert (not OK and VM.Used (Object) = Count);
         for P in VM.Page_Number loop
            for I in Table_Index loop pragma Assert (Saved (P, I) = VM.Entry_Value (Object, P, I)); end loop;
         end loop;
      end loop;
   end Reject;
begin
   for P in VM.Page_Number loop Tables (P) := Unsigned_64 (P) * 4096; end loop;
   VM.Initialize (Object, Tables, OK); pragma Assert (OK);
   -- The backing policy is global to this pool, not inferred from another
   -- mapping in this image. Even an empty second VM must reject an override.
   for Policy in Cache_Policy loop
      if Policy /= Write_Back then
         declare
            Other : VM.Image;
         begin
            VM.Initialize (Other, Tables, OK); pragma Assert (OK);
            Buffers.Bind_Range (Other, Backing, 4096, 0, 4096,
                               Policy, Read_Write, OK);
            pragma Assert (not OK and VM.Used (Other) = 1);
            for P in VM.Page_Number loop
               for I in Table_Index loop
                  pragma Assert (VM.Entry_Value (Other, P, I) = 0);
               end loop;
            end loop;
         end;
      end if;
   end loop;
   -- Slice crosses a 2MiB table boundary; backing offset is not GPU offset.
   Buffers.Bind_Range (Object, Backing, 16#1FF000#, 4096, 3 * 4096,
                      Write_Back, Read_Write, OK);
   pragma Assert (OK);
   for P in 0 .. 2 loop
      pragma Assert ((VM.Lookup (Object, 16#1FF000# + Unsigned_64 (P) * 4096) and
        not Unsigned_64'(4095)) = 16#0200_1000# + Unsigned_64 (P) * 4096);
   end loop;
   Reject ((Ready => False), 16#400000#, 0, 4096);
   Reject (Backing, 0, 0, 4096);
   Reject (Backing, 2 ** 48, 0, 4096);
   Reject (Backing, 2 ** 48 - 4096, 0, 8192);
   Reject (Backing, 16#400000#, Unsigned_64'Last, 4096);
   Reject (Backing, 16#400000#, Backing.Bytes, 4096);
   Reject (Backing, 16#400000#, 0, 0);
   Reject (Backing, 16#400000#, 0, 4096, Read_Only);
   Reject (Backing, 16#1FE000#, 0, 8192); -- late collision is atomic
   for Bit in 0 .. 11 loop
      Reject (Backing, 16#400000# + 2 ** Bit, 0, 4096);
      Reject (Backing, 16#400000#, 2 ** Bit, 4096);
      Reject (Backing, 16#400000#, 0, 4096 + 2 ** Bit);
   end loop;
   declare Bad : Replies.Backing := Backing; begin
      Bad.CPU_Address := Bad.CPU_Address + 4096;
      Reject (Bad, 16#400000#, 0, 4096);
      Bad := Replies.From_Linear (4096, Backing.CPU_Address, Backing.Bytes, 4096);
      Reject (Bad, 16#400000#, 0, 4096); -- page-table backing alias
   end;
   -- Largest permitted buffer, ending exactly at raw48 limit.
   Buffers.Bind_Range (Object, Backing, 2 ** 48 - Backing.Bytes,
                      0, Backing.Bytes, Write_Back, Read_Write, OK);
   pragma Assert (OK);
   for P in 0 .. 4095 loop
      pragma Assert ((VM.Lookup (Object, 2 ** 48 - Backing.Bytes + Unsigned_64 (P) * 4096) and
        not Unsigned_64'(4095)) = Intel_GPU_Buffer_Reply.Page_Address (Backing, 0) + Unsigned_64 (P) * 4096);
   end loop;
   -- The wrong offset must not remove another slice of the same allocation.
   Buffers.Unbind_Range (Object, Backing, 16#1FF000#, 0, 3 * 4096, OK);
   pragma Assert (not OK and VM.Lookup (Object, 16#1FF000#) = 16#02001003#);
   Buffers.Unbind_Range (Object, Backing, 16#1FF000#, 4096, 3 * 4096, OK);
   pragma Assert (OK);
   for P in 0 .. 2 loop
      pragma Assert (VM.Lookup (Object, 16#1FF000# + Unsigned_64 (P) * 4096) = 0);
   end loop;
   -- The second alias is still present, and still excludes DMA reuse.
   pragma Assert (not VM.DMA_Disjoint (Object, Intel_GPU_Buffer_Reply.Page_Address (Backing, 0) + 4096, 4096));
   Buffers.Unbind_Range (Object, Backing, 2 ** 48 - Backing.Bytes, 0, Backing.Bytes, OK);
   pragma Assert (OK);
   for P in 0 .. 4095 loop
      pragma Assert (VM.Lookup (Object, 2 ** 48 - Backing.Bytes + Unsigned_64 (P) * 4096) = 0);
   end loop;
   VM.Seal (Object, OK); pragma Assert (not OK);
   Buffers.Bind_Range (Object, Backing, 4096, 4096, 4096, Write_Back, Read_Write, OK);
   pragma Assert (OK);
   VM.Seal (Object, OK); pragma Assert (OK);
   Buffers.Unbind_Range (Object, Backing, 4096, 4096, 4096, OK);
   pragma Assert (not OK and VM.Lookup (Object, 4096) = 16#02001003#);
   Reject (Backing, 16#400000#, 0, 4096);
   Ada.Text_IO.Put_Line ("VM buffer PASS: bind/unbind slices, boundary crossing, full16MiB, raw48 end, malformed ranges, aliases and sealed denial");
end VM_Buffer_Tests;
