with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_Physical_Extents;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Buffer;
procedure VM_Extents_Tests is
   package E renames Intel_GPU_Physical_Extents;
   package VM is new Intel_GPU_VM_Image (24);
   package Binder is new Intel_GPU_VM_Buffer (VM);
   Bases : E.Addresses;
   Map : E.Map;
   Backing, Part : Intel_GPU_Buffer_Reply.Extent_View;
   Tables : VM.Backing_Pages;
   Object : VM.Image;
   OK : Boolean;
   Count : Natural;
begin
   for I in E.Block_Index loop
      Bases (I) := 2 ** 32 - Unsigned_64 (2 * I + 1) * E.Block_Bytes;
   end loop;
   E.Admit (Bases, Map, OK); pragma Assert (OK);
   Backing := Intel_GPU_Buffer_Reply.From_Extents (Map, 1, 0, E.Capacity);
   for I in VM.Page_Number loop Tables (I) := Unsigned_64 (I) * 4096; end loop;
   VM.Initialize (Object, Tables, OK); pragma Assert (OK);
   Binder.Bind_Range (Object, Backing, 16#200000#, 0, E.Capacity, OK);
   pragma Assert (OK);
   for P in 0 .. 8191 loop
      pragma Assert (VM.Lookup (Object, 16#200000# + Unsigned_64 (P) * 4096) =
        Encode_Leaf (Bases (E.Block_Index (P / 512)) + Unsigned_64 (P mod 512) * 4096,
                     Write_Back, Read_Write));
   end loop;
   Count := VM.Used (Object);
   -- A suballocation exposes only its own bytes, even though the admitted
   -- arena contains more backing. Its offsets are not arena-relative.
   Part := Intel_GPU_Buffer_Reply.Slice (Backing, E.Block_Bytes - 4096, 8192);
   Binder.Bind_Range (Object, Part, 16#5000000#, 0, 12288, OK);
   pragma Assert (not OK and VM.Used (Object) = Count);
   Binder.Bind_Range (Object, Part, 16#5000000#, 0, 8192, OK);
   pragma Assert (OK and VM.Lookup (Object, 16#5000000#) =
     Encode_Leaf (Bases (0) + E.Block_Bytes - 4096, Write_Back, Read_Write));
   pragma Assert (VM.Lookup (Object, 16#5001000#) =
     Encode_Leaf (Bases (1), Write_Back, Read_Write));
   Binder.Unbind_Range (Object, Part, 16#5000000#, 0, 8192, OK);
   pragma Assert (OK);
   Count := VM.Used (Object);
   -- Late collision must not install the first, otherwise free, page.
   Binder.Bind_Range (Object, Backing, 16#1FF000#, 0, 8192, OK);
   pragma Assert (not OK and VM.Used (Object) = Count and
     VM.Lookup (Object, 16#1FF000#) = 0);
   Binder.Bind_Range (Object, Backing, 2 ** 48 - 4096, 0, 8192, OK);
   pragma Assert (not OK and VM.Used (Object) = Count);
   Binder.Bind_Range (Object, Backing, 16#4000000#, E.Capacity - 4096, 8192, OK);
   pragma Assert (not OK and VM.Used (Object) = Count);
   -- Wrong backing offset cannot partially unmap a cross-block slice.
   Binder.Unbind_Range (Object, Backing, 16#3FF000#, 0, 8192, OK);
   pragma Assert (not OK and VM.Lookup (Object, 16#3FF000#) /= 0 and
     VM.Lookup (Object, 16#400000#) /= 0);
   Binder.Unbind_Range (Object, Backing, 16#3FF000#, E.Block_Bytes - 4096, 8192, OK);
   pragma Assert (OK and VM.Lookup (Object, 16#3FF000#) = 0 and
     VM.Lookup (Object, 16#400000#) = 0);
   Binder.Bind_Range (Object, Backing, 16#3FF000#, E.Block_Bytes - 4096, 8192, OK);
   pragma Assert (OK);
   pragma Assert (not Binder.Matches_Range (Object, Backing, 16#200000#, 0, E.Capacity));
   VM.Seal (Object, OK); pragma Assert (OK);
   pragma Assert (Binder.Matches_Range (Object, Backing, 16#200000#, 0, E.Capacity));
   pragma Assert (Binder.Matches_Range (Object, Backing, 16#3FFFFF#, E.Block_Bytes - 1, 2));
   pragma Assert (not Binder.Matches_Range (Object, Backing, 16#200000#, 4096, 4096));
   pragma Assert (not Binder.Matches_Range (Object, Backing, 16#200000#, 0, Unsigned_64'Last));
   Binder.Unbind_Range (Object, Backing, 16#200000#, 0, 4096, OK);
   pragma Assert (not OK and Binder.Matches_Range (Object, Backing, 16#200000#, 0, 4096));
   Binder.Bind_Range (Object, Backing, 16#4000000#, 0, 4096, OK);
   pragma Assert (not OK and VM.Lookup (Object, 16#4000000#) = 0);
   Ada.Text_IO.Put_Line ("VM extents PASS: 32MiB scattered backing, all8192 PTEs, atomic collision and sealed/range rejection (offline hosted)");
end VM_Extents_Tests;
