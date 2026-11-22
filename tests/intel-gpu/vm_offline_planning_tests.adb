with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Growth;
procedure VM_Offline_Planning_Tests is
   package VM is new Intel_GPU_VM_Image (8);
   package G is new VM.Growth;
   use type G.Plan_Status;
   Cases : Natural := 0;
begin
   for Backed in 4 .. 8 loop
      for Depth in 1 .. 3 loop
         declare
            Source : VM.Image;
            Backing : VM.Backing_Pages := [others => 0];
            GPU : constant Unsigned_64 := 2 ** (12 + 9 * Depth);
            New_Pages : VM.Data_Pages (1 .. Depth) := [others => 0];
            Needed : G.Offline_Requirements;
            OK : Boolean;
            First : Natural;
            Epoch : Unsigned_64;
         begin
            for P in 1 .. Backed loop Backing (P) := Unsigned_64 (P) * 4096; end loop;
            VM.Initialize (Source, Backing, OK, Backing_Count => Backed); pragma Assert (OK);
            VM.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK); pragma Assert (OK);
            Epoch := VM.Revision (Source);
            pragma Assert (G.Inspect (Source, GPU, 4096).Status = G.Invalid_Range);
            Needed := G.Inspect_Offline (Source, GPU, 4096);
            pragma Assert (Needed.Topology.Status = G.Ready and Needed.Topology.Fits_Reserved);
            pragma Assert (Needed.Topology.Additional_Tables = Depth);
            pragma Assert (Needed.Additional_Backing = (if Depth > Backed - 4 then Depth - (Backed - 4) else 0));
            pragma Assert (VM.Revision (Source) = Epoch and VM.Used (Source) = 4);
            if Needed.Additional_Backing /= 0 then
               for I in 1 .. Needed.Additional_Backing loop
                  New_Pages (I) := 16#200000# + Unsigned_64 (I) * 4096;
               end loop;
               VM.Append_Offline_Backing (Source, New_Pages (1 .. Needed.Additional_Backing), First, OK);
               pragma Assert (OK and First = Backed + 1);
            end if;
            Needed := G.Inspect_Offline (Source, GPU, 4096);
            pragma Assert (Needed.Topology.Status = G.Ready and Needed.Additional_Backing = 0);
            VM.Map_Page (Source, GPU, 16#101000#, Write_Back, Read_Write, OK); pragma Assert (OK);
            pragma Assert (VM.Used (Source) = 4 + Depth);
            pragma Assert (VM.Lookup (Source, 4096) = Encode_Leaf (16#100000#, Write_Back, Read_Write));
            pragma Assert (G.Inspect_Offline (Source, GPU, 4096).Topology.Status = G.Occupied);
            VM.Seal (Source, OK); pragma Assert (OK);
            pragma Assert (G.Inspect_Offline (Source, GPU + 4096, 4096).Topology.Status = G.Invalid_Range);
            pragma Assert (G.Inspect (Source, GPU + 4096, 4096).Status = G.Ready);
            Cases := Cases + 1;
         end;
      end loop;
   end loop;
   declare
      package Small is new Intel_GPU_VM_Image (8, 4);
      package SG is new Small.Growth;
      use type SG.Plan_Status;
      Source : Small.Image;
      Needed : SG.Offline_Requirements;
      OK : Boolean;
   begin
      pragma Assert (SG.Inspect_Offline (Source, 4096, 4096).Topology.Status = SG.Invalid_Range);
      Small.Initialize (Source, [1 => 4096, 2 => 8192, 3 => 12288, 4 => 16384, others => 0],
        OK, Backing_Count => 4); pragma Assert (OK);
      Small.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK); pragma Assert (OK);
      Needed := SG.Inspect_Offline (Source, 2 ** 39, 4096);
      pragma Assert (Needed.Topology.Status = SG.Ready and not Needed.Topology.Fits_Reserved);
      pragma Assert (Needed.Additional_Backing = 3);
      pragma Assert (SG.Inspect_Offline (Source, 3, 4096).Topology.Status = SG.Invalid_Range);
      pragma Assert (SG.Inspect_Offline (Source, 4096, 0).Topology.Status = SG.Invalid_Range);
      pragma Assert (SG.Inspect_Offline (Source, 2 ** 48 - 4096, 8192).Topology.Status = SG.Invalid_Range);
      pragma Assert (SG.Inspect_Offline (Source, 4096, 8192).Topology.Status = SG.Occupied);
      Cases := Cases + 7;
   end;
   Ada.Text_IO.Put_Line ("Offline planning PASS" & Natural'Image (Cases) &
     ": reserved backing subtraction, metadata quota, immutable preflight, append/map composition, live/offline separation");
end VM_Offline_Planning_Tests;
