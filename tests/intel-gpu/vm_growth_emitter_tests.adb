with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Growth;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
procedure VM_Growth_Emitter_Tests is
   package VM is new Intel_GPU_VM_Image (8);
   package G is new VM.Growth;
begin
   for Fault in 0 .. 7 loop
      declare
         Source : VM.Image;
         Held : Boolean := True;
         Calls, Count : Natural := 0;
         OK : Boolean;
         Epoch : Unsigned_64;
         function Authorized return Boolean is (Held);
         procedure Emit (Ordinal : Positive; Item : G.Node; Accepted : out Boolean) is
         begin
            Calls := Calls + 1;
            pragma Assert (Calls = Ordinal and Ordinal <= 3);
            pragma Assert (Item.Existing_Parent = (if Ordinal = 1 then 1 else 0));
            pragma Assert (Item.New_Parent = (if Ordinal = 1 then 0 else Ordinal - 1));
            pragma Assert (Natural (Item.Level) = 3 - Ordinal);
            Accepted := not (Fault in 1 .. 3 and then Calls = Fault);
            if Fault in 4 .. 6 and then Calls = Fault - 3 then Held := False; end if;
         end Emit;
         procedure Describe is new G.Describe_Into (Authorized, Emit);
      begin
         VM.Initialize (Source, [4096, 8192, 12288, 16384, others => 0], OK,
           Backing_Count => 4); pragma Assert (OK);
         VM.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK);
         pragma Assert (OK);
         VM.Seal (Source, OK); pragma Assert (OK);
         Epoch := VM.Revision (Source);
         Describe (Source, 2 ** 39, 4096, (if Fault = 7 then 2 else 3), Count, OK);
         pragma Assert (OK = (Fault = 0) and Count = (if Fault = 0 then 3 else 0));
         pragma Assert (Calls = (case Fault is when 0 => 3, when 1 .. 3 => Fault,
           when 4 .. 6 => Fault - 3, when others => 0));
         pragma Assert (VM.Revision (Source) = Epoch and VM.Used (Source) = 4);
         pragma Assert (VM.Lookup (Source, 4096) = Encode_Leaf (16#100000#, Write_Back, Read_Write));
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Topology emitter PASS8: ordered parent references, callback rejection/owner loss at each node, capacity preflight, immutable source");
end VM_Growth_Emitter_Tests;
