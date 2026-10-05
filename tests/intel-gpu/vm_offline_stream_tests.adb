with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_VM_Image;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
procedure VM_Offline_Stream_Tests is
   package VM is new Intel_GPU_VM_Image (8);
begin
   for Fault in 0 .. 4 loop
      declare
         Source : VM.Image;
         Held, Retry : Boolean := True;
         Calls, First : Natural := 0;
         Epoch : Unsigned_64;
         OK : Boolean;
         function Authorized return Boolean is (Held);
         function Read_Page (Ordinal : Positive) return Unsigned_64 is
         begin
            Calls := Calls + 1;
            pragma Assert (Calls = Ordinal and VM.Revision (Source) = Epoch);
            pragma Assert (VM.Table_Backing_DMA (Source, 5) = 0);
            if not Retry and Ordinal = 3 then
               case Fault is
                  when 1 => Held := False;
                  when 2 => return 4096;
                  when 3 => return 16#300000#;
                  when 4 => return 0;
                  when others => null;
               end case;
            end if;
            return 16#300000# + Unsigned_64 (Ordinal - 1) * 4096;
         end Read_Page;
         procedure Append is new VM.Append_Offline_From_Pages (Read_Page, Authorized);
      begin
         VM.Initialize (Source, [4096, 8192, 12288, 16384, others => 0], OK,
           Backing_Count => 4); pragma Assert (OK);
         VM.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK);
         pragma Assert (OK);
         Epoch := VM.Revision (Source);
         Retry := False;
         Append (Source, 0, First, OK);
         pragma Assert (not OK and First = 0 and Calls = 0);
         Append (Source, 5, First, OK); -- insufficient metadata, no callback
         pragma Assert (not OK and Calls = 0);
         Append (Source, 3, First, OK);
         pragma Assert (Calls = 3 and OK = (Fault = 0));
         if not OK then
            pragma Assert (First = 0 and VM.Revision (Source) = Epoch);
            pragma Assert (VM.Used (Source) = 4 and VM.Table_Backing_DMA (Source, 5) = 0);
            Held := True; Retry := True; Calls := 0;
            Append (Source, 3, First, OK);
            pragma Assert (OK and Calls = 3);
         end if;
         pragma Assert (First = 5 and VM.Revision (Source) = Epoch + 1);
         pragma Assert (VM.Used (Source) = 4 and VM.Table_Backing_DMA (Source, 7) = 16#302000#);
         VM.Map_Page (Source, 2 ** 39, 16#101000#, Write_Back, Read_Write, OK);
         pragma Assert (OK and VM.Used (Source) = 7);
         pragma Assert (VM.Lookup (Source, 4096) = Encode_Leaf (16#100000#, Write_Back, Read_Write));
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Offline stream PASS5: read-once staging, atomic prefix/epoch admission, callback loss/alias/duplicate/zero rejection and retry (host metadata)");
end VM_Offline_Stream_Tests;
