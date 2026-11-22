with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Growth;
with Intel_GPU_VM_Image.Growth.Backing;
with Intel_GPU_VM_Image.Growth.Backing.Writer;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
procedure VM_Growth_Ownership_Tests is
   package VM is new Intel_GPU_VM_Image (8);
   package G is new VM.Growth;
   Total : Natural := 0;
begin
   -- First measure every provenance check on the successful path. Then revoke
   -- exclusion INSIDE each one, returning the old positive ownership result.
   -- No hardware callback is allowed after that revocation, even once.
   for Trial in 0 .. 3 loop
      for Fault in 0 .. (if Trial = 0 then 0 elsif Trial = 1 then Total else 3087) loop
         declare
            Source : VM.Image;
            DMA : VM.Backing_Pages;
            type Words is array (Table_Index) of Unsigned_64;
            RAM : array (1 .. 12) of Words := [others => [others => 0]];
            Held : Boolean := True;
            Checks, IOs : Natural := 0;
            function Exclusive return Boolean is (Held);
            function Owned (Address : Unsigned_64) return Boolean is
            begin
               Checks := Checks + 1;
               if Trial = 1 and then Fault /= 0 and then Checks = Fault then Held := False; end if;
               return Address in 4096 .. 12 * 4096 and then Address mod 4096 = 0;
            end Owned;
            procedure Read_Word (Address : Unsigned_64; Index : Table_Index;
                                 Value : out Unsigned_64; OK : out Boolean) is
            begin
               pragma Assert (Held, "read after ownership revocation");
               IOs := IOs + 1;
               Value := RAM (Natural (Address / 4096)) (Index); OK := True;
            end Read_Word;
            procedure Write_Word (Address : Unsigned_64; Index : Table_Index;
                                  Value : Unsigned_64; OK : out Boolean) is
            begin
               pragma Assert (Held, "write after ownership revocation");
               IOs := IOs + 1;
               RAM (Natural (Address / 4096)) (Index) := Value; OK := True;
            end Write_Word;
            function Flush (Address : Unsigned_64) return Boolean is
            begin
               pragma Assert (Held, "flush after ownership revocation");
               IOs := IOs + 1;
               return Address mod 4096 = 0;
            end Flush;
            package B is new G.Backing (Owned);
            package W is new B.Writer (Exclusive, Read_Word, Write_Word, Flush, Exclusive);
            procedure Publish (Object : in out W.State; Source : in out VM.Image;
              GPU, Bytes, Root : Unsigned_64; Pages : VM.Data_Pages; OK : out Boolean) is
               Previous : Natural;
            begin
               Previous := IOs;
               W.Start (Object, Source, GPU, Bytes, Root, Pages, OK);
               pragma Assert (IOs = Previous);
               if not OK then return; end if;
               while W.Pending (Object) loop
                  Previous := IOs;
                  if Fault /= 0 and then IOs = Fault - 1 then
                     if Trial = 2 then
                        Held := False; -- authority disappears while yielded
                     elsif Trial = 3 then
                        W.Commit (Object, Source, OK); -- premature adoption
                        pragma Assert (not OK and not W.Pending (Object));
                     end if;
                  end if;
                  W.Step (Object, Source);
                  pragma Assert (IOs - Previous <= 1);
               end loop;
               OK := W.Published (Object);
            end Publish;
            Receipt : W.State;
            OK : Boolean;
            Before : Natural;
         begin
            for P in DMA'Range loop DMA (P) := Unsigned_64 (P) * 4096; end loop;
            VM.Initialize (Source, DMA, OK); pragma Assert (OK);
            VM.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK);
            pragma Assert (OK);
            VM.Seal (Source, OK); pragma Assert (OK);
            for P in 1 .. VM.Used (Source) loop
               for I in Table_Index loop RAM (P) (I) := VM.Entry_Value (Source, P, I); end loop;
            end loop;
            RAM (9) := RAM (1);
            Publish (Receipt, Source, 2 ** 39, 4096, 9 * 4096,
                       [10 * 4096, 11 * 4096, 12 * 4096], OK);
            pragma Assert (OK = (Fault = 0));
            if Trial = 0 then Total := Checks; end if;
            pragma Assert (VM.Used (Source) = 4);
            Before := IOs;
            if Fault /= 0 then
               -- Restoring authority cannot replay an uncertain transaction.
               Held := True;
               Publish (Receipt, Source, 2 ** 39, 4096, 9 * 4096,
                          [10 * 4096, 11 * 4096, 12 * 4096], OK);
               pragma Assert (not OK and IOs = Before);
               W.Commit (Receipt, Source, OK);
               pragma Assert (not OK and VM.Used (Source) = 4);
            end if;
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Growth provenance callback revocation PASS" & Natural'Image (Total) &
     " boundaries; yield revocation/early commit PASS6174 (host RAM, no GPU validation)");
end VM_Growth_Ownership_Tests;
