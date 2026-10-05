with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Growth;
with Intel_GPU_VM_Image.Growth.Backing;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_PPGTT_Scratch;
procedure VM_Growth_Backing_Tests is
   package VM is new Intel_GPU_VM_Image (8);
   package G is new VM.Growth;
   Denied : Unsigned_64 := 0;
   function Owned (DMA : Unsigned_64) return Boolean is (DMA /= Denied);
   package B is new G.Backing (Owned);
   Root : constant Unsigned_64 := 16#90000#;
begin
   for Scratch_On in Boolean loop
      declare
         Source : VM.Image;
         DMA : VM.Backing_Pages;
         Scratch : Intel_GPU_PPGTT_Scratch.Backing_Pages := [others => 0];
         OK : Boolean;
      begin
         for P in DMA'Range loop DMA (P) := Unsigned_64 (P) * 4096; end loop;
         if Scratch_On then
            for L in Scratch'Range loop Scratch (L) := 16#20000# + Unsigned_64 (L) * 4096; end loop;
         end if;
         VM.Initialize (Source, DMA, OK, Scratch); pragma Assert (OK);
         VM.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK); pragma Assert (OK);
         VM.Seal (Source, OK); pragma Assert (OK);
         for Depth in 1 .. 3 loop
            declare
               Pages : VM.Data_Pages (5 .. 4 + Depth);
               Output : B.Links (7 .. 6 + Depth);
               GPU : constant Unsigned_64 := 2 ** (12 + 9 * Depth);
            begin
               for I in Pages'Range loop Pages (I) := 16#40000# + Unsigned_64 (I) * 4096; end loop;
               B.Resolve (Source, GPU, 4096, Root, Pages, Output, OK);
               pragma Assert (OK);
               for N in 1 .. Depth loop
                  pragma Assert (Output (6 + N).Parent_DMA =
                    (if N > 1 then Pages (3 + N)
                     elsif Depth = 3 then Root else DMA (4 - Depth)));
                  pragma Assert (Output (6 + N).Child_DMA = Pages (4 + N));
                  pragma Assert (Output (6 + N).Value = Encode_Directory (Pages (4 + N)));
                  pragma Assert (Output (6 + N).Expected = Intel_GPU_PPGTT_Scratch.Fallback (Scratch, Depth - N + 1));
                  pragma Assert (Output (6 + N).Fill = Intel_GPU_PPGTT_Scratch.Fallback (Scratch, Depth - N));
               end loop;
               -- Root, used/reserved tables, mapped data, scratch, malformed,
               -- duplicate and unauthenticated pages all reject before output.
               for Fault in 1 .. 9 loop
                  declare
                     Bad : VM.Data_Pages := Pages;
                  begin
                     case Fault is
                        when 1 => Bad (5) := Root;
                        when 2 => Bad (5) := DMA (1);
                        when 3 => Bad (5) := DMA (8);
                        when 4 => Bad (5) := 16#100000#;
                        when 5 => Bad (5) := Scratch (0);
                        when 6 => Bad (5) := 123;
                        when 7 => Denied := Bad (5);
                        when 8 => Denied := Root;
                        when 9 =>
                           if Depth > 1 then Bad (6) := Bad (5);
                           else Bad (5) := 2 ** 32; end if;
                     end case;
                     B.Resolve (Source, GPU, 4096, Root, Bad, Output, OK);
                     pragma Assert (not OK);
                     pragma Assert (for all Item of Output => Item.Parent_DMA = 0 and Item.Value = 0);
                     Denied := 0;
                  end;
               end loop;
            end;
         end loop;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Growth backing PASS: 6 plans/54 rejection cases; retained root, scratch fallback, aliases and ownership (no hardware writes)");
end VM_Growth_Backing_Tests;
