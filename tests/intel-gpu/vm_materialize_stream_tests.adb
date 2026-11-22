with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Materialize;
procedure VM_Materialize_Stream_Tests is
   package VM is new Intel_GPU_VM_Image (8);
   type Page is array (Table_Index) of Unsigned_64;
   Sentinel : constant Unsigned_64 := 16#A5A5A5A5A5A5A5A5#;
   RAM : array (0 .. 5) of Page := [others => [others => Sentinel]]
     with Alignment => 4096, Volatile;
   Source : VM.Image;
   Held : Boolean := True;
   Lookups, Flushes, Revoke_At, Bad_At, Alias_At : Natural := 0;
   function Owner return Boolean is (Held);
   function Flush (CPU : Unsigned_64) return Boolean is
   begin
      Flushes := Flushes + 1;
      pragma Assert (Held and CPU = Unsigned_64 (To_Integer (RAM (5 - Flushes)'Address)));
      return True;
   end Flush;
   package Writer is new Intel_GPU_VM_Materialize (VM, Owner, Flush);
   function Lookup (Ordinal : Positive) return Writer.Page_Mapping is
      Result : Writer.Page_Mapping;
   begin
      pragma Assert (Held and Ordinal <= 4);
      Lookups := Lookups + 1;
      Result := (Unsigned_64 (To_Integer (RAM (Ordinal)'Address)), Unsigned_64 (Ordinal) * 4096);
      if Lookups = Revoke_At then Held := False; end if;
      if Lookups = Bad_At then Result.DMA := 16#BAD000#; end if;
      if Ordinal = Alias_At then Result.CPU := Unsigned_64 (To_Integer (RAM (1)'Address)); end if;
      return Result;
   end Lookup;
   procedure Prepare is new Writer.Prepare_From_Mappings (Lookup);
   OK : Boolean;
   procedure Run (Expected : Boolean; Count : Natural := 4; Untouched : Boolean := False) is
      State : Writer.State;
      Root : Unsigned_64;
      Calls : Natural;
   begin
      RAM := [others => [others => Sentinel]];
      Held := True; Lookups := 0; Flushes := 0;
      Prepare (State, Source, Count, Root, OK);
      pragma Assert (OK = Expected and Root = (if Expected then 4096 else 0));
      for I in Table_Index loop
         pragma Assert (RAM (0) (I) = Sentinel and RAM (5) (I) = Sentinel);
      end loop;
      if Expected then
         pragma Assert (Flushes = 4);
         for P in 1 .. 4 loop
            for I in Table_Index loop pragma Assert (RAM (P) (I) = VM.Entry_Value (Source, P, I)); end loop;
         end loop;
      elsif Untouched then
         pragma Assert (Flushes = 0);
         for P in 1 .. 4 loop
            for I in Table_Index loop pragma Assert (RAM (P) (I) = Sentinel); end loop;
         end loop;
      end if;
      Calls := Lookups;
      Prepare (State, Source, Count, Root, OK);
      pragma Assert (not OK and Root = 0 and Lookups = Calls);
   end Run;
   Bound : Natural;
begin
   VM.Initialize (Source, [4096, 8192, 12288, 16384, others => 0], OK, Backing_Count => 4);
   pragma Assert (OK);
   VM.Map_Page (Source, 4096, 16#100000#, Write_Back, Read_Write, OK); pragma Assert (OK);
   VM.Seal (Source, OK); pragma Assert (OK);
   Run (True); Bound := Lookups;
   pragma Assert (Bound = 22);
   for N in 0 .. 3 loop Run (False, N, True); pragma Assert (Lookups = 0); end loop;
   Alias_At := 4; Run (False, Untouched => True); Alias_At := 0;
   for N in 1 .. Bound loop
      Revoke_At := N; Run (False, Untouched => N <= 11);
      pragma Assert (Lookups = N);
   end loop;
   Revoke_At := 0;
   for N in 1 .. Bound loop
      Bad_At := N; Run (False, Untouched => N <= 11);
      pragma Assert (Lookups = N);
   end loop;
   Bad_At := 0; Run (True);
   Ada.Text_IO.Put_Line ("Streamed materializer PASS: 22 lookup revocations, 22 bad DMA results, alias/short admission, guards, no retry");
end VM_Materialize_Stream_Tests;
