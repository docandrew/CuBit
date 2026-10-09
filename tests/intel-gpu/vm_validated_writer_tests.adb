with Ada.Text_IO;
with Interfaces; use Interfaces;
with Interfaces.C;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Image.Insertion;
with Intel_GPU_VM_Image.Test_Mutation;
with Intel_GPU_Application_Image;
with Intel_GPU_Application_Image.Updates;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Submission_Image;
procedure VM_Validated_Writer_Tests is
   function Mmap (Address : System.Address; Length : Interfaces.C.size_t;
                  Protection, Flags, FD : Interfaces.C.int;
                  Offset : Interfaces.C.long) return System.Address
     with Import, Convention => C, External_Name => "mmap";
   function Munmap (Address : System.Address; Length : Interfaces.C.size_t)
     return Interfaces.C.int with Import, Convention => C, External_Name => "munmap";
   use type Interfaces.C.int;
   use type Interfaces.C.size_t;
   Base : constant Unsigned_64 := 16#7000_0000_0000#;
   Span : constant := 16#40000#;
   Mapping : System.Address;
begin
   Mapping := Mmap (To_Address (Integer_Address (Base)), 2 * Span, 3, 16#100022#, -1, 0);
   pragma Assert (Mapping = To_Address (Integer_Address (Base)));
   for Fault in 0 .. 12 loop
      declare
         package VM is new Intel_GPU_VM_Image (5);
         package Mutation is new VM.Test_Mutation;
         Source : VM.Image;
         Held : Boolean := True;
         Armed, OK : Boolean := False;
         Flushes : Natural := 0;
         function Owner return Boolean is (Held);
         function Flush (CPU : Unsigned_64) return Boolean is
         begin
            pragma Assert (CPU >= Base and CPU < Base + 2 * Span);
            if Armed then
               Flushes := Flushes + 1;
               if Fault = 7 then return False; end if;
               if Fault = 8 then Held := False; end if;
               if Fault = 10 then Mutation.Change_Revision (Source); end if;
            end if;
            return True;
         end Flush;
         package App is new Intel_GPU_Application_Image (VM, Owner, Flush);
         package Updates is new App.Updates (Owner);
         Writer : App.State;
         Pages : constant VM.Backing_Pages :=
           [16#4000000#, 16#4001000#, 16#4002000#, 16#4003000#, 16#4004000#];
         Backing : App.Tables.Mappings;
         type Leaf_Words is array (Table_Index) of Unsigned_64;
         Leaf : Leaf_Words with Import, Volatile,
           Address => To_Address (Integer_Address (Base + Span + 3 * 4096));
         procedure Write_Leaf
           (Table_DMA : Unsigned_64; Index : Table_Index;
            Expected, Replacement : Unsigned_64; Success : out Boolean);
         procedure Invalidate (Success : out Boolean) is
         begin Success := True; end Invalidate;
         package Insert is new VM.Insertion (Owner, Write_Leaf, Invalidate);
         Receipt, Other : Insert.Controller;
         procedure Write_Checked is new Updates.Insert_Validated_Leaf (Insert);
         procedure Write_Leaf
           (Table_DMA : Unsigned_64; Index : Table_Index;
            Expected, Replacement : Unsigned_64; Success : out Boolean) is
            M : App.Tables.Page_Mapping := Backing (4);
            DMA : Unsigned_64 := Table_DMA;
            PTE : Unsigned_64 := Replacement;
            Slot : Table_Index := Index;
         begin
            case Fault is
               when 1 => PTE := 16#5002003#;
               when 2 => Slot := Index + 1;
               when 3 => DMA := Pages (3);
               when 4 => M.DMA := Pages (3);
               when 5 => M.CPU := Base;
               when 11 => PTE := Pages (5) + 3;
               when others => null;
            end case;
            if Fault = 12 then
               Write_Checked (Writer, Source, Other, M, DMA, Slot, Expected, PTE, Success);
            else
               Write_Checked (Writer, Source, Receipt, M, DMA, Slot, Expected, PTE, Success);
            end if;
         end Write_Leaf;
      begin
         for P in VM.Page_Number loop
            Backing (P) := (Base + Span + Unsigned_64 (P - 1) * 4096, Pages (P));
         end loop;
         VM.Initialize (Source, Pages, OK); pragma Assert (OK);
         VM.Map_Page (Source, 4096, 16#5000000#, Write_Back, Read_Write, OK);
         pragma Assert (OK);
         VM.Seal (Source, OK); pragma Assert (OK);
         App.Prepare (Writer, Source, Backing (1 .. 4),
           Intel_GPU_Buffer_Reply.From_Linear (16#1000000#, Base, Span, 16#1000000#),
           16#2000000#, Intel_GPU_Submission_Image.GGTT_Bytes, OK);
         pragma Assert (OK and Leaf (2) = 0);
         Armed := True;
         if Fault = 6 then Leaf (2) := 16#BAD0003#; end if;
         if Fault = 9 then
            Write_Checked (Writer, Source, Receipt, Backing (4), Pages (4), 2, 0, 16#5001003#, OK);
            pragma Assert (not OK and Leaf (2) = 0 and Flushes = 0);
         end if;
         Insert.Publish (Receipt, Source, VM.Revision (Source), 8192,
           [16#5001000#], Write_Back, Read_Write, OK);
         pragma Assert (OK = (Fault = 0));
         pragma Assert (Updates.Failed (Writer) = (Fault /= 0));
         pragma Assert (VM.Lookup (Source, 8192) = 0); -- no metadata commit here
         pragma Assert (Leaf (2) =
           (if Fault in 0 | 7 | 8 | 10 then 16#5001003#
            elsif Fault = 6 then 16#BAD0003# else 0));
         pragma Assert (Flushes = (if Fault in 0 | 7 | 8 | 10 then 1 else 0));
         if Fault /= 0 then
            Held := True; Armed := False; Leaf (2) := 0;
            Write_Checked (Writer, Source, Receipt, Backing (4), Pages (4), 2, 0, 16#5001003#, OK);
            pragma Assert (not OK and Leaf (2) = 0);
         end if;
      end;
   end loop;
   pragma Assert (Munmap (Mapping, 2 * Span) = 0);
   Ada.Text_IO.Put_Line ("validated insertion writer PASS13: exact receipt, mismatched word/index/table/mapping, CPU overlap, stale hardware, flush/owner/revision loss, out-of-scope and foreign receipt; sticky failure (host RAM, mock flush)");
end VM_Validated_Writer_Tests;
