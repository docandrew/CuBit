with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_ADLN_PPGTT; use Intel_GPU_ADLN_PPGTT;
with Intel_GPU_VM_Image;
with Intel_GPU_VM_Materialize;
procedure VM_Root_Stream_Tests is
   package VM is new Intel_GPU_VM_Image (8);
   type Page is array (Table_Index) of Unsigned_64;
   type Pages is array (Natural range 0 .. 9) of Page;
   Sentinel : constant Unsigned_64 := 16#A5A5A5A5A5A5A5A5#;
   RAM : Pages with Alignment => 4096, Volatile;
   Old_Image, New_Image : VM.Image;
   Old_DMA, New_DMA : VM.Backing_Pages;
   OK : Boolean;
   Calls, Owner_Calls, Fail_Flush, Fail_Owner : Natural := 0;
   Corrupt_Old : Boolean := False;
   Lookup_Calls, Reject_At, Revoke_At, Lookup_Count : Natural := 0;
   Revoked : Boolean := False;
   function Owner return Boolean is
   begin
      Owner_Calls := Owner_Calls + 1;
      return Owner_Calls /= Fail_Owner and then not Revoked;
   end Owner;
   function CPU (P : Natural) return Unsigned_64 is
     (Unsigned_64 (To_Integer (RAM (P)'Address)));
   function Flush (Address : Unsigned_64) return Boolean is
   begin
      Calls := Calls + 1;
      if Calls <= VM.Used (New_Image) then
         pragma Assert (Address = CPU (VM.Used (New_Image) + 1 - Calls));
         -- No hardware-root entry can change before all child backing is
         -- visible and verified (the candidate root is also staged).
         for I in Table_Index loop
            pragma Assert (RAM (0) (I) = VM.Entry_Value (Old_Image, 1, I));
         end loop;
      else
         pragma Assert (Address = CPU (0));
      end if;
      if Corrupt_Old and Calls = VM.Used (New_Image) then RAM (0) (0) := 42; end if;
      return Calls /= Fail_Flush;
   end Flush;
   package Writer is new Intel_GPU_VM_Materialize (VM, Owner, Flush);
   Backing : Writer.Mappings;
   function Lookup (Ordinal : Positive) return Writer.Page_Mapping is
      Item : Writer.Page_Mapping := Backing (Ordinal);
   begin
      pragma Assert (not Revoked);
      Lookup_Calls := Lookup_Calls + 1;
      if Lookup_Calls = Reject_At then Item.CPU := CPU (0); end if;
      if Lookup_Calls = Revoke_At then Revoked := True; end if;
      return Item;
   end Lookup;
   procedure Stream is new Writer.Publish_Update_From_Mappings (Lookup);
   procedure Publish (State : in out Writer.State; View : Writer.Mapping_View;
     Success : out Boolean) is
   begin
      if View'First /= 1 then Success := False; return; end if;
      Stream (State, Old_Image, New_Image, View'Length, (CPU (0), 4096), Success);
   end Publish;
   procedure Run (Expected : Boolean; Stale : Boolean := False) is
      State : Writer.State;
      Success : Boolean;
      Saved : Natural;
   begin
      RAM := [others => [others => Sentinel]];
      for I in Table_Index loop RAM (0) (I) := VM.Entry_Value (Old_Image, 1, I); end loop;
      if Stale then RAM (0) (0) := 42; end if;
      Calls := 0; Owner_Calls := 0; Lookup_Calls := 0; Revoked := False;
      Writer.Publish_Update (State, Old_Image, New_Image, Backing,
                             (CPU (0), 4096), Success);
      pragma Assert (Success = Expected);
      if Expected then
         pragma Assert (Calls = VM.Used (New_Image) + 1);
         for I in Table_Index loop
            pragma Assert (RAM (0) (I) = VM.Entry_Value (New_Image, 1, I));
         end loop;
      end if;
      for I in Table_Index loop pragma Assert (RAM (9) (I) = Sentinel); end loop;
      if Stale then pragma Assert (Calls = 0); end if;
      Saved := Owner_Calls;
      Writer.Publish_Update (State, Old_Image, New_Image, Backing,
                             (CPU (0), 4096), Success);
      pragma Assert (not Success and Owner_Calls = Saved);
   end Run;
   Owner_Count : Natural;
begin
   for P in VM.Page_Number loop
      Old_DMA (P) := Unsigned_64 (P) * 4096;
      New_DMA (P) := 16#100000# + Unsigned_64 (P) * 4096;
      Backing (P) := (CPU (P), New_DMA (P));
   end loop;
   VM.Initialize (Old_Image, Old_DMA, OK); pragma Assert (OK);
   VM.Map_Page (Old_Image, 4096, 16#200000#, Write_Back, Read_Write, OK);
   pragma Assert (OK); VM.Seal (Old_Image, OK); pragma Assert (OK);
   VM.Prepare_Update (New_Image, Old_Image, New_DMA, OK); pragma Assert (OK);
   VM.Map_Page (New_Image, 2 ** 39, 16#201000#, Write_Back, Read_Write, OK);
   pragma Assert (OK); VM.Seal (New_Image, OK); pragma Assert (OK);
   Run (True); Owner_Count := Owner_Calls;
   for Failure in 1 .. Owner_Count loop Fail_Owner := Failure; Run (False); end loop;
   Fail_Owner := 0;
   for Failure in 1 .. VM.Used (New_Image) + 1 loop
      Fail_Flush := Failure; Run (False);
   end loop;
   Fail_Flush := 0;
   Run (False, Stale => True);
   Corrupt_Old := True; Run (False); Corrupt_Old := False;
   Run (True); Lookup_Count := Lookup_Calls;
   for N in 1 .. Lookup_Count loop
      Reject_At := N; Run (False);
      pragma Assert (Lookup_Calls = N);
      for I in Table_Index loop
         pragma Assert (RAM (0) (I) = VM.Entry_Value (Old_Image, 1, I));
      end loop;
   end loop;
   Reject_At := 0;
   for N in 1 .. Lookup_Count loop
      Revoke_At := N; Run (False);
      pragma Assert (Lookup_Calls = N);
      for I in Table_Index loop
         pragma Assert (RAM (0) (I) = VM.Entry_Value (Old_Image, 1, I));
      end loop;
   end loop;
   Revoke_At := 0;
   Backing (8).CPU := CPU (0); -- unused backing is never accessed
   Run (True);
   Backing (1).CPU := CPU (0); Run (False);
   Ada.Text_IO.Put_Line ("Stable root publication PASS: child visibility first, old-root checks, root readback, all owner/flush failures, no retry (host RAM, not GPU)");
end VM_Root_Stream_Tests;
