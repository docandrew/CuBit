with System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_DMA_Cache;
with Intel_GPU_Ring_Registers;
package body Intel_GPU_Native_Queue_Ring is
   package RR renames Intel_GPU_Ring_Reservation;
   use type RR.Outcome;

   Page_Bytes : constant Unsigned_64 := 4_096;
   Canonical_Limit : constant Unsigned_64 := 2 ** 47;
   MI_NOOP : constant Unsigned_32 := 0;

   procedure Barrier is
   begin
      System.Machine_Code.Asm ("mfence", Clobber => "memory", Volatile => True);
   end Barrier;

   procedure Write
     (Words : Word_Array; Count : Natural; Plan : RR.Plan;
      Expected_Tail : Unsigned_32; Status : out Report)
   is
      package Registers renames Intel_GPU_Ring_Registers;
      Base : constant Unsigned_64 := CPU_Base;
      Bytes : constant Unsigned_64 := Backing_Bytes;
      Ring_Base : Unsigned_64;

      function Owned return Boolean is
        (CPU_Base = Base and then Backing_Bytes = Bytes and then Owner_Ready and then
         Coherent_Ready);

      procedure Store (Offset, Value : Unsigned_32) is
         Word : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Ring_Base + Unsigned_64 (Offset)));
      begin
         Word := Value;
      end Store;

      -- Only command-ring pages: the GPU reads them but never writes them.
      -- The concurrently GPU-written saved-context page is never flushed.
      function Publish (Offset, Length : Unsigned_32) return Boolean is
         First : constant Unsigned_64 := Unsigned_64 (Offset) / Page_Bytes * Page_Bytes;
         Last : constant Unsigned_64 :=
           (Unsigned_64 (Offset) + Unsigned_64 (Length) + Page_Bytes - 1) / Page_Bytes * Page_Bytes;
      begin
         return Intel_GPU_DMA_Cache.Flush_Range (Ring_Base + First, Last - First);
      end Publish;
   begin
      Status := (Result => Bad_Request, Raw_Head => 0, Raw_Tail => 0);
      if Base = 0 or else Base mod Page_Bytes /= 0 or else Bytes < Minimum_Backing or else
        Base >= Canonical_Limit or else Bytes > Canonical_Limit - Base or else
        Plan.Status /= RR.Ready or else Count = 0 or else Count > Max_Words or else
        Plan.Consumed /= Plan.Padding + Unsigned_32 (Count) * 4 or else
        Expected_Tail >= RR.Ring_Bytes or else Expected_Tail mod 8 /= 0 or else
        Plan.Tail >= RR.Ring_Bytes or else Plan.Tail /= Plan.Start + Unsigned_32 (Count) * 4 or else
        (Plan.Padding /= 0 and then
           (Plan.Start /= 0 or else Plan.Padding /= RR.Ring_Bytes - Expected_Tail)) or else
        (Plan.Padding = 0 and then Plan.Start /= Expected_Tail)
      then
         return;
      elsif not Owned then
         Status.Result := Not_Owned;
         return;
      end if;
      Ring_Base := Base + Ring_Offset;
      Barrier;
      declare
         Saved_Head : constant Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Base + Saved_Head_Offset));
         Saved_Tail : constant Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Base + Saved_Tail_Offset));
      begin
         Status.Raw_Head := Saved_Head;
         Status.Raw_Tail := Saved_Tail;
      end;
      -- The window is the truth; the saved tail's offset field must agree.
      if Registers.Offset (Registers.Decode_Tail (Status.Raw_Tail)) /= Expected_Tail then
         Status.Result := Tail_Mismatch;
         return;
      end if;
      if Plan.Padding /= 0 then
         for I in 0 .. Plan.Padding / 4 - 1 loop
            Store (Expected_Tail + I * 4, MI_NOOP);
         end loop;
         if not Publish (Expected_Tail, Plan.Padding) then
            Status.Result := Flush_Failed;
            return;
         elsif not Owned then
            Status.Result := Ownership_Lost;
            return;
         end if;
      end if;
      for I in 0 .. Count - 1 loop
         Store (Plan.Start + Unsigned_32 (I) * 4, Words (I));
      end loop;
      if not Publish (Plan.Start, Unsigned_32 (Count) * 4) then
         Status.Result := Flush_Failed;
         return;
      elsif not Owned then
         Status.Result := Ownership_Lost;
         return;
      end if;
      Barrier;
      declare
         Saved : Unsigned_32 with Import, Volatile_Full_Access,
           Address => To_Address (Integer_Address (Base + Saved_Tail_Offset));
      begin
         Saved := Plan.Tail;
      end;
      -- ADL-N coherent system memory; the CT publication of the kick adds
      -- another barrier before its descriptor tail.
      Barrier;
      Status.Result := (if Owned then Written else Ownership_Lost);
   end Write;
end Intel_GPU_Native_Queue_Ring;
