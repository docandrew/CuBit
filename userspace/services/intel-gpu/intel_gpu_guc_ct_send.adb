package body Intel_GPU_GuC_CT_Send is
   use Interfaces;
   function State (Object : Channel) return Phase is (Object.Value);
   procedure Initialize
     (Object : in out Channel; Registered : Boolean;
      Size : Intel_GPU_GuC_CTB.Ring_Size; Initial_Tail : Unsigned_32) is
   begin
      if Object.Value /= Uninitialized then return; end if;
      Object.Value := Broken;
      if not Registered or else Initial_Tail >= Size then return; end if;
      Object.Size := Size; Object.Tail := Initial_Tail; Object.Value := Active;
   end Initialize;
   procedure Send
     (Object : in out Channel; Payload : Words; Fence : Unsigned_16;
      Status : out Result)
   is
      Head, Tail, Descriptor_Status, Cursor : Unsigned_32;
      OK : Boolean;
      Plan : Intel_GPU_GuC_CTB.Plan;
      use type Intel_GPU_GuC_CTB.Result;
   begin
      Status := Rejected;
      if Object.Value /= Active or else Payload'Length not in 1 .. 255 then return; end if;
      Read_Descriptor (Head, Tail, Descriptor_Status, OK);
      if not OK then Object.Value := Broken; Status := Corrupt; return; end if;
      Plan := Intel_GPU_GuC_CTB.Send
        (Object.Size, Head, Tail, Object.Tail, Descriptor_Status,
         Unsigned_32 (Payload'Length), Fence);
      if Plan.State = Intel_GPU_GuC_CTB.Full then Status := Would_Block; return; end if;
      if Plan.State /= Intel_GPU_GuC_CTB.Ready then
         Object.Value := Broken; Status := Corrupt; return;
      end if;
      -- A failing callback may already have written. No rollback or replay.
      Object.Value := Broken; Status := Quarantined;
      Cursor := Plan.Start;
      Write_Word (Cursor, Intel_GPU_GuC_CTB.Header (Fence, Unsigned_32 (Payload'Length)), OK);
      if not OK then return; end if;
      Cursor := (Cursor + 1) mod Object.Size;
      for Value of Payload loop
         Write_Word (Cursor, Value, OK);
         if not OK then return; end if;
         Cursor := (Cursor + 1) mod Object.Size;
      end loop;
      Make_Visible (OK);
      if not OK then return; end if;
      Write_Tail (Plan.Next_Cursor, OK);
      if not OK then return; end if;
      Make_Visible (OK);
      if not OK then return; end if;
      Notify (OK);
      if not OK then return; end if;
      Object.Tail := Plan.Next_Cursor; Object.Value := Active; Status := Queued;
   end Send;
end Intel_GPU_GuC_CT_Send;
