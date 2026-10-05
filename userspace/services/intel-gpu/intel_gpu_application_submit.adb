package body Intel_GPU_Application_Submit is
   use type System.Address;
   function Receipt_Confirmed (Object : State; Receipt : Completion_Receipt)
      return Boolean is
     (Receipt.Attempted and then Receipt.Origin = Object'Address and then
      Completion_Confirmed (Object, Receipt.Sequence));
   function Receipt_Sequence (Object : State; Receipt : Completion_Receipt)
      return Unsigned_32 is
     (if Receipt_Confirmed (Object, Receipt) then Receipt.Sequence else 0);
   procedure Execute_With_Receipt
     (Object : in out State; Handle, GPU, Offset, Bytes : Unsigned_64;
      Receipt : in out Completion_Receipt; Status : out Result) is
      Sequence : Unsigned_32;
   begin
      Status := Rejected;
      if Receipt.Attempted then return; end if;
      Receipt.Attempted := True;
      Execute (Object, Handle, GPU, Offset, Bytes, Status, Sequence);
      if Status = Complete then
         Receipt.Origin := Object'Address;
         Receipt.Sequence := Sequence;
      end if;
   end Execute_With_Receipt;
   function Current (Object : State) return Phase is (Object.Value);
   function Last_Completed (Object : State) return Unsigned_32 is (Object.Completed);
   function Completion_Confirmed (Object : State; Sequence : Unsigned_32)
      return Boolean is
     (Object.Value = Idle and then Sequence > 1 and then Sequence <= Object.Completed);
   procedure Initialize (Object : in out State; Setup_Complete : Boolean) is
   begin
      if Object.Value /= Uninitialized then return; end if;
      if Setup_Complete and then Owner_Ready then
         Object.Completed := 1;
         Object.Value := Idle;
      else
         Object.Value := Failed;
         Quarantine;
      end if;
   end Initialize;
   procedure Execute
     (Object : in out State; Handle, GPU, Offset, Bytes : Unsigned_64;
      Status : out Result; Completion : out Unsigned_32) is
      OK : Boolean;
      Next : Unsigned_32;
      Attempt : Completion_Attempt;
      procedure Fail is
      begin
         Object.Value := Failed;
         Status := Faulted;
         Quarantine;
      end Fail;
      function Still_Ready return Boolean is
      begin
         if not OK or else not Owner_Ready then Fail; return False; end if;
         return True;
      end Still_Ready;
   begin
      Status := Rejected; Completion := 0;
      if Object.Value /= Idle then return; end if;
      -- Ownership checks can dispatch events just like BO validation. Close
      -- nested admission before the first callback, not only before Batch.
      Object.Value := Checking;
      if not Owner_Ready then Fail; return; end if;
      if Object.Completed = Unsigned_32'Last then
         Object.Value := Failed; Status := Exhausted; Quarantine; return;
      end if;
      -- Do not narrow wire fields or allow wrapping/unaligned batch extents.
      if Handle = 0 or else Handle > Unsigned_64 (Unsigned_32'Last) or else
        GPU = 0 or else GPU >= 2 ** 48 or else GPU mod 8 /= 0 or else
        Offset mod 8 /= 0 or else Bytes = 0 or else Bytes mod 4 /= 0 or else
        Bytes > 16 * 1024 * 1024 or else Offset > 16 * 1024 * 1024 - Bytes or else
        Bytes > 2 ** 48 - GPU
      then Object.Value := Idle; return; end if;
      -- Validation is also inside the serialized operation. A callback may
      -- service events; it must not expose Idle to a nested submission before
      -- this attempt has armed its completion state.
      if not Batch_Ready (Handle, GPU, Offset, Bytes) then
         if not Owner_Ready then Fail;
         else Object.Value := Idle; Status := Batch_Denied; end if;
         return;
      end if;
      if not Owner_Ready then Fail; return; end if;
      Object.Value := Executing;
      Next := Object.Completed + 1;
      Arm (Attempt, Object.Completed, Next, OK);
      if not Still_Ready then return; end if;
      Enable (OK);
      if not Still_Ready then return; end if;
      Publish (GPU, Next, OK);
      if not Still_Ready then return; end if;
      Notify (OK);
      if not Still_Ready then return; end if;
      Wait_Completion (Attempt, OK);
      if not Still_Ready then return; end if;
      Disable (OK);
      if not Still_Ready then return; end if;
      Object.Completed := Next;
      Object.Value := Idle;
      Completion := Next;
      Status := Complete;
   end Execute;
end Intel_GPU_Application_Submit;
