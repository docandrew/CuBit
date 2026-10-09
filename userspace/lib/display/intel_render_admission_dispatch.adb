with CuBit.Process_IDs;
package body Intel_Render_Admission_Dispatch is
   package Native renames Intel_Render_Admission_Native;
   package Core renames Intel_Render_Admission;
   package Grants renames CuBit.Capability_Grants;
   use type Core.Phase;
   use type CuBit.Messages.CapabilitySlot;

   function State (Object : Dispatcher; ID : Ticket) return Core.Phase is
     (if ID = 0 or else ID > Object.Used then Core.Failed
      else Native.State (Object.Entries (ID).Request));

   function Runnable (Object : Dispatcher) return Boolean is
   begin
      for I in 1 .. Object.Used loop
         if State (Object, I) in Core.Reserve_Ready | Core.Delegate_Ready |
           Core.Activate_Ready | Core.Abort_Ready then
            return True;
         end if;
      end loop;
      return False;
   end Runnable;

   function Next_Deadline (Object : Dispatcher) return Unsigned_64 is
      Earliest : Unsigned_64 := Unsigned_64'Last;
   begin
      for I in 1 .. Object.Used loop
         if not Object.Entries (I).Deadline_Observed and then
           State (Object, I) in Core.Reserve_Ready | Core.Reserve_Pending |
             Core.Delegate_Ready | Core.Activate_Ready | Core.Activate_Pending
         then
            Earliest := Unsigned_64'Min (Earliest, Object.Entries (I).Deadline);
         end if;
      end loop;
      return Earliest;
   end Next_Deadline;

   procedure Cancel (Object : in out Dispatcher; ID : Ticket) is
   begin
      if ID /= 0 and then ID <= Object.Used then
         Object.Entries (ID).Deadline_Observed := True;
         Native.Cancel (Object.Entries (ID).Request);
      end if;
   end Cancel;

   procedure Start
     (Object : in out Dispatcher; Target : Grants.Recipient;
      Source, Application_Source, Destination : CuBit.Messages.CapabilitySlot;
      Now, Deadline : Unsigned_64; ID : out Ticket) is
   begin
      ID := 0;
      if not Grants.Valid (Target) or Object.Clock_Failed or
        Object.Tokens_Exhausted or Object.Used = Capacity or
        Now < Object.Last_Time or Deadline <= Now or First_Token = 0 or
        Last_Token < First_Token or Last_Token = Unsigned_64'Last
      then return; end if;
      -- Reserve, Activate and Abort each own one unique token. Delegation
      -- is a syscall, not IPC. Never admit work that could exhaust cleanup.
      if Last_Token - Object.Next_Token < 2 then return; end if;
      for I in 1 .. Object.Used loop
         if Object.Entries (I).Identity = CuBit.Process_IDs.To_Word (Grants.Incarnation (Target)) and
           Object.Entries (I).Destination = Destination
         then return; end if;
      end loop;
      Object.Used := Object.Used + 1;
      ID := Object.Used;
      Object.Last_Time := Now;
      Object.Entries (ID).Identity := CuBit.Process_IDs.To_Word (Grants.Incarnation (Target));
      Object.Entries (ID).Destination := Destination;
      Object.Entries (ID).Deadline := Deadline;
      Object.Entries (ID).Token_Base := Object.Next_Token;
      if Last_Token - Object.Next_Token < 5 then
         Object.Tokens_Exhausted := True;
      else
         Object.Next_Token := Object.Next_Token + 3;
      end if;
      Native.Start (Object.Entries (ID).Request, Target, Source,
                    Application_Source, Destination);
   end Start;

   procedure Observe_Time (Object : in out Dispatcher; Now : Unsigned_64) is
      Phase : Core.Phase;
   begin
      Object.Clock_Failed := Object.Clock_Failed or Now < Object.Last_Time;
      Object.Last_Time := Now;
      for J in 1 .. Object.Used loop
         Phase := State (Object, J);
         if not Object.Entries (J).Deadline_Observed and then
           (Object.Clock_Failed or else
            (Now >= Object.Entries (J).Deadline and Phase /= Core.Active))
         then Cancel (Object, J); end if;
      end loop;
   end Observe_Time;

   procedure Step (Object : in out Dispatcher; Now : Unsigned_64) is
      I : Positive;
   begin
      Observe_Time (Object, Now);
      for Attempt in 1 .. Capacity loop
         I := Object.Cursor;
         Object.Cursor := I mod Capacity + 1;
         if I <= Object.Used and then State (Object, I) in
           Core.Reserve_Ready | Core.Delegate_Ready | Core.Activate_Ready |
           Core.Abort_Ready
         then
            Native.Advance (Object.Entries (I).Request,
              Object.Entries (I).Token_Base +
                (case State (Object, I) is
                   when Core.Activate_Ready => 1,
                   when Core.Abort_Ready => 2,
                   when others => 0));
            return;
         end if;
      end loop;
   end Step;

   procedure Complete
     (Object : in out Dispatcher; Receipt : CuBit.Messages.CompletionEntry;
      Now : Unsigned_64; Consumed : out Boolean) is
   begin
      Observe_Time (Object, Now);
      Consumed := False;
      for I in 1 .. Object.Used loop
         Native.Complete (Object.Entries (I).Request, Receipt, Consumed);
         exit when Consumed;
      end loop;
   end Complete;
end Intel_Render_Admission_Dispatch;
