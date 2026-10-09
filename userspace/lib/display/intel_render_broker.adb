with CuBit.Capability_Grants;
with Intel_GPU_Broker_Request;
with Intel_Render_Admission;
with CuBit.Process_IDs;
package body Intel_Render_Broker is
   package M renames CuBit.Messages;
   package G renames CuBit.Capability_Grants;
   package R renames Intel_GPU_Broker_Request;
   package C renames Intel_Render_Admission;
   use type L.Phase;
   use type L.Outcome;
   use type M.CapabilitySlot;

   function State (Object : Broker; ID : Ticket) return L.Phase is
     (L.State (Object.Launches, ID));

   procedure Begin_Request
     (Object : in out Broker; Expected_Launcher, Sender, Stamped_Tag : Unsigned_64;
      Request : M.Message; Now, Deadline : Unsigned_64; ID : out Ticket)
   is
      Decoded : constant R.Decoded := R.Decode
        (Expected_Launcher, Sender, Stamped_Tag, Request.tag.label,
         Request.tag.length, Request.tag.flags, Request.tag.reserved,
         [Request.words (0), Request.words (1),
          Request.words (2), Request.words (3)]);
      Target : G.Recipient;
      Slot : M.CapabilitySlot;
      Reserved : Ticket;
   begin
      ID := 0;
      if not Decoded.Valid or else L.Count (Object.Launches) = L.Capacity
      then return; end if;
      Target := G.Capture (M.CapabilitySlot (Decoded.Source));
      if not G.Endpoint_Matches (M.CapabilitySlot (Decoded.Source),
                                G.Incarnation (Target)) or else
        not L.Can_Reserve (Object.Launches, Decoded, CuBit.Process_IDs.To_Word (G.Incarnation (Target)))
      then return; end if;
      Slot := Reply_Slot (L.Count (Object.Launches) + 1);
      -- Never overwrite a source, implicit reply, or another retained reply.
      -- Other service slots remain a trusted caller reservation obligation.
      if Slot = Driver_Source or Slot = 63 or
        Slot in M.CapabilitySlot (R.Source_Slot'First) ..
                M.CapabilitySlot (R.Source_Slot'Last)
      then return; end if;
      for I in 1 .. L.Count (Object.Launches) loop
         if Object.Replies (I) = Slot then return; end if;
      end loop;
      if M.saveReplyCap (Unsigned_64 (Slot)) /= 1 then return; end if;
      -- Can_Reserve is stable within this single-owner, non-reentrant call.
      L.Reserve (Object.Launches, Decoded, CuBit.Process_IDs.To_Word (G.Incarnation (Target)), Reserved);
      ID := Reserved;
      Object.Replies (ID) := Slot;
      D.Start (Object.Admissions, Target, Driver_Source,
        M.CapabilitySlot (Decoded.Source), M.CapabilitySlot (Decoded.Destination),
        Now, Deadline, Object.IDs (ID));
      if Object.IDs (ID) = 0 then
         L.Finish (Object.Launches, ID, L.Rejected);
      end if;
   end Begin_Request;

   function Terminal (Object : Broker; ID : Positive) return Boolean is
     (D.State (Object.Admissions, Object.IDs (ID)) in
        C.Active | C.Failed | C.Quarantined);

   function Runnable (Object : Broker) return Boolean is
   begin
      for I in 1 .. L.Count (Object.Launches) loop
         if State (Object, I) = L.Reply_Ready or else
           (State (Object, I) = L.Pending and then Terminal (Object, I))
         then return True; end if;
      end loop;
      return D.Runnable (Object.Admissions);
   end Runnable;

   function Next_Deadline (Object : Broker) return Unsigned_64 is
     (D.Next_Deadline (Object.Admissions));

   procedure Step (Object : in out Broker; Now : Unsigned_64) is
      I : Positive;
      Taken, Sent : Boolean;
      Reply : M.Message := M.NULL_MESSAGE;
   begin
      -- Advance admission even when replies are ready, so neither side can
      -- starve. Each call performs at most one admission operation and reply.
      D.Step (Object.Admissions, Now);
      for Attempt in 1 .. L.Capacity loop
         I := Object.Cursor;
         Object.Cursor := I mod L.Capacity + 1;
         if I <= L.Count (Object.Launches) then
            if State (Object, I) = L.Pending and then Terminal (Object, I) then
               L.Finish (Object.Launches, I,
                 (case D.State (Object.Admissions, Object.IDs (I)) is
                    when C.Active => L.Admitted,
                    when C.Failed => L.Rejected,
                    when others => L.Uncertain));
            end if;
            L.Take_Reply (Object.Launches, I, Taken);
            if Taken then
               Reply.tag := (R.Label, 4, 0, 0);
               -- [version, status: 0 admitted/1 rejected/2 uncertain, nonce,
               -- captured application identity]. Not a new authority token.
               Reply.words := [R.Version,
                 Unsigned_64 (L.Outcome'Pos (L.Result (Object.Launches, I))),
                 L.Nonce (Object.Launches, I), L.Identity (Object.Launches, I)];
               Sent := M.replyCap (Object.Replies (I), Reply) = 1;
               L.Delivered (Object.Launches, I, Sent);
               if L.Abort_Required (Object.Launches, I) then
                  D.Cancel (Object.Admissions, Object.IDs (I));
               end if;
               return;
            end if;
         end if;
      end loop;
   end Step;

   procedure Complete
     (Object : in out Broker; Receipt : M.CompletionEntry;
      Now : Unsigned_64; Consumed : out Boolean) is
   begin
      D.Complete (Object.Admissions, Receipt, Now, Consumed);
   end Complete;

   procedure Cancel (Object : in out Broker; ID : Ticket) is
   begin
      if ID /= 0 and then ID <= L.Count (Object.Launches) then
         D.Cancel (Object.Admissions, Object.IDs (ID));
      end if;
   end Cancel;
end Intel_Render_Broker;
