with CuBit.Handles.IPC;
with Intel_GPU_Broker_Request;
with Intel_Render_Admission;
package body Intel_Render_Broker is
   package M renames CuBit.Messages;
   package H renames CuBit.Handles;
   package IPC renames CuBit.Handles.IPC;
   package R renames Intel_GPU_Broker_Request;
   package C renames Intel_Render_Admission;
   use type L.Phase;
   use type L.Outcome;
   use type IPC.Reply_Outcome;

   function State (Object : Broker; ID : Ticket) return L.Phase is
     (L.State (Object.Launches, ID));

   procedure Begin_Request
     (Object : in out Broker; Expected_Launcher, Sender, Stamped_Tag : Unsigned_64;
      Request : M.Message; Now, Deadline : Unsigned_64; ID : out Ticket)
   is
      Decoded : constant R.Decoded := R.Decode
        (Expected_Launcher, Sender, Stamped_Tag, Request.tag.label,
         Request.tag.length, CuBit.Messages.Flag_Byte (Request.tag), Request.tag.reserved,
         [Request.words (0), Request.words (1),
          Request.words (2), Request.words (3)]);
      Reserved : Ticket;
   begin
      ID := 0;
      if not Decoded.Valid or else L.Count (Object.Launches) = L.Capacity
        or else not L.Can_Reserve (Object.Launches, Decoded)
      then return; end if;
      declare
         Saved : constant IPC.Saved_Reply := IPC.Save_Reply;
      begin
         if not Saved.Saved then return; end if;
         -- Can_Reserve is stable within this single-owner, non-reentrant call.
         L.Reserve (Object.Launches, Decoded, Reserved);
         ID := Reserved;
         if ID = 0 then
            --  Not reachable (Can_Reserve held); answer rather than leak.
            declare
               Ignore : constant Boolean := IPC.Answer (Saved.Handle, M.NULL_MESSAGE);
            begin
               return;
            end;
         end if;
         Object.Replies (ID) := Saved.Handle;
      end;
      D.Start (Object.Admissions, Decoded.Identity, Driver, Driver_PID,
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
               Reply.tag := (label => R.Label, length => 4,
                             protocol => 0, ipc => <>, reserved => 0);
               -- [version, status: 0 admitted/1 rejected/2 uncertain, nonce,
               -- application identity]. Not a new authority token.
               Reply.words := [R.Version,
                 Unsigned_64 (L.Outcome'Pos (L.Result (Object.Launches, I))),
                 L.Nonce (Object.Launches, I), L.Identity (Object.Launches, I)];
               if L.Result (Object.Launches, I) = L.Admitted then
                  declare
                     Result : constant IPC.Reply_Result := IPC.Reply_Saved_With
                       (Object.Replies (I), Reply,
                        H.Badged (Render_Source,
                                  (Read | Write | Grant => True, others => False),
                                  D.Session (Object.Admissions, Object.IDs (I))));
                  begin
                     Sent := Result.Outcome = IPC.Transferred;
                     if Result.Outcome = IPC.Refused then
                        -- The saved reply is kept on a refusal: answer it.
                        Reply.words (1) := Unsigned_64 (L.Outcome'Pos (L.Uncertain));
                        Sent := False;
                        declare
                           Ignore : constant Boolean := IPC.Answer (Object.Replies (I), Reply);
                        begin
                           null;
                        end;
                     end if;
                  end;
               else
                  Sent := IPC.Answer (Object.Replies (I), Reply);
               end if;
               Object.Replies (I) := H.No_Reply;
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
