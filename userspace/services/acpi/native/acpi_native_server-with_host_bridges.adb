pragma Ada_2022;
package body ACPI_Native_Server.With_Host_Bridges is
   package O renames ACPI_Host_Bridge_Owners;
   use type O.Phase, O.B.Phase;
   function Admission_Open return Boolean is
     (not Bridges.Blocks_AML and then
        (not Bridges.Observe.Stop_Requested or else Bridges.Retired or else Bridges.Observe.Stage = O.Retained));
   procedure Bridge_Step (Result : out Owner_Outcome; Serving : Boolean := True) is
      Step : O.Step_Result;
   begin
      Bridges.Disarm;
      Bridges.Advance (Step);
      if Serving and then Bridges.Observe.Stage = O.Retained then
         --  Publication is quarantined; keep its resources and serve other work.
         Result := Owner_Idle;
         return;
      end if;
      Result := (case Step is when O.Idle => Owner_Idle,
         when O.Runnable => Owner_Pending, when O.Failed => Owner_Refused);
   end Bridge_Step;
   procedure Advance_Publication (Server : in out ACPI_Requests.State; Result : out Owner_Outcome) is
   begin
      if Power_Blocks_Runtime then Bridges.Request_Stop; end if;
      if not Bridges.Observe.Attempted and then not Bridges.Observe.Stop_Requested then
         Maybe_Start (Server);
      end if;
      Bridge_Step (Result, Serving => False);
   end Advance_Publication;
   procedure Advance (Server : in out ACPI_Requests.State; Result : out Owner_Outcome) is
      Normal : Owner_Outcome := Owner_Idle;
   begin
      if Bridges.Observe.Stage = O.Retained then
         Result := Owner_Idle;
         if not Bridges.Blocks_AML and then not Power_Blocks_Runtime then
            Advance_Normal (Server, Result);
         end if;
         return;
      end if;
      if Power_Blocks_Runtime then
         -- Enter may hold its saved reply, but its hardware step must wait
         -- for actual query/channel/original-grant retirement.
         Bridges.Request_Stop;
         Bridge_Step (Result);
         if Result /= Owner_Refused and then Bridges.Retired then
            Advance_Power_Only (Server, Normal);
            if Normal /= Owner_Idle then Result := Normal; end if;
         end if;
         return;
      end if;
      if Bridges.Observe.Stop_Requested and then not Bridges.Retired then
         Bridge_Step (Result); return;
      end if;
      if not Bridges.Blocks_AML then
         Advance_Normal (Server, Normal);
         if Normal = Owner_Refused then Result := Normal; return; end if;
         if not Bridges.Observe.Attempted and then not Bridges.Observe.Stop_Requested then Maybe_Start (Server); end if;
      end if;
      Bridge_Step (Result);
      if Result = Owner_Idle and then Normal = Owner_Pending then Result := Owner_Pending; end if;
   end Advance;
   procedure Drain (Server : in out ACPI_Requests.State; Result : out Owner_Outcome) is
      pragma Unreferenced (Server);
   begin
      Bridges.Request_Stop;
      Bridge_Step (Result, Serving => False);
   end Drain;
   procedure Maintain (Deadline : out Interfaces.Unsigned_64; May_Continue : out Boolean) is
      Armed : Boolean;
   begin
      if Bridges.Blocks_AML then
         Maintain_Exclusive (Deadline, May_Continue);
      else
         Maintain_Normal (Deadline, May_Continue);
      end if;
      if not May_Continue then return; end if;
      --  Retained resources are not a reason to terminate the serving loop.
      --  Keep the normal maintenance deadline; never poll or replay this publication.
      if Bridges.Observe.Stage = O.Retained then return; end if;
      Deadline := Interfaces.Unsigned_64'Min (Deadline, Bridges.Wake_Deadline);
      Bridges.Prepare_Wait (Armed);
      if Bridges.Current_Transport.Stage = O.B.Awaiting_Receipt and then not Armed then
         -- Arm found a ready receipt. An already-past deadline gives it the
         -- next serialized turn instead of sleeping through the completion.
         Deadline := 0;
      end if;
   end Maintain;
   function Diagnostics_Quiet (Server : ACPI_Requests.State) return Boolean is
     (not Bridges.Blocks_AML and then Diagnostics_Normal (Server));
   procedure Handle_Completion (Server : in out ACPI_Requests.State;
      Item : CuBit.Messages.CompletionEntry; Handled : out Boolean) is
      pragma Unreferenced (Server);
   begin Bridges.Complete_Open (Item, Handled); end Handle_Completion;
end ACPI_Native_Server.With_Host_Bridges;
