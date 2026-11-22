package body Intel_GPU_Allocation_Growth is
   use type Growth.Phase;
   procedure Configure
     (Object : in out Dispatcher; Metadata_Bytes : Unsigned_64;
      Record_Quota : Positive; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Object.Configured then return; end if;
      Growth.Configure (Object.Controller, Metadata_Bytes, Record_Quota, Accepted);
      if Accepted then
         Object.Configured := True;
         Object.Limit := Record_Quota;
         Object.Metadata_Limit := Metadata_Bytes;
      end if;
   end Configure;
   function Pending (Object : Dispatcher) return Boolean is (Object.Active);
   function Growth_Allowance
     (Object : Dispatcher; Committed_Records : Positive) return Natural is
     (if not Object.Configured or else Object.Revoked or else not Owner_Ready or else
         Growth.Snapshot (Object.Controller).State = Growth.Failed or else
         Growth.Snapshot (Object.Controller).Published_Bytes >= Object.Metadata_Limit or else
         Committed_Records >= Object.Limit
      then 0 else Object.Limit - Committed_Records);
   function Record_Budget
     (Object : Dispatcher; Committed_Records : Positive;
      Unused_Records : Natural) return Natural is
      In_Policy : constant Natural := Natural'Min (Committed_Records, Object.Limit);
      Used : Natural;
   begin
      if not Object.Configured or else Object.Revoked or else not Owner_Ready or else
        Growth.Snapshot (Object.Controller).State = Growth.Failed or else
        Unused_Records > Committed_Records then return 0; end if;
      Used := Committed_Records - Unused_Records;
      if Used > In_Policy then return 0; end if;
      -- A chunk may initialize records beyond the policy quota. Those records
      -- cannot be allocated, so do not count them as unused policy identities.
      return In_Policy - Used + Growth_Allowance (Object, Committed_Records);
   end Record_Budget;
   procedure Begin_Request
     (Object : in out Dispatcher; Index : Positive;
      Pages : Intel_GPU_Buffer_Reply.Layout.Page_Count;
      Generation : Unsigned_32; Accepted : out Boolean) is
      OK : Boolean;
   begin
      Accepted := False;
      if not Object.Configured or else Object.Active or else Object.Revoked or else
        Index > Object.Limit or else Generation = 0 or else
        not Owner_Ready or else not Save_Reply then return; end if;
      Object.Index := Index;
      Object.Pages := Pages;
      Object.Generation := Generation;
      Object.Active := True;
      Growth.Request (Object.Controller, Index, OK);
      Object.Reject := not OK;
      -- Even if the arena is quarantined, the saved request gets one failure
      -- response from Step. Returning False here would permit a second reply.
      Accepted := True;
   end Begin_Request;
   procedure Step (Object : in out Dispatcher) is
      Buffer : Intel_GPU_Buffer_Reply.Extent_View;
      Granted : Boolean := False;
      More : Boolean := False;
   begin
      if not Object.Active then return; end if;
      -- Continue no metadata writes after owner loss. A partial arena remains
      -- retained; this dispatcher must not be rebound to another incarnation.
      if not Owner_Ready then
         Object.Reject := True;
         Object.Revoked := True;
      end if;
      if not Object.Reject and then
        Growth.Snapshot (Object.Controller).State not in Growth.Idle | Growth.Failed
      then
         Growth.Step (Object.Controller);
         return;
      end if;
      if not Object.Reject and then
        Growth.Snapshot (Object.Controller).State = Growth.Idle
      then
         Acquire (Object.Index, Object.Pages, Object.Generation, Buffer, Granted, More);
         if More then return; end if;
      end if;
      Object.Active := False;
      Respond (Object.Index, Object.Generation, Buffer, Granted);
   end Step;
end Intel_GPU_Allocation_Growth;
