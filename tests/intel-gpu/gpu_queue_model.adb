package body GPU_Queue_Model is
   package Policy renames Intel_GPU_GuC_Submission_Policy;
   use type Intel_GPU_Ring_Reservation.Outcome, Q.Opcode;

   type Owner_Array is array (0 .. Ring_Bytes - 1) of Unsigned_64;
   type Context_Model is record
      Owners : Owner_Array := [others => 0];
      Tail : Unsigned_32 := Setup_Bytes;
      Published, Submitted, Done : Unsigned_64 := 1;
      Progress : Natural := 0;
      Enabled, Enabling : Boolean := False;
      Enable_Wait : Natural := 0;
   end record;
   type Context_Models is array (Session_Id, Q.Context_Index) of Context_Model;
   Models : access Context_Models := new Context_Models;
   Sel_S : Session_Id := 1;
   Sel_C : Q.Context_Index := 0;
   Kick_Count : Natural := 0;

   procedure Reset is
   begin
      Models.all := [others => [others => (others => <>)]];
      for S in Session_Id loop
         for C in Q.Context_Index loop
            Models (S, C).Owners (0 .. Setup_Bytes - 1) := [others => 1];
         end loop;
         if Client_Regions (S) = null then
            Client_Regions (S) := new Region;
            Server_Regions (S) := new Region;
         end if;
         Client_Regions (S).all := [others => 0];
         Server_Regions (S).all := [others => 0];
      end loop;
      Latency := 3; Hung := False; Regress := False; Torn := False; Ownership := True;
      Ahead := False; Enable_Delay := 0;
      Backpressure_Every := 0; Execute_Bytes := 384; Signal_Bytes := 120;
      Overwrites := 0; Bad_Tails := 0; Kicks := 0; Enables := 0; Quarantines := 0;
      Calls_OK := 0; Calls_Failed := 0; Last_Call_Value := 0;
      Wakes_Answered := 0; Last_Wake := Q.Woken; Kick_Count := 0;
      Quarantined := [others => False];
   end Reset;

   procedure Advance is
   begin
      Clock := Clock + Microseconds_Per_Turn;
      for S in Session_Id loop
         for C in Q.Context_Index loop
            declare
               M : Context_Model renames Models (S, C);
            begin
               if M.Enabling then
                  if M.Enable_Wait > 0 then
                     M.Enable_Wait := M.Enable_Wait - 1;
                  else
                     M.Enabling := False;
                     M.Enabled := True;
                  end if;
               end if;
               if not Hung and then M.Done < M.Submitted then
                  M.Progress := M.Progress + 1;
                  if M.Progress >= Latency then
                     M.Done := M.Done + 1;
                     M.Progress := 0;
                  end if;
               end if;
            end;
         end loop;
      end loop;
   end Advance;

   function Select_Context (S : Session_Id; C : Q.Context_Index) return Boolean is
   begin
      Sel_S := S;
      Sel_C := C;
      return Ownership;
   end Select_Context;

   function Owner_Ready return Boolean is (Ownership);

   function Batch_Ready (Handle, GPU, Offset, Bytes : Unsigned_64) return Boolean is
     (Handle /= Unsigned_64 (Unmapped_Handle) and then GPU /= 0 and then Bytes /= 0 and then
      Offset < 2 ** 24);

   function Scheduling_Resident return Boolean is (Models (Sel_S, Sel_C).Enabled);
   -- As Context_Table.Publish_Allowed: not while an enable is unacknowledged.
   function Publish_Ready return Boolean is (not Models (Sel_S, Sel_C).Enabling);

   procedure Write_Segment
     (Operation : Q.Opcode; V, Batch : Unsigned_64;
      Plan : Intel_GPU_Ring_Reservation.Plan; Expected_Tail : Unsigned_32; OK : out Boolean)
   is
      M : Context_Model renames Models (Sel_S, Sel_C);
      procedure Claim (First, Bytes : Unsigned_32) is
      begin
         for B in First .. First + Bytes - 1 loop
            if M.Owners (Natural (B)) /= 0 and then not (M.Owners (Natural (B)) < M.Done) then
               Overwrites := Overwrites + 1;
            end if;
            M.Owners (Natural (B)) := V;
         end loop;
      end Claim;
      pragma Unreferenced (Batch);
   begin
      OK := False;
      if Plan.Status /= Intel_GPU_Ring_Reservation.Ready or else Expected_Tail /= M.Tail or else
        V /= M.Published + 1 or else Plan.Consumed /= Plan.Padding + Segment_Bytes (Operation)
      then
         Bad_Tails := Bad_Tails + 1;
         return;
      end if;
      if Plan.Padding /= 0 then
         Claim (M.Tail, Plan.Padding);
      end if;
      Claim (Plan.Start, Segment_Bytes (Operation));
      M.Tail := Plan.Tail mod Ring_Bytes;
      M.Published := V;
      OK := True;
   end Write_Segment;

   function Segment_Bytes (Operation : Q.Opcode) return Unsigned_32 is
     (if Operation = Q.Signal then Signal_Bytes else Execute_Bytes);

   procedure Kick (Enable : Boolean; Result : out Policy.Kick_Result) is
      M : Context_Model renames Models (Sel_S, Sel_C);
   begin
      Kick_Count := Kick_Count + 1;
      if Backpressure_Every /= 0 and then Kick_Count mod Backpressure_Every = 0 then
         Result := Policy.Kick_Backpressure;
         return;
      end if;
      Kicks := Kicks + 1;
      if Enable then
         Enables := Enables + 1;
         if not M.Enabled then
            M.Enabling := True;
            M.Enable_Wait := Enable_Delay;
         end if;
      end if;
      M.Submitted := M.Published;
      Result := Policy.Kick_Queued;
   end Kick;

   procedure Read_Timeline (V : out Unsigned_64; OK : out Boolean) is
      M : Context_Model renames Models (Sel_S, Sel_C);
   begin
      OK := not Torn;
      V := (if Regress and then M.Done > 0 then M.Done - 1
            elsif Ahead then M.Published + 1 else M.Done);
   end Read_Timeline;

   function Now_Us return Unsigned_64 is (Clock);

   procedure Quarantine (S : Session_Id; Why : Q.Fault_Reason) is
      pragma Unreferenced (Why);
   begin
      Quarantines := Quarantines + 1;
      Quarantined (S) := True;
   end Quarantine;

   procedure Call_Finished (S : Session_Id; V : Unsigned_64; OK : Boolean) is
      pragma Unreferenced (S);
   begin
      if OK then Calls_OK := Calls_OK + 1; else Calls_Failed := Calls_Failed + 1; end if;
      Last_Call_Value := V;
   end Call_Finished;

   procedure Answer_Wake (S : Session_Id; Result : Q.Wake_Result) is
   begin
      Wakes_Answered := Wakes_Answered + 1;
      Last_Wake := Result;
      Last_Wake_Session := S;
   end Answer_Wake;

   function Client_Region (S : Session_Id) return System.Address is
     (Client_Regions (S).all'Address);
   function Server_Region (S : Session_Id) return System.Address is
     (Server_Regions (S).all'Address);

   function Completed (S : Session_Id; C : Q.Context_Index) return Unsigned_64 is
     (Models (S, C).Done);
end GPU_Queue_Model;
