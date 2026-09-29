package body Intel_GPU_GuC_CT_Register is
   function Attempted (Object : Registration) return Boolean is (Object.Started);
   function Enabled (Object : Registration) return Boolean is
     (Object.Ready and then Owner_Ready);
   function Last_Step (Object : Registration) return Natural is (Object.Step);
   function Last_Reply (Object : Registration) return Interfaces.Unsigned_32 is
     (Object.Reply);
   procedure Execute (Object : in out Registration;
     GPU_Start, Backing_Bytes, Pin_Bias : Interfaces.Unsigned_64;
     Status : out Result) is
      Plan : constant Intel_GPU_GuC_CT_Setup.Plan :=
        Intel_GPU_GuC_CT_Setup.Prepare (GPU_Start, Backing_Bytes, Pin_Bias);
      OK : Boolean;
   begin
      Status := Rejected;
      if Object.Started or else not Plan.Valid or else not Owner_Ready then return; end if;
      Object.Started := True;
      for Step in 1 .. 7 loop
         if not Owner_Ready then Status := Ownership_Lost; return; end if;
         Object.Step := Step;
         Object.Reply := 0;
         Exchange ((if Step = 7 then Plan.Enable else Plan.Register_Buffers (Step)),
                   Object.Reply, OK);
         if not Owner_Ready then Status := Ownership_Lost; return; end if;
         if not OK then Status := Transport_Failed; return; end if;
         if Step = 7 then
            if not Intel_GPU_GuC_CT_Setup.Enabled_Response (Object.Reply) then
               Status := Enable_Refused; return;
            end if;
         elsif not Intel_GPU_GuC_CT_Setup.Registered_Response (Object.Reply) then
            Status := Registration_Refused; return;
         end if;
      end loop;
      Object.Ready := True;
      Status := Complete;
   end Execute;
end Intel_GPU_GuC_CT_Register;
