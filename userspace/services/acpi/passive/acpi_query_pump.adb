pragma Ada_2022;
package body ACPI_Query_Pump with SPARK_Mode is
   use type Sessions.Outcome;
   procedure Step
     (Item : in out State; Owner : in out Loans.Backend_Context; Retry_Delay : Retry_Interval;
      Request_Data, Response_Data : out Pages.Byte_Array; Result : out Report) is
      Now, Deadline, Length : Wire.Word;
      Clock_Usable, Ready, Usable, Work : Boolean;
      Kind : Request_Kind;
      Sender : Loans.Peer_Identity;
      Loan : Loans.Loan_Reference;
      Stamp : ACPI_Protocol.Role_Tag;
   begin
      Result := (others => <>); Request_Data := [others => 0]; Response_Data := [others => 0];
      Prepare_Local (Owner, Now, Deadline, Clock_Usable, Ready);
      Result.Observed_Now := Now; Result.Previous_Now := Item.Last_Now;
      if not Ready then return; end if;
      if Clock_Usable then
         if Now >= Wait_Forever then Result.Status := Clock_Invalid; return; end if;
         if Item.Seen_Clock and then Now < Item.Last_Now then Result.Status := Clock_Regressed; return; end if;
         Item.Seen_Clock := True; Item.Last_Now := Now;
      end if;
      Sessions.Progress (Item.Session, Owner, Result.Cleanup);
      Work := Result.Cleanup.Status in Sessions.Answered | Sessions.Delivery_Lost;
      Poll_Firmware (Owner, Result.Firmware_Handled); Work := Work or Result.Firmware_Handled;
      Poll_Request (Owner, Kind, Sender, Loan, Stamp, Length);
      Result.Request_Taken := Kind /= None_Ready;
      if Kind = Query_Ready then
         Sessions.Begin_Request (Item.Session, Owner, Stamp, Sender, Loan, Length,
           Request_Data, Response_Data, Result.Request);
         if Result.Request.Immediate_Reply then
            Reply_Immediate (Owner, Sender, Result.Request, Result.Immediate_Delivered);
         end if;
      end if;
      Work := Work or Result.Request_Taken;
      if Work then Result.Status := Worked; return; end if;
      if Pending (Item) then
         if not Clock_Usable then Result.Status := Retry_Clock_Unavailable; return; end if;
         if Retry_Delay >= Wait_Forever - Now then Result.Status := Deadline_Overflow; return; end if;
         Deadline := Wire.Word'Min (Deadline, Now + Retry_Delay);
      end if;
      Result.Wait_Deadline := Deadline;
      Wait_Activity (Owner, Deadline, Usable);
      Result.Status := (if Usable then Waited else Wait_Unavailable);
   end Step;
end ACPI_Query_Pump;
