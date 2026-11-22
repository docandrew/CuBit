pragma Ada_2022;
with ACPI_Endpoint;
with ACPI_Service;
package body ACPI_Notification_Consumer with SPARK_Mode is
   use ACPI_Requests;
   use type Interfaces.Unsigned_8;
   use type Interfaces.Unsigned_16;
   use type Interfaces.Unsigned_32;
   Queue_Capacity : constant Word := Word (ACPI_Service.Notification_Count'Last);
   Last_Kind : constant Word := Notification_Kind_Code'Pos (Notification_Kind_Code'Last);
   Last_Class : constant Word := Notification_Class_Code'Pos (Notification_Class_Code'Last);
   Last_Number : constant Word := Word (Interfaces.Unsigned_16'Last);
   Last_Segment : constant Word := Word (Interfaces.Unsigned_32'Last);
   procedure Retire (S : in out State) is
   begin
      S.Pending := No_Token; S.Step := Stopped; S.Ack_Confirmed := False;
      S.Item := (others => <>); S.Next_Segment := 0;
   end Retire;
   procedure Start_Session (S : in out State; Outcome : out Result) is
   begin
      Retire (S); S.Revision := 0; S.Error := OK;
      if S.Exhausted or else S.Lifetime >= Final_Session then
         S.Exhausted := True; Outcome := Counter_Exhausted; return;
      end if;
      S.Lifetime := S.Lifetime + 1; S.Step := Synchronize; Outcome := Applied;
   end Start_Session;
   procedure Next_Request (S : in out State; Request : out Packet;
                           Correlation : out Token; Outcome : out Result) is
   begin
      Request := (others => <>); Correlation := No_Token; Outcome := Idle;
      if S.Step in Stopped | Deliver or else S.Pending /= No_Token then return; end if;
      if S.Sequence >= Final_Request then
         Retire (S); S.Exhausted := True; Outcome := Counter_Exhausted; return;
      end if;
      case S.Step is
         when Synchronize => Request := (Label => Read_Notification_State, others => <>);
         when Claim => Request := (Label => Claim_Notification, Data => [S.Revision,0,0,0], others => <>);
         when Header => Request := (Label => Read_Notification, Data => [S.Item.ID,Notification_Data_Page,0,0], others => <>);
         when Identity => Request := (Label => Read_Notification, Data => [S.Item.ID,Notification_Identity_Page,0,0], others => <>);
         when Path => Request := (Label => Read_Notification, Data => [S.Item.ID,Notification_Path_Page,Word(S.Next_Segment),0], others => <>);
         when Acknowledge => Request := (Label => Acknowledge_Notification, Data => [S.Revision,S.Item.ID,0,0], others => <>);
         when Stopped | Deliver => return;
      end case;
      S.Sequence := S.Sequence + 1; S.Pending := (S.Lifetime,S.Sequence);
      Correlation := S.Pending; Outcome := Ready;
   end Next_Request;
   function Matches (S : State; Correlation : Token) return Boolean is
     (Correlation /= No_Token and then Correlation = S.Pending and then
      Correlation.Lifetime = S.Lifetime and then S.Step /= Stopped);
   function Segment_Valid (Value : Word) return Boolean is
      B : Word;
   begin
      for I in 0 .. 3 loop
         B := Interfaces.Shift_Right (Value,I*8) and 255;
         if not (B = Character'Pos('_') or else B in Character'Pos('A') .. Character'Pos('Z')
           or else (I > 0 and then B in Character'Pos('0') .. Character'Pos('9'))) then return False; end if;
      end loop;
      return True;
   end Segment_Valid;
   procedure Accept_Reply (S : in out State; Correlation : Token;
                           Reply : Packet; Outcome : out Result) is
      W : Words renames Reply.Data;
      Failed : ACPI_Requests.Outcome;
      procedure Reject is
      begin Retire(S); Outcome := Invalid_Reply; end Reject;
   begin
      Outcome := Ignored;
      if not Matches(S,Correlation) then return; end if;
      S.Pending := No_Token;
      if Reply.Length /= 4 or else Reply.Flags /= 0 or else Reply.Reserved /= 0
        or else (for some V of W => V > Max_Revision) then Reject; return; end if;
      if Reply.Label = ACPI_Endpoint.Reply_Error then
         if W(0) not in 1 .. ACPI_Requests.Outcome'Pos(ACPI_Requests.Outcome'Last)
           or else W(2) /= 0 or else W(3) /= 0 then Reject; return; end if;
         Failed := ACPI_Requests.Outcome'Val(W(0));
         if not (Failed in Denied | Malformed
           or else (Failed = Stale and then S.Step /= Synchronize)
           or else (Failed = Wrong_Order and then S.Step = Claim)
           or else (Failed = Resource_Limit and then S.Step in Claim | Acknowledge)
           or else (Failed = Index_Out_Of_Range and then S.Step = Path)) then Reject; return; end if;
         -- No-authority denial hides revision as zero; an authenticated
         -- observer's denied mutation carries the current revision.
         if W(1) < S.Revision and then not (Failed = Denied and then W(1) = 0)
         then Reject; return; end if;
         S.Error := Failed;
         if Failed = Stale then
            S.Revision := W(1); S.Step := Synchronize;
            if not S.Ack_Confirmed then S.Item := (others => <>); S.Next_Segment := 0; end if;
         else Retire(S); end if;
         Outcome := Remote_Error; return;
      elsif Reply.Label /= ACPI_Endpoint.Reply_OK then Reject; return; end if;
      if W(0) < S.Revision then Reject; return; end if;
      S.Error := OK;
      case S.Step is
         when Synchronize =>
            if W(1)>3 or else W(2)>Queue_Capacity then Reject; return; end if;
            S.Revision:=W(0); S.Step:=(if S.Ack_Confirmed then Acknowledge else Claim);
         when Claim =>
            if W(2)>Queue_Capacity or else W(3)>Word(Maximum_Path)
              or else (W(1)=0 and then (W(2)/=0 or else W(3)/=0)) then Reject; return; end if;
            S.Revision:=W(0); S.Item:=(others=><>); S.Item.ID:=W(1);
            if W(1)=0 then S.Step:=Synchronize; Outcome:=Empty; return; end if;
            S.Item.Length:=Path_Count(W(3)); S.Next_Segment:=0; S.Step:=Header;
         when Header =>
            if W(1)>Last_Kind or else W(2)>Last_Class or else W(3)>Last_Number then Reject; return; end if;
            S.Revision:=W(0); S.Item.Kind:=Notification_Kind_Code'Val(W(1));
            S.Item.Class:=Notification_Class_Code'Val(W(2)); S.Item.Number:=Interfaces.Unsigned_16(W(3)); S.Step:=Identity;
         when Identity =>
            if W(2)=0 or else W(3)>1 then Reject; return; end if;
            S.Revision:=W(0); S.Item.Node:=W(1); S.Item.Incarnation:=W(2); S.Item.Current_At_Claim:=W(3)=1;
            S.Step:=(if S.Item.Length=0 then Deliver else Path);
         when Path =>
            if W(1)>Last_Segment or else W(2)/=Word(S.Item.Length) or else W(3)/=0
              or else S.Next_Segment>=S.Item.Length or else not Segment_Valid(W(1)) then Reject; return; end if;
            S.Revision:=W(0); S.Item.Path(S.Next_Segment+1):=Interfaces.Unsigned_32(W(1));
            S.Next_Segment:=S.Next_Segment+1;
            if S.Next_Segment=S.Item.Length then S.Step:=Deliver; end if;
         when Acknowledge =>
            if not S.Ack_Confirmed or else S.Item.ID=0 or else W(1)/=S.Item.ID
              or else W(2)>Queue_Capacity or else W(3)/=0 then Reject; return; end if;
            S.Revision:=W(0); S.Item:=(others=><>); S.Ack_Confirmed:=False; S.Step:=Synchronize;
         when Stopped | Deliver => Reject; return;
      end case;
      Outcome:=Applied;
   end Accept_Reply;
   procedure Transport_Unknown (S : in out State; Correlation : Token; Outcome : out Result) is
   begin
      Outcome:=Ignored;
      if not Matches(S,Correlation) then return; end if;
      S.Pending:=No_Token;
      if S.Step=Claim then S.Step:=Synchronize; end if;
      -- An uncertain ACK retries the same confirmed receipt, not a new claim.
      Outcome:=Applied;
   end Transport_Unknown;
   procedure Confirm_Delivery (S : in out State; Lifetime : Word;
                               ID : Receipt; Outcome : out Result) is
   begin
      Outcome:=Invalid_Delivery;
      if S.Step/=Deliver or else Lifetime/=S.Lifetime or else ID=0 or else ID/=S.Item.ID then return; end if;
      S.Ack_Confirmed:=True; S.Step:=Acknowledge; Outcome:=Applied;
   end Confirm_Delivery;
end ACPI_Notification_Consumer;
