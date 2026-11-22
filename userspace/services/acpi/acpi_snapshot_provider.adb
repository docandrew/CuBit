pragma Ada_2022;
with Interfaces;
with Firmware_Tables;
with ACPI_Endpoint;
package body ACPI_Snapshot_Provider with SPARK_Mode is
   use Interfaces;
   use ACPI_Requests;
   package body Driver is
      function Status (S : State) return Progress is (S.Result);
      function Cleanup (S : State) return Cleanup_State is (S.Loan);
      function Accepted_Tables (S : State) return Natural is (S.Accepted);
      function Remote_Status (S : State) return ACPI_Requests.Outcome is (S.Remote);
      procedure Stop (S : in out State; Result : Progress) is
      begin S.At_Step := Stopped; S.Result := Result; end Stop;
      function Envelope (P : Packet) return Boolean is
        (P.Length = 4 and then P.Flags = 0 and then P.Reserved = 0 and then
         (for all W of P.Data => W <= Max_Revision));
      procedure Begin_Catalog (S : in out State; Items : Catalog) is
         Total : Unsigned_64 := 0;
      begin
         if S.Used then return; end if;
         S.Used := True;
         if Items'Length = 0 or else Items'Length > Max_Tables then
            Stop (S, Local_Limit); return;
         end if;
         for I in Items'Range loop
            if Items(I).Length < Firmware_Tables.Table_Header_Size or else
              Total > Max_Revision - Unsigned_64(Items(I).Length)
            then Stop(S,Local_Limit); return; end if;
            for J in Items'First .. I-1 loop
               if Items(I).ID = Items(J).ID then Stop(S,Local_Limit); return; end if;
            end loop;
            Total := Total + Unsigned_64(Items(I).Length);
         end loop;
         for I in Items'Range loop S.Items(I-Items'First+1) := Items(I); end loop;
         S.Count := Items'Length; S.Result := Running; S.At_Step := Phase_Query;
      end Begin_Catalog;
      procedure Retry_Cleanup (S : in out State) is
         Released : Boolean;
      begin
         if S.Loan = Clear then return; end if;
         Release_Provider_Grant(Released);
         if Released then
            S.Loan := Clear;
            if S.Result = Waiting_For_Cleanup then S.Result := Running; end if;
         end if;
      end Retry_Cleanup;
      procedure Advance (S : in out State) is
         P,Q : Packet;
         Known : Boolean;
         Loan : Cleanup_State := Clear;
         Total, Largest : Unsigned_64 := 0;
      begin
         if S.Result /= Running or else S.Loan /= Clear then return; end if;
         case S.At_Step is
            when Phase_Query => P := (Label=>Read_Metrics,others=><>);
            when Capacity_Query => P := (Label=>Read_Metrics,Data=>[4,0,0,0],others=><>);
            when Start => P := (Label=>Start_Snapshot,Data=>[S.Version,Unsigned_64(S.Count),0,0],others=><>);
            when Finish => P := (Label=>Finish_Snapshot,Data=>[S.Version,0,0,0],others=><>);
            when Importing => P := (others=><>);
            when Dormant | Stopped => return;
         end case;
         if S.At_Step in Start | Importing | Finish and then S.Version = Max_Revision then
            Stop(S,Local_Limit); return;
         end if;
         if S.At_Step = Importing then
            Import_Table(S.Accepted+1,S.Version,S.Items(S.Accepted+1),Q,Known,Loan);
            S.Loan := Loan;
         else Exchange(P,Q,Known); end if;
         if not Known then Stop(S,Unknown_Outcome); return; end if;
         if not Envelope(Q) then Stop(S,Invalid_Response); return; end if;
         if Q.Label = ACPI_Endpoint.Reply_Error then
            if Q.Data(0) = 0 or else Q.Data(0) > Outcome'Pos(Outcome'Last) or else
               Q.Data(3) /= 0 or else
               (Q.Data(1) < S.Version and then Q.Data(0) /= Outcome'Pos(Denied))
            then Stop(S,Invalid_Response); return; end if;
            S.Remote := Outcome'Val(Q.Data(0)); Stop(S,Rejected); return;
         elsif Q.Label /= ACPI_Endpoint.Reply_OK or else Q.Data(0) < S.Version then
            Stop(S,Invalid_Response); return;
         end if;
         case S.At_Step is
            when Phase_Query =>
               if Q.Data(1..3) /= [0,0,0] then Stop(S,Rejected); return; end if;
               S.Version := Q.Data(0); S.At_Step := Capacity_Query;
            when Capacity_Query =>
               if Q.Data(0) /= S.Version then Stop(S,Rejected); return; end if;
               for I in 1 .. S.Count loop
                  Total := Total + Unsigned_64(S.Items(I).Length);
                  Largest := Unsigned_64'Max(Largest,Unsigned_64(S.Items(I).Length));
               end loop;
               if Q.Data(1) < Largest or else Q.Data(2) < Total or else Q.Data(3) < Unsigned_64(S.Count) then
                  Stop(S,Local_Limit); return;
               end if;
               S.At_Step := Start;
            when Start | Importing | Finish =>
               if Q.Data /= [S.Version+1,0,0,0] then Stop(S,Invalid_Response); return; end if;
               S.Version := Q.Data(0);
               case S.At_Step is
                  when Start => S.At_Step := Importing;
                  when Importing =>
                     S.Accepted := S.Accepted + 1;
                     if S.Accepted = S.Count then S.At_Step := Finish; end if;
                     if S.Loan /= Clear then S.Result := Waiting_For_Cleanup; end if;
                  when Finish => Stop(S,Completed);
                  when others => null;
               end case;
            when Dormant | Stopped => null;
         end case;
      end Advance;
      procedure Observe_Completion (Result : out Observation) is
         Pages : constant array (Natural range 0 .. 4) of Unsigned_64 :=
           [0,Metrics_Completion_State_Page,Metrics_Completion_Access_Page,
            Metrics_Module_State_Page,Metrics_Module_Error_Page];
         Q : Packet; Known : Boolean; Token : Revision := 0;
      begin
         Result := (others=><>);
         for I in Pages'Range loop
            Exchange((Label=>Read_Metrics,Data=>[Pages(I),0,0,0],others=><>),Q,Known);
            if not Known or else not Envelope(Q) or else Q.Label /= ACPI_Endpoint.Reply_OK then return; end if;
            if I = 0 then
               if Q.Data(1) > Phase'Pos(Phase'Last) then return; end if;
               Token := Q.Data(0);
            elsif Q.Data(0) /= Token then return;
            end if;
            Result.Pages(I) := Q.Data;
         end loop;
         Result.Available := True;
      end Observe_Completion;
   end Driver;
end ACPI_Snapshot_Provider;
