pragma Ada_2022;
package body AML_Dynamic_Region_Connections with SPARK_Mode is
   function Matches (Run : State; Region : Region_Identity) return Boolean is
     (Run.Items (Region.Node).Current /= Absent and then
      Run.Items (Region.Node).Incarnation = Region.Incarnation);

   function Active_Region_Count (Run : State) return Natural is
     (Run.Active_Used);

   function Active_Region_Node (Run : State; Index : Positive) return Region_Node is
     (Run.Active (Index));

   function Pending_Region_Count (Run : State) return Natural is
     (Run.Pending_Used);

   function Pending_Region_Node (Run : State; Index : Positive) return Region_Node is
     (Run.Pending (Index));

   function Callbacks_This_Turn (Run : State) return Natural is
     (Run.Taken);

   function Current_Turn_Generation (Run : State) return Natural is
     (Run.Turn_Generation);

   procedure Add_Pending (Run : in out State; Node : Region_Node) is
   begin
      if Run.Pending_Position (Node) = 0 and then Run.Pending_Used < Maximum_Regions then
         Run.Pending_Used := Run.Pending_Used + 1;
         Run.Pending (Region_Node (Run.Pending_Used)) := Node;
         Run.Pending_Position (Node) := Run.Pending_Used;
      end if;
   end Add_Pending;

   procedure Remove_Pending (Run : in out State; Node : Region_Node) is
      Position : constant Natural := Run.Pending_Position (Node);
   begin
      if Position = 0 or else Position > Run.Pending_Used then return; end if;
      declare
         Last : constant Region_Node := Run.Pending (Region_Node (Run.Pending_Used));
      begin
         Run.Pending (Region_Node (Position)) := Last;
         Run.Pending_Position (Last) := Position;
         Run.Pending (Region_Node (Run.Pending_Used)) := Region_Node'First;
         Run.Pending_Position (Node) := 0;
         Run.Pending_Used := Run.Pending_Used - 1;
         if Run.Cursor = Position then
            Run.Cursor := Position - 1;
         elsif Run.Cursor > Run.Pending_Used then
            Run.Cursor := 0;
         end if;
      end;
   end Remove_Pending;

   procedure Publish
     (Run : in out State; Region : Region_Identity;
      Space : Interfaces.Unsigned_8; Accepted : out Boolean) is
   begin
      Accepted := Run.Items (Region.Node).Current = Absent
        and then Run.Active_Used < Maximum_Regions;
      if Accepted then
         Run.Active_Used := Run.Active_Used + 1;
         Run.Active (Region_Node (Run.Active_Used)) := Region.Node;
         Run.Items (Region.Node) :=
           (Incarnation => Region.Incarnation, Space => Space,
            Current => Pending_Connect, Attempt_Generation => 0);
         Add_Pending (Run, Region.Node);
      end if;
   end Publish;

   procedure Adopt_Bulk
     (Run : in out State; Region : Region_Identity;
      Outcome : Bulk_Outcome; Accepted : out Boolean) is
   begin
      Accepted := Matches (Run, Region)
        and then Run.Items (Region.Node).Current = Pending_Connect
        and then Run.Items (Region.Node).Attempt_Generation = 0;
      if Accepted then
         Run.Items (Region.Node).Current :=
           (case Outcome is
              when Announced => Externally_Announced,
              when Covered => Externally_Covered,
              when Skipped => Externally_Skipped);
         Remove_Pending (Run, Region.Node);
      end if;
   end Adopt_Bulk;

   procedure Release_Bulk
     (Run : in out State; Region : Region_Identity;
      Succeeded : Boolean; Accepted : out Boolean) is
   begin
      Accepted := Matches (Run, Region)
        and then Run.Items (Region.Node).Current in
          Externally_Announced | Externally_Covered | Externally_Skipped;
      if Accepted then
         Run.Items (Region.Node).Current :=
           (if Succeeded then Externally_Closed else External_Disconnect_Failed);
      end if;
   end Release_Bulk;

   procedure Begin_Immediate
     (Run : in out State; Region : Region_Identity;
      Handler_Generation : Positive; Accepted : out Boolean) is
   begin
      Accepted := Matches (Run, Region)
        and then Run.Items (Region.Node).Current in Pending_Connect | Connect_Failed
        and then Run.Items (Region.Node).Attempt_Generation < Handler_Generation;
      if Accepted then
         Run.Items (Region.Node).Current := Connecting;
         Run.Items (Region.Node).Attempt_Generation := Handler_Generation;
         Remove_Pending (Run, Region.Node);
      end if;
   end Begin_Immediate;

   procedure Begin_Turn
     (Run : in out State; Turn_ID, Handler_Generation : Positive;
      Accepted : out Boolean) is
   begin
      Accepted := Turn_ID > Run.Last_Turn
        and then Handler_Generation >= Run.Turn_Generation;
      if Accepted then
         Run.Last_Turn := Turn_ID;
         Run.Turn_Generation := Handler_Generation;
         Run.Taken := 0;
      end if;
   end Begin_Turn;

   procedure Next_Candidate
     (Run : in out State; Found : out Boolean;
      Region : out Region_Identity; Space : out Interfaces.Unsigned_8) is
      Candidate : Region_Node;
   begin
      Found := False;
      Region := (Region_Node'First, 1);
      Space := 0;
      if Run.Turn_Generation = 0 or else Run.Taken = Max_Per_Turn
        or else Run.Pending_Used = 0 then return; end if;
      Run.Cursor := (Run.Cursor mod Run.Pending_Used) + 1;
      Candidate := Run.Pending (Region_Node (Run.Cursor));
      if Run.Items (Candidate).Incarnation = 0
        or else Run.Items (Candidate).Current not in Pending_Connect | Connect_Failed
      then return; end if;
      Found := True;
      Region := (Candidate, Positive (Run.Items (Candidate).Incarnation));
      Space := Run.Items (Candidate).Space;
   end Next_Candidate;

   procedure Claim_Candidate
     (Run : in out State; Region : Region_Identity; Accepted : out Boolean) is
   begin
      Accepted := Run.Turn_Generation > 0 and then Run.Taken < Max_Per_Turn
        and then Matches (Run, Region)
        and then Run.Items (Region.Node).Current in Pending_Connect | Connect_Failed
        and then Run.Items (Region.Node).Attempt_Generation < Run.Turn_Generation;
      if Accepted then
         Run.Items (Region.Node).Current := Connecting;
         Run.Items (Region.Node).Attempt_Generation := Run.Turn_Generation;
         Run.Taken := Run.Taken + 1;
         Remove_Pending (Run, Region.Node);
      end if;
   end Claim_Candidate;

   procedure Take_Next
     (Run : in out State; Ready : Ready_Set;
      Found : out Boolean; Region : out Region_Identity;
      Space : out Interfaces.Unsigned_8) is
      Candidate : Region_Node;
   begin
      Found := False;
      Region := (Region_Node'First, 1);
      Space := 0;
      if Run.Turn_Generation = 0 or else Run.Taken = Max_Per_Turn
        or else Run.Pending_Used = 0 then return; end if;
      for Step in 1 .. Run.Pending_Used loop
         declare
            Position : constant Region_Node :=
              Region_Node ((Run.Cursor mod Run.Pending_Used) + 1);
         begin
            Candidate := Run.Pending (Position);
            Run.Cursor := Position;
         end;
         if Ready (Candidate)
           and then Run.Items (Candidate).Incarnation > 0
           and then Run.Items (Candidate).Current in Pending_Connect | Connect_Failed
           and then Run.Items (Candidate).Attempt_Generation < Run.Turn_Generation
         then
            Run.Items (Candidate).Current := Connecting;
            Run.Items (Candidate).Attempt_Generation := Run.Turn_Generation;
            Run.Taken := Run.Taken + 1;
            Remove_Pending (Run, Candidate);
            Found := True;
            Region := (Candidate, Positive (Run.Items (Candidate).Incarnation));
            Space := Run.Items (Candidate).Space;
            return;
         end if;
      end loop;
   end Take_Next;

   procedure Finish_Connect
     (Run : in out State; Region : Region_Identity;
      Handler_Generation : Positive; Succeeded : Boolean;
      Accepted : out Boolean) is
   begin
      Accepted := Matches (Run, Region)
        and then Run.Items (Region.Node).Current = Connecting
        and then Run.Items (Region.Node).Attempt_Generation = Handler_Generation;
      if Accepted then
         Run.Items (Region.Node).Current :=
           (if Succeeded then Connected else Connect_Failed);
         if not Succeeded then Add_Pending (Run, Region.Node); end if;
      end if;
   end Finish_Connect;

   procedure Begin_Disconnect
     (Run : in out State; Region : Region_Identity; Accepted : out Boolean) is
   begin
      Accepted := Matches (Run, Region)
        and then Run.Items (Region.Node).Current = Connected;
      if Accepted then Run.Items (Region.Node).Current := Disconnecting; end if;
   end Begin_Disconnect;

   procedure Finish_Disconnect
     (Run : in out State; Region : Region_Identity;
      Succeeded : Boolean; Accepted : out Boolean) is
   begin
      Accepted := Matches (Run, Region)
        and then Run.Items (Region.Node).Current = Disconnecting;
      if Accepted then
         Run.Items (Region.Node).Current :=
           (if Succeeded then Pending_Connect else Disconnect_Failed);
         if Succeeded then Add_Pending (Run, Region.Node); end if;
      end if;
   end Finish_Disconnect;

   procedure Remove_Active (Run : in out State; Node : Region_Node) is
   begin
      for Index in 1 .. Run.Active_Used loop
         if Run.Active (Index) = Node then
            Run.Active (Index) :=
              Run.Active (Region_Node (Run.Active_Used));
            Run.Active (Region_Node (Run.Active_Used)) := Region_Node'First;
            Run.Active_Used := Run.Active_Used - 1;
            return;
         end if;
      end loop;
   end Remove_Active;

   procedure Retire
     (Run : in out State; Region : Region_Identity; Accepted : out Boolean) is
   begin
      Accepted := Matches (Run, Region)
        and then Run.Items (Region.Node).Current in Pending_Connect | Connect_Failed | Externally_Closed;
      if Accepted then
         Remove_Pending (Run, Region.Node);
         Run.Items (Region.Node) := (others => <>);
         Remove_Active (Run, Region.Node);
      end if;
   end Retire;

   procedure Retire_Method_Local
     (Run : in out State; Region : Region_Identity; Accepted : out Boolean) is
   begin
      Accepted := Matches (Run, Region)
        and then Run.Items (Region.Node).Current in
          Pending_Connect | Connected | Connect_Failed | Externally_Closed;
      if Accepted then
         Remove_Pending (Run, Region.Node);
         Run.Items (Region.Node) := (others => <>);
         Remove_Active (Run, Region.Node);
      end if;
   end Retire_Method_Local;
end AML_Dynamic_Region_Connections;
