pragma Ada_2022;
with Interfaces;

--  Process-local bookkeeping only. Neither a declaration nor this ledger
--  grants address-space authority or authenticates a provider.
generic
   Maximum_Regions : Positive;
package AML_Dynamic_Region_Connections with SPARK_Mode is
   subtype Region_Node is Positive range 1 .. Maximum_Regions;
   type Region_Identity is record
      Node : Region_Node;
      Incarnation : Positive;
   end record;
   type Ready_Set is array (Region_Node) of Boolean;
   type Phase is
     (Absent, Pending_Connect, Connecting, Connected,
      Connect_Failed, Disconnecting, Disconnect_Failed,
      Externally_Announced, Externally_Covered, Externally_Skipped,
      Externally_Closed, External_Disconnect_Failed);
   type Observation is record
      Current : Phase := Absent;
      Space : Interfaces.Unsigned_8 := 0;
      Attempt_Generation : Natural := 0;
   end record;
   type State is private;

   Max_Per_Turn : constant Positive := 4;
   Callback_Fuel : constant Positive := 100_000;

   function Observe (Run : State; Region : Region_Identity) return Observation
     with Annotate => (GNATprove, Inline_For_Proof);
   --  Dense list of live published region nodes for lifecycle bookkeeping.
   --  Retire removes its exact entry.
   function Active_Region_Count (Run : State) return Natural;
   function Active_Region_Node (Run : State; Index : Positive) return Region_Node
     with Pre => Index <= Active_Region_Count (Run);
   --  Only regions still eligible for a future connect occupy this list.
   --  Connected and externally adopted regions cost no service-loop scan.
   function Pending_Region_Count (Run : State) return Natural;
   function Pending_Region_Node (Run : State; Index : Positive) return Region_Node
     with Pre => Index <= Pending_Region_Count (Run);
   function Callbacks_This_Turn (Run : State) return Natural
     with Post => Callbacks_This_Turn'Result <= Max_Per_Turn;
   function Current_Turn_Generation (Run : State) return Natural;
   procedure Publish
     (Run : in out State; Region : Region_Identity;
      Space : Interfaces.Unsigned_8; Accepted : out Boolean);
   --  Immediate declaration path uses the declaring method's own fuel and
   --  depth. The provider generation is copied only after a trusted ack.
   --  Native startup owns these exact node incarnations. Only Announced
   --  carries an actual _REG(1) and its matching native _REG(0) obligation;
   --  Covered was coalesced with that group, Skipped made no callback.
   type Bulk_Outcome is (Announced, Covered, Skipped);
   procedure Adopt_Bulk
     (Run : in out State; Region : Region_Identity;
      Outcome : Bulk_Outcome; Accepted : out Boolean);
   --  Call only after the native group disconnect has completed. A failed
   --  disconnect remains terminal and cannot be retried as a late connect.
   procedure Release_Bulk
     (Run : in out State; Region : Region_Identity;
      Succeeded : Boolean; Accepted : out Boolean);
   procedure Begin_Immediate
     (Run : in out State; Region : Region_Identity;
      Handler_Generation : Positive; Accepted : out Boolean)
     with Post => (if Accepted then
       Observe (Run, Region).Current = Connecting
       and then Observe (Run, Region).Attempt_Generation = Handler_Generation
       and then Observe (Run'Old, Region).Current in Pending_Connect | Connect_Failed
       and then Observe (Run'Old, Region).Attempt_Generation < Handler_Generation);
   --  Call only after the provider install has been acknowledged. Ready is a
   --  trusted snapshot; false entries are never selected for AML callbacks.
   --  Turn_ID is issued by the serialized service loop. Duplicate or stale
   --  IDs cannot reset the four-callback allowance; generations never rewind.
   procedure Begin_Turn
     (Run : in out State; Turn_ID, Handler_Generation : Positive;
      Accepted : out Boolean)
     with Post => (if Accepted then Callbacks_This_Turn (Run) = 0);
   --  A serialized service turn walks only the dense pending list. A
   --  candidate is not charged or marked attempted until its trusted handler
   --  snapshot and method shape have been checked by the caller.
   procedure Next_Candidate
     (Run : in out State; Found : out Boolean;
      Region : out Region_Identity; Space : out Interfaces.Unsigned_8)
     with Post => (if Found then
       Observe (Run, Region).Current in Pending_Connect | Connect_Failed);
   procedure Claim_Candidate
     (Run : in out State; Region : Region_Identity; Accepted : out Boolean)
     with Post => (if Accepted then
       Observe (Run, Region).Current = Connecting
       and then Observe (Run, Region).Attempt_Generation = Current_Turn_Generation (Run)
       and then Observe (Run'Old, Region).Current in Pending_Connect | Connect_Failed
       and then Observe (Run'Old, Region).Attempt_Generation < Current_Turn_Generation (Run)
       and then Callbacks_This_Turn (Run) = Callbacks_This_Turn (Run'Old) + 1);
   procedure Take_Next
     (Run : in out State; Ready : Ready_Set;
      Found : out Boolean; Region : out Region_Identity;
      Space : out Interfaces.Unsigned_8)
     with Post => (if Found then
       Ready (Region.Node)
       and then Observe (Run, Region).Current = Connecting
       and then Observe (Run, Region).Attempt_Generation = Current_Turn_Generation (Run)
       and then Observe (Run'Old, Region).Current in Pending_Connect | Connect_Failed
       and then Observe (Run'Old, Region).Attempt_Generation < Current_Turn_Generation (Run)
       and then Callbacks_This_Turn (Run) = Callbacks_This_Turn (Run'Old) + 1);
   procedure Finish_Connect
     (Run : in out State; Region : Region_Identity;
      Handler_Generation : Positive; Succeeded : Boolean;
      Accepted : out Boolean)
     with Post => (if Accepted then
       Observe (Run'Old, Region).Current = Connecting
       and then Observe (Run'Old, Region).Attempt_Generation = Handler_Generation
       and then Observe (Run, Region).Current =
         (if Succeeded then Connected else Connect_Failed)
       and then Observe (Run, Region).Attempt_Generation = Handler_Generation);
   procedure Begin_Disconnect
     (Run : in out State; Region : Region_Identity; Accepted : out Boolean)
     with Post => (if Accepted then
       Observe (Run'Old, Region).Current = Connected
       and then Observe (Run, Region).Current = Disconnecting);
   procedure Finish_Disconnect
     (Run : in out State; Region : Region_Identity;
      Succeeded : Boolean; Accepted : out Boolean)
     with Post => (if Accepted then
       Observe (Run'Old, Region).Current = Disconnecting
       and then Observe (Run, Region).Current =
         (if Succeeded then Pending_Connect else Disconnect_Failed));
   --  Refuse to discard an owed or uncertain disconnect, or an in-flight AML
   --  callback. A recycled node is a different identity.
   procedure Retire
     (Run : in out State; Region : Region_Identity; Accepted : out Boolean);
   --  A method-local region expires with its declaring invocation. ACPICA
   --  does not call _REG (0) at method exit, even after a successful connect.
   procedure Retire_Method_Local
     (Run : in out State; Region : Region_Identity; Accepted : out Boolean);
private
   type Region_Entry is record
      Incarnation : Natural := 0;
      Space : Interfaces.Unsigned_8 := 0;
      Current : Phase := Absent;
      Attempt_Generation : Natural := 0;
   end record;
   type Entries is array (Region_Node) of Region_Entry;
   type Active_List is array (Region_Node) of Region_Node;
   type Pending_Positions is array (Region_Node) of Natural range 0 .. Maximum_Regions;
   type State is record
      Items : Entries := [others => <>];
      Active : Active_List := [others => Region_Node'First];
      Active_Used : Natural range 0 .. Maximum_Regions := 0;
      Pending : Active_List := [others => Region_Node'First];
      Pending_Position : Pending_Positions := [others => 0];
      Pending_Used : Natural range 0 .. Maximum_Regions := 0;
      --  Position in Pending, not a namespace node number.
      Cursor : Natural range 0 .. Maximum_Regions := 0;
      Last_Turn : Natural := 0;
      Turn_Generation : Natural := 0;
      Taken : Natural range 0 .. Max_Per_Turn := 0;
   end record;
   function Observe (Run : State; Region : Region_Identity) return Observation is
     (if Run.Items (Region.Node).Current = Absent
        or else Run.Items (Region.Node).Incarnation /= Region.Incarnation
      then (others => <>)
      else (Current => Run.Items (Region.Node).Current,
            Space => Run.Items (Region.Node).Space,
            Attempt_Generation => Run.Items (Region.Node).Attempt_Generation));
end AML_Dynamic_Region_Connections;
