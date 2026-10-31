with AML_Frame_Handles;
with AML_Execution_Limits;
-- Declaration authentication. Intent alone grants no authority: the owner
-- admits each method or module lease against its live frame and session.
package AML_Declaration_Contexts with SPARK_Mode, Pure is
   use type AML_Frame_Handles.Frame_Handle;
   Max_Active_Executions : constant Positive := AML_Frame_Handles.Max_Active_Frames;
   type Request is private;
   Method_Request : constant Request;
   Module_Request : constant Request;
   function Is_Module (Intent : Request) return Boolean;
   type Lease is private;
   No_Lease : constant Lease;
   function Is_Module (Item : Lease) return Boolean;
   type Scope_Node is new Natural;
   type Scope_Incarnation is new Natural;
   type Scope_Identity is private;
   No_Scope : constant Scope_Identity;
   Root_Scope : constant Scope_Identity;
   function Present (Item : Scope_Identity) return Boolean;
   function Node_Of (Item : Scope_Identity) return Scope_Node;
   function Stamp_Of (Item : Scope_Identity) return Scope_Incarnation;
   function Make_Scope (Node : Scope_Node; Stamp : Scope_Incarnation) return Scope_Identity
     with Pre => Node /= 0 or else Stamp = 0,
       Post => Present (Make_Scope'Result)
         and then Node_Of (Make_Scope'Result) = Node
         and then Stamp_Of (Make_Scope'Result) = Stamp;
   -- Module invocation stamps and lexical entry incarnations are independent.
   -- Scope_Of remains the definition lifetime owner(root for every module).
   function Entry_Identity (Item : Lease) return Scope_Identity;
   subtype Lexical_Depth is Natural range 0 .. AML_Execution_Limits.Max_Lexical_Depth;
   type State is private;
   function Valid (Store : State) return Boolean;
   function Empty return State with Post => Valid (Empty'Result);
   function Is_Live (Store : State; Item : Lease) return Boolean;
   function Added_Lease (Current, Prior : State; Item : Lease) return Boolean with Ghost;
   function Removed_Lease (Current, Prior : State; Item : Lease) return Boolean with Ghost;
   function Lexical_Top (Store : State; Owner : Lease) return Scope_Identity;
   function Lexical_Count (Store : State; Owner : Lease) return Lexical_Depth;
   function Pushed_Scope (Current, Prior : State; Owner : Lease; Child : Scope_Identity) return Boolean with Ghost;
   function Popped_Scope (Current, Prior : State; Owner : Lease) return Boolean with Ghost;
   -- Descriptive node identities grant no authority. Namespace wrappers must
   -- resolve/validate live nodes and incarnations before calling this ledger.
   procedure Push_Scope (Store : in out State; Owner : Lease;
      Expected_Top, Child : Scope_Identity; Allowed : out Boolean)
     with Pre => Valid (Store), Post => Valid (Store)
       and then (if Allowed then Pushed_Scope (Store, Store'Old, Owner, Child)
         and then Lexical_Top (Store, Owner) = Child
         and then Lexical_Count (Store, Owner) = Lexical_Count (Store'Old, Owner) + 1
       else Store = Store'Old);
   procedure Pop_Scope (Store : in out State; Owner : Lease;
      Expected_Top : Scope_Identity; Allowed : out Boolean)
     with Pre => Valid (Store), Post => Valid (Store)
       and then (if Allowed then Popped_Scope (Store, Store'Old, Owner)
         and then Lexical_Count (Store, Owner) + 1 = Lexical_Count (Store'Old, Owner)
       else Store = Store'Old);
   function Scope_Of (Item : Lease) return Natural;
   function Stamp_Of (Item : Lease) return Natural;
   function Frame_Of (Item : Lease) return AML_Frame_Handles.Frame_Handle;
   -- The owner must supply a genuinely opened, fresh frame; this ledger does
   -- not mint or independently authenticate frames. Reopening an already
   -- retired identical frame handle violates that owner protocol.
   procedure Open (Store : in out State; Intent : Request;
      Frame : AML_Frame_Handles.Frame_Handle; Scope, Stamp : Natural;
      Item : out Lease; Allowed : out Boolean)
     with Pre => Valid (Store), Post => Valid (Store)
       and then (if Allowed then Added_Lease (Store, Store'Old, Item) and then Is_Live (Store, Item)
         and then Scope_Of (Item) = Scope and then Stamp_Of (Item) = Stamp
         and then Frame_Of (Item) = Frame
         and then Is_Module (Item) = Is_Module (Intent)
         and then Lexical_Top (Store, Item) = Entry_Identity (Item)
         and then Lexical_Count (Store, Item) = 0
       else Store = Store'Old and then Item = No_Lease);
   -- The embedding owner authenticates the selected live scope and frame.
   procedure Open_Module (Store : in out State;
      Frame : AML_Frame_Handles.Frame_Handle; Initial_Scope : Scope_Identity;
      Invocation : AML_Frame_Handles.Invocation_Serial;
      Item : out Lease; Allowed : out Boolean)
     with Pre => Valid (Store), Post => Valid (Store)
       and then (if Allowed then Added_Lease (Store, Store'Old, Item)
          and then Is_Live (Store, Item) and then Is_Module (Item)
          and then Scope_Of (Item) = Natural (Node_Of (Root_Scope))
          and then Stamp_Of (Item) = Natural (Invocation)
          and then Frame_Of (Item) = Frame
          and then Entry_Identity (Item) = Initial_Scope
          and then Lexical_Top (Store, Item) = Initial_Scope
          and then Lexical_Count (Store, Item) = 0
        else Store = Store'Old and then Item = No_Lease);
   procedure Close (Store : in out State; Item : Lease)
     with Pre => Valid (Store), Post => Valid (Store)
       and then Removed_Lease (Store, Store'Old, Item)
       and then not Is_Live (Store, Item)
       and then Lexical_Top (Store, Item) = No_Scope
       and then (if not Is_Live (Store'Old, Item) then Store = Store'Old);
   -- Executing runtime OperationRegion only; not a deferred table descriptor.
   type Operation_Region_Reservation is private;
   No_Operation_Region : constant Operation_Region_Reservation;
   function Reserve_Operation_Region
     (Owner : Lease; Node, Stamp : Natural) return Operation_Region_Reservation;
   function Issued_By (Token : Operation_Region_Reservation; Owner : Lease) return Boolean;
   function Node_Of (Token : Operation_Region_Reservation) return Natural;
   function Stamp_Of (Token : Operation_Region_Reservation) return Natural;
   type Region_Reservation is private;
   No_Region : constant Region_Reservation;
   function Reserve_Region (Owner : Lease; Node, Stamp : Natural) return Region_Reservation;
   function Issued_By (Token : Region_Reservation; Owner : Lease) return Boolean;
   function Node_Of (Token : Region_Reservation) return Natural;
   function Stamp_Of (Token : Region_Reservation) return Natural;
private
   type Request_Kind is (No_Request, Method_Execution, Module_Execution);
   type Request is record Kind : Request_Kind := No_Request; end record;
   Method_Request : constant Request := (Kind => Method_Execution);
   Module_Request : constant Request := (Kind => Module_Execution);
   subtype Lease_Slot is Natural range 0 .. Max_Active_Executions;
   type Lease is record
      Slot : Lease_Slot := 0;
      Intent : Request := (Kind => No_Request);
      Frame : AML_Frame_Handles.Frame_Handle := AML_Frame_Handles.No_Frame;
      Scope, Stamp : Natural := 0;
      Entry_Node : Scope_Node := 0;
      Entry_Stamp : Scope_Incarnation := 0;
   end record;
   No_Lease : constant Lease := (others => <>);
   type Entries is array (Positive range 1 .. Max_Active_Executions) of Lease;
   type Scope_Identity is record
      Exists : Boolean := False;
      Node : Scope_Node := 0;
      Stamp : Scope_Incarnation := 0;
   end record;
   No_Scope : constant Scope_Identity := (others => <>);
   Root_Scope : constant Scope_Identity := (Exists => True, Node => 0, Stamp => 0);
   type Scope_Entries is array (Lexical_Depth) of Scope_Identity;
   type Lexical_Stack is record
      Depth : Lexical_Depth := 0;
      Items : Scope_Entries := [others => No_Scope];
   end record;
   Empty_Stack : constant Lexical_Stack := (others => <>);
   type Lexical_Stacks is array (Positive range 1 .. Max_Active_Executions) of Lexical_Stack;
   type State is record
      Items : Entries := [others => No_Lease];
      Scopes : Lexical_Stacks := [others => Empty_Stack];
   end record;
   type Operation_Region_Reservation is record
      Owner : Lease := No_Lease;
      Node, Stamp : Natural := 0;
   end record;
   No_Operation_Region : constant Operation_Region_Reservation := (others => <>);
   type Region_Reservation is record
      Owner : Lease := No_Lease;
      Node, Stamp : Natural := 0;
   end record;
   No_Region : constant Region_Reservation := (others => <>);
end AML_Declaration_Contexts;
