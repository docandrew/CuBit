package body AML_Declaration_Contexts with SPARK_Mode is
   function Present (Item : Scope_Identity) return Boolean is (Item.Exists);
   function Make_Scope (Node : Scope_Node; Stamp : Scope_Incarnation) return Scope_Identity is
     ((Exists => True, Node => Node, Stamp => Stamp));
   function Node_Of (Item : Scope_Identity) return Scope_Node is (Item.Node);
   function Stamp_Of (Item : Scope_Identity) return Scope_Incarnation is (Item.Stamp);
   function Entry_Identity (Item : Lease) return Scope_Identity is
     (if Item = No_Lease then No_Scope elsif Item.Scope = 0 then Root_Scope
      else (Exists => True, Node => Scope_Node (Item.Scope), Stamp => Scope_Incarnation (Item.Stamp)));
   function Initial_Stack (Item : Lease) return Lexical_Stack is
     ((Depth => 0, Items => [0 => Entry_Identity (Item), others => No_Scope]));
   function Valid (Store : State) return Boolean is
     (for all I in Store.Items'Range =>
       (if Store.Items (I) = No_Lease then Store.Scopes (I) = Empty_Stack else
         Store.Items (I).Slot = I and then AML_Frame_Handles.Present (Store.Items (I).Frame)
         and then Store.Scopes (I).Items (0) = Entry_Identity (Store.Items (I))
         and then (for all K in Lexical_Depth =>
           (if K <= Store.Scopes (I).Depth then
              Present (Store.Scopes (I).Items (K))
              and then (Store.Scopes (I).Items (K).Node /= 0 or else Store.Scopes (I).Items (K).Stamp = 0)
            else Store.Scopes (I).Items (K) = No_Scope))
         and then (for all J in Store.Items'First .. I - 1 =>
           Store.Items (J) = No_Lease or else Store.Items (J).Frame /= Store.Items (I).Frame)));
   function Is_Module (Intent : Request) return Boolean is (Intent = Module_Request);
   function Is_Module (Item : Lease) return Boolean is (Is_Module (Item.Intent));
   function Empty return State is ((others => <>));
   function Is_Live (Store : State; Item : Lease) return Boolean is
     (Item.Slot /= 0 and then Store.Items (Item.Slot) = Item);
   function Added_Lease (Current, Prior : State; Item : Lease) return Boolean is
     (Item.Slot /= 0 and then Prior.Items (Item.Slot) = No_Lease
      and then Current = (Prior with delta Items => (Prior.Items with delta Item.Slot => Item),
         Scopes => (Prior.Scopes with delta Item.Slot => Initial_Stack (Item))));
   function Removed_Lease (Current, Prior : State; Item : Lease) return Boolean is
     (if Is_Live (Prior, Item) then
        Current = (Prior with delta Items => (Prior.Items with delta Item.Slot => No_Lease),
          Scopes => (Prior.Scopes with delta Item.Slot => Empty_Stack))
      else Current = Prior);
   function Scope_Of (Item : Lease) return Natural is (Item.Scope);
   function Stamp_Of (Item : Lease) return Natural is (Item.Stamp);
   function Frame_Of (Item : Lease) return AML_Frame_Handles.Frame_Handle is (Item.Frame);
   procedure Open (Store : in out State; Intent : Request;
      Frame : AML_Frame_Handles.Frame_Handle; Scope, Stamp : Natural;
      Item : out Lease; Allowed : out Boolean) is
      Free : Lease_Slot := 0;
   begin
      Item := No_Lease; Allowed := False;
      if (Intent /= Method_Request and then Intent /= Module_Request) or else not AML_Frame_Handles.Present (Frame) then return; end if;
      for I in Store.Items'Range loop
         if Store.Items (I) = No_Lease then
            if Free = 0 then Free := I; end if;
         elsif Store.Items (I).Frame = Frame then return; end if;
         pragma Loop_Invariant (Free = 0 or else Store.Items (Free) = No_Lease);
         pragma Loop_Invariant
           (for all J in Store.Items'First .. I =>
              Store.Items (J) = No_Lease or else Store.Items (J).Frame /= Frame);
      end loop;
      if Free = 0 then return; end if;
      Item := (Slot => Free, Intent => Intent, Frame => Frame, Scope => Scope, Stamp => Stamp);
      Store.Items (Free) := Item; Store.Scopes (Free) := Initial_Stack (Item); Allowed := True;
   end Open;
   procedure Close (Store : in out State; Item : Lease) is
   begin
      if Is_Live (Store, Item) then
         Store.Items (Item.Slot) := No_Lease; Store.Scopes (Item.Slot) := Empty_Stack;
      end if;
   end Close;
   function Lexical_Top (Store : State; Owner : Lease) return Scope_Identity is
     (if Is_Live (Store, Owner) then Store.Scopes (Owner.Slot).Items (Store.Scopes (Owner.Slot).Depth) else No_Scope);
   function Lexical_Count (Store : State; Owner : Lease) return Lexical_Depth is
     (if Is_Live (Store, Owner) then Store.Scopes (Owner.Slot).Depth else 0);
   function Pushed_Scope (Current, Prior : State; Owner : Lease; Child : Scope_Identity) return Boolean is
     (Is_Live (Prior, Owner) and then Prior.Scopes (Owner.Slot).Depth < Lexical_Depth'Last
      and then Current = (Prior with delta Scopes => (Prior.Scopes with delta Owner.Slot =>
        (Depth => Prior.Scopes (Owner.Slot).Depth + 1,
         Items => (Prior.Scopes (Owner.Slot).Items with delta Prior.Scopes (Owner.Slot).Depth + 1 => Child)))));
   function Popped_Scope (Current, Prior : State; Owner : Lease) return Boolean is
     (Is_Live (Prior, Owner) and then Prior.Scopes (Owner.Slot).Depth > 0
      and then Current = (Prior with delta Scopes => (Prior.Scopes with delta Owner.Slot =>
        (Depth => Prior.Scopes (Owner.Slot).Depth - 1,
         Items => (Prior.Scopes (Owner.Slot).Items with delta Prior.Scopes (Owner.Slot).Depth => No_Scope)))));
   procedure Push_Scope (Store : in out State; Owner : Lease;
      Expected_Top, Child : Scope_Identity; Allowed : out Boolean) is
   begin
      Allowed := False;
      if not Is_Live (Store, Owner) or else not Present (Child)
        or else (Child.Node = 0 and then Child.Stamp /= 0)
        or else Lexical_Top (Store, Owner) /= Expected_Top
        or else Store.Scopes (Owner.Slot).Depth = Lexical_Depth'Last then return; end if;
      Store.Scopes (Owner.Slot).Depth := Store.Scopes (Owner.Slot).Depth + 1;
      Store.Scopes (Owner.Slot).Items (Store.Scopes (Owner.Slot).Depth) := Child;
      Allowed := True;
   end Push_Scope;
   procedure Pop_Scope (Store : in out State; Owner : Lease;
      Expected_Top : Scope_Identity; Allowed : out Boolean) is
   begin
      Allowed := False;
      if not Is_Live (Store, Owner) or else Lexical_Top (Store, Owner) /= Expected_Top
        or else Store.Scopes (Owner.Slot).Depth = 0 then return; end if;
      Store.Scopes (Owner.Slot).Items (Store.Scopes (Owner.Slot).Depth) := No_Scope;
      Store.Scopes (Owner.Slot).Depth := Store.Scopes (Owner.Slot).Depth - 1;
      Allowed := True;
   end Pop_Scope;
   function Reserve_Region (Owner : Lease; Node, Stamp : Natural) return Region_Reservation is
     (if Owner = No_Lease or else Node = 0 then No_Region
      else (Owner => Owner, Node => Node, Stamp => Stamp));
   function Issued_By (Token : Region_Reservation; Owner : Lease) return Boolean is
     (Owner /= No_Lease and then Token.Node /= 0 and then Token.Owner = Owner);
   function Node_Of (Token : Region_Reservation) return Natural is (Token.Node);
   function Stamp_Of (Token : Region_Reservation) return Natural is (Token.Stamp);
   function Reserve_Operation_Region
     (Owner : Lease; Node, Stamp : Natural) return Operation_Region_Reservation is
     (if Owner = No_Lease or else Node = 0 then No_Operation_Region
      else (Owner => Owner, Node => Node, Stamp => Stamp));
   function Issued_By (Token : Operation_Region_Reservation; Owner : Lease) return Boolean is
     (Owner /= No_Lease and then Token.Node /= 0 and then Token.Owner = Owner);
   function Node_Of (Token : Operation_Region_Reservation) return Natural is (Token.Node);
   function Stamp_Of (Token : Operation_Region_Reservation) return Natural is (Token.Stamp);
end AML_Declaration_Contexts;
