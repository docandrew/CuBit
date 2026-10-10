with AML_Identity;
with AML_Frame_Handles;
with AML_Object_Identifiers;
with AML_Index_Handles;
package AML_References with SPARK_Mode, Pure is
   -- Internal interpreter values. An owner never exposes its live identity.
   type Object_Handle is private;
   No_Object_Handle : constant Object_Handle;
   function Bind_Object (Token : AML_Identity.Identity; Item : AML_Object_Identifiers.Object_Address) return Object_Handle;
   function Belongs_To (H : Object_Handle; Token : AML_Identity.Identity) return Boolean;
   function Source (H : Object_Handle) return AML_Object_Identifiers.Object_ID;
   function Address (H : Object_Handle) return AML_Object_Identifiers.Object_Address;
   type Reference is private;
   type Node_Position is new Natural;
   type Node_Incarnation is new Natural;
   subtype Incarnation_Budget is Node_Incarnation range 1 .. Node_Incarnation'Last;
   type Reference_Kind is (Absent, Byte_Slot, Package_Slot, Named_Cell, Frame_Cell, Name_Member);
   function Kind (R : Reference) return Reference_Kind;
   function Well_Formed (R : Reference) return Boolean;
   function Bind_Named
     (Token : AML_Identity.Identity; Node : Node_Position; Incarnation : Node_Incarnation)
      return Reference;
   function Bind_Name_Member
     (Token : AML_Identity.Identity; Node : Node_Position; Incarnation : Node_Incarnation)
      return Reference;
   function Named_Node (R : Reference) return Node_Position with Pre => Kind (R) in Named_Cell | Name_Member;
   function Incarnation (R : Reference) return Node_Incarnation with Pre => Kind (R) in Named_Cell | Name_Member;
   function Bind_Frame (Cell : AML_Frame_Handles.Cell_Handle) return Reference;
   function Frame_Item (R : Reference) return AML_Frame_Handles.Cell_Handle
     with Pre => Kind (R) = Frame_Cell;
   function Belongs_To (R : Reference; Token : AML_Identity.Identity) return Boolean;
   function Bind (Token : AML_Identity.Identity;
     Item : AML_Index_Handles.Byte_Reference) return Reference;
   function Bind (Token : AML_Identity.Identity;
     Item : AML_Index_Handles.Package_Reference) return Reference;
   function Byte_Item (R : Reference) return AML_Index_Handles.Byte_Reference
     with Pre => Kind (R) = Byte_Slot;
   function Package_Item (R : Reference) return AML_Index_Handles.Package_Reference
     with Pre => Kind (R) = Package_Slot;
   function Target (R : Reference) return AML_Object_Identifiers.Object_ID;
   function Offset (R : Reference) return Natural;
   No_Reference : constant Reference;
private
   use type AML_Identity.Identity;
   type Object_Handle is record
      Token : AML_Identity.Identity := AML_Identity.No_Identity;
      Item : AML_Object_Identifiers.Object_Address := AML_Object_Identifiers.No_Address;
   end record;
   No_Object_Handle : constant Object_Handle := (others => <>);
   function Bind_Object (Token : AML_Identity.Identity; Item : AML_Object_Identifiers.Object_Address) return Object_Handle is
     (if Token = AML_Identity.No_Identity or else not AML_Object_Identifiers.Present (Item)
      then No_Object_Handle else (Token => Token, Item => Item));
   function Belongs_To (H : Object_Handle; Token : AML_Identity.Identity) return Boolean is
     (Token /= AML_Identity.No_Identity and then H.Token = Token and then AML_Object_Identifiers.Present (H.Item));
   function Source (H : Object_Handle) return AML_Object_Identifiers.Object_ID is (AML_Object_Identifiers.Slot_Of (H.Item));
   function Address (H : Object_Handle) return AML_Object_Identifiers.Object_Address is (H.Item);
   type Reference is record
      Tag : Reference_Kind := Absent;
      Token : AML_Identity.Identity := AML_Identity.No_Identity;
      Node : Node_Position := 0;
      Stamp : Node_Incarnation := 0;
      Cell : AML_Frame_Handles.Cell_Handle := AML_Frame_Handles.No_Cell;
      Bytes : AML_Index_Handles.Byte_Reference := AML_Index_Handles.No_Byte_Reference;
      Elements : AML_Index_Handles.Package_Reference := AML_Index_Handles.No_Package_Reference;
   end record;
   No_Reference : constant Reference := (others => <>);
   function Kind (R : Reference) return Reference_Kind is (R.Tag);
   function Well_Formed (R : Reference) return Boolean is
     (case R.Tag is
        when Absent => False,
        when Byte_Slot => R.Token /= AML_Identity.No_Identity
          and then AML_Index_Handles.Present (R.Bytes),
        when Package_Slot => R.Token /= AML_Identity.No_Identity
          and then AML_Index_Handles.Present (R.Elements),
        when Named_Cell | Name_Member => R.Token /= AML_Identity.No_Identity
          and then R.Node > 0 and then R.Stamp > 0,
        when Frame_Cell => AML_Frame_Handles.Present (R.Cell));
   function Belongs_To (R : Reference; Token : AML_Identity.Identity) return Boolean is
     (Token /= AML_Identity.No_Identity and then R.Token = Token and then R.Tag /= Absent);
   function Bind (Token : AML_Identity.Identity;
     Item : AML_Index_Handles.Byte_Reference) return Reference is
     (Tag => Byte_Slot, Token => Token, Bytes => Item, others => <>);
   function Bind (Token : AML_Identity.Identity;
     Item : AML_Index_Handles.Package_Reference) return Reference is
     (Tag => Package_Slot, Token => Token, Elements => Item, others => <>);
   function Bind_Named
     (Token : AML_Identity.Identity; Node : Node_Position; Incarnation : Node_Incarnation)
      return Reference is
     (if Token = AML_Identity.No_Identity or else Node = 0 or else Incarnation = 0 then No_Reference
      else (Tag => Named_Cell, Token => Token, Node => Node, Stamp => Incarnation, others => <>));
   function Bind_Name_Member
     (Token : AML_Identity.Identity; Node : Node_Position; Incarnation : Node_Incarnation)
      return Reference is
     (if Token = AML_Identity.No_Identity or else Node = 0 or else Incarnation = 0 then No_Reference
      else (Tag => Name_Member, Token => Token, Node => Node, Stamp => Incarnation, others => <>));
   function Named_Node (R : Reference) return Node_Position is (R.Node);
   function Incarnation (R : Reference) return Node_Incarnation is (R.Stamp);
   function Bind_Frame (Cell : AML_Frame_Handles.Cell_Handle) return Reference is
     (if AML_Frame_Handles.Present (Cell) then (Tag => Frame_Cell, Cell => Cell, others => <>)
      else No_Reference);
   function Frame_Item (R : Reference) return AML_Frame_Handles.Cell_Handle is (R.Cell);
   function Byte_Item (R : Reference) return AML_Index_Handles.Byte_Reference is (R.Bytes);
   function Package_Item (R : Reference) return AML_Index_Handles.Package_Reference is (R.Elements);
   function Target (R : Reference) return AML_Object_Identifiers.Object_ID is
     (case R.Tag is when Absent | Named_Cell | Frame_Cell | Name_Member => 0,
      when Byte_Slot => AML_Index_Handles.Owner (R.Bytes),
      when Package_Slot => AML_Index_Handles.Owner (R.Elements));
   function Offset (R : Reference) return Natural is
     (case R.Tag is when Absent | Named_Cell | Frame_Cell | Name_Member => 0,
      when Byte_Slot => AML_Index_Handles.Offset (R.Bytes),
      when Package_Slot => AML_Index_Handles.Offset (R.Elements));
end AML_References;
