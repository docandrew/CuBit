pragma Ada_2022;
with AML_Names;
with AML_Objects;
-- Internal arena-local metadata only; IDs are not authority or live references.
generic
   type Scope_ID is (<>);
   Max_Members : Positive;
   Max_Segments : Positive;
package AML_Pending_Members with SPARK_Mode is
   subtype Member_Count is Natural range 0 .. Max_Members;
   subtype Segment_Count is Natural range 0 .. Max_Segments;
   subtype Element_Index is Natural range 0 .. AML_Objects.Max_Elements - 1;
   type State is private;
   function Count (Journal : State) return Member_Count;
   function Segments_Used (Journal : State) return Segment_Count;
   function Valid (Journal : State) return Boolean;
   function Empty return State
     with Post => Valid (Empty'Result) and then Count (Empty'Result) = 0
       and then Segments_Used (Empty'Result) = 0;
   function Valid_Path (Path : AML_Names.Name_Result) return Boolean;
   -- Consumed is reconstructed using the shortest legal NameString encoding;
   -- unused Parts are canonical "____", rather than retaining caller padding.
   function Canonical (Path : AML_Names.Name_Result) return AML_Names.Name_Result
     with Pre => Valid_Path (Path),
          Post => Valid_Path (Canonical'Result);
   type Member_Result (Found : Boolean := False) is record
      case Found is
         when True =>
            Package_ID : AML_Objects.Object_ID range 1 .. AML_Objects.Max_Objects;
            Element : Element_Index;
            Scope : Scope_ID;
            Path : AML_Names.Name_Result (AML_Names.Accepted);
         when False => null;
      end case;
   end record;
   function Item (Journal : State; Index : Natural) return Member_Result
     with Pre => Valid (Journal),
          Post => Item'Result.Found = (Index in 1 .. Count (Journal));
   type Append_Status is (Appended, Invalid_Input, Member_Limit, Segment_Limit);
   procedure Append
     (Journal : in out State; Package_ID : AML_Objects.Object_ID;
      Element : Natural; Scope : Scope_ID; Path : AML_Names.Name_Result;
      Status : out Append_Status)
     with Pre => Valid (Journal),
          Post => Valid (Journal) and then
            (if Status /= Appended then Journal = Journal'Old
             else Count (Journal) = Count (Journal'Old) + 1
               and then Segments_Used (Journal) =
                 Segments_Used (Journal'Old) + Path.Count
               and then Item (Journal, Count (Journal)) =
                 Member_Result'(True, Package_ID, Element, Scope, Canonical (Path))
               and then (for all I in 1 .. Count (Journal'Old) =>
                 Item (Journal, I) = Item (Journal'Old, I)));
   procedure Clear (Journal : out State)
     with Post => Valid (Journal) and then Journal = Empty;
private
   subtype Path_Count is Natural range 0 .. AML_Names.Segment_Array'Length;
   type Member is record
      Package_ID : AML_Objects.Object_ID := 0;
      Element : Element_Index := 0;
      Scope : Scope_ID := Scope_ID'First;
      Rooted : Boolean := False;
      Parents : Path_Count := 0;
      Length : Path_Count := 0;
      Start : Segment_Count := 0;
   end record;
   type Member_Array is array (Positive range <>) of Member;
   type Segment_Pool is array (Positive range <>) of AML_Names.Segment;
   type State is record
      Used : Member_Count := 0;
      Filled : Segment_Count := 0;
      Members : Member_Array (1 .. Max_Members) := [others => <>];
      Segments : Segment_Pool (1 .. Max_Segments) := [others => "____"];
   end record;
end AML_Pending_Members;
