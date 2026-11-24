with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed;
with CCL.Types; use CCL.Types;

procedure Registry_Tests is
   function Numbered (Number : Type_Reference) return CCL.Types.Name is
     (Named ("Empty_" & Ada.Strings.Fixed.Trim (Number'Image, Both)));
   R : Registry;
   D : Description;
   Ref, Point, Outcome : Type_Reference;
   Status : Definition_Result;
   procedure Reject (Expected : Definition_Result) is
      Before : constant Registry := R;
   begin
      Define (R, D, Ref, Status);
      pragma Assert (Status = Expected and Ref = Invalid_Type and R = Before);
   end Reject;
begin
   pragma Assert (Known (R, Integer_Type) and not Known (R, Invalid_Type));
   pragma Assert (not Known (R, Declared_Type'First));
   pragma Assert (Find (R, Named ("String")) = String_Type);
   pragma Assert (Cells (R, Invalid_Type) = 0);
   pragma Assert (not Valid_Name (Named ("")) and not Valid_Name (Named ("1a")));
   pragma Assert (not Valid_Name (Named ("a.b")));
   pragma Assert (not Valid_Name (Named (String'(1 .. 33 => 'a'))));
   D := (Identifier => Named ("Point"), Form => Product, Count => 2,
         Parts => [1 => (Named ("x"), Integer_Type),
                   2 => (Named ("y"), Integer_Type), others => <>]);
   Define (R, D, Point, Status);
   pragma Assert (Status = Defined and Cells (R, Point) = 3);
   pragma Assert (Describe (R, Point) = D);
   Reject (Duplicate_Name);
   D := (Identifier => Named ("Outcome"), Form => Sum, Count => 2,
         Parts => [1 => (Named ("Some"), Point),
                   2 => (Named ("None"), Unit_Type), others => <>]);
   Define (R, D, Outcome, Status);
   pragma Assert (Status = Defined and Cells (R, Outcome) = 4);
   pragma Assert (not Is_Enumeration (R, Outcome));
   D := (Identifier => Named ("Color"), Form => Sum, Count => 2,
         Parts => [1 => (Named ("Red"), Unit_Type),
                   2 => (Named ("Blue"), Unit_Type), others => <>]);
   Define (R, D, Ref, Status);
   pragma Assert (Status = Defined and Is_Enumeration (R, Ref));
   D.Identifier := Named ("Bad");
   D.Parts (2).Identifier := Named ("Red");
   Reject (Duplicate_Name);
   D.Parts (2).Identifier := Named ("Blue");
   D.Parts (2).Payload := Last (R) + 1;
   Reject (Invalid_Reference);
   D.Parts (2).Payload := Invalid_Type;
   Reject (Invalid_Reference);
   D.Count := 0;
   Reject (Invalid_Shape);
   D.Form := Primitive;
   Reject (Invalid_Shape);
   D.Form := Product;
   D.Identifier := Named ("Integer");
   Reject (Duplicate_Name);
   D.Identifier := Named ("No.Dot");
   Reject (Invalid_Name);
   --  1 + 16 * 3 = 49 cells, then 1 + 5 * 49 = 246; reject overflow.
   D := (Identifier => Named ("Wide"), Form => Product, Count => 16, others => <>);
   for I in 1 .. D.Count loop
      D.Parts (I) := (Named ("field" & Character'Val (Character'Pos ('A') + I - 1)), Point);
   end loop;
   Define (R, D, Ref, Status);
   pragma Assert (Status = Defined and Cells (R, Ref) = 49);
   D.Identifier := Named ("TooWide");
   for I in 1 .. D.Count loop D.Parts (I).Payload := Ref; end loop;
   Reject (Layout_Too_Large);
   D.Count := 5;
   Define (R, D, Ref, Status);
   pragma Assert (Status = Defined and Cells (R, Ref) = 246);
   --  The registry grows from what is declared: past the old fixed size
   --  (216) to the named ceiling, without reusing identities or changing
   --  earlier types; a copy taken on the way keeps what it had (copies
   --  share storage until one changes); at the ceiling every way of adding
   --  a type is refused with its capacity result and the registry unchanged.
   declare
      Old_Size : constant := 216;
      Snapshot : Registry;
      Snapshot_Last : Type_Reference := Invalid_Type;
   begin
      while Last (R) < Type_Reference'Last loop
         D := (Form => Product, others => <>);
         D.Identifier := Numbered (Last (R));
         Define (R, D, Ref, Status);
         pragma Assert (Status = Defined and Ref = Last (R) and Find (R, D.Identifier) = Ref);
         if Ref = Unit_Type + Old_Size + 1 then
            Snapshot := R; Snapshot_Last := Ref;
         end if;
      end loop;
      pragma Assert (Last (R) - Unit_Type = Declaration_Ceiling);
      pragma Assert (Snapshot_Last > Unit_Type + Old_Size and Last (Snapshot) = Snapshot_Last);
      pragma Assert (Find (Snapshot, Numbered (Snapshot_Last - 1)) = Snapshot_Last);
      pragma Assert (Find (Snapshot, Numbered (1_000)) = Invalid_Type);
      pragma Assert (Find (R, Numbered (1_000)) = 1_001);
      D.Identifier := Named ("Full"); Reject (Registry_Full);
      declare
         Before : constant Registry := R;
         List_Status : List_Result;
         Handle_Status : Stream_Result;
         Function_Status : Function_Result;
         Resource_Status : Unary_Resource_Result;
         Range_Status : Definition_Result;
         Imported_As : Import_Result;
         Source : Registry;
      begin
         Specialize_List (R, Point, Ref, List_Status);
         pragma Assert (List_Status = List_Registry_Full and Ref = Invalid_Type and R = Before);
         Specialize_Stream (R, Point, Ref, Handle_Status);
         pragma Assert (Handle_Status = Stream_Registry_Full and R = Before);
         Specialize_Task (R, Point, Ref, Handle_Status);
         pragma Assert (Handle_Status = Stream_Registry_Full and R = Before);
         Specialize_Function (R, [others => Integer_Type], 1, Point, Ref, Function_Status);
         pragma Assert (Function_Status = Function_Registry_Full and R = Before);
         Specialize_Unary_Resource (R, Named ("Box"), Named ("Value"), Point, Ref, Resource_Status);
         pragma Assert (Resource_Status = Resource_Registry_Full and R = Before);
         Define_Range (R, Named ("Percent"), 0, 100, Ref, Range_Status);
         pragma Assert (Range_Status = Registry_Full and R = Before);
         Define (Source, (Identifier => Named ("Elsewhere"), Form => Product, others => <>),
                 Ref, Status);
         pragma Assert (Status = Defined);
         Import_Definition (Source, Ref, R, Point, Imported_As);
         pragma Assert (Imported_As = Import_Full and Point = Invalid_Type and R = Before);
         Point := Find (R, Named ("Point"));
      end;
   end;
   pragma Assert (Cells (R, Point) = 3 and Cells (R, Outcome) = 4);
   Put_Line ("CCL type registry: products, sums, bounds, atomic rejection PASS");
end Registry_Tests;
