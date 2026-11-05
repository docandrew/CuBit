pragma Ada_2022;
package body AML_Pending_Members with SPARK_Mode is
   use type AML_Names.Parse_Status;
   function Count (Journal : State) return Member_Count is (Journal.Used);
   function Segments_Used (Journal : State) return Segment_Count is (Journal.Filled);
   function Empty return State is ((others => <>));
   function Valid_Path (Path : AML_Names.Name_Result) return Boolean is
     (Path.Kind = AML_Names.Accepted and then Path.Count > 0
      and then (not Path.Rooted or else Path.Parents = 0)
      and then (for all I in 1 .. Path.Count => AML_Names.Valid (Path.Parts (I))));
   function Canonical (Path : AML_Names.Name_Result) return AML_Names.Name_Result is
      Segment_Bytes : constant := AML_Names.Segment'Length;
      Dual_Name_Prefix_Bytes : constant := 1;
      Multi_Name_Prefix_Bytes : constant := 2;
      Result : AML_Names.Name_Result (AML_Names.Accepted) :=
        (Kind => AML_Names.Accepted, Rooted => Path.Rooted,
         Parents => Path.Parents, Count => Path.Count,
         Parts => [others => "____"],
         Consumed => (if Path.Rooted then 1 else Path.Parents) +
           Segment_Bytes * Path.Count +
           (if Path.Count = 1 then 0 elsif Path.Count = 2 then
               Dual_Name_Prefix_Bytes else Multi_Name_Prefix_Bytes));
   begin
      Result.Parts (1 .. Path.Count) := Path.Parts (1 .. Path.Count);
      return Result;
   end Canonical;
   function Valid (Journal : State) return Boolean is
      Next : Segment_Count := 0;
   begin
      for I in 1 .. Journal.Used loop
         declare M : constant Member := Journal.Members (I); begin
            if M.Package_ID = 0 or else M.Length = 0
              or else (M.Rooted and then M.Parents /= 0)
              or else M.Start /= Next
              or else M.Length > Journal.Filled - Next
            then return False; end if;
            Next := Next + M.Length;
         end;
      end loop;
      return Next = Journal.Filled and then
        (for all I in 1 .. Journal.Filled => AML_Names.Valid (Journal.Segments (I)));
   end Valid;
   function Item (Journal : State; Index : Natural) return Member_Result is
   begin
      if Index = 0 or else Index > Journal.Used then return (Found => False); end if;
      declare
         M : constant Member := Journal.Members (Index);
         Path : AML_Names.Name_Result (AML_Names.Accepted) :=
           (AML_Names.Accepted, M.Rooted, M.Parents, M.Length,
            [others => "____"], 1);
      begin
         for I in 1 .. M.Length loop
            Path.Parts (I) := Journal.Segments (M.Start + I);
         end loop;
         return (True, M.Package_ID, M.Element, M.Scope, Canonical (Path));
      end;
   end Item;
   procedure Append
     (Journal : in out State; Package_ID : AML_Objects.Object_ID;
      Element : Natural; Scope : Scope_ID; Path : AML_Names.Name_Result;
      Status : out Append_Status)
   is
   begin
      Status := Invalid_Input;
      if Package_ID = 0 or else Element > Element_Index'Last
        or else not Valid_Path (Path)
      then return; end if;
      if Journal.Used = Max_Members then Status := Member_Limit; return; end if;
      if Path.Count > Max_Segments - Journal.Filled then
         Status := Segment_Limit; return;
      end if;
      for I in 1 .. Path.Count loop
         Journal.Segments (Journal.Filled + I) := Path.Parts (I);
      end loop;
      Journal.Members (Journal.Used + 1) :=
        (Package_ID, Element, Scope, Path.Rooted, Path.Parents,
         Path.Count, Journal.Filled);
      Journal.Filled := Journal.Filled + Path.Count;
      Journal.Used := Journal.Used + 1;
      Status := Appended;
   end Append;
   procedure Clear (Journal : out State) is
   begin
      Journal := Empty;
   end Clear;
end AML_Pending_Members;
