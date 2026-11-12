pragma Ada_2022;
package body AML_Pending_Members with SPARK_Mode is
   use type AML_Names.Parse_Status;
   Segment_Byte_Count : constant Positive := AML_Names.Segment'Length;
   Root_Prefix_Bytes : constant Positive := 1;
   Dual_Prefix_Bytes : constant Positive := 1;
   Multi_Prefix_Bytes : constant Positive := 2;
   function Encoded_Bytes(Rooted : Boolean; Parents,Parts : Natural) return Positive is
     ((if Rooted then Root_Prefix_Bytes else Parents) + Segment_Byte_Count*Parts +
       (if Parts=1 then 0 elsif Parts=2 then Dual_Prefix_Bytes else Multi_Prefix_Bytes))
     with Pre => Parts in 1 .. AML_Names.Segment_Array'Length and then Parents <= Path_Count'Last;
   function Canonical_Matches(Path,Result : AML_Names.Name_Result) return Boolean is
     (Result.Kind = AML_Names.Accepted and then Result.Rooted = Path.Rooted
      and then Result.Parents = Path.Parents and then Result.Count = Path.Count
      and then Result.Consumed = Encoded_Bytes(Path.Rooted,Path.Parents,Path.Count)
      and then (for all I in Result.Parts'Range => Result.Parts(I) =
        (if I <= Path.Count then Path.Parts(I) else "____")));
   function Item_Matches(Journal : State; Index : Natural; Result : Member_Result) return Boolean is
     (Result.Found = (Index in 1 .. Journal.Used)
      and then (if Result.Found then
        Result.Package_ID = Journal.Members(Index).Package_ID
        and then Result.Element = Journal.Members(Index).Element
        and then Result.Scope = Journal.Members(Index).Scope
        and then Result.Path.Rooted = Journal.Members(Index).Rooted
        and then Result.Path.Parents = Journal.Members(Index).Parents
        and then Result.Path.Count = Journal.Members(Index).Length
        and then Result.Path.Consumed = Encoded_Bytes(Journal.Members(Index).Rooted,
          Journal.Members(Index).Parents,Journal.Members(Index).Length)
        and then (for all I in Result.Path.Parts'Range => Result.Path.Parts(I) =
          (if I <= Journal.Members(Index).Length
           then Journal.Segments(Journal.Members(Index).Start+I) else "____"))));
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
     ((for all I in 1 .. Journal.Used =>
         Journal.Members(I).Package_ID /= 0
         and then Journal.Members(I).Length > 0
         and then (not Journal.Members(I).Rooted or else Journal.Members(I).Parents = 0)
         and then Journal.Members(I).Start <= Journal.Filled
         and then Journal.Members(I).Length <= Journal.Filled - Journal.Members(I).Start
         and then (if I = 1 then Journal.Members(I).Start = 0 else
           Journal.Members(I-1).Start <= Journal.Filled
           and then Journal.Members(I-1).Length <= Journal.Filled - Journal.Members(I-1).Start
           and then Journal.Members(I).Start =
             Journal.Members(I-1).Start + Journal.Members(I-1).Length))
      and then (if Journal.Used = 0 then Journal.Filled = 0 else
        Journal.Members(Journal.Used).Start <= Journal.Filled
        and then Journal.Members(Journal.Used).Length =
          Journal.Filled - Journal.Members(Journal.Used).Start)
      and then (for all I in 1 .. Journal.Filled => AML_Names.Valid(Journal.Segments(I))));
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
            pragma Loop_Invariant
              (for all J in 1 .. I => Path.Parts(J) = Journal.Segments(M.Start+J));
         end loop;
         return (True, M.Package_ID, M.Element, M.Scope, Canonical (Path));
      end;
   end Item;
   function Admission
     (Journal : State; Package_ID : AML_Objects.Object_ID; Element : Natural;
      Path : AML_Names.Name_Result) return Append_Status is
   begin
      if Package_ID = 0 or else Element > Element_Index'Last
        or else not Valid_Path (Path)
      then return Invalid_Input; end if;
      if Journal.Used = Max_Members then return Member_Limit; end if;
      if Path.Count > Max_Segments - Journal.Filled then return Segment_Limit; end if;
      return Appended;
   end Admission;
   procedure Append
     (Journal : in out State; Package_ID : AML_Objects.Object_ID;
      Element : Natural; Scope : Scope_ID; Path : AML_Names.Name_Result;
      Status : out Append_Status)
   is
   begin
      Status := Admission (Journal,Package_ID,Element,Path);
      if Status /= Appended then return; end if;
      for I in 1 .. Path.Count loop
         Journal.Segments (Journal.Filled + I) := Path.Parts (I);
         pragma Loop_Invariant
           (for all J in 1 .. Journal.Filled+I => AML_Names.Valid(Journal.Segments(J)));
         pragma Loop_Invariant
           (for all J in 1 .. I => Journal.Segments(Journal.Filled+J) = Path.Parts(J));
      end loop;
      Journal.Members (Journal.Used + 1) :=
        (Package_ID, Element, Scope, Path.Rooted, Path.Parents,
         Path.Count, Journal.Filled);
      Journal.Filled := Journal.Filled + Path.Count;
      Journal.Used := Journal.Used + 1;
      Status := Appended;
   end Append;
   function Truncated_Prefix(After,Before : State; Keep : Member_Count) return Boolean is
     (After.Used=Keep and then After.Filled=
        (if Keep=0 then 0 else Before.Members(Keep).Start+Before.Members(Keep).Length)
      and then (for all I in Before.Members'Range => After.Members(I)=Before.Members(I))
      and then (for all I in Before.Segments'Range => After.Segments(I)=Before.Segments(I)));
   procedure Prefix_Bounds(Journal : State; Keep : Member_Count)
     with Ghost, Pre => Valid(Journal) and then Keep in 1 .. Count(Journal),
       Post => (for all I in 1 .. Keep =>
         Journal.Members(I).Start <= Journal.Members(Keep).Start+Journal.Members(Keep).Length
         and then Journal.Members(I).Length <=
           Journal.Members(Keep).Start+Journal.Members(Keep).Length-Journal.Members(I).Start);
   procedure Prefix_Bounds(Journal : State; Keep : Member_Count) is
      Boundary : constant Segment_Count:=Journal.Members(Keep).Start+Journal.Members(Keep).Length;
   begin
      for I in reverse 1 .. Keep loop
         if I<Keep then
            pragma Assert(Journal.Members(I).Start+Journal.Members(I).Length=Journal.Members(I+1).Start);
         end if;
         pragma Assert(Journal.Members(I).Start <= Boundary);
         pragma Assert(Journal.Members(I).Length <= Boundary-Journal.Members(I).Start);
         pragma Loop_Invariant(for all J in I .. Keep =>
           Journal.Members(J).Start <= Boundary
           and then Journal.Members(J).Length <= Boundary-Journal.Members(J).Start);
      end loop;
   end Prefix_Bounds;
   procedure Truncate(Journal : in out State; Keep : Member_Count; Status : out Truncate_Status) is
      Filled : Segment_Count;
   begin
      if Keep>Journal.Used then Status:=Invalid_Prefix;return;end if;
      if Keep>0 then Prefix_Bounds(Journal,Keep);end if;
      Filled:=(if Keep=0 then 0 else Journal.Members(Keep).Start+Journal.Members(Keep).Length);
      Journal.Used:=Keep;Journal.Filled:=Filled;Status:=Truncated;
   end Truncate;
   procedure Clear (Journal : out State) is
   begin
      Journal := Empty;
   end Clear;
end AML_Pending_Members;
