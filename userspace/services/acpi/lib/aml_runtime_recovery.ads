with AML_Decode;
with AML_Runtime_Exceptions;
package AML_Runtime_Recovery with SPARK_Mode, Pure is
   use type AML_Decode.Byte;
   use type AML_Decode.Integer_Value;
   subtype Status_Code is AML_Runtime_Exceptions.Status_Code;
   type Fault_Origin is (No_Fault, Local_Cell, Argument_Cell, Divide_Fault, Package_Fault, Missing_Return);
   function Frame_Fault (Slot : Natural) return Fault_Origin is
     (if Slot in 1 .. 8 then Local_Cell elsif Slot in 9 .. 15 then Argument_Cell else No_Fault);
   function Code (Fault : Fault_Origin) return Status_Code is
     (case Fault is when No_Fault => 0, when Local_Cell => 16#3005#,
        when Argument_Cell => 16#3006#, when Divide_Fault => 16#300C#,
        when Package_Fault => 16#300B#,
        when Missing_Return => 16#3012#);
   type Boundary is private;
   function Valid (Item : Boundary) return Boolean;
   function Empty return Boundary with Post => Valid (Empty'Result);
   function Depth_At_Start (Item : Boundary) return Natural;
   function Parents_Left (Item : Boundary) return Natural;
   function Needs_Parent (Item : Boundary) return Boolean;
   function Parent_Index (Item : Boundary) return Positive
     with Pre => Valid (Item) and then Needs_Parent (Item),
       Post => Parent_Index'Result <= Depth_At_Start (Item);
   function Needs_Report (Item : Boundary) return Boolean;
   function Reports_Left (Item : Boundary) return Natural;
   function Report_Code (Item : Boundary) return Status_Code
     with Pre => Valid (Item) and then Needs_Report (Item), Post => Report_Code'Result /= 0;
   function Completed (Item : Boundary) return Boolean;
   function Origin_Code (Item : Boundary) return Status_Code;
   type Predicate_Mode is (Then_Only, Both_Branches);
   function Ignored_Predicate (Item : Boundary) return Predicate_Mode
     with Pre => Valid (Item) and then Completed (Item);
   function Begin_Boundary
     (Fault : Fault_Origin; Depth : Natural; Operation_Complete, At_Consumer : Boolean)
      return Boundary
     with Post => Valid (Begin_Boundary'Result)
       and then Depth_At_Start (Begin_Boundary'Result) = Depth
       and then Parents_Left (Begin_Boundary'Result) <= Depth;
   function Parent_Allowed (Opcode : AML_Decode.Byte; Target_Complete : Boolean) return Boolean is
     (Opcode = 16#92# or else (Opcode = 16#70# and then Target_Complete));
   procedure Supply_Parent
     (Item : in out Boundary; Opcode : AML_Decode.Byte; Target_Complete : Boolean := False)
     with Pre => Valid (Item) and then Needs_Parent (Item),
       Post => Valid (Item) and then Parents_Left (Item) < Parents_Left (Item'Old)
         and then Depth_At_Start (Item) = Depth_At_Start (Item'Old)
         and then Origin_Code (Item) = Origin_Code (Item'Old)
         and then (if not Parent_Allowed (Opcode, Target_Complete) then not Needs_Report (Item) and then not Completed (Item)
           elsif Parents_Left (Item'Old) = 1 then
             Needs_Report (Item) and then Reports_Left (Item) = Depth_At_Start (Item)
             and then Report_Code (Item) = Origin_Code (Item));
   procedure Reply (Item : in out Boundary; Continue_Execution : Boolean)
     with Pre => Valid (Item) and then Needs_Report (Item),
       Post => Valid (Item) and then Reports_Left (Item) < Reports_Left (Item'Old)
         and then Depth_At_Start (Item) = Depth_At_Start (Item'Old)
         and then Origin_Code (Item) = Origin_Code (Item'Old)
         and then (if Continue_Execution then
           (if Reports_Left (Item'Old) = 1 then Completed (Item)
            else Needs_Report (Item) and then Report_Code (Item) = Code (Missing_Return))
           else not Completed (Item) and then not Needs_Report (Item));

   type Bounds is record
      Start, Predicate_End, Then_End, Else_Start, Resume, Outer_Limit : Natural := 0;
      Has_Else, Is_Loop : Boolean := False;
   end record;
   function Valid (Item : Bounds) return Boolean is
     (Item.Start <= Item.Predicate_End and then Item.Predicate_End <= Item.Then_End
      and then Item.Then_End <= Item.Else_Start and then Item.Else_Start <= Item.Resume
      and then Item.Resume <= Item.Outer_Limit
      and then (if not Item.Has_Else then Item.Then_End = Item.Else_Start and then Item.Then_End = Item.Resume)
      and then not (Item.Is_Loop and Item.Has_Else));
   type Continuation is record
      Index, Outer_Limit, Resume, Restart : Natural := 0;
      Loop_Frame : Boolean := False;
   end record;
   type Continuations is array (Positive range 1 .. 2) of Continuation;
   type Plan_Kind is (Planned, Block_Limit);
   type Predicate_Plan is record
      Kind : Plan_Kind := Block_Limit;
      Count : Natural range 0 .. 2 := 0;
      New_Depth, Next_Offset, Next_Limit : Natural := 0;
      Frames : Continuations := [others => <>];
   end record;
   function Plan_Valid (Plan : Predicate_Plan; Input : Bounds; Depth, Capacity : Natural) return Boolean is
     (if Plan.Kind = Planned then
        Plan.New_Depth <= Capacity and then Plan.New_Depth >= Depth
        and then Plan.New_Depth - Depth = Plan.Count
        and then Plan.Next_Offset = Input.Predicate_End and then Plan.Next_Limit = Input.Then_End
        and then (for all I in 1 .. Plan.Count =>
          Plan.Frames (I).Index > Depth and then Plan.Frames (I).Index <= Plan.New_Depth
          and then Plan.Frames (I).Index = Depth + I
          and then Plan.Frames (I).Resume <= Input.Outer_Limit
          and then Plan.Frames (I).Outer_Limit <= Input.Outer_Limit
          and then Plan.Frames (I).Restart <= Input.Outer_Limit)
      else Plan.Count = 0);
   function Plan_Matches
     (Plan : Predicate_Plan; Input : Bounds; Mode : Predicate_Mode) return Boolean is
     (if Plan.Kind /= Planned then True
      elsif Mode = Both_Branches and then Input.Has_Else then
        Plan.Count = (if Input.Resume = Input.Outer_Limit then 1 else 2)
        and then Plan.Frames (Plan.Count).Resume = Input.Else_Start
        and then Plan.Frames (Plan.Count).Outer_Limit = Input.Resume
        and then not Plan.Frames (Plan.Count).Loop_Frame
        and then (if Plan.Count = 2 then Plan.Frames (1).Resume = Input.Resume
          and then Plan.Frames (1).Outer_Limit = Input.Outer_Limit)
      else Plan.Count = (if Input.Is_Loop or else Input.Resume /= Input.Outer_Limit then 1 else 0)
        and then (if Plan.Count = 1 then Plan.Frames (1).Resume = Input.Resume
          and then Plan.Frames (1).Outer_Limit = Input.Outer_Limit
          and then Plan.Frames (1).Loop_Frame = Input.Is_Loop));
   function Plan_Predicate
     (Input : Bounds; Depth, Capacity : Natural; Mode : Predicate_Mode) return Predicate_Plan
     with Pre => Valid (Input) and then Depth <= Capacity,
       Post => Plan_Valid (Plan_Predicate'Result, Input, Depth, Capacity)
         and then Plan_Matches (Plan_Predicate'Result, Input, Mode);
private
   type Phase is (Rejected, Parents, Reporting, Finished, Aborted);
   type Boundary is record
      Step : Phase := Rejected;
      Fault : Fault_Origin := No_Fault;
      Depth, Parent, Missing, Reports : Natural := 0;
      Consumer, First_Report : Boolean := False;
   end record;
   function Depth_At_Start (Item : Boundary) return Natural is (Item.Depth);
   function Parents_Left (Item : Boundary) return Natural is (Item.Parent);
   function Needs_Parent (Item : Boundary) return Boolean is (Item.Step = Parents);
   function Needs_Report (Item : Boundary) return Boolean is (Item.Step = Reporting);
   function Reports_Left (Item : Boundary) return Natural is (Item.Reports);
   function Completed (Item : Boundary) return Boolean is (Item.Step = Finished);
   function Origin_Code (Item : Boundary) return Status_Code is (Code (Item.Fault));
end AML_Runtime_Recovery;
