package body AML_Runtime_Recovery with SPARK_Mode is
   function Valid (Item : Boundary) return Boolean is
     ((if Item.Depth = 0 then Item.Missing = 0
       else Item.Missing <= Item.Depth - 1)
      and then (if Item.Consumer then Item.Depth = 0 and then Item.Fault in Local_Cell | Argument_Cell)
      and then (case Item.Step is
        when Parents => Item.Fault /= No_Fault and then Item.Depth > 0 and then Item.First_Report
          and then Item.Parent > 0 and then Item.Parent = Item.Depth - 1 - Item.Missing
          and then Item.Reports = 0,
        when Reporting => Item.Fault /= No_Fault and then Item.Parent = 0
          and then Item.Reports > 0 and then Item.Missing < Natural'Last
          and then Item.Reports <= Item.Missing + 1,
        when Finished | Aborted | Rejected => Item.Parent = 0 and then Item.Reports = 0));
   function Empty return Boundary is ((others => <>));
   function Parent_Index (Item : Boundary) return Positive is (Item.Parent);
   function Report_Code (Item : Boundary) return Status_Code is
     (if Item.First_Report then Code (Item.Fault) else Code (Missing_Return));
   function Ignored_Predicate (Item : Boundary) return Predicate_Mode is
     (if Item.Consumer then Both_Branches else Then_Only);
   function Begin_Boundary
     (Fault : Fault_Origin; Depth : Natural; Operation_Complete, At_Consumer : Boolean)
      return Boundary is
      Item : Boundary := (Depth => Depth, others => <>);
   begin
      if Fault = No_Fault or else not Operation_Complete
        or else (At_Consumer and then (Depth /= 0 or else Fault not in Local_Cell | Argument_Cell))
      then return Item; end if;
      Item.Fault := Fault;
      Item.Consumer := At_Consumer;
      Item.First_Report := True;
      if Depth > 1 then
         Item.Parent := Depth - 1;
         Item.Step := Parents;
      else
         Item.Reports := 1;
         Item.Step := Reporting;
      end if;
      return Item;
   end Begin_Boundary;
   procedure Supply_Parent
     (Item : in out Boundary; Opcode : AML_Decode.Byte; Target_Complete : Boolean := False) is
   begin
      if not Parent_Allowed (Opcode, Target_Complete) then
         Item.Step := Rejected;
         Item.Parent := 0;
         return;
      end if;
      Item.Missing := Item.Missing + 1;
      Item.Parent := Item.Parent - 1;
      if Item.Parent = 0 then
         Item.Reports := Item.Missing + 1;
         Item.Step := Reporting;
      end if;
   end Supply_Parent;
   procedure Reply (Item : in out Boundary; Continue_Execution : Boolean) is
   begin
      Item.First_Report := False;
      if not Continue_Execution then
         Item.Reports := 0;
         Item.Step := Aborted;
      else
         Item.Reports := Item.Reports - 1;
         if Item.Reports = 0 then Item.Step := Finished; end if;
      end if;
   end Reply;
   function Plan_Predicate
     (Input : Bounds; Depth, Capacity : Natural; Mode : Predicate_Mode) return Predicate_Plan is
      Result : Predicate_Plan;
      Both : constant Boolean := Mode = Both_Branches and then Input.Has_Else;
      Tail : constant Boolean := Input.Resume = Input.Outer_Limit;
      Need : Natural range 0 .. 2;
   begin
      if Both then Need := (if Tail then 1 else 2);
      else Need := (if Input.Is_Loop or else not Tail then 1 else 0);
      end if;
      if Capacity - Depth < Need then return Result; end if;
      Result.Kind := Planned;
      Result.Count := Need;
      Result.New_Depth := Depth + Need;
      Result.Next_Offset := Input.Predicate_End;
      Result.Next_Limit := Input.Then_End;
      if Both then
         if not Tail then
            Result.Frames (1) := (Index => Depth + 1, Outer_Limit => Input.Outer_Limit,
              Resume => Input.Resume, Restart => Input.Start, Loop_Frame => False);
         end if;
         Result.Frames (Need) := (Index => Depth + Need, Outer_Limit => Input.Resume,
           Resume => Input.Else_Start, Restart => Input.Start, Loop_Frame => False);
      elsif Need = 1 then
         Result.Frames (1) := (Index => Depth + 1, Outer_Limit => Input.Outer_Limit,
           Resume => Input.Resume, Restart => Input.Start, Loop_Frame => Input.Is_Loop);
      end if;
      return Result;
   end Plan_Predicate;
end AML_Runtime_Recovery;
