package body ACPI_EC_Registers with SPARK_Mode is
   procedure Install (Item : Admission; Accepted : out Boolean) is
      Candidate : Slot_Index := Slot_Index'First;
      Found : Boolean := False;
   begin
      Accepted := False;
      if Item.Register = 0 or Item.Epoch = 0 or Item.Owner = 0
        or (not Item.Readable and not Item.Writable)
        or (Item.Writable and not Item.Exact_Write_Safe)
        or (not Item.Writable and Item.Write_Mask /= 0)
      then
         return;
      end if;
      for I in Slot_Index loop
         if Entries (I).Used and then Entries (I).Item.Register = Item.Register then
            if Entries (I).Enabled or (Occupied and not Querying and Selected = I)
              or Item.Epoch <= Entries (I).Item.Epoch
            then
               return;
            end if;
            Entries (I) := (Used => True, Enabled => True, Item => Item);
            Accepted := True;
            return;
         elsif not Entries (I).Used and not Found then
            Candidate := I;
            Found := True;
         end if;
      end loop;
      if Found then
         Entries (Candidate) := (Used => True, Enabled => True, Item => Item);
         Accepted := True;
      end if;
   end Install;

   procedure Revoke (Register : Register_ID; Epoch : Epoch_ID;
                     Accepted : out Boolean) is
   begin
      Accepted := False;
      for I in Slot_Index loop
         if Entries (I).Used and Entries (I).Enabled
           and Entries (I).Item.Register = Register
           and Entries (I).Item.Epoch = Epoch
           and not (Occupied and not Querying and Selected = I)
         then
            Entries (I).Enabled := False;
            Accepted := True;
            return;
         end if;
      end loop;
   end Revoke;

   procedure Submit
     (Sender : Owner_ID; Register : Register_ID; Epoch : Epoch_ID;
      For_Write : Boolean; Value : Byte; Deadline : Microseconds;
      Budget : Work_Budget; Result : out Submission;
      Token : out Operation_ID)
   is
      Accepted : Boolean;
   begin
      Token := 0;
      Result := Denied;
      if Occupied then
         Result := Busy;
         return;
      elsif Sequence = Operation_ID'Last then
         Result := Exhausted;
         return;
      end if;
      for I in Slot_Index loop
         if Entries (I).Enabled and then Entries (I).Item.Register = Register
           and then Entries (I).Item.Epoch = Epoch
           and then Entries (I).Item.Owner = Sender
           and then (if For_Write then
             Entries (I).Item.Writable and Entries (I).Item.Exact_Write_Safe
             and (Value and not Entries (I).Item.Write_Mask) = 0
           else Entries (I).Item.Readable and Value = 0)
         then
            Engine.Start ((if For_Write then Write_Cell else Read_Cell),
                          Entries (I).Item.Cell, Value, Deadline, Budget, Accepted);
            if not Accepted then
               Result := Engine_Unavailable;
               return;
            end if;
            Sequence := Sequence + 1;
            Token := Sequence;
            Selected := I;
            Request_Deadline := Deadline;
            Writing := For_Write;
            Querying := False;
            Occupied := True;
            Delivered := False;
            Result := Started;
            return;
         end if;
      end loop;
   end Submit;

   procedure Submit_Query
     (Deadline : Microseconds; Budget : Work_Budget; Result : out Submission;
      Token : out Operation_ID)
   is
      Accepted : Boolean;
   begin
      Token := 0;
      if Occupied then
         Result := Busy;
      elsif Sequence = Operation_ID'Last then
         Result := Exhausted;
      else
         Engine.Start (Query_Event, 0, 0, Deadline, Budget, Accepted);
         if not Accepted then
            Result := Engine_Unavailable;
            return;
         end if;
         Sequence := Sequence + 1;
         Token := Sequence;
         Occupied := True;
         Delivered := False;
         Querying := True;
         Request_Deadline := Deadline;
         Result := Started;
      end if;
   end Submit_Query;

   procedure Take_Query (Item : out Query_Completion) is
      Accepted : Boolean;
   begin
      Item := (others => <>);
      if not Occupied or not Querying or Delivered or Engine.Status in Idle | Running then
         return;
      end if;
      Item := (Present => True, Token => Sequence, Status => Engine.Status,
               Event => (if Engine.Status = Succeeded then Engine.Result_Byte else 0),
               Effects_Possible => Engine.Effects_Possible,
               Needs_Recovery => Engine.Needs_Recovery);
      Delivered := True;
      if not Item.Needs_Recovery then
         Engine.Acknowledge (Accepted);
         if Accepted then
            Occupied := False;
         end if;
      end if;
   end Take_Query;

   procedure Step is
   begin
      if Occupied and not Delivered then
         Engine.Step;
      end if;
   end Step;
   procedure Request_Stop is
   begin
      if Occupied and not Delivered then
         Engine.Request_Stop;
      end if;
   end Request_Stop;
   procedure Take_Completion (Item : out Completion) is
      Accepted : Boolean;
   begin
      Item := (others => <>);
      if not Occupied or Querying or Delivered or Engine.Status in Idle | Running then
         return;
      end if;
      Item := (Present => True, Token => Sequence,
               Register => Entries (Selected).Item.Register,
               Epoch => Entries (Selected).Item.Epoch,
               Owner => Entries (Selected).Item.Owner,
               Status => Engine.Status,
               Value => (if not Writing and Engine.Status = Succeeded
                         then Engine.Result_Byte else 0),
               Effects_Possible => Engine.Effects_Possible,
               Needs_Recovery => Engine.Needs_Recovery);
      Delivered := True;
      if not Item.Needs_Recovery then
         Engine.Acknowledge (Accepted);
         if Accepted then
            Occupied := False;
         end if;
      end if;
   end Take_Completion;
   function Next_Progress (Now : Microseconds) return Progress is
      Earliest : Microseconds;
   begin
      if not Occupied or else Delivered then return (No_Work, 0);
      elsif Engine.Status /= Running or else Now >= Request_Deadline then
         return (Run_Step, 0);
      end if;
      Earliest := Engine.Wake_At;
      if Earliest > Now then
         return (Wait_Until, Microseconds'Min (Earliest, Request_Deadline));
      else return (Run_Step, 0);
      end if;
   end Next_Progress;
   function Active return Boolean is (Occupied);
   function Retained_For_Recovery return Boolean is
     (Occupied and then Engine.Needs_Recovery);
end ACPI_EC_Registers;
