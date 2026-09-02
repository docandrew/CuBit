package body CCL.Debug_Maps with
   SPARK_Mode => On
is
   use type CCL.VM.Program_Length;

   procedure Initialize (Item : out Debug_Map) is
   begin
      Item := (others => <>);
   end Initialize;

   procedure Add
     (Item   : in out Debug_Map;
      New_Entry : Debug_Entry;
      Result : out Add_Result)
   is
   begin
      if Item.Count = MAX_ENTRIES then
         Result := Map_Full;
      else
         Item.Entries (Entry_Index (Item.Count)) := New_Entry;
         Item.Count := Item.Count + 1;
         Result := Added;
      end if;
   end Add;

   procedure Validate
     (Item           : Debug_Map;
      Program_Length : CCL.VM.Program_Length;
      Error          : out Validation_Error)
   is
      Current : Debug_Entry;
   begin
      Error := Debug_Map_Valid;
      if Item.Count > 0 then
         for Index in 0 .. Item.Count - 1 loop
            Current := Item.Entries (Index);
            if Current.First_PC >= Current.End_PC then
               Error := Empty_PC_Range;
            elsif Current.End_PC > Program_Length then
               Error := PC_Outside_Program;
            elsif Current.Source_First = 0 or else
              Current.Source_First >= Current.Source_End
            then
               Error := Invalid_Source_Range;
            elsif Current.Node = CCL.Language.NO_NODE then
               Error := Invalid_Node_Reference;
            end if;
            exit when Error /= Debug_Map_Valid;
         end loop;
      end if;
   end Validate;

   procedure Find_Innermost
     (Item  : Debug_Map;
      PC    : CCL.VM.Instruction_Index;
      Match : out Debug_Entry;
      Found : out Boolean)
   is
      Candidate : Debug_Entry;
      Best_Size : CCL.VM.Program_Length := CCL.VM.Program_Length'Last;
      Candidate_Size : CCL.VM.Program_Length;
      Position : constant CCL.VM.Program_Length :=
        CCL.VM.Program_Length (PC);
   begin
      Match := (others => <>);
      Found := False;
      if Item.Count > 0 then
         for Index in 0 .. Item.Count - 1 loop
            Candidate := Item.Entries (Index);
            if Candidate.First_PC <= Position and then
              Position < Candidate.End_PC
            then
               Candidate_Size := Candidate.End_PC - Candidate.First_PC;
               if not Found or else Candidate_Size < Best_Size then
                  Match := Candidate;
                  Best_Size := Candidate_Size;
                  Found := True;
               end if;
            end if;
         end loop;
      end if;
   end Find_Innermost;
end CCL.Debug_Maps;
