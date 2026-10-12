--  What a run hands its host when a slice ends: the status, the result
--  value and its text, a list result's first elements, a record's or
--  table's canonical CCL literal, and a waiting import's argument text.
private package CCL.VM.Results with SPARK_Mode => On is

   --  The Execution_Result of a slice that ended with Status; Waiting and
   --  Waiting_Owned are the slice's own import wait.
   procedure Report
     (Item : Validated_Program; State : Machine_State; Status : Execution_Status;
      Waiting, Waiting_Owned : Boolean; Result : out Execution_Result)
     with Global => null,
          Pre => Is_Valid (Item) and then
                 (not Waiting or else State.Waiting_Import < Item.Content.Imports_Length),
          Post => Result.Steps = CCL.Execution_Budgets.Steps (State.Execution_Budget);
end CCL.VM.Results;
