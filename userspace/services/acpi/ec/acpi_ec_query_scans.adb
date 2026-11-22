pragma Ada_2022;
package body ACPI_EC_Query_Scans with SPARK_Mode is
   function View (Item : State) return Description is (Item.Data);
   function Create (Queries : Allowance) return State is
     ((Data => (Stage => Ready, Remaining => Queries, Empty => No_Empty_Response)));
   procedure Start_Iteration (Item : in out State) is
   begin
      Item.Data := (Item.Data with delta
        Stage => Waiting, Remaining => Item.Data.Remaining - 1);
   end Start_Iteration;
   procedure Record_Event (Item : in out State) is
   begin
      Item.Data := (Item.Data with delta
        Stage => (if Item.Data.Remaining = 0 then Exhausted else Ready));
   end Record_Event;
   procedure Record_Empty (Item : in out State; Kind : Observed_Empty) is
   begin
      Item.Data := (Item.Data with delta Stage => Complete, Empty => Kind);
   end Record_Empty;
   procedure Fail (Item : in out State) is
   begin
      Item.Data := (Item.Data with delta Stage => Failed);
   end Fail;
   procedure Extend (Item : in out State; Queries : Allowance; Accepted : out Boolean) is
   begin
      Accepted := Item.Data.Stage = Exhausted;
      if Accepted then
         Item.Data := (Item.Data with delta Stage => Ready, Remaining => Queries);
      end if;
   end Extend;
end ACPI_EC_Query_Scans;
