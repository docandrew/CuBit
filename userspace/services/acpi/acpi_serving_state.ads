pragma Ada_2022;
package ACPI_Serving_State with Pure, SPARK_Mode is
   pragma Unevaluated_Use_Of_Old(Allow);
   type Mode is (Not_Serving, Local_Runtime, Observer_Only);
   for Mode use (Not_Serving=>0,Local_Runtime=>1,Observer_Only=>2);
   type Reason is (None, Runtime_Not_Admitted);
   for Reason use (None=>0,Runtime_Not_Admitted=>1);
   type Eligibility is record
      Catalog_Complete, Results_Clear, Region_Unavailable, Prefix_Terminal,
      No_Fatal, Regions_Unannounced, Regions_Clear, Regions_Idle,
      Fields_Idle, Fields_Clear, Fields_Certain, EC_Quiet,
      SCI_Not_Armed, Events_Not_Started, Consumer_Owner_Quiet : Boolean := False;
   end record;
   function Eligible(Facts:Eligibility) return Boolean
     with Post => Eligible'Result =
       (Facts.Catalog_Complete and then Facts.Results_Clear and then
        Facts.Region_Unavailable and then Facts.Prefix_Terminal and then
        Facts.No_Fatal and then Facts.Regions_Unannounced and then
        Facts.Regions_Clear and then Facts.Regions_Idle and then
        Facts.Fields_Idle and then Facts.Fields_Clear and then
        Facts.Fields_Certain and then Facts.EC_Quiet and then
        Facts.SCI_Not_Armed and then Facts.Events_Not_Started and then
        Facts.Consumer_Owner_Quiet);
   type Snapshot is record
      Consumed, Selected : Boolean := False;
      Current : Mode := Not_Serving;
      Why : Reason := None;
   end record;
   type Session is limited private
     with Default_Initial_Condition => Observe(Session) =
       (Consumed=>False,Selected=>False,Current=>Not_Serving,Why=>None);
   function Observe(Run:Session) return Snapshot;
   -- Supplied facts do not confer hardware authority or retire any owner.
   -- Every first declaration consumes the choice, including a refusal.
   procedure Select_Runtime(Run:in out Session;Runtime_Admitted:Boolean;Accepted:out Boolean)
     with Contract_Cases =>
       (Observe(Run).Consumed => not Accepted and then Observe(Run)=Observe(Run)'Old,
        not Observe(Run).Consumed and then Runtime_Admitted =>
          Accepted and then Observe(Run)=(Observe(Run)'Old with delta
            Consumed=>True,Selected=>True,Current=>Local_Runtime),
        not Observe(Run).Consumed and then not Runtime_Admitted =>
          not Accepted and then Observe(Run)=(Observe(Run)'Old with delta Consumed=>True));
   procedure Select_Observer(Run:in out Session;Facts:Eligibility;
      Physical_Retirement_Confirmed:Boolean;Accepted:out Boolean)
     with Contract_Cases =>
       (Observe(Run).Consumed => not Accepted and then Observe(Run)=Observe(Run)'Old,
        not Observe(Run).Consumed and then Physical_Retirement_Confirmed and then Eligible(Facts) =>
          Accepted and then Observe(Run)=
            (Consumed=>True,Selected=>True,Current=>Observer_Only,Why=>Runtime_Not_Admitted),
        not Observe(Run).Consumed and then (not Physical_Retirement_Confirmed or else not Eligible(Facts)) =>
          not Accepted and then Observe(Run)=(Observe(Run)'Old with delta Consumed=>True));
private
   type Session is limited record Info:Snapshot;end record;
end ACPI_Serving_State;
