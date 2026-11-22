pragma Ada_2022;
package body ACPI_Serving_State with SPARK_Mode is
   function Observe(Run:Session) return Snapshot is(Run.Info);
   function Eligible(Facts:Eligibility) return Boolean is
     (Facts.Catalog_Complete and then Facts.Results_Clear and then
      Facts.Region_Unavailable and then Facts.Prefix_Terminal and then
      Facts.No_Fatal and then Facts.Regions_Unannounced and then
      Facts.Regions_Clear and then Facts.Regions_Idle and then
      Facts.Fields_Idle and then Facts.Fields_Clear and then
      Facts.Fields_Certain and then Facts.EC_Quiet and then
      Facts.SCI_Not_Armed and then Facts.Events_Not_Started and then
      Facts.Consumer_Owner_Quiet);
   procedure Select_Runtime(Run:in out Session;Runtime_Admitted:Boolean;Accepted:out Boolean) is
   begin
      Accepted:=False;if Run.Info.Consumed then return;end if;
      Run.Info.Consumed:=True;if not Runtime_Admitted then return;end if;
      Run.Info.Current:=Local_Runtime;Run.Info.Selected:=True;Accepted:=True;
   end Select_Runtime;
   procedure Select_Observer(Run:in out Session;Facts:Eligibility;
      Physical_Retirement_Confirmed:Boolean;Accepted:out Boolean) is
   begin
      Accepted:=False;if Run.Info.Consumed then return;end if;
      Run.Info.Consumed:=True;
      if not Physical_Retirement_Confirmed or else not Eligible(Facts) then return;end if;
      Run.Info.Current:=Observer_Only;Run.Info.Why:=Runtime_Not_Admitted;
      Run.Info.Selected:=True;Accepted:=True;
   end Select_Observer;
end ACPI_Serving_State;
