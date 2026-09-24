pragma Ada_2022;
package body CuBit.Placement_Tickets with SPARK_Mode is
   use type W.Interaction_State;

   function Prepare
     (Registry : R.State; Window : Window_Version;
      Desired : W.Preference;
      Previous : W.Display_Choice := (Known => False);
      Policy : W.Fallback_Policy := W.Await_Preferred_Output;
      Interaction : W.Interaction_State := W.Idle) return Ticket
   is
      View : constant R.Snapshot := R.Capture (Registry);
   begin
      return (View, Window,
        W.Plan (Desired, R.Areas (View), Previous, Policy, Interaction));
   end Prepare;

   function Fresh
     (Registry : R.State; Item : Ticket; Window : Window_Version;
      Interaction : W.Interaction_State) return Boolean is
     (Item.Plan.Target.Available and then R.Current (Registry, Item.View)
      and then Item.Owner = Window and then Interaction = W.Idle);

   function Check
     (Registry : R.State; Item : Ticket; Window : Window_Version;
      Interaction : W.Interaction_State) return Admission is
   begin
      if not Item.Plan.Target.Available then
         return No_Proposal;
      elsif not R.Current (Registry, Item.View) then
         return Output_State_Changed;
      elsif Item.Owner.Instance /= Window.Instance then
         return Window_Replaced;
      elsif Item.Owner.Intent /= Window.Intent then
         return User_Intent_Changed;
      elsif Interaction /= W.Idle then
         return Interaction_In_Progress;
      end if;
      return May_Apply;
   end Check;
end CuBit.Placement_Tickets;
