with CCL.Callbacks;
with CCL.Catalog;
with CCL.Host_Values;
with CCL.Language;
with Interfaces;
package CCL.UI_Buttons with SPARK_Mode is
   type Operation is (On_Click, Set_Text, Close_Button);
   function Name (Op : Operation) return String is
     (case Op is when On_Click => "button-on-click", when Set_Text => "button-text",
      when Close_Button => "button-close");
   type Model is limited private;
   procedure Apply
     (Item : in out Model; Op : Operation; Argument : CCL.Host_Values.Value;
      Catalog : CCL.Catalog.Interface_Catalog; Grants : CCL.Catalog.Granted_Bindings;
      Success : out Boolean);
   procedure Click (Item : in out Model; Result : out CCL.Callbacks.Events.Enqueue_Result);
   procedure Close (Item : in out Model);
   function Visible (Item : Model) return Boolean;
   function Enabled (Item : Model) return Boolean;
   function Caption (Item : Model) return String;
   function Changed (Item : Model) return Boolean;
   procedure Painted (Item : in out Model);
   function Pending (Item : Model) return CCL.Callbacks.Events.Pending_Count;
   function Discarded (Item : Model) return CCL.Callbacks.Events.Pending_Count;
   generic
      type Host_Context is limited private;
      with procedure Invoke
        (Context : in out Host_Context; Binding : Interfaces.Unsigned_32;
         Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result);
   procedure Dispatch_One
     (Item : in out Model; Grants : CCL.Catalog.Granted_Bindings;
      Context : in out Host_Context; Ran : out Boolean; Outcome : out CCL.Language.Interpretation_Result);
private
   use type CCL.Callbacks.Events.Lifecycle;
   type Model is limited record
      Registration : CCL.Callbacks.Registration;
      Target : CCL.Callbacks.Events.Reference;
      Text : CCL.Host_Values.Text;
      Shown : Boolean := False;
      Dirty : Boolean := False;
      Dropped : CCL.Callbacks.Events.Pending_Count := 0;
   end record;
   function Visible (Item : Model) return Boolean is (Item.Shown);
   function Enabled (Item : Model) return Boolean is
     (Item.Shown and CCL.Callbacks.State (Item.Registration) in
        CCL.Callbacks.Events.Listening | CCL.Callbacks.Events.Executing);
   function Caption (Item : Model) return String is
     (if Item.Text.Length = 0 then "CCL button" else Item.Text.Data (1 .. Item.Text.Length));
   function Changed (Item : Model) return Boolean is (Item.Dirty);
   function Pending (Item : Model) return CCL.Callbacks.Events.Pending_Count is
     (CCL.Callbacks.Pending (Item.Registration));
   function Discarded (Item : Model) return CCL.Callbacks.Events.Pending_Count is (Item.Dropped);
end CCL.UI_Buttons;
