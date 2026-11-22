with Interfaces;
with CCL.VM;
with CCL.Host_Values;

--  One host-owned numeric/text label. This model has no desktop pointers, IPC,
--  filesystem access, callback, or authority acquisition of its own.
package CCL.UI_Labels with SPARK_Mode is
   type Operation is (Set_Value, Set_Visible, Set_Text);
   function Name (Op : Operation) return String is
     (case Op is when Set_Value => "label-value", when Set_Visible => "label-visible",
      when Set_Text => "label-text");
   type Model is private;
   procedure Apply
     (Item : in out Model; Op : Operation; Argument : CCL.VM.Value;
      Success : out Boolean);
   procedure Apply_Value
     (Item : in out Model; Op : Operation; Argument : CCL.Host_Values.Value;
      Success : out Boolean);
   function Image (Item : Model) return String;
   function Visible (Item : Model) return Boolean;
   function Value (Item : Model) return Interfaces.Integer_64;
   function Changed (Item : Model) return Boolean;
   procedure Painted (Item : in out Model);
private
   type Content_Kind is (Numeric, Textual);
   type Model is record
      Kind : Content_Kind := Numeric;
      Text : CCL.Host_Values.Text;
      Shown : Boolean := False;
      Number : Interfaces.Integer_64 := 0;
      Dirty : Boolean := False;
   end record;
   function Visible (Item : Model) return Boolean is (Item.Shown);
   function Value (Item : Model) return Interfaces.Integer_64 is (Item.Number);
   function Changed (Item : Model) return Boolean is (Item.Dirty);
end CCL.UI_Labels;
