package body CCL.UI_Labels with SPARK_Mode is
   use type CCL.Host_Values.Value_Kind;
   use type Interfaces.Integer_64;
   procedure Apply
     (Item : in out Model; Op : Operation; Argument : CCL.VM.Value;
      Success : out Boolean) is
   begin
      Apply_Value (Item, Op, CCL.Host_Values.From_Scalar (Argument), Success);
   end Apply;
   function Image (Item : Model) return String is
     (if Item.Kind = Numeric then Interfaces.Integer_64'Image (Item.Number)
      else " " & Item.Text.Data (1 .. Item.Text.Length));
   procedure Apply_Value
     (Item : in out Model; Op : Operation; Argument : CCL.Host_Values.Value;
      Success : out Boolean) is
   begin
      Success := False;
      case Op is
         when Set_Value =>
            if Argument.Kind = CCL.Host_Values.Integer_Value then
               Item.Dirty := Item.Dirty or not Item.Shown or Item.Kind /= Numeric or (Item.Number /= Argument.Integer);
               Item.Kind := Numeric;
               Item.Number := Argument.Integer;
               Item.Shown := True;
               Success := True;
            end if;
         when Set_Visible =>
            if Argument.Kind = CCL.Host_Values.Boolean_Value then
               Item.Dirty := Item.Dirty or (Item.Shown /= Argument.Boolean);
               Item.Shown := Argument.Boolean;
               Success := True;
            end if;
         when Set_Text =>
            if Argument.Kind = CCL.Host_Values.Text_Value then
               Item.Dirty := Item.Dirty or else not Item.Shown or else Item.Kind /= Textual or else
                 Item.Text.Data (1 .. Item.Text.Length) /= Argument.Content.Data (1 .. Argument.Content.Length);
               Item.Kind := Textual;
               Item.Text := Argument.Content;
               Item.Shown := True;
               Success := True;
            end if;
      end case;
   end Apply_Value;
   procedure Painted (Item : in out Model) is
   begin
      Item.Dirty := False;
   end Painted;
end CCL.UI_Labels;
