package body CCL.UI_Outputs with SPARK_Mode is
   use type CCL.Host_Values.Value_Kind;
   procedure Append
     (Item : in out Model; Text : String; Accepted : out Boolean) is
   begin
      --  Reserve one byte for LF, including for an empty output line.
      Accepted := Text'Length < Maximum_Length - Item.Last;
      if Accepted then
         Item.Data (Item.Last + 1 .. Item.Last + Text'Length) := Text;
         Item.Last := Item.Last + Text'Length + 1;
         Item.Data (Item.Last) := ASCII.LF;
         Item.Dirty := True;
      end if;
   end Append;

   procedure Clear (Item : in out Model) is
   begin
      Item.Dirty := Item.Dirty or Item.Last /= 0;
      Item.Last := 0;
      Item.Data := [others => ' '];
   end Clear;

   procedure Painted (Item : in out Model) is
   begin
      Item.Dirty := False;
   end Painted;

   procedure Apply
     (Item : in out Model; Op : Operation; Argument : CCL.Host_Values.Value;
      Accepted : out Boolean) is
   begin
      Accepted := False;
      case Op is
         when Append_Line =>
            if Argument.Kind = CCL.Host_Values.Text_Value then
               Append (Item, Argument.Content.Data
                 (1 .. Argument.Content.Length), Accepted);
            end if;
         when Clear_Output =>
            Clear (Item);
            Accepted := True;
      end case;
   end Apply;
end CCL.UI_Outputs;
