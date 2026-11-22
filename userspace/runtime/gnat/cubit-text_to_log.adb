pragma Ada_2022;
package body CuBit.Text_To_Log with SPARK_Mode is
   procedure Append (Item : in out Adapter; Byte : Character);
   procedure Emit
     (Item : in out Adapter; At_Time : Logs.Timestamp; Output : out Step);
   function Kind (Item : Step) return Step_Kind is (Item.Data.Kind);
   function Value (Item : Step) return Logs.Log_Record is (Item.Data.Value);
   function Reason (Item : Step) return Drop_Reason is (Item.Data.Reason);
   procedure Append (Item : in out Adapter; Byte : Character) is
   begin
      if not Item.Oversized then
         if Item.Length = Logs.Maximum_Text_Bytes then
            Item.Oversized := True;
         else
            Item.Length := Item.Length + 1;
            Item.Data (Item.Length) := Byte;
         end if;
      end if;
   end Append;

   procedure Emit
     (Item : in out Adapter; At_Time : Logs.Timestamp; Output : out Step) is
      Value : Logs.Decoded;
   begin
      if Item.Oversized then
         Output.Data := (Kind => Line_Dropped, Reason => Oversized_Line);
      else
         Value := Logs.Make
           (Item.Data (1 .. Item.Length), Item.Level, At_Time);
         if Value.Success then
            Output.Data := (Kind => Record_Ready, Value => Value.Value);
         else
            Output.Data := (Kind => Line_Dropped,
                       Reason => Invalid_UTF8_Or_Control);
         end if;
      end if;
      Item.Length := 0;
      Item.Pending_CR := False;
      Item.Oversized := False;
      Item.Data := [others => Character'Val (0)];
   end Emit;

   procedure Feed
     (Item : in out Adapter; Byte : Character; At_Time : Logs.Timestamp;
      Output : out Step) is
   begin
      Output.Data := (Kind => Need_More);
      if Item.Resynchronizing then
         if Byte = Character'Val (10) then
            Item.Resynchronizing := False;
         end if;
      elsif Byte = Character'Val (10) then
         Emit (Item, At_Time, Output);
      else
         if Item.Pending_CR then
            Append (Item, Character'Val (13));
            Item.Pending_CR := False;
         end if;
         if Byte = Character'Val (13) then
            Item.Pending_CR := True;
         else
            Append (Item, Byte);
         end if;
      end if;
   end Feed;

   procedure Finish
     (Item : in out Adapter; At_Time : Logs.Timestamp; Output : out Step) is
   begin
      if Item.Resynchronizing then
         Item.Resynchronizing := False;
         Output.Data := (Kind => Need_More);
         return;
      end if;
      if Item.Pending_CR then
         Append (Item, Character'Val (13));
      end if;
      if Item.Length /= 0 or Item.Oversized then
         Emit (Item, At_Time, Output);
      else
         Output.Data := (Kind => Need_More);
      end if;
   end Finish;

   procedure Report_Gap (Item : out Adapter; Output : out Step) is
   begin
      Item.Length := 0;
      Item.Pending_CR := False;
      Item.Oversized := False;
      Item.Resynchronizing := True;
      Item.Data := [others => Character'Val (0)];
      Output.Data := (Kind => Line_Dropped, Reason => Upstream_Gap);
   end Report_Gap;
end CuBit.Text_To_Log;
