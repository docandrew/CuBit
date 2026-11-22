pragma Ada_2022;
package body AML_Dispatch_Tails with SPARK_Mode is
   function Length (Item : Context) return Count is (Item.Used);
   function Batch (Item : Context) return AML_Profile_Events.Logical_Table_ID is (Item.Current);
   function Read (Item : Context; Ordinal : Position) return Observation is
     (Item.Items (if Item.Used = Capacity then
        (Item.Next - 1 + Ordinal - 1) mod Capacity + 1 else Ordinal));
   procedure Observe (Item : in out Context; Event : AML_Profile_Events.Event) is
   begin
      case Event.Kind is
         when AML_Profile_Events.Table_Enter => Item.Current := Event.Logical_Table;
         when AML_Profile_Events.Table_Leave => Item.Current := AML_Profile_Events.Unknown_Table;
         when AML_Profile_Events.Statement_Visit | AML_Profile_Events.Operand_Visit =>
            Item.Items (Item.Next) := (Event, Item.Current);
            Item.Next := (if Item.Next = Position'Last then Position'First else Item.Next + 1);
            Item.Used := Count'Min (Item.Used + 1, Capacity);
      end case;
   end Observe;
end AML_Dispatch_Tails;
