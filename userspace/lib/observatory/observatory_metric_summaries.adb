package body Observatory_Metric_Summaries with SPARK_Mode is
   function Metadata (Row : P.Summary_Row) return R.Decoded_Record is
      Words : constant R.Slot_Words :=
        [R.Record_Kind'Enum_Rep (R.Describe), Row (P.Row_Key),
         Row (P.Row_Kind), Row (P.Row_Unit),
         Row (P.Row_First_Name), Row (P.Row_First_Name + 1),
         Row (P.Row_First_Name + 2), Row (P.Row_First_Name + 3)];
   begin
      return R.Decode (Words);
   end Metadata;


   function Decode (Row : P.Summary_Row) return Decoded is
   begin
      if not Valid_Row (Row) then return (Success => False); end if;
      return (Success => True, Value => (Words => Row));
   end Decode;

   function Declaration (Item : View) return R.Decoded_Record is
   begin
      return Metadata (Item.Words);
   end Declaration;
end Observatory_Metric_Summaries;
