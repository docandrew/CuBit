pragma Ada_2022;
package body CuBit.Metric_Batches with SPARK_Mode is
   procedure Append
     (Item : in out Builder; Pages : in out Page_Pair;
      Value : Records.Metric_Record; Accepted : out Boolean) is
      Fill : constant Page_Id := Item.Fill;
   begin
      Accepted := not Item.Busy (Fill) and then
        Item.Counts (Fill) < Records.Maximum_Records;
      if Accepted then
         Records.Put_Slot
           (Pages (Fill), Item.Counts (Fill) + 1, Records.Encode (Value));
         Item.Counts (Fill) := Item.Counts (Fill) + 1;
      else
         Item.Loss := Saturating_Increment (Item.Loss);
      end if;
   end Append;

   procedure Seal
     (Item : in out Builder; Pages : in out Page_Pair; Sealed : out Boolean;
      Page : out Page_Id; Bytes : out Unsigned_64) is
      Fill : constant Page_Id := Item.Fill;
   begin
      Page := Fill;
      Bytes := 0;
      Sealed := not Item.Busy (Fill) and then Item.Counts (Fill) > 0
        and then Item.Next in Records.Batch_Sequence;
      if not Sealed then
         return;
      end if;
      Records.Put_Slot
        (Pages (Fill), 0,
         Records.Encode_Header
           ((Records => Item.Counts (Fill), Sequence => Item.Next,
             Producer_Dropped => Item.Loss,
             Clock => Records.Monotonic_Microseconds)));
      Bytes := Records.Batch_Bytes (Item.Counts (Fill));
      Item.Busy (Fill) := True;
      Item.Next := Item.Next + 1;
      Item.Fill := Other (Fill);
   end Seal;

   procedure Complete (Item : in out Builder; Page : Page_Id) is
   begin
      if Item.Busy (Item.Fill) then
         Item.Fill := Page;
      end if;
      Item.Busy (Page) := False;
      Item.Counts (Page) := 0;
   end Complete;
end CuBit.Metric_Batches;
