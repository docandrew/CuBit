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

   procedure Append_Group
     (Item : in out Builder; Pages : in out Page_Pair;
      Values : Records.Trace_Group; Accepted : out Boolean) is
      Fill : constant Page_Id := Item.Fill;
      Initial : constant Records.Record_Count := Item.Counts (Fill);
   begin
      Accepted := Has_Group_Room (Item);
      if not Accepted then
         Item.Loss := Drop_Group (Item.Loss);
         return;
      end if;
      for I in Records.Trace_Part loop
         Records.Put_Slot
           (Pages (Fill), Initial + I + 1, Records.Encode (Values (I)));
         pragma Loop_Invariant
           (for all J in Records.Trace_Part'First .. I =>
              Records.Slot (Pages (Fill), Initial + J + 1) =
                Records.Encode (Values (J)));
         pragma Loop_Invariant
           (for all W in Records.Page_Word_Index =>
              (if W / Records.Words_Per_Slot <= Initial or else
                  W / Records.Words_Per_Slot > Initial + I + 1
               then Pages (Fill) (W) = Pages'Loop_Entry (Fill) (W)));
         pragma Loop_Invariant
           (Pages (Other (Fill)) = Pages'Loop_Entry (Other (Fill)));
      end loop;
      Item.Counts (Fill) := Initial + 4;
   end Append_Group;

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
