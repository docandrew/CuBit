pragma Ada_2022;
package body Firmware_Tables.Exposure with SPARK_Mode is
   function Page_Window (D : Descriptor) return Window is
      Offset : constant Page_Offset := D.Physical mod Page_Size;
      First : constant Address_Value := D.Physical - Offset;
      End_Byte : constant Address_Value := D.Physical + Address_Value (D.Extent - 1);
      Last : constant Address_Value := End_Byte + (Page_Size - 1 - End_Byte mod Page_Size);
   begin
      pragma Assert (First <= D.Physical);
      pragma Assert (D.Physical - First = D.Physical mod Page_Size);
      pragma Assert (D.Physical - First <= Page_Size - 1);
      pragma Assert (End_Byte >= D.Physical);
      pragma Assert (Page_Size - 1 - End_Byte mod Page_Size <= Address_Value'Last - End_Byte);
      pragma Assert (Last >= End_Byte);
      return (First => First, Last => Last,
              Offset => D.Physical - First,
              Pages => (Last - First) / Page_Size + 1);
   end Page_Window;

   function Known_Byte (S : State; At_Address : Address_Value) return Boolean is
     (for some I in 1 .. Count (S) =>
       Item (S, I).Physical <= At_Address and then
       At_Address <= Item (S, I).Physical + Address_Value (Item (S, I).Extent - 1));
   function Known_Range (S : State; First, Last : Address_Value) return Boolean is
     (for all A in First .. Last => Known_Byte (S, A));

   procedure Certify_Range
     (S : State; Index : Positive; First, Last : Address_Value)
     with Ghost,
       Pre => Index <= Count (S) and then First <= Last
         and then Item (S, Index).Physical <= First
         and then Last <= Item (S, Index).Physical +
           Address_Value (Item (S, Index).Extent - 1),
       Post => Known_Range (S, First, Last);
   procedure Certify_Range
     (S : State; Index : Positive; First, Last : Address_Value) is
   begin
      pragma Assert (for all A in First .. Last =>
        Item (S, Index).Physical <= A and then
        A <= Item (S, Index).Physical + Address_Value (Item (S, Index).Extent - 1));
      pragma Assert (for all A in First .. Last => Known_Byte (S, A));
   end Certify_Range;

   type Coverage is record
      Found : Boolean := False;
      Last : Address_Value := 0;
   end record;
   function Cover (S : State; Cursor : Address_Value) return Coverage
     with Pre => Count (S) > 0,
       Post => (if Cover'Result.Found then
       Cover'Result.Last >= Cursor and then
       Known_Range (S, Cursor, Cover'Result.Last))
   is
      Result : Coverage;
   begin
      for I in 1 .. Count (S) loop
         pragma Loop_Invariant (if Result.Found then
           Result.Last >= Cursor and then Known_Range (S, Cursor, Result.Last));
         declare
            D : constant Descriptor := Item (S, I);
            Last : constant Address_Value := D.Physical + Address_Value (D.Extent - 1);
         begin
            if D.Physical <= Cursor and then Cursor <= Last
              and then (not Result.Found or else Last > Result.Last)
            then
               Certify_Range (S, I, Cursor, Last);
               Result := (Found => True, Last => Last);
            end if;
         end;
      end loop;
      return Result;
   end Cover;

   function Describe (S : State; Index : Positive) return Plan is
      Span : constant Window := Page_Window (Item (S, Index));
      Cursor : Address_Value := Span.First;
   begin
      -- Each successful step passes the end of at least one table. A bounded
      -- scan handles overlap and unsorted discovery without per-byte work.
      for Step in 1 .. Count (S) loop
         pragma Loop_Invariant (Cursor in Span.First .. Span.Last);
         pragma Loop_Invariant (if Cursor > Span.First then
           Known_Range (S, Span.First, Cursor - 1));
         declare
            Covered : constant Coverage := Cover (S, Cursor);
         begin
            if not Covered.Found then
               return (Copy_Required, Span);
            elsif Covered.Last >= Span.Last then
               pragma Assert (Known_Range (S, Span.First, Span.Last));
               return (Retained_Candidate, Span);
            else
               pragma Assert (Known_Range (S, Span.First, Covered.Last));
               Cursor := Covered.Last + 1;
            end if;
         end;
      end loop;
      return (Copy_Required, Span);
   end Describe;
end Firmware_Tables.Exposure;
