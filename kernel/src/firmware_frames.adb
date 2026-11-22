pragma Ada_2022;
package body Firmware_Frames with SPARK_Mode is
   function Whole_Pages (Low, High : Byte_Address) return Span is
      Start : constant Boundary := Low / Page_Bytes +
        (if Low mod Page_Bytes = 0 then 0 else 1);
      Finish : constant Boundary := High / Page_Bytes +
        (if High mod Page_Bytes = Page_Bytes - 1 then 1 else 0);
   begin
      return (Start, Finish);
   end Whole_Pages;

   function Touched_Pages (Low, High : Byte_Address) return Span is
   begin
      return (Low / Page_Bytes, High / Page_Bytes + 1);
   end Touched_Pages;

   function Largest_Block (Low, High : Frame; Maximum : Block_Order)
     return Block_Order is
   begin
      for Order in reverse Block_Order range 0 .. Maximum loop
         if Block_Pages (Order) <= High - Low + 1 and then
           Low mod Block_Pages (Order) = 0
         then
            return Order;
         end if;
      end loop;
      return 0;
   end Largest_Block;

   function Classify (Map : Region_Array; Owner : Natural;
                      Low, High : Frame) return Decision is
      Partial : Boolean := False;
   begin
      for I in Map'Range loop
         if Conflicts (Map, Owner, I, Low, High) then
            if Contains (Map (I).Pages, Low, High) then
               return Reject;
            end if;
            Partial := True;
         end if;
         pragma Loop_Invariant
           (if not Partial then
              (for all Prior in Map'First .. I =>
                 not Conflicts (Map, Owner, Prior, Low, High)));
         pragma Loop_Invariant (if Low = High then not Partial);
      end loop;
      return (if Partial then Split else Admit);
   end Classify;

   procedure Prove_Unique_Owner
     (Map : Region_Array; Left, Right : Natural; Item : Frame) is
   begin
      null;
   end Prove_Unique_Owner;
end Firmware_Frames;
