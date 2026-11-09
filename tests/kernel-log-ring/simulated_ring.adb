pragma Ada_2022;
with Kernel_Log_Publication.Drain;

package body Simulated_Ring is
   type Area_Access is access Ring_Area;
   Area : constant Area_Access := new Ring_Area;
   Ring_Capacity : Record_Capacity := Maximum_Records;

   procedure Reset (Capacity : Record_Capacity) is
   begin
      Area.all := (others => <>);
      Ring_Capacity := Capacity;
      Area.Head.Capacity := Capacity;
      Area.Head.Version := Layout_Version;
      Seen := 0;
      Delivered_Count := 0;
      Run_Count := 0;
      Delivered_Text_Ok := True;
   end Reset;

   function Total return Unsigned_64 is (Area.Head.Total);

   function Decimal (Value : Unsigned_64) return String is
      Image : constant String := Unsigned_64'Image (Value);
   begin
      return Image (Image'First + 1 .. Image'Last);
   end Decimal;

   procedure Write (Text : String; Time_Ms : Unsigned_64 := 0) is
      Number : constant Record_Number := Next_Number (Area.Head.Total);
      Where  : constant Slot := Slot_Of (Number, Ring_Capacity);
   begin
      Area.Records (Where).Sequence := Writing;
      Area.Records (Where) := Make_Line (Time_Ms, Text, False);
      Area.Records (Where).Sequence := Number;
      Area.Head.Total := Number;
   end Write;

   procedure Begin_Overwrite (Number : Record_Number) is
   begin
      Area.Records (Slot_Of (Number, Ring_Capacity)).Sequence := Writing;
   end Begin_Overwrite;

   function Load_Header return Header is (Area.Head);
   function Load_Sequence (Where : Slot) return Unsigned_64 is (Area.Records (Where).Sequence);
   function Load_Record (Where : Slot) return Log_Record is (Area.Records (Where));

   procedure Deliver (Number : Record_Number; Item : Log_Record) is
   begin
      Delivered_Count := Delivered_Count + 1;
      Delivered (Delivered_Count) := Number;
      --  The tests write "line N" as record N's text.
      if Text_Of (Item) /= "line " & Decimal (Number) then
         Delivered_Text_Ok := False;
      end if;
   end Deliver;

   procedure Dropped (First, Last : Record_Number) is
   begin
      Run_Count := Run_Count + 1;
      Runs (Run_Count) := (First, Last);
   end Dropped;

   procedure Step is new Kernel_Log_Publication.Drain
     (Load_Header, Load_Sequence, Load_Record, Deliver, Dropped);

   procedure Drain (Budget : Record_Capacity; Accounted : out Unsigned_64; More : out Boolean) is
   begin
      Step (Seen, Budget, Accounted, More);
   end Drain;
end Simulated_Ring;
