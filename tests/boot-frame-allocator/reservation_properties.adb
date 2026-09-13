package body Reservation_Properties with SPARK_Mode is
   procedure Reserve_Twice
     (This : in out Core.State; Left_Size, Right_Size : Core.Request_Size;
      Left, Right : out Core.Frame)
   is
   begin
      Core.Reserve (This, Left_Size, Left);
      Core.Reserve (This, Right_Size, Right);
   end Reserve_Twice;

   procedure Reserve_For_Handoff
     (This : in out Core.State; Size : Core.Request_Size; First : out Core.Frame)
   is
   begin
      Core.Reserve (This, Size, First);
   end Reserve_For_Handoff;
end Reservation_Properties;
