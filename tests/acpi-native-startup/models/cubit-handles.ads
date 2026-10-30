package CuBit.Handles with Pure is
   type Notification is private;
   No_Notification : constant Notification;
private
   type Notification is null record;
   No_Notification : constant Notification := (null record);
end CuBit.Handles;
