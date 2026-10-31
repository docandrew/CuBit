package CuBit.Handles with Pure is
   type Notification is private;
   No_Notification : constant Notification;
   type Endpoint is private;
   No_Endpoint : constant Endpoint;
private
   type Notification is null record;
   No_Notification : constant Notification := (null record);
   type Endpoint is null record;
   No_Endpoint : constant Endpoint := (null record);
end CuBit.Handles;
