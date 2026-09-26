package body System.Tasking.Protected_Objects is

   function Get_Ceiling
     (Object : Protection_Access) return System.Any_Priority is
   begin
      return Object.Ceiling;
   end Get_Ceiling;

   procedure Initialize_Protection
     (Object           : Protection_Access;
      Ceiling_Priority : Integer) is
   begin
      Object.Ceiling :=
        (if Ceiling_Priority in System.Any_Priority
         then Ceiling_Priority else System.Any_Priority'Last);
      Object.Locked := False;
   end Initialize_Protection;

   procedure Lock (Object : Protection_Access) is
   begin
      if Object.Locked then
         raise Program_Error;
      end if;
      Object.Locked := True;
   end Lock;

   procedure Lock_Read_Only (Object : Protection_Access) is
   begin
      Lock (Object);
   end Lock_Read_Only;

   procedure Set_Ceiling
     (Object : Protection_Access;
      Prio   : System.Any_Priority) is
   begin
      Object.Ceiling := Prio;
   end Set_Ceiling;

   procedure Unlock (Object : Protection_Access) is
   begin
      Object.Locked := False;
   end Unlock;

end System.Tasking.Protected_Objects;
