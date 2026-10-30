with Owner_Test_Control;
package body CuBit.Messages is
   use type Owner_Test_Control.Scenario;
   function registerDriver
     (Role : Interfaces.Unsigned_64; Authority : CuBit.Handles.Notification)
      return Interfaces.Unsigned_64 is
      pragma Unreferenced (Authority);
      package T renames Owner_Test_Control;
   begin
      T.Registrations := T.Registrations + 1;
      T.Registered_Role := Role;
      T.Sequence := T.Sequence + 1; T.Registered_At := T.Sequence;
      if T.Selected = T.Register_Refused then return Interfaces.Unsigned_64'Last; end if;
      return 0;
   end registerDriver;
end CuBit.Messages;
