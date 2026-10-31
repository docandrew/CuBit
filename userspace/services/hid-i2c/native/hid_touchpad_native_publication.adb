pragma Ada_2022;
package body HID_Touchpad_Native_Publication is
   package M renames CuBit.Messages;
   function Consumer return CuBit.Process_IDs.Process_ID is
     (M.Registered_Driver (M.DRIVER_MOUSE));
   function Try_Publish (Target : CuBit.Process_IDs.Process_ID; Message : M.Message) return Boolean is
     (M.trySendEvent (Target, Message, Authority));
end HID_Touchpad_Native_Publication;
