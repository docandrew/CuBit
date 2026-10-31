pragma Ada_2022;
with CuBit.Handles;
with CuBit.Messages;
with CuBit.Process_IDs;
with HID_I2C_Touchpad;
with HID_Touchpad_Publication;
generic
   with package App is new HID_I2C_Touchpad (<>);
   Authority : CuBit.Handles.Notification;
   Maximum_Per_Turn : HID_Touchpad_Publication.Burst;
package HID_Touchpad_Native_Publication is
   function Consumer return CuBit.Process_IDs.Process_ID;
   function Try_Publish (Target : CuBit.Process_IDs.Process_ID; Message : CuBit.Messages.Message) return Boolean;
   package Output is new HID_Touchpad_Publication.Publisher (App, Maximum_Per_Turn, Consumer, Try_Publish);
   -- Authority is generated Startup.Mouse_Events from existing
   -- (request-notification mouse publish mouse-events). No new runtime protocol,
   -- roles, service-name lookup, peer Call or sender-selected endpoint/address.
end HID_Touchpad_Native_Publication;
