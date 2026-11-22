pragma Ada_2022;
with GPIO_Control_Wire;
package GPIO_HID_Server_Model is
   package W renames GPIO_Control_Wire;
   procedure Initialize;
   procedure Submit (Item : W.Request; Token : W.Word; Accepted : out Boolean);
   procedure Drive (Now : W.Word);
   procedure Trigger_Event (Now : W.Word);
   procedure Take_Reply (Token : out W.Word; Value : out W.Completion);
   procedure Take_Event (Value : out W.Event);
   function Masked return Boolean;
   function Quiescent return Boolean;
   function Checks return Natural;
   function Ack_Count return Natural;
   function Arm_Count return Natural;
   function Release_Count return Natural;
   -- Hosted transport/MMIO fixture around actual dispatcher/channel/lease/
   -- controller. Queues never invoke a client while Submit is on-stack.
end GPIO_HID_Server_Model;
