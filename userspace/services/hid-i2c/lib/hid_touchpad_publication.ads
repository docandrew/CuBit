pragma Ada_2022;
with CuBit.Messages;
with CuBit.Process_IDs;
with HID_I2C_Touchpad;
with Input_Pending;
package HID_Touchpad_Publication with SPARK_Mode is
   subtype Burst is Input_Pending.Count_Type range 1 .. Input_Pending.Capacity;
   generic
      with package App is new HID_I2C_Touchpad (<>);
      Maximum_Per_Turn : Burst;
      with function Consumer return CuBit.Process_IDs.Process_ID;
      with function Try_Publish (Target : CuBit.Process_IDs.Process_ID; Message : CuBit.Messages.Message) return Boolean;
   package Publisher is
      type Report is record
         Consumer_Identity : CuBit.Process_IDs.Process_ID := CuBit.Process_IDs.No_Process;
         Accepted : Input_Pending.Count_Type := 0;
         Pending : Input_Pending.Count_Type := 0;
         Consumer_Changed, Blocked, No_Consumer : Boolean := False;
      end record;
      procedure Flush (Now : Input_Pending.Word; Result : out Report);
      -- One registry observation and a bounded nonblocking drain. Uses existing
      -- CuBit.Input relative-pointer codec and publisher badge contract. Changes
      -- of full consumer lifetime reset stale backlog via App.Consumer_Changed.
      -- Sequence/recovery/acquisition timestamp stay with the queued report;
      -- failed publication never acknowledges/removes/coerces its head.
      -- Callbacks serialized/nonraising/nonreentrant; no pointer data callbacks.
   private
      Current_Consumer : CuBit.Process_IDs.Process_ID := CuBit.Process_IDs.No_Process;
   end Publisher;
end HID_Touchpad_Publication;
