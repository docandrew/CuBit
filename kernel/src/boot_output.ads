with System;
-- Hardware-independent routing prevents diagnostic consumers from depending
-- on renderer/lock initialization. Install once on the boot CPU, before APs or
-- userspace start; retirement is permanent even if no renderer was installed.
package Boot_Output with SPARK_Mode => Off is
   type Text_Sink is access procedure (C : Character);
   type Panic_Sink is access procedure (Message : System.Address);
   type Retire_Sink is access procedure;
   procedure Install (Text : Text_Sink; Panic : Panic_Sink; Retire : Retire_Sink);
   function Is_Retired return Boolean;
   procedure Append (C : Character);
   procedure Panic (Message : System.Address);
   procedure Retire;
end Boot_Output;
