with Test_Control;
package body Desktop_Capability_IO with SPARK_Mode => Off is
 procedure Inspect(Slot : Interfaces.Unsigned_64; Data : out Words; Succeeded : out Boolean) is
 begin Data := (0=>1,1=>3,3=>1,others=>0);
 if Test_Control.Empty_Slot then Data := (others=>0); end if;
 Succeeded := Test_Control.Inspect_OK; end;
end Desktop_Capability_IO;
