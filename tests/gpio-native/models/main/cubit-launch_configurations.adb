with Main_Model;
with Interrupt_Line_ABI;
package body CuBit.Launch_Configurations is
 package LC renames Launch_Configuration;package B renames Main_Model;
 use type B.Scenario,LC.Physical_Address;
 procedure Get(Value:out LC.Generic_ACPI_Configuration;Valid:out Boolean) is
 begin
 Value:=(HID=>"INTC1057",Epoch=>16#F234_5678_0000_0022#,Segments=>2,Window_Total=>4,others=><>);
 if B.Current /= B.No_Parent then
 Value.Line := (Present => True, Vector => Interrupt_Line_ABI.Vector_Of (14),
 Trigger => Interrupt_Line_ABI.Level, Polarity => Interrupt_Line_ABI.Active_Low, Shared => True, GSI => 14);
 end if;
 Value.Path(1):="_SB_";Value.Path(2):="GPI0";
 for I in 1..4 loop Value.Windows(I):=(16#1000_0000#+LC.Physical_Address(I)*8192,8192);end loop;
 Valid:=B.Current/=B.Missing_Configuration;
 case B.Current is
 when B.Wrong_Controller=>Value.HID:="INTC0000";
 when B.Invalid_Launch_Path=>Value.Path(1):="1BAD";
 when B.Wrong_Window_Count=>Value.Window_Total:=3;Value.Windows(4):=(0,0);
 when B.Late_Window_Mismatch=>Value.Windows(4).First:=Value.Windows(4).First+4096;
 when B.Overlapping_Community=>Value.Windows(4):=Value.Windows(3);
 when B.Unaligned_Community=>Value.Windows(4).Bytes:=8193;
 when B.Zero_Launch_Epoch=>Value.Epoch:=0;
 when others=>null;end case;
 end Get;
end CuBit.Launch_Configurations;
