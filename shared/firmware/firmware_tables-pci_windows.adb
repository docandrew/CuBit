pragma Ada_2022;
package body Firmware_Tables.PCI_Windows with SPARK_Mode is
   function Select_Bus
     (Item : M.Allocation; Segment : M.Segment_Number; Bus : M.Bus_Number;
      Mode : Access_Mode; Limit : Address_Value) return Selection
   is
      First : Address_Value;
   begin
      if Mode /= Read_Only then return (Status => Write_Refused);
      elsif not Valid (Item, Limit) then return (Status => Invalid_Allocation);
      elsif Segment /= Item.Segment then return (Status => Wrong_Segment);
      elsif Bus < Item.First_Bus or else Bus > Item.Last_Bus then return (Status => Outside_Buses);
      end if;
      First := Item.Base + Address_Value (Bus) * Bus_Bytes;
      return (Status => Ready, Segment => Segment, Bus => Bus,
        First => First, Pages => Bus_Pages, Mode => Read_Only);
   end Select_Bus;
end Firmware_Tables.PCI_Windows;
