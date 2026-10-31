pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with ACPI_Service; use ACPI_Service;
with ACPI_Resource_Reader;
with ACPI_Resource_Data; use ACPI_Resource_Data;
with ACPI_Resource_Data.Basic_Descriptors;
with ACPI_Resource_Data.Extended_Interrupts;
with ACPI_Resource_Data.Address_Spaces;
with ACPI_Resource_Data.Address_Spaces.Geometry;
with ACPI_Resource_Data.Memory_Ranges;
with ACPI_Resource_Data.Generic_Registers;
with ACPI_Resource_Data.DMA_Descriptors;
with ACPI_Resource_Data.Dependent_Groups;
with ACPI_Resource_Data.Address_Spaces.Extended_Descriptors;
with ACPI_Resource_Data.GPIO_Connections;
with ACPI_Resource_Data.Serial_Connections;
with ACPI_Resource_Data.Pin_Descriptors;
with ACPI_Resource_Data.Clock_Inputs;
with AML_Execute;
with Firmware_Tables;
with Resource_Oracle;
procedure Resource_Service_Tests is
   package C renames ACPI_Resource_Data.Clock_Inputs;
   use type C.Decode_Status;
   use type C.Clock_Mode;
   use type C.Frequency_Scale;
   use type C.Frequency_Numerator;
   use type C.Frequency_Divisor;
   use type C.Hertz_Numerator;
   package P renames ACPI_Resource_Data.Pin_Descriptors;
   use type P.Decode_Status;
   use type P.Descriptor_Kind;
   use type P.Resource_Role;
   use type P.Pin_Number;
   use type P.Pull_Configuration;
   use type P.Function_Number;
   use type P.Configuration_Type;
   use type P.Configuration_Value;
   package S renames ACPI_Resource_Data.Serial_Connections;
   use type S.Decode_Status;
   use type S.Bus_Kind;
   use type S.Resource_Role;
   use type S.Initiator;
   use type S.Address_Mode;
   use type S.Slave_Address;
   use type S.Rate;
   use type S.Wire_Mode;
   use type S.Signal_Polarity;
   use type S.Clock_Phase;
   use type S.Clock_Polarity;
   use type S.Device_Selection;
   use type S.Transfer_Bits;
   use type S.Flow_Control;
   use type S.Data_Bits;
   use type S.Stop_Bits;
   use type S.Byte_Order;
   use type S.Parity;
   use type S.Serial_Lines;
   use type S.FIFO_Bytes;
   use type S.PHY_Kind;
   use type S.Local_Port;
   use type S.Descriptor_Revision;
   package Reader is new ACPI_Resource_Reader (ACPI_Service);
   use type Reader.Copy_Status;
   package B renames ACPI_Resource_Data.Basic_Descriptors;
   package E renames ACPI_Resource_Data.Extended_Interrupts;
   package A renames ACPI_Resource_Data.Address_Spaces;
   package G renames A.Geometry;
   package M renames ACPI_Resource_Data.Memory_Ranges;
   package GR renames ACPI_Resource_Data.Generic_Registers;
   package D renames ACPI_Resource_Data.DMA_Descriptors;
   package DG renames ACPI_Resource_Data.Dependent_Groups;
   package EA renames ACPI_Resource_Data.Address_Spaces.Extended_Descriptors;
   package GPIO renames ACPI_Resource_Data.GPIO_Connections;
   use type GPIO.Decode_Status;
   use type GPIO.Connection_Kind;
   use type GPIO.Resource_Role;
   use type GPIO.Interrupt_Mode;
   use type GPIO.Interrupt_Polarity;
   use type GPIO.IO_Restriction;
   use type GPIO.Pin_Number;
   use type GPIO.Pin_Configuration;
   use type GPIO.Drive_Hundredths_Milliampere;
   use type GPIO.Debounce_Hundredths_Millisecond;
   use type EA.Decode_Status;
   use type EA.Extended_Kind;
   use type EA.Resource_Role;
   use type EA.Attribute_Value;
   use type EA.Cache_Bits;
   use type DG.Analysis_Status;
   use type DG.Priority;
   use type D.Decode_Status;
   use type D.Resource_Kind;
   use type D.Channel_Mask;
   use type D.Channel_Speed;
   use type D.Transfer_Preference;
   use type D.Request_Line;
   use type D.Channel_Number;
   use type D.Fixed_Transfer_Width;
   use type M.Decode_Status;
   use type M.Memory_Encoding;
   use type M.Memory_Value;
   use type GR.Decode_Status;
   use type Interfaces.Unsigned_64;
   use type G.Geometry_Status;
   use type A.Decode_Status;
   use type A.Address_Width;
   use type A.Address_Value;
   use type A.Resource_Kind;
   use type A.Decode_Mode;
   use type A.Memory_Cache_Claim;
   use type E.Decode_Status;
   use type E.Interrupt_Number;
   use type E.Resource_Role;
   use type E.Interrupt_Mode;
   use type E.Interrupt_Polarity;
   use type B.Decode_Status;
   use type B.Resource_Kind;
   use type B.Port_Address;
   use type B.IRQ_Mask;
   use type B.Interrupt_Mode;
   use type B.Interrupt_Polarity;
   use type B.IO_Decoding;
   use type B.Memory_Base_32;
   use type B.Memory_Length_32;
   use type AML_Execute.Execution_Status;
   use type Firmware_Tables.Bytes;
   use type Firmware_Tables.Byte;
   use type Values.Access_Status;
   use type Values.Value_Handle;
   use type Values.Value_Kind;
   Service : aliased State (1, Resource_Oracle.Raw_Table'Length, Resource_Oracle.Raw_Table'Length, Load_Capacity => 1);
   Staged_Result : Stage_Status;
   Frozen : Freeze_Status;
   Access_Result : Values.Access_Status;
   Execution : Execution_Report;
   Members : Namespace.Initialization_Report;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Label : String) is
   begin Checks:=Checks+1; if not Good then raise Program_Error with Label; end if; end Check;
   procedure Typed (Data : Firmware_Tables.Bytes; Index : Resource_Oracle.Case_Index) is
      Cursor : Byte_Offset:=0;
   begin
      declare Alternatives : DG.Alternative_Array(1 .. 2); Grouping : DG.Analysis_Result; begin
         DG.Analyze(Data,Alternatives,Grouping);
         Check(Grouping.Status=DG.Grouped and then Grouping.Value.Alternatives=
           (if Index=3 then 1 elsif Index=14 then 2 else 0),"actual Core grouping analysis");
         if Index=3 then
            Check(Alternatives(1).First=1 and then Alternatives(1).Limit=5
              and then Alternatives(1).Items=1 and then not Alternatives(1).Information_Present
              and then Alternatives(1).Compatibility=DG.Acceptable
              and then Alternatives(1).Performance=DG.Acceptable,"normal default group");
         elsif Index=14 then
            Check(Grouping.Value.Common_Before=1 and then Grouping.Value.Common_After=1
              and then Grouping.Value.Group_First=2 and then Grouping.Value.Group_Limit=21
              and then Grouping.Value.End_Tag=23,"normal common/group boundaries");
            Check(Alternatives(1).Marker=2 and then Alternatives(1).First=4 and then Alternatives(1).Limit=11
              and then Alternatives(1).Items=2 and then Alternatives(1).Compatibility=DG.Good
              and then Alternatives(1).Performance=DG.Acceptable,"normal first alternative");
            Check(Alternatives(2).Marker=11 and then Alternatives(2).First=13 and then Alternatives(2).Limit=20
              and then Alternatives(2).Items=2 and then Alternatives(2).Compatibility=DG.Acceptable
              and then Alternatives(2).Performance=DG.Good,"normal second alternative");
         end if;
      end;
      for Ordinal in 1 .. Resource_Oracle.Items(Index) loop
         declare
            Item : constant Frame:=Read_Frame(Data,Cursor);
            R : constant B.Decode_Result:=B.Decode(Data,Cursor);
         begin
            Check(Item.Status=Decoded,"typed frame exists");
            if Index=19 then
               declare Clock : constant C.Decode_Result := C.Decode (Data, Cursor); begin
                  Check (Clock.Status = C.Decoded and then Clock.Next_Offset = Item.Next_Offset,
                    "normal clock descriptor frame");
                  Check (Clock.Value.Mode = (if Ordinal = 2 then C.Variable_Frequency else C.Fixed_Frequency)
                    and then Clock.Value.Scale = (case Ordinal is when 1 => C.Megahertz, when 2 => C.Hertz, when others => C.Kilohertz),
                    "normal clock mode and units");
                  Check (Clock.Value.Numerator = (case Ordinal is when 1 => 100, when 2 => 19_200, when others => 16#FFFF_FFFF#)
                    and then Clock.Value.Divisor = (case Ordinal is when 1 => 3, when 2 => 1, when others => 65_535),
                    "normal clock raw frequency fraction");
                  Check (Clock.Value.Source_Index = (if Ordinal = 2 then 3 else 0)
                    and then Clock.Value.Source.First = Cursor + 13
                    and then Clock.Value.Source.Length = (if Ordinal = 1 then 0 else 10)
                    and then Clock.Value.Source.First + Clock.Value.Source.Length = Clock.Next_Offset,
                    "normal clock optional source extent");
                  declare Hz : constant C.Rational_Hertz := C.In_Hertz (Clock.Value); begin
                     Check (Hz.Numerator = (case Ordinal is when 1 => 100_000_000, when 2 => 19_200, when others => 4_294_967_295_000)
                       and then Hz.Divisor = Clock.Value.Divisor, "normal exact rational Hertz");
                  end;
               end;
            elsif Index=18 then
               declare Pin : constant P.Decode_Result := P.Decode (Data, Cursor); begin
                  Check (Pin.Status = P.Decoded and then Pin.Next_Offset = Item.Next_Offset,
                    "normal pin descriptor frame");
                  Check (Pin.Information.Kind = P.Descriptor_Kind'Val (Ordinal - 1)
                    and then Pin.Shared = (Ordinal in 1 | 4)
                    and then Pin.Role = (if Ordinal in 3 | 5 then P.Produces_And_Consumes else P.Consumes_Resource),
                    "normal pin kind/role/sharing");
                  Check (Pin.Count = (case Ordinal is when 1 | 3 => 3, when 2 => 2, when others => 0)
                    and then Pin.Pins.Length = Byte_Count (Pin.Count * P.Pin_Bytes)
                    and then Pin.Source.Length = (if Ordinal = 3 then 0 else 6)
                    and then Pin.Label.Length = (if Ordinal <= 2 then 0 else 4)
                    and then Pin.Vendor.Length = (if Ordinal in 2 | 4 then 1 else 2)
                    and then Pin.Vendor.First + Pin.Vendor.Length = Pin.Next_Offset,
                    "normal pin spans and count");
                  case Ordinal is
                     when 1 =>
                        Check (Pin.Information.Pull = 2 and then Pin.Information.Function_ID = 16#BEEF#,
                          "normal function fields");
                        Check (P.Read_Pin (Data, Cursor, 1).Available and then P.Read_Pin (Data, Cursor, 1).Value = 0
                          and then P.Read_Pin (Data, Cursor, 2).Available and then P.Read_Pin (Data, Cursor, 2).Value = 16#1234#
                          and then P.Read_Pin (Data, Cursor, 3).Available and then P.Read_Pin (Data, Cursor, 3).Value = 65_535,
                          "normal function pins");
                     when 2 =>
                        Check (Pin.Information.Configuration = 13 and then Pin.Information.Value = 1,
                          "normal configuration fields");
                        Check (P.Read_Pin (Data, Cursor, 1).Available and then P.Read_Pin (Data, Cursor, 1).Value = 1
                          and then P.Read_Pin (Data, Cursor, 2).Available and then P.Read_Pin (Data, Cursor, 2).Value = 2,
                          "normal configuration pins");
                     when 3 =>
                        Check (P.Read_Pin (Data, Cursor, 1).Available and then P.Read_Pin (Data, Cursor, 1).Value = 5
                          and then P.Read_Pin (Data, Cursor, 2).Available and then P.Read_Pin (Data, Cursor, 2).Value = 6
                          and then P.Read_Pin (Data, Cursor, 3).Available and then P.Read_Pin (Data, Cursor, 3).Value = 7,
                          "normal group pins");
                     when 4 => Check (Pin.Information.Group_Function_ID = 16#1234#, "normal group function fields");
                     when 5 => Check (Pin.Information.Configuration = 128 and then Pin.Information.Value = 16#FFFF_FFFF#,
                          "normal group vendor configuration fields");
                     when others => raise Program_Error with "unexpected pin ordinal";
                  end case;
                  Check (not P.Read_Pin (Data, Cursor, Pin.Count + 1).Available, "normal pin index bounds");
                  if Ordinal >= 3 then
                     Check (Data (Data'First + Natural (Pin.Label.First) .. Data'First + Natural (Pin.Label.First) + 3)
                       = [71,82,80,0], "normal group label bytes");
                  end if;
               end;
            elsif Index=17 then
               declare Serial : constant S.Decode_Result := S.Decode (Data, Cursor); begin
                  Check (Serial.Status = S.Decoded and then Serial.Next_Offset = Item.Next_Offset,
                    "normal serial connection frame");
                  Check (Serial.Revision = (if Ordinal in 2 | 4 | 6 then 2 else 1)
                    and then Serial.Shared = (Ordinal in 2 | 4 | 6)
                    and then Serial.Role = (if Ordinal in 2 | 4 | 6 then S.Produces_And_Consumes else S.Consumes_Resource)
                    and then Serial.Initiated_By = (if Ordinal in 2 | 4 | 7 then S.Device_Initiated else S.Controller_Initiated),
                    "normal serial revision and general flags");
                  Check (Serial.Source_Index = (case Ordinal is when 1 => 3, when 2 => 2, when 7 => 7, when others => 0)
                    and then Serial.Source.Length = 10
                    and then Serial.Vendor.Length = (if Ordinal in 2 | 4 | 6 then 1 else 2)
                    and then Serial.Vendor.First + Serial.Vendor.Length = Serial.Source.First
                    and then Serial.Source.First + Serial.Source.Length = Serial.Next_Offset,
                    "normal serial source and vendor partitions");
                  case Ordinal is
                     when 1 | 2 =>
                        Check (Serial.Information.Kind = S.I2C_Bus
                          and then Serial.Information.Addressing = (if Ordinal = 1 then S.Seven_Bit else S.Ten_Bit)
                          and then Serial.Information.Address = (if Ordinal = 1 then 16#5A# else 16#2FF#)
                          and then Serial.Information.Connection_Speed = (if Ordinal = 1 then 400_000 else 1_000_000)
                          and then Serial.Information.Legacy_Virtual_Register = 0, "normal I2C settings");
                     when 3 | 4 =>
                        Check (Serial.Information.Kind = S.SPI_Bus
                          and then Serial.Information.Selection = (if Ordinal = 3 then 16#BEEF# else 1)
                          and then Serial.Information.Wires = (if Ordinal = 3 then S.Three_Wire else S.Four_Wire)
                          and then Serial.Information.Selection_Polarity = (if Ordinal = 3 then S.Active_High else S.Active_Low)
                          and then Serial.Information.Transfer_Unit = (if Ordinal = 3 then 16 else 8)
                          and then Serial.Information.Phase = (if Ordinal = 3 then S.Second_Phase else S.First_Phase)
                          and then Serial.Information.Clock = (if Ordinal = 3 then S.Starts_High else S.Starts_Low)
                          and then Serial.Information.Maximum_Speed = (if Ordinal = 3 then 10_000_000 else 25_000_000),
                          "normal SPI settings");
                     when 5 | 6 =>
                        Check (Serial.Information.Kind = S.UART_Bus
                          and then Serial.Information.Baud_Rate = (if Ordinal = 5 then 115_200 else 921_600)
                          and then Serial.Information.Bits = (if Ordinal = 5 then S.Eight_Bits else S.Nine_Bits)
                          and then Serial.Information.Stops = (if Ordinal = 5 then S.One_Stop_Bit else S.Two_Stop_Bits)
                          and then Serial.Information.Order = (if Ordinal = 5 then S.Big_Endian else S.Little_Endian)
                          and then Serial.Information.Flow = (if Ordinal = 5 then S.Hardware_Flow_Control else S.XON_XOFF)
                          and then Serial.Information.Parity_Mode = (if Ordinal = 5 then S.Odd_Parity else S.Even_Parity)
                          and then Serial.Information.Lines = (if Ordinal = 5 then 16#FC# else 16#80#)
                          and then Serial.Information.Receive_FIFO = (if Ordinal = 5 then 64 else 16#1234#)
                          and then Serial.Information.Transmit_FIFO = (if Ordinal = 5 then 128 else 16#FEDC#),
                          "normal UART settings");
                     when 7 =>
                        Check (Serial.Information.Kind = S.CSI2_Bus and then Serial.Information.PHY = S.D_PHY
                          and then Serial.Information.Port = 17, "normal CSI2 settings");
                     when others => raise Program_Error with "unexpected serial ordinal";
                  end case;
               end;
            elsif Index=16 then
               declare R : constant GPIO.Decode_Result := GPIO.Decode(Data,Cursor); begin
                  Check(R.Status=GPIO.Decoded and then R.Next_Offset=Item.Next_Offset,"normal GPIO frame");
                  Check(R.Role=(if Ordinal=3 then GPIO.Produces_And_Consumes else GPIO.Consumes_Resource)
                    and then R.Shared=(Ordinal/=3),"normal GPIO role and sharing");
                  Check(R.Source.First=R.Pins.First+R.Pins.Length
                    and then R.Vendor.First=R.Source.First+R.Source.Length
                    and then R.Vendor.First+R.Vendor.Length=R.Next_Offset,"normal GPIO absolute spans");
                  case Ordinal is
                     when 1 =>
                        Check(R.Information.Kind=GPIO.Interrupt_Connection
                          and then R.Information.Mode=GPIO.Edge_Triggered
                          and then R.Information.Polarity=GPIO.Active_Both and then R.Information.Wake_Capable
                          and then R.Configuration=1 and then R.Drive_Strength=0 and then R.Debounce_Timeout=1234
                          and then R.Count=1 and then R.Source.Length=10 and then R.Vendor.Length=2,"normal GpioInt fields");
                        Check(GPIO.Read_Pin(Data,Cursor,1).Available
                          and then GPIO.Read_Pin(Data,Cursor,1).Value=16#1234#,"normal GpioInt pin");
                     when 2 =>
                        Check(R.Information.Kind=GPIO.IO_Connection and then R.Information.Restriction=GPIO.Output_Only
                          and then R.Configuration=2 and then R.Drive_Strength=5678 and then R.Debounce_Timeout=4321
                          and then R.Count=3 and then R.Source.Length=5 and then R.Vendor.Length=0,"normal GpioIo fields");
                        Check(GPIO.Read_Pin(Data,Cursor,1).Available and then GPIO.Read_Pin(Data,Cursor,1).Value=0
                          and then GPIO.Read_Pin(Data,Cursor,2).Available and then GPIO.Read_Pin(Data,Cursor,2).Value=16#1234#
                          and then GPIO.Read_Pin(Data,Cursor,3).Available and then GPIO.Read_Pin(Data,Cursor,3).Value=GPIO.No_Pin,
                          "normal pin table preserves No_Pin");
                     when 3 =>
                        Check(R.Information.Kind=GPIO.IO_Connection and then R.Information.Restriction=GPIO.Unrestricted_Preserve
                          and then R.Configuration=128 and then R.Drive_Strength=65535 and then R.Debounce_Timeout=0
                          and then R.Count=2 and then R.Source.Length=6 and then R.Vendor.Length=2,"normal producer/vendor GPIO");
                        Check(GPIO.Read_Pin(Data,Cursor,1).Available and then GPIO.Read_Pin(Data,Cursor,1).Value=16#ABCD#
                          and then GPIO.Read_Pin(Data,Cursor,2).Available and then GPIO.Read_Pin(Data,Cursor,2).Value=16#ABCE#,
                          "normal two pin list");
                     when others => raise Program_Error with "unexpected GPIO ordinal";
                  end case;
                  Check(not GPIO.Read_Pin(Data,Cursor,R.Count+1).Available,"normal GPIO pin limit");
               end;
            elsif Index=15 then
               declare Ext : constant EA.Decode_Result:=EA.Decode(Data,Cursor); begin
                  Check(Ext.Status=EA.Decoded and then Ext.Next_Offset=Item.Next_Offset,"normal extended address frame");
                  Check(Ext.Role=(if Ordinal=2 then EA.Produces_And_Consumes else EA.Consumes_Resource)
                    and then Ext.Mode=A.Positive_Decode and then Ext.Minimum_Fixed and then Ext.Maximum_Fixed
                    and then Ext.Granularity=0 and then Ext.Translation=0,"extended role and geometry flags");
                  case Ordinal is
                     when 1 => Check(Ext.Information.Kind=EA.Memory_Range and then Ext.Minimum=16#200000000#
                       and then Ext.Maximum=16#2000FFFFF# and then Ext.Length=16#100000# and then Ext.Attributes=8
                       and then Ext.Information.Declared_Writeable and then Ext.Information.Ignored_Cache_Bits=1,
                       "extended memory attributes separate from ignored cache bits");
                     when 2 => Check(Ext.Information.Kind=EA.IO_Range and then Ext.Minimum=16#5000#
                       and then Ext.Maximum=16#5FFF# and then Ext.Length=16#1000# and then Ext.Attributes=0,"extended IO");
                     when others => Check(Ext.Information.Kind=EA.Vendor_Range and then Ext.Information.Vendor_Type=192
                       and then Ext.Information.Raw=16#A5# and then Ext.Minimum=16#6000#
                       and then Ext.Maximum=16#6FFF# and then Ext.Length=16#1000# and then Ext.Attributes=0,"extended vendor");
                  end case;
                  declare Shape : constant G.Geometry_Result:=G.Check(EA.Geometry_Of(Ext),G.Assigned_Use); begin
                     Check(Shape.Status=G.Feasible and then Shape.First=Ext.Minimum
                       and then Shape.Last=Ext.Maximum,"extended geometry adapter");
                  end;
               end;
            elsif Index=14 then
               if Ordinal in 3 | 6 then
                  Check(R.Status=B.Decoded and then R.Value.Kind=B.Fixed_IO_Resource
                    and then R.Value.Fixed_IO.Base=(if Ordinal=3 then 16#3F8# else 16#2F8#)
                    and then R.Value.Fixed_IO.Length=8,"alternative fixed IO fields");
               elsif Ordinal in 4 | 7 then
                  Check(R.Status=B.Decoded and then R.Value.Kind=B.IRQ_Resource
                    and then R.Value.IRQ.Mask=(if Ordinal=4 then 16#20# else 16#200#),"alternative IRQ fields");
               else Check(R.Status=B.Unsupported_Descriptor,"markers/vendors remain outside basic item decoder"); end if;
            elsif Index=13 then
               declare DMA : constant D.Decode_Result:=D.Decode(Data,Cursor); begin
                  Check(DMA.Status=D.Decoded and then DMA.Next_Offset=Item.Next_Offset,"normal DMA frame");
                  case Ordinal is
                     when 1 => Check(DMA.Value.Kind=D.Legacy_DMA and then DMA.Value.Mask=4
                       and then DMA.Value.Speed=D.Compatibility and then not DMA.Value.Bus_Master_Claim
                       and then DMA.Value.Preference=D.Only_8,"normal compatibility DMA");
                     when 2 => Check(DMA.Value.Kind=D.Legacy_DMA and then DMA.Value.Mask=16#8A#
                       and then DMA.Value.Speed=D.Type_F and then DMA.Value.Bus_Master_Claim
                       and then DMA.Value.Preference=D.Either_8_16,"normal type F DMA");
                     when others =>
                        Check(DMA.Value.Kind=D.Fixed_DMA and then DMA.Value.Request=16#1234#
                          and then DMA.Value.Channel=16#ABCD# and then DMA.Value.Width=D.Width_256,"normal fixed DMA");
                        Check(D.Decode(Data,Cursor,D.Possible_Resources).Status=D.Wrong_Use
                          and then D.Decode(Data,Cursor,D.Set_Resources).Status=D.Wrong_Use,"fixed DMA not PRS or SRS");
                  end case;
               end;
            elsif Index in 10 .. 11 then
               declare Mem : constant M.Decode_Result:=M.Decode(Data,Cursor); begin
                  Check(Mem.Status=M.Decoded and then Mem.Next_Offset=Item.Next_Offset,"normal legacy memory frame");
                  Check(Mem.Declared_Writeable=(Ordinal=1),"normal memory write claim");
                  if Index=10 then
                     Check(Mem.Encoding=M.Memory_24 and then Mem.Minimum_Base=(if Ordinal=1 then 16#100000# else 16#300000#)
                       and then Mem.Maximum_Base=(if Ordinal=1 then 16#200000# else 16#300000#)
                       and then Mem.Length=(if Ordinal=1 then 16#10000# else 16#100#)
                       and then Mem.Encoded_Alignment=(if Ordinal=1 then 16#1000# else 0)
                       and then Mem.Alignment=(if Ordinal=1 then 16#1000# else 65_536),"Memory24 scaled bases/length and zero alignment");
                  else
                     Check(Mem.Encoding=M.Memory_32 and then Mem.Minimum_Base=(if Ordinal=1 then 16#80000000# else 0)
                       and then Mem.Maximum_Base=(if Ordinal=1 then 16#90000000# else 16#FFF#)
                       and then Mem.Length=(if Ordinal=1 then 16#100000# else 16#1000#)
                       and then Mem.Alignment=(if Ordinal=1 then 16#1000# else 0),"Memory32 full fields and raw alignment");
                  end if;
               end;
            elsif Index=12 then
               declare Reg : constant GR.Decode_Result:=GR.Decode(Data,Cursor); begin
                  Check(Reg.Status=GR.Decoded and then Reg.Next_Offset=Item.Next_Offset,"normal generic register frame");
                  case Ordinal is
                     when 1 => Check(Reg.Value.Space=0 and then Reg.Value.Width=16 and then Reg.Value.Bit_Offset=8
                       and then Reg.Value.Access_Size=3 and then Reg.Value.Address=16#123456789ABCDEF0#,"normal memory GAS");
                     when 2 => Check(Reg.Value.Space=1 and then Reg.Value.Width=8 and then Reg.Value.Bit_Offset=0
                       and then Reg.Value.Access_Size=1 and then Reg.Value.Address=16#3F8#,"normal IO GAS");
                     when others => Check(Reg.Value.Space=127 and then Reg.Value.Width=64 and then Reg.Value.Bit_Offset=0
                       and then Reg.Value.Access_Size=0 and then Reg.Value.Address=0,"FFH zero address is retained metadata");
                  end case;
               end;
            elsif Index>=8 then
               declare Addr : constant A.Decode_Result:=A.Decode(Data,Cursor,A.Require_Zero); begin
                  Check(Addr.Status=A.Decoded and then Addr.Next_Offset=Item.Next_Offset,"normal address frame");
                  Check(Addr.Width=(case Ordinal is when 1=>A.Word_Width,when 2=>A.DWord_Width,when others=>A.QWord_Width)
                    and then Addr.Mode=A.Positive_Decode and then Addr.Minimum_Fixed and then Addr.Maximum_Fixed
                    and then Addr.Granularity=0,"normal widths and general flags");
                  declare Shape : constant G.Geometry_Result:=G.Assess(Addr,G.Assigned_Use); begin
                     Check(Shape.Status=G.Feasible and then Shape.First=Addr.Minimum
                       and then Shape.Last=Addr.Maximum,"normal assigned address geometry");
                  end;
                  if Index=8 then
                     Check(not Addr.Source.Present,"no address source");
                     case Ordinal is
                        when 1 => Check(Addr.Information.Kind=A.Bus_Range and then Addr.Minimum=0
                          and then Addr.Maximum=255 and then Addr.Translation=0 and then Addr.Length=256,"Word bus range");
                        when 2 => Check(Addr.Information.Kind=A.Memory_Range and then Addr.Minimum=16#80000000#
                          and then Addr.Maximum=16#8FFFFFFF# and then Addr.Translation=0 and then Addr.Length=16#10000000#
                          and then Addr.Information.Declared_Writeable and then Addr.Information.Cache=A.Non_Cacheable,"DWord memory range");
                        when others => Check(Addr.Information.Kind=A.Memory_Range and then Addr.Minimum=16#100000000#
                          and then Addr.Maximum=16#1FFFFFFFF# and then Addr.Translation=0 and then Addr.Length=16#100000000#
                          and then Addr.Information.Cache=A.Prefetchable,"QWord memory above 4GiB");
                     end case;
                  else
                     Check(Addr.Information.Kind=A.IO_Range and then Addr.Source.Present
                       and then Addr.Source.Index=(case Ordinal is when 1=>7,when 2=>3,when others=>0)
                       and then Addr.Source.Name_Length=9,"width-specific source index");
                     Check(Addr.Minimum=(case Ordinal is when 1=>16#1000#,when 2=>16#10000#,when others=>16#100000000#)
                       and then Addr.Maximum=(case Ordinal is when 1=>16#1FFF#,when 2=>16#1FFFF#,when others=>16#100000FFF#)
                       and then Addr.Translation=(if Ordinal=2 then 16#100000# else 0)
                       and then Addr.Length=(if Ordinal=2 then 16#10000# else 16#1000#),"IO address fields");
                     declare Expected_Name : constant String:="\_SB.PCI0"; begin
                        for J in Expected_Name'Range loop
                           Check(Data(Data'First+Natural(Addr.Source.Name_Offset)+J-Expected_Name'First)=
                             Character'Pos(Expected_Name(J)),"address source copy");
                        end loop;
                     end;
                  end if;
               end;
            elsif Index>=5 then
               declare Ext : constant E.Decode_Result:=E.Decode(Data,Cursor,
                 (if Index=6 then E.Possible_Resources else E.Current_Resources)); begin
                  Check(Ext.Status=E.Decoded and then Ext.Next_Offset=Item.Next_Offset,"normal extended interrupt");
                  if Index=5 then
                     Check(Ext.Count=1 and then Ext.Interrupts(1)=16#10203040#
                       and then Ext.Role=E.Consumer and then Ext.Mode=E.Level_Triggered
                       and then Ext.Polarity=E.Active_Low and then Ext.Shared and then Ext.Wake_Capable
                       and then not Ext.Source.Present,"extended consumer flags and number");
                  elsif Index=6 then
                     Check(Ext.Count=3 and then Ext.Interrupts(1)=16 and then Ext.Interrupts(2)=255
                       and then Ext.Interrupts(3)=16#FFFFFFFF# and then Ext.Role=E.Producer
                       and then Ext.Mode=E.Edge_Triggered and then Ext.Polarity=E.Active_Low
                       and then not Ext.Shared and then not Ext.Wake_Capable,"_PRS alternative list");
                     Check(E.Decode(Data,Cursor).Status=E.Wrong_Usage_Count,"_PRS list not treated as current allocation");
                  else
                     Check(Ext.Count=1 and then Ext.Interrupts(1)=33 and then Ext.Source.Present
                       and then Ext.Source.Index=2 and then Ext.Source.Name_Length=9,"resource source metadata");
                     declare Expected_Name : constant String:="\_SB.PCI0"; begin
                        for J in Expected_Name'Range loop
                           Check(Data(Data'First+Natural(Ext.Source.Name_Offset)+J-Expected_Name'First)=
                             Character'Pos(Expected_Name(J)),"resource source copied bytes");
                        end loop;
                     end;
                     Check(E.Decode(Data,Cursor,E.Current_Resources,E.Require_Zero).Status=E.Bad_Source_Index,
                       "negotiation policy checked independently");
                  end if;
               end;
            elsif Index in 1 | 4 or else (Index=3 and then Ordinal=2) then
               Check(R.Status=B.Decoded and then R.Next_Offset=Item.Next_Offset,"typed service resource");
               if Index=1 then
                  case Ordinal is
                     when 1 => Check(R.Value.Kind=B.Fixed_IO_Resource and then R.Value.Fixed_IO.Base=16#3F8#
                       and then R.Value.Fixed_IO.Length=8,"normal FixedIO fields");
                     when 2 => Check(R.Value.Kind=B.IRQ_Resource and then R.Value.IRQ.Mask=16#20#
                       and then R.Value.IRQ.Mode=B.Edge_Triggered and then R.Value.IRQ.Polarity=B.Active_High
                       and then not R.Value.IRQ.Shared and then not R.Value.IRQ.Wake_Capable
                       and then not R.Value.IRQ.Information_Present,"normal IRQNoFlags defaults");
                     when 3 => Check(R.Value.Kind=B.Fixed_Memory_32_Resource and then R.Value.Memory.Base=16#FEC00000#
                       and then R.Value.Memory.Length=16#1000# and then R.Value.Memory.Declared_Writeable,"normal fixed memory fields");
                     when others => raise Program_Error;
                  end case;
               elsif Index=3 then
                  Check(R.Value.Kind=B.Fixed_IO_Resource and then R.Value.Fixed_IO.Base=16#2F8#,"dependent-group IO metadata");
               else
                  case Ordinal is
                     when 1 => Check(R.Value.Kind=B.IO_Resource and then R.Value.IO.Decoding=B.Decode_16
                       and then R.Value.IO.Minimum=16#1200# and then R.Value.IO.Maximum=16#FE00#
                       and then R.Value.IO.Alignment=16#10# and then R.Value.IO.Length=8,"normal variable IO");
                     when 2 => Check(R.Value.Kind=B.IRQ_Resource and then R.Value.IRQ.Mask=16#20#
                       and then R.Value.IRQ.Mode=B.Edge_Triggered and then R.Value.IRQ.Polarity=B.Active_High
                       and then R.Value.IRQ.Shared and then R.Value.IRQ.Wake_Capable
                       and then R.Value.IRQ.Information_Present,"normal edge IRQ information");
                     when 3 => Check(R.Value.Kind=B.IRQ_Resource and then R.Value.IRQ.Mask=16#200#
                       and then R.Value.IRQ.Mode=B.Level_Triggered and then R.Value.IRQ.Polarity=B.Active_Low
                       and then R.Value.IRQ.Shared and then R.Value.IRQ.Wake_Capable,"normal level IRQ information");
                     when others => raise Program_Error;
                  end case;
               end if;
            else Check(R.Status=B.Unsupported_Descriptor,"other families stay explicitly unsupported");
            end if;
            Cursor:=Item.Next_Offset;
         end;
      end loop;
   end Typed;
   procedure Reader_Failures is
      Device, Child : Namespace.Node_ID;
      Status : Values.Access_Status;
      Item : Node_Descriptor;
      Value : Values.Result;
      Handle, Saved : Values.Value_Handle := Values.No_Value;
      Data : Firmware_Tables.Bytes (1 .. 64) := [others => 16#AA#];
      Report : Reader.Report;
      Before : constant Namespace.Owned.Retained_Root_Count := Retained_Results (Service);
      type Names is array (Positive range <>) of Firmware_Tables.Signature;
      Bad : constant Names := ["BCHK", "BEND", "BTRL", "BSHT", "BFRM", "BSTR", "BPKG"];
      type Causes is array (Positive range <>) of Template_Status;
      Expected : constant Causes := [Bad_Checksum, Missing_End_Tag, Trailing_Data, Missing_End_Tag, Malformed_Frame];
      procedure Select_Item (Name : Firmware_Tables.Signature) is
      begin
         Find_Child (Service, Device, Name, Child, Status);
         Check (Status = Values.Available and then Child /= Namespace.Root, "reader fixture child");
         Describe_Node (Service, Child, Item, Status);
         Check (Status = Values.Available, "reader fixture descriptor");
      end Select_Item;
      procedure Counter_One is
      begin
         Select_Item ("CNT0");
         Evaluate_Named (Service, Item, 1_000, Value, Status);
         Check (Status = Values.Available and then Value.Status = AML_Execute.Returned and then Value.Number = 1,
           "capacity retry must not evaluate AML again");
      end Counter_One;
   begin
      Reader.Copy_Template (Service, Values.No_Value, Data, Report);
      Check (Report.Status = Reader.Invalid_Result and then Report.Copied = 0
        and then (for all B of Data => B = 0), "invalid handle output atomicity");
      Find_Child (Service, Namespace.Root, "RCQ0", Device, Status);
      Check (Status = Values.Available and then Device /= Namespace.Root, "reader fixture device");
      Select_Item ("_CRS");
      Evaluate_Named (Service, Item, 1_000, Value, Status);
      Check (Status = Values.Available and then Value.Status = AML_Execute.Object_Returned, "one _CRS evaluation");
      Handle := Value.Handle;
      declare Small : Firmware_Tables.Bytes (1 .. 1) := [others => 16#AA#]; begin
         Reader.Copy_Template (Service, Handle, Small, Report);
         Check (Report.Status = Reader.Insufficient_Capacity and then Report.Required = 6
           and then Report.Copied = 0 and then not Report.Template_Checked and then Small (1) = 0,
           "size diagnostic without truncation");
      end;
      Counter_One;
      Reader.Copy_Template (Service, Handle, Data, Report);
      Check (Report.Status = Reader.Copied_Template and then Report.Required = 6 and then Report.Copied = 6
        and then Data (1 .. 6) = [16#4B#,16#F8#,3,8,16#79#,0]
        and then (for all I in 7 .. Data'Last => Data (I) = 0), "retry same retained result");
      Counter_One;
      Check (Retained_Results (Service) = Before + 1, "reader neither releases nor adds roots");
      declare Other : State (1, Resource_Oracle.Raw_Table'Length, Resource_Oracle.Raw_Table'Length, Load_Capacity => 1); begin
         Reader.Copy_Template (Other, Handle, Data, Report);
         Check (Report.Status = Reader.Invalid_Result and then Report.Copied = 0
           and then (for all B of Data => B = 0), "wrong service handle rejected");
      end;
      Reader.Copy_Template (Service, Handle, Data, Report, 0);
      Check (Report.Status = Reader.Invalid_Template and then Report.Template_Checked
        and then Report.Framing.Status = Item_Limit and then Report.Copied = 0
        and then (for all B of Data => B = 0), "quota failure publishes no bytes");
      Saved := Handle;
      Release_Result (Service, Handle, Status);
      Check (Status = Values.Available and then Retained_Results (Service) = Before, "caller releases borrowed result");
      Reader.Copy_Template (Service, Saved, Data, Report);
      Check (Report.Status = Reader.Invalid_Result and then (for all B of Data => B = 0), "stale handle rejected");
      for I in Bad'Range loop
         Select_Item (Bad (I));
         Evaluate_Named (Service, Item, 1_000, Value, Status);
         Check (Status = Values.Available and then Value.Status = AML_Execute.Object_Returned, "bad fixture retained");
         Handle := Value.Handle;
         Reader.Copy_Template (Service, Handle, Data, Report);
         Check (Report.Status = (if I <= Expected'Length then Reader.Invalid_Template else Reader.Not_A_Buffer)
           and then Report.Copied = 0 and then (for all B of Data => B = 0), "bad type or framing output atomicity");
         if I <= Expected'Length then
            Check (Report.Template_Checked and then Report.Framing.Status = Expected (I), "exact framing diagnostic");
         else Check (not Report.Template_Checked, "wrong type not scanned"); end if;
         Check (Retained_Results (Service) = Before + 1, "error leaves caller's retained root");
         Release_Result (Service, Handle, Status);
         Check (Status = Values.Available and then Retained_Results (Service) = Before, "error caller cleanup");
      end loop;
   end Reader_Failures;
begin
   Stage_Table(Service,1,DSDT,Resource_Oracle.Raw_Table,Staged_Result);
   Check(Staged_Result=Staged,"stage exact normal iASL table");
   Freeze_Catalog(Service,Frozen); Check(Frozen=Catalog_Frozen_OK,"freeze");
   Execute_Frozen(Service,100_000,Execution,Access_Result);
   Check(Access_Result=Values.Available and then Execution.Phase=Module_Execution_Reported,"real module execution");
   Initialize_Members(Service,Members,Access_Result);
   Check(Access_Result=Values.Available,"initialize members");
   for Index in Resource_Oracle.Case_Index loop
      declare
         Device, Resource : Namespace.Node_ID;
         Name : String(1 .. 4):="DVC0";
         Hex_Digits : constant String:="0123456789ABCDEF";
         Item : Node_Descriptor;
         Result : Values.Result;
         Description : Values.Value_Description;
         Expected : constant Firmware_Tables.Bytes:=Resource_Oracle.Template(Index);
         Data : Firmware_Tables.Bytes(1 .. Expected'Length);
         Copied : Natural;
         Handle : Values.Value_Handle:=Values.No_Value;
         Before : constant Namespace.Owned.Retained_Root_Count:=Retained_Results(Service);
      begin
         if Index=19 then Name:="CLK0";
         elsif Index=18 then Name:="PIN0";
         elsif Index=17 then Name:="SBL0";
         elsif Index=16 then Name:="GPI0"; else Name(4):=Hex_Digits(Index+1); end if;
         Find_Child(Service,Namespace.Root,Name,Device,Access_Result);
         Check(Access_Result=Values.Available and then Device/=Namespace.Root,"device lookup");
         Find_Child(Service,Device,(if Index in 6 | 14 then "_PRS" else "_CRS"),Resource,Access_Result);
         Check(Access_Result=Values.Available and then Resource/=Namespace.Root,"_CRS lookup");
         Describe_Node(Service,Resource,Item,Access_Result); Check(Access_Result=Values.Available,"copied descriptor");
         Evaluate_Named(Service,Item,1_000,Result,Access_Result);
         Check(Access_Result=Values.Available and then Result.Status=AML_Execute.Object_Returned,"retained resource buffer");
         Handle:=Result.Handle;
         Describe_Result(Service,Handle,Description,Access_Result);
         Check(Access_Result=Values.Available and then Description.Kind=Values.Buffer_Description
           and then Description.Length=Expected'Length,"resource buffer kind/extent");
         Read_Result_Bytes(Service,Handle,0,Data,Copied,Access_Result);
         Check(Access_Result=Values.Available and then Copied=Expected'Length,"bounded owned copy");
         Check(Data(1 .. Copied)=Expected,"all bytes match normal ACPICA observations");
         declare
            Wide : Firmware_Tables.Bytes (Positive'Last - Expected'Length - 2 .. Positive'Last) := [others => 16#AA#];
            Report : Reader.Report;
         begin
            Reader.Copy_Template (Service, Handle, Wide, Report);
            Check (Report.Status = Reader.Copied_Template and then Report.Copied = Byte_Count (Expected'Length)
              and then Wide (Wide'First .. Wide'First + (Expected'Length - 1)) = Expected
              and then (for all I in Wide'Last - 2 .. Wide'Last => Wide (I) = 0), "reader exact bytes and unused suffix at high bounds");
            Check (Report.Template_Checked and then Report.Framing.Items = Item_Count (Resource_Oracle.Items (Index))
              and then Retained_Results (Service) = Before + 1, "reader preserves borrowed result");
         end;
         declare R : constant Scan_Result:=Scan(Data(1 .. Copied)); begin
            Check(R.Status=Complete and then R.Items=Item_Count(Resource_Oracle.Items(Index))
              and then R.Position=Byte_Count(Copied),"_CRS framing from actual service");
         end;
         Typed(Data(1 .. Copied),Index);
         Release_Result(Service,Handle,Access_Result);
         Check(Access_Result=Values.Available and then Handle=Values.No_Value
           and then Retained_Results(Service)=Before,"retained cleanup");
      end;
   end loop;
   Reader_Failures;
   Ada.Text_IO.Put_Line("Resource service checks" & Checks'Image);
end Resource_Service_Tests;
