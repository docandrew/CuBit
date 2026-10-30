pragma Ada_2022;
package body ACPI_Passive_Query.Resource_Records with SPARK_Mode is
   use type U8;
   use type U16;
   Empty_Node : constant Node_Ref := (0, 0, 0);
   Empty_Path : constant Byte_Span := (0, 0);
   Memory_24_Block_Bytes : constant U32 := 256;
   Zero_Alignment_24 : constant U32 := 65_536;
   Maximum_Memory_24_Value : constant U32 := U32 (U16'Last) * Memory_24_Block_Bytes;
   function Valid_Controller (Item : Controller_Target; Limit : Node_Limit;
                              Payload_Bytes : U32) return Boolean is
     (case Item.Kind is
       when No_Controller => Item.Node = Empty_Node and then Item.Path = Empty_Path,
       when Resolved_Node => Item.Node.Instance /= 0 and then Item.Node.Revision /= 0
         and then Item.Node.Index in 1 .. Limit and then Item.Path = Empty_Path,
       when Unresolved_Path => Item.Node = Empty_Node and then Item.Path.Length > 0
         and then Item.Path.Offset <= Payload_Bytes
         and then Item.Path.Length <= Payload_Bytes - Item.Path.Offset);
   function Valid (Item : Resource_Record; Limit : Node_Limit; Payload_Bytes : U32)
     return Boolean is
   begin
      if not Valid_Controller (Item.Controller, Limit, Payload_Bytes)
        or else Item.Name > (if Item.Encoding = Small_Item then 15 else 127)
        or else Item.Total_Subentries = 0 or else Item.Subentry >= Item.Total_Subentries
      then return False; end if;
      if Item.Kind not in Interrupt_Entry | GPIO_Pin and then
        (Item.Subentry /= 0 or else Item.Total_Subentries /= 1) then return False; end if;
      if Item.Kind not in Address_Window | Interrupt_Entry | GPIO_Pin | I2C_Connection
        and then (Item.Controller.Kind /= No_Controller
                  or else Item.ResourceSource_Index /= 0 or else Item.ResourceSource_Offset /= 0)
      then return False; end if;
      if Item.Kind not in Interrupt_Entry | GPIO_Pin | I2C_Connection and then Item.Shared
      then return False; end if;
      case Item.Kind is
         when Fixed_Memory =>
            return Item.Encoding = Large_Item and then Item.Name = 6
              and then Item.Fixed.Writeable = ((Item.Fixed.Information and 1) /= 0);
         when Memory_Range =>
            return Item.Encoding = Large_Item
              and then Item.Name = (if Item.Memory.Encoding = Memory_24 then 1 else 5)
              and then Item.Memory.Writeable = ((Item.Memory.Information and 1) /= 0)
              and then Item.Memory.Alignment =
                (if Item.Memory.Encoding = Memory_24 and then Item.Memory.Encoded_Alignment = 0
                 then Zero_Alignment_24 else Item.Memory.Encoded_Alignment)
              and then (if Item.Memory.Encoding = Memory_24 then
                Item.Memory.Minimum_Base <= Maximum_Memory_24_Value
                and then Item.Memory.Maximum_Base <= Maximum_Memory_24_Value
                and then Item.Memory.Length <= Maximum_Memory_24_Value
                and then Item.Memory.Minimum_Base mod Memory_24_Block_Bytes = 0
                and then Item.Memory.Maximum_Base mod Memory_24_Block_Bytes = 0
                and then Item.Memory.Length mod Memory_24_Block_Bytes = 0
                and then Item.Memory.Encoded_Alignment <= U32 (U16'Last));
         when Address_Window =>
            return Item.Encoding = Large_Item and then Item.Name =
              (case Item.Window.Encoding is when Word_Address => 8, when DWord_Address => 7,
                when QWord_Address => 10, when Extended_Address => 11)
              and then (if Item.Window.Encoding = Word_Address then
                Item.Window.Granularity <= Word (U16'Last) and then
                Item.Window.Minimum <= Word (U16'Last) and then
                Item.Window.Maximum <= Word (U16'Last) and then
                Item.Window.Translation_Offset <= Word (U16'Last) and then
                Item.Window.Length <= Word (U16'Last)
              elsif Item.Window.Encoding = DWord_Address then
                Item.Window.Granularity <= Word (U32'Last) and then
                Item.Window.Minimum <= Word (U32'Last) and then
                Item.Window.Maximum <= Word (U32'Last) and then
                Item.Window.Translation_Offset <= Word (U32'Last) and then
                Item.Window.Length <= Word (U32'Last))
              and then (if Item.Window.Encoding /= Extended_Address then
                 Item.Window.Revision = 0 and then Item.Window.Attributes = 0
                else Item.Window.Revision = 1
                  and then (if Item.Window.Resource_Type /= 0 then Item.Window.Attributes = 0)
                  and then Item.Controller.Kind = No_Controller
                  and then Item.ResourceSource_Index = 0 and then Item.ResourceSource_Offset = 0);
         when IO_Range =>
            return Item.Encoding = Small_Item and then Item.Name = (if Item.IO.Fixed then 9 else 8)
              and then (if Item.IO.Fixed then Item.IO.Minimum = Item.IO.Maximum
                and then Item.IO.Width = Decode_10 and then Item.IO.Alignment = 0);
         when Interrupt_Entry =>
            return Item.IRQ.Active /= Active_Both and then
              ((Item.Encoding = Small_Item and then Item.Name = 4 and then Item.IRQ.GSI <= 15
                and then Item.Controller.Kind = No_Controller)
               or else (Item.Encoding = Large_Item and then Item.Name = 9));
         when GPIO_Pin =>
            return Item.Encoding = Large_Item and then Item.Name = 12
              and then (if Item.GPIO.Kind = Interrupt_Connection then
                Item.Total_Subentries = 1 and then Item.GPIO.Restriction = Unrestricted
                else Item.GPIO.Mode = Level and then Item.GPIO.Active = Active_High
                  and then not Item.GPIO.Wake);
         when I2C_Connection =>
            return Item.Encoding = Large_Item and then Item.Name = 14
              and then Item.I2C.Revision in 1 .. 2
              and then Item.I2C.Type_Revision = 1
              and then (if Item.I2C.Revision = 1 then not Item.Shared)
              and then Item.I2C.Slave_Address <= (if Item.I2C.Addressing = Seven_Bit then 127 else 1_023);
         when Unsupported_Descriptor => return True;
      end case;
   end Valid;
   function B (Value : Boolean) return Word is (if Value then 1 else 0);
   function Encode (Item : Resource_Record; Limit : Node_Limit; Payload_Bytes : U32)
     return Record_Words is
      pragma Unreferenced (Limit, Payload_Bytes);
      Data : Record_Words := [others => 0];
   begin
      Data (0) := Record_Magic; Data (1) := Record_Version;
      Data (2) := Resource_Kind'Pos (Item.Kind);
      Data (3) := Word (Item.Source_Offset); Data (4) := Descriptor_Encoding'Pos (Item.Encoding);
      Data (5) := Word (Item.Name); Data (6) := Word (Item.Subentry);
      Data (7) := Word (Item.Total_Subentries);
      Data (8) := Controller_Kind'Pos (Item.Controller.Kind);
      Data (9) := Word (Item.Controller.Node.Instance);
      Data (10) := Word (Item.Controller.Node.Revision);
      Data (11) := Word (Item.Controller.Node.Index);
      Data (12) := Word (Item.Controller.Path.Offset); Data (13) := Word (Item.Controller.Path.Length);
      Data (14) := Word (Item.ResourceSource_Index); Data (15) := Word (Item.ResourceSource_Offset);
      Data (16) := Word (Item.Raw_General_Flags); Data (17) := Word (Item.Raw_Specific_Flags);
      Data (18) := Role'Pos (Item.Resource_Role); Data (19) := B (Item.Shared);
      case Item.Kind is
         when Fixed_Memory =>
            Data (20) := Word (Item.Fixed.Information); Data (21) := B (Item.Fixed.Writeable);
            Data (22) := Word (Item.Fixed.Base); Data (23) := Word (Item.Fixed.Length);
         when Memory_Range =>
            Data (20) := Memory_Encoding'Pos (Item.Memory.Encoding);
            Data (21) := Word (Item.Memory.Information); Data (22) := B (Item.Memory.Writeable);
            Data (23) := Word (Item.Memory.Minimum_Base); Data (24) := Word (Item.Memory.Maximum_Base);
            Data (25) := Word (Item.Memory.Encoded_Alignment); Data (26) := Word (Item.Memory.Alignment);
            Data (27) := Word (Item.Memory.Length);
         when Address_Window =>
            Data (20) := Address_Encoding'Pos (Item.Window.Encoding);
            Data (21) := Word (Item.Window.Resource_Type); Data (22) := Decode_Mode'Pos (Item.Window.Decode);
            Data (23) := B (Item.Window.Minimum_Fixed); Data (24) := B (Item.Window.Maximum_Fixed);
            Data (25) := Word (Item.Window.Type_Flags); Data (26) := Item.Window.Granularity;
            Data (27) := Item.Window.Minimum; Data (28) := Item.Window.Maximum;
            Data (29) := Item.Window.Translation_Offset; Data (30) := Item.Window.Length;
            Data (31) := Word (Item.Window.Revision); Data (32) := Item.Window.Attributes;
         when IO_Range =>
            Data (20) := B (Item.IO.Fixed); Data (21) := IO_Decode_Width'Pos (Item.IO.Width);
            Data (22) := Word (Item.IO.Minimum); Data (23) := Word (Item.IO.Maximum);
            Data (24) := Word (Item.IO.Alignment); Data (25) := Word (Item.IO.Length);
         when Interrupt_Entry =>
            Data (20) := Trigger'Pos (Item.IRQ.Mode); Data (21) := Polarity'Pos (Item.IRQ.Active);
            Data (22) := B (Item.IRQ.Wake); Data (23) := Word (Item.IRQ.GSI);
         when GPIO_Pin =>
            Data (20) := GPIO_Kind'Pos (Item.GPIO.Kind); Data (21) := Word (Item.GPIO.Pin);
            Data (22) := Word (Item.GPIO.Configuration);
            Data (23) := Word (Item.GPIO.Drive_Hundredths_Milliampere);
            Data (24) := Word (Item.GPIO.Debounce_Ten_Microseconds);
            Data (25) := Trigger'Pos (Item.GPIO.Mode); Data (26) := Polarity'Pos (Item.GPIO.Active);
            Data (27) := B (Item.GPIO.Wake); Data (28) := IO_Restriction'Pos (Item.GPIO.Restriction);
         when I2C_Connection =>
            Data (20) := Word (Item.I2C.Revision); Data (21) := Word (Item.I2C.Type_Revision);
            Data (22) := Bus_Initiator'Pos (Item.I2C.Initiator);
            Data (23) := I2C_Address_Mode'Pos (Item.I2C.Addressing);
            Data (24) := Word (Item.I2C.Slave_Address); Data (25) := Word (Item.I2C.Speed_Hertz);
            Data (26) := Word (Item.I2C.Legacy_Virtual_Register);
         when Unsupported_Descriptor => null;
      end case;
      return Data;
   end Encode;
   function Decode (Data : Record_Words; Limit : Node_Limit; Payload_Bytes : U32)
     return Resource_Result is
      Item : Resource_Record;
   begin
      if Data (0) /= Record_Magic or else Data (1) /= Record_Version
        or else Data (2) > Resource_Kind'Pos (Resource_Kind'Last)
        or else Data (3) > Word (U32'Last) or else Data (4) > 1
        or else Data (5) > Word (U8'Last) or else Data (6) > Word (U32'Last)
        or else Data (7) > Word (U32'Last) or else Data (8) > 2
        or else Data (10) > Word (Generation'Last) or else Data (11) > Word (Node_Index'Last)
        or else Data (12) > Word (U32'Last) or else Data (13) > Word (U32'Last)
        or else Data (14) > Word (U8'Last) or else Data (15) > Word (U32'Last)
        or else Data (16) > Word (U16'Last) or else Data (17) > Word (U16'Last)
        or else Data (18) > 1 or else Data (19) > 1
      then return (Accepted => False); end if;
      case Resource_Kind'Val (Natural (Data (2))) is
         when Fixed_Memory =>
            if Data (20) > Word (U8'Last) or else Data (21) > 1
              or else Data (22) > Word (U32'Last) or else Data (23) > Word (U32'Last)
            then return (Accepted => False); end if;
            Item := (Kind => Fixed_Memory, Fixed =>
              (U8 (Data (20)), Data (21) = 1, U32 (Data (22)), U32 (Data (23))), others => <>);
         when Memory_Range =>
            if Data (20) > 1 or else Data (21) > Word (U8'Last) or else Data (22) > 1
              or else (for some I in 23 .. 27 => Data (I) > Word (U32'Last))
            then return (Accepted => False); end if;
            Item := (Kind => Memory_Range, Memory => (Memory_Encoding'Val (Natural (Data (20))),
              U8 (Data (21)), Data (22) = 1, U32 (Data (23)), U32 (Data (24)),
              U32 (Data (25)), U32 (Data (26)), U32 (Data (27))), others => <>);
         when Address_Window =>
            if Data (20) > 3 or else Data (21) > Word (U8'Last) or else Data (22) > 1
              or else Data (23) > 1 or else Data (24) > 1
              or else Data (25) > Word (U8'Last) or else Data (31) > Word (U8'Last)
            then return (Accepted => False); end if;
            Item := (Kind => Address_Window, Window => (Address_Encoding'Val (Natural (Data (20))),
              U8 (Data (21)), Decode_Mode'Val (Natural (Data (22))), Data (23) = 1, Data (24) = 1,
              U8 (Data (25)), Data (26), Data (27), Data (28), Data (29), Data (30),
              U8 (Data (31)), Data (32)), others => <>);
         when IO_Range =>
            if Data (20) > 1 or else Data (21) > 1 or else Data (22) > Word (U16'Last)
              or else Data (23) > Word (U16'Last) or else Data (24) > Word (U8'Last)
              or else Data (25) > Word (U8'Last) then return (Accepted => False); end if;
            Item := (Kind => IO_Range, IO => (Data (20) = 1,
              IO_Decode_Width'Val (Natural (Data (21))), U16 (Data (22)), U16 (Data (23)),
              U8 (Data (24)), U8 (Data (25))), others => <>);
         when Interrupt_Entry =>
            if Data (20) > 1 or else Data (21) > 2 or else Data (22) > 1
              or else Data (23) > Word (U32'Last) then return (Accepted => False); end if;
            Item := (Kind => Interrupt_Entry, IRQ => (Trigger'Val (Natural (Data (20))),
              Polarity'Val (Natural (Data (21))), Data (22) = 1, U32 (Data (23))), others => <>);
         when GPIO_Pin =>
            if Data (20) > 1 or else Data (21) > Word (U16'Last) or else Data (22) > Word (U8'Last)
              or else Data (23) > Word (U16'Last) or else Data (24) > Word (U16'Last)
              or else Data (25) > 1 or else Data (26) > 2 or else Data (27) > 1
              or else Data (28) > 3 then return (Accepted => False); end if;
            Item := (Kind => GPIO_Pin, GPIO => (GPIO_Kind'Val (Natural (Data (20))),
              U16 (Data (21)), U8 (Data (22)), U16 (Data (23)), U16 (Data (24)),
              Trigger'Val (Natural (Data (25))), Polarity'Val (Natural (Data (26))), Data (27) = 1,
              IO_Restriction'Val (Natural (Data (28)))), others => <>);
         when I2C_Connection =>
            if Data (20) > Word (U8'Last) or else Data (21) > Word (U8'Last)
              or else Data (22) > 1 or else Data (23) > 1 or else Data (24) > Word (U16'Last)
              or else Data (25) > Word (U32'Last) or else Data (26) > Word (U8'Last)
            then return (Accepted => False); end if;
            Item := (Kind => I2C_Connection, I2C => (U8 (Data (20)), U8 (Data (21)),
              Bus_Initiator'Val (Natural (Data (22))), I2C_Address_Mode'Val (Natural (Data (23))),
              U16 (Data (24)), U32 (Data (25)), U8 (Data (26))), others => <>);
         when Unsupported_Descriptor => Item := (Kind => Unsupported_Descriptor, others => <>);
      end case;
      Item.Encoding := Descriptor_Encoding'Val (Natural (Data (4))); Item.Name := U8 (Data (5));
      Item.Source_Offset := U32 (Data (3)); Item.Subentry := U32 (Data (6));
      Item.Total_Subentries := U32 (Data (7));
      Item.Controller := (Controller_Kind'Val (Natural (Data (8))),
         (Instance_ID (Data (9)), Generation (Data (10)), Node_Index (Data (11))),
         (U32 (Data (12)), U32 (Data (13))));
      Item.ResourceSource_Index := U8 (Data (14)); Item.ResourceSource_Offset := U32 (Data (15));
      Item.Raw_General_Flags := U16 (Data (16)); Item.Raw_Specific_Flags := U16 (Data (17));
      Item.Resource_Role := Role'Val (Natural (Data (18))); Item.Shared := Data (19) = 1;
      if Valid (Item, Limit, Payload_Bytes) and then Encode (Item, Limit, Payload_Bytes) = Data then
         return (True, Item);
      end if;
      return (Accepted => False);
   end Decode;
end ACPI_Passive_Query.Resource_Records;
