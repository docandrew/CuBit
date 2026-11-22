pragma Ada_2022;
package body HID_Offer_Payload with SPARK_Mode is
   use Interfaces;
   Octet_Bits : constant := 8;
   Octet_Modulus : constant Word := 256;
   subtype Integer_Bytes is Positive range 1 .. 8;
   function Get(Data:Byte_Array;Offset:Natural;Width:Integer_Bytes) return Word is
      Value:Word:=0;
   begin
      for I in 0 .. Width-1 loop
         Value:=Value or Shift_Left(Word(Data(Data'First+Offset+I)),Octet_Bits*I);
      end loop;
      return Value;
   end Get;
   function Valid_PCI(Item:PCI_Observation) return Boolean is
     ((if Item.Has_SEG then Item.SEG<=PCI_Segment_Last else Item.SEG=0)
      and then (if Item.Has_BBN then Item.BBN<=PCI_Bus_Last else Item.BBN=0)
      and then (if Item.Has_ADR then
         Item.ADR/PCI_ADR_Function_Base<=PCI_Device_Last and then
         Item.ADR mod PCI_ADR_Function_Base<=PCI_Function_Last else Item.ADR=0));
   function Location(Item:PCI_Observation) return PCI_Location is
     ((Segment=>Half(Item.SEG),Bus=>Byte(Item.BBN),
       Device=>PCI_Device_Number(Item.ADR/PCI_ADR_Function_Base),
       Func=>PCI_Function_Number(Item.ADR mod PCI_ADR_Function_Base)));
   function Lead(C:Character) return Boolean is(C in 'A'..'Z'|'_');
   function Name_Char(C:Character) return Boolean is(Lead(C) or else C in '0'..'9');
   function Path_Error(Item:Canonical_Path;Offset:Payload_Offset) return Payload_Offset is
   begin
      for I in Path_Index loop
         for J in Name_Segment'Range loop
            if (if I>Item.Count then Item.Names(I)(J)/=NUL
                elsif J=Name_Segment'First then not Lead(Item.Names(I)(J))
                else not Name_Char(Item.Names(I)(J)))
            then return Offset+(I-1)*Name_Segment_Bytes+J-1;end if;
         end loop;
      end loop;
      return 0;
   end Path_Error;
   function Canonical_Span(Item:Owned_Span) return Boolean is
     (for all I in Item.Length+1 .. Max_Span_Bytes=>Item.Data(I)=0);
   function Valid_HID(Item:Text_8) return Boolean is
      Padding:Boolean:=False;
   begin
      if Item(Item'First) not in '!'..'~' then return False;end if;
      for C of Item loop
         if C=NUL then Padding:=True;
         elsif Padding or else C not in '!'..'~' then return False;end if;
      end loop;
      return True;
   end Valid_HID;
   function Bad_Field(Item:Request) return Payload_Offset is
      At_Path:Payload_Offset;
   begin
      if Item.Epoch=0 then return Epoch_Offset;end if;
      if Item.Observation_Sequence=0 then return Observation_Offset;end if;
      if (if Item.PCI.Has_SEG then Item.PCI.SEG>PCI_Segment_Last else Item.PCI.SEG/=0) then return SEG_Offset;end if;
      if (if Item.PCI.Has_BBN then Item.PCI.BBN>PCI_Bus_Last else Item.PCI.BBN/=0) then return BBN_Offset;end if;
      if (if Item.PCI.Has_ADR then Item.PCI.ADR/PCI_ADR_Function_Base>PCI_Device_Last or else
         Item.PCI.ADR mod PCI_ADR_Function_Base>PCI_Function_Last else Item.PCI.ADR/=0) then return ADR_Offset;end if;
      if Item.Maximum_Hz=0 then return Hertz_Offset;end if;
      if Item.Address>Ten_Bit_Last or else (Item.Addressing=Seven_Bit and then Item.Address>Seven_Bit_Last) then return Address_Offset;end if;
      if Item.I2C_Revision not in 1|2 then return I2C_Revision_Offset;end if;
      if Item.I2C_Role/=Consumer then return I2C_Role_Offset;end if;
      if Item.Initiated_By/=Controller_Initiated then return Initiator_Offset;end if;
      if Item.IRQ_Trigger=Level and then Item.IRQ_Polarity=Active_Both then return Polarity_Offset;end if;
      if Item.GPIO_Role/=Consumer then return GPIO_Role_Offset;end if;
      if Item.Device_Path.Count=0 then return Device_Count_Offset;end if;
      if Item.I2C_Path.Count=0 then return I2C_Count_Offset;end if;
      if Item.GPIO_Path.Count=0 then return GPIO_Count_Offset;end if;
      if not Canonical_Span(Item.I2C_Source) then return I2C_Source_Length_Offset;end if;
      if not Canonical_Span(Item.I2C_Vendor) then return I2C_Vendor_Length_Offset;end if;
      if not Canonical_Span(Item.GPIO_Source) then return GPIO_Source_Length_Offset;end if;
      if not Canonical_Span(Item.GPIO_Vendor) then return GPIO_Vendor_Length_Offset;end if;
      if Item.GPIO_UID_Kind=No_UID and then Item.GPIO_UID/=0 then return UID_Offset;end if;
      if not Valid_HID(Item.GPIO_HID) then return HID_Offset;end if;
      for I in Text_4'Range loop
         if Item.Table(I) not in '!'..'~' then return Table_Offset+I-1;end if;
      end loop;
      for I in Text_8'Range loop
         if Item.Table_ID(I) not in ' '..'~' then return Table_ID_Offset+I-1;end if;
      end loop;
      At_Path:=Path_Error(Item.Device_Path,Device_Path_Offset);if At_Path/=0 then return At_Path;end if;
      At_Path:=Path_Error(Item.I2C_Path,I2C_Path_Offset);if At_Path/=0 then return At_Path;end if;
      return Path_Error(Item.GPIO_Path,GPIO_Path_Offset);
   end Bad_Field;
   function Valid(Item:Request) return Boolean is(Bad_Field(Item)=0);
   procedure Encode(Item:Request;Data:out Byte_Array) is
      Cursor:Natural:=Header_Bytes;
      function Bit(Value:Boolean) return Word is(if Value then 1 else 0);
      procedure Put(Offset:Natural;Width:Integer_Bytes;Value:Word) is
      begin
         for I in 0 .. Width-1 loop
            Data(Data'First+Offset+I):=Byte(Shift_Right(Value,Octet_Bits*I) mod Octet_Modulus);
         end loop;
      end Put;
      procedure Text(Offset:Natural;Value:String) is
      begin
         for I in Value'Range loop Data(Data'First+Offset+(I-Value'First)):=Byte(Character'Pos(Value(I)));end loop;
      end Text;
      procedure Path(Offset:Natural;Value:Canonical_Path) is
      begin
         for I in Path_Index loop Text(Offset+(I-1)*Name_Segment_Bytes,Value.Names(I));end loop;
      end Path;
      procedure Span(Value:Owned_Span) is
      begin
         for I in 1 .. Value.Length loop Data(Data'First+Cursor):=Value.Data(I);Cursor:=Cursor+1;end loop;
      end Span;
   begin
      Data:=[others=>0];
      Put(Version_Offset,4,Format_Version);Put(Length_Offset,4,Word(Encoded_Length(Item)));
      Put(Epoch_Offset,8,Item.Epoch);Put(Catalog_Offset,8,Item.Catalog_Revision);Put(Observation_Offset,8,Item.Observation_Sequence);
      Put(SEG_Offset,8,Item.PCI.SEG);Put(BBN_Offset,8,Item.PCI.BBN);Put(ADR_Offset,8,Item.PCI.ADR);
      Put(Hertz_Offset,4,Word(Item.Maximum_Hz));Put(Address_Offset,2,Word(Item.Address));
      Put(Descriptor_Offset,2,Word(Item.Descriptor_Register));Put(Pin_Offset,2,Word(Item.Pin));
      Put(I2C_Revision_Offset,1,Word(Item.I2C_Revision));Put(I2C_Type_Offset,1,Word(Item.I2C_Type_Revision));Put(I2C_Index_Offset,1,Word(Item.I2C_Source_Index));
      Put(PCI_Known_Offset,1,Bit(Item.PCI.Has_SEG)+2*Bit(Item.PCI.Has_BBN)+4*Bit(Item.PCI.Has_ADR));
      Put(I2C_Flags_Offset,2,Word(Item.I2C_Specific_Flags));Put(I2C_Role_Offset,1,Resource_Role'Enum_Rep(Item.I2C_Role));
      Put(Initiator_Offset,1,Initiator'Enum_Rep(Item.Initiated_By));Put(I2C_Shared_Offset,1,Bit(Item.I2C_Shared));
      Put(Addressing_Offset,1,Address_Mode'Enum_Rep(Item.Addressing));Put(Trigger_Offset,1,Trigger'Enum_Rep(Item.IRQ_Trigger));
      Put(Polarity_Offset,1,Polarity'Enum_Rep(Item.IRQ_Polarity));Put(IRQ_Shared_Offset,1,Bit(Item.IRQ_Shared));Put(Wake_Offset,1,Bit(Item.Wake_Capable));
      Put(GPIO_Role_Offset,1,Resource_Role'Enum_Rep(Item.GPIO_Role));Put(Pin_Configuration_Offset,1,Word(Item.Pin_Configuration));
      Put(Drive_Offset,2,Word(Item.Drive));Put(Debounce_Offset,2,Word(Item.Debounce));Put(UID_Kind_Offset,1,UID_Kind'Enum_Rep(Item.GPIO_UID_Kind));
      Put(Device_Count_Offset,1,Word(Item.Device_Path.Count));Put(I2C_Count_Offset,1,Word(Item.I2C_Path.Count));Put(GPIO_Count_Offset,1,Word(Item.GPIO_Path.Count));
      Put(I2C_Source_Length_Offset,2,Word(Item.I2C_Source.Length));Put(I2C_Vendor_Length_Offset,2,Word(Item.I2C_Vendor.Length));
      Put(GPIO_Source_Length_Offset,2,Word(Item.GPIO_Source.Length));Put(GPIO_Vendor_Length_Offset,2,Word(Item.GPIO_Vendor.Length));
      Put(UID_Offset,8,Item.GPIO_UID);Text(HID_Offset,Item.GPIO_HID);Text(Table_Offset,Item.Table);Text(Table_ID_Offset,Item.Table_ID);
      Put(Table_Digest_Offset,8,Item.Table_Digest);Put(Initializer_Count_Offset,4,Word(Item.Initializer_Count));Put(Phase_Offset,1,Phase'Enum_Rep(Item.Observed));
      Put(Initializer_Digest_Offset,8,Item.Initializer_Digest);Put(Resource_Digest_Offset,8,Item.Resource_Digest);Text(Compatible_Offset,Compatible_ID);
      Path(Device_Path_Offset,Item.Device_Path);Path(I2C_Path_Offset,Item.I2C_Path);Path(GPIO_Path_Offset,Item.GPIO_Path);
      Span(Item.I2C_Source);Span(Item.I2C_Vendor);Span(Item.GPIO_Source);Span(Item.GPIO_Vendor);
   end Encode;
   procedure Decode(Data:Byte_Array;Item:out Request;Result:out Decode_Result) is
      R:Request;Known:Word;Cursor:Natural:=Header_Bytes;At_Field:Payload_Offset;
      procedure Fail(Offset:Payload_Offset) is
      begin Result:=(Status=>Malformed,At_Byte=>Offset);end Fail;
      function Read(Offset:Natural;Width:Integer_Bytes) return Word is(Get(Data,Offset,Width));
      procedure Text(Offset:Natural;Value:out String) is
      begin
         for I in Value'Range loop Value(I):=Character'Val(Data(Data'First+Offset+(I-Value'First)));end loop;
      end Text;
      procedure Path(Offset:Natural;Value:in out Canonical_Path) is
      begin
         for I in Path_Index loop Text(Offset+(I-1)*Name_Segment_Bytes,Value.Names(I));end loop;
      end Path;
      procedure Span(Value:in out Owned_Span) is
      begin
         for I in 1 .. Value.Length loop Value.Data(I):=Data(Data'First+Cursor);Cursor:=Cursor+1;end loop;
      end Span;
   begin
      Item:=Empty_Request;Result:=(Status=>Malformed,At_Byte=>0);
      if Data'Length<Header_Bytes then Fail(Data'Length);return;end if;
      if Data'Length>Max_Payload_Bytes then Fail(Length_Offset);return;end if;
      if Read(Version_Offset,4)/=Format_Version then Fail(Version_Offset);return;end if;
      if Read(Length_Offset,4)/=Word(Data'Length) then Fail(Length_Offset);return;end if;
      Known:=Read(PCI_Known_Offset,1);if Known>7 then Fail(PCI_Known_Offset);return;end if;
      if Read(I2C_Role_Offset,1)>1 then Fail(I2C_Role_Offset);return;end if;
      if Read(Initiator_Offset,1)>1 then Fail(Initiator_Offset);return;end if;
      if Read(I2C_Shared_Offset,1)>1 then Fail(I2C_Shared_Offset);return;end if;
      if Read(Addressing_Offset,1)>1 then Fail(Addressing_Offset);return;end if;
      if Read(Trigger_Offset,1)>1 then Fail(Trigger_Offset);return;end if;
      if Read(Polarity_Offset,1)>2 then Fail(Polarity_Offset);return;end if;
      if Read(IRQ_Shared_Offset,1)>1 then Fail(IRQ_Shared_Offset);return;end if;
      if Read(Wake_Offset,1)>1 then Fail(Wake_Offset);return;end if;
      if Read(GPIO_Role_Offset,1)>1 then Fail(GPIO_Role_Offset);return;end if;
      if Read(UID_Kind_Offset,1)>2 then Fail(UID_Kind_Offset);return;end if;
      for Offset in Device_Count_Offset .. GPIO_Count_Offset loop
         if Read(Offset,1)>Word(Max_Path_Segments) then Fail(Offset);return;end if;
      end loop;
      for I in 0 .. Span_Kinds-1 loop
         if Read(I2C_Source_Length_Offset+2*I,2)>Word(Max_Span_Bytes) then Fail(I2C_Source_Length_Offset+2*I);return;end if;
      end loop;
      if Read(Reserved_Offset,2)/=0 then Fail(Reserved_Offset);return;end if;
      if Read(Phase_Offset,1)/=Phase'Enum_Rep(After_Device_Initialization) then Fail(Phase_Offset);return;end if;
      for Offset in Reserved_Tail_Offset .. Initializer_Digest_Offset-1 loop
         if Read(Offset,1)/=0 then Fail(Offset);return;end if;
      end loop;
      for I in Text_8'Range loop
         if Read(Compatible_Offset+I-1,1)/=Word(Character'Pos(Compatible_ID(I))) then Fail(Compatible_Offset+I-1);return;end if;
      end loop;
      R.Epoch:=Read(Epoch_Offset,8);R.Catalog_Revision:=Read(Catalog_Offset,8);R.Observation_Sequence:=Read(Observation_Offset,8);
      R.PCI:=(Has_SEG=>(Known mod 2=1),Has_BBN=>((Known/2) mod 2=1),Has_ADR=>(Known/4=1),SEG=>Read(SEG_Offset,8),BBN=>Read(BBN_Offset,8),ADR=>Read(ADR_Offset,8));
      R.Maximum_Hz:=Small_Word(Read(Hertz_Offset,4));R.Address:=Half(Read(Address_Offset,2));
      R.Descriptor_Register:=Half(Read(Descriptor_Offset,2));R.Pin:=Half(Read(Pin_Offset,2));
      R.I2C_Revision:=Byte(Read(I2C_Revision_Offset,1));R.I2C_Type_Revision:=Byte(Read(I2C_Type_Offset,1));R.I2C_Source_Index:=Byte(Read(I2C_Index_Offset,1));
      R.I2C_Specific_Flags:=Half(Read(I2C_Flags_Offset,2));R.I2C_Role:=Resource_Role'Enum_Val(Read(I2C_Role_Offset,1));
      R.Initiated_By:=Initiator'Enum_Val(Read(Initiator_Offset,1));R.I2C_Shared:=Read(I2C_Shared_Offset,1)=1;
      R.Addressing:=Address_Mode'Enum_Val(Read(Addressing_Offset,1));R.IRQ_Trigger:=Trigger'Enum_Val(Read(Trigger_Offset,1));
      R.IRQ_Polarity:=Polarity'Enum_Val(Read(Polarity_Offset,1));R.IRQ_Shared:=Read(IRQ_Shared_Offset,1)=1;R.Wake_Capable:=Read(Wake_Offset,1)=1;
      R.GPIO_Role:=Resource_Role'Enum_Val(Read(GPIO_Role_Offset,1));R.Pin_Configuration:=Byte(Read(Pin_Configuration_Offset,1));
      R.Drive:=Half(Read(Drive_Offset,2));R.Debounce:=Half(Read(Debounce_Offset,2));R.GPIO_UID_Kind:=UID_Kind'Enum_Val(Read(UID_Kind_Offset,1));
      R.Device_Path.Count:=Path_Count(Read(Device_Count_Offset,1));R.I2C_Path.Count:=Path_Count(Read(I2C_Count_Offset,1));R.GPIO_Path.Count:=Path_Count(Read(GPIO_Count_Offset,1));
      R.I2C_Source.Length:=Span_Length(Read(I2C_Source_Length_Offset,2));R.I2C_Vendor.Length:=Span_Length(Read(I2C_Vendor_Length_Offset,2));
      R.GPIO_Source.Length:=Span_Length(Read(GPIO_Source_Length_Offset,2));R.GPIO_Vendor.Length:=Span_Length(Read(GPIO_Vendor_Length_Offset,2));
      if Data'Length/=Encoded_Length(R) then Fail(Length_Offset);return;end if;
      R.GPIO_UID:=Read(UID_Offset,8);Text(HID_Offset,R.GPIO_HID);Text(Table_Offset,R.Table);Text(Table_ID_Offset,R.Table_ID);
      R.Table_Digest:=Read(Table_Digest_Offset,8);R.Initializer_Count:=Small_Word(Read(Initializer_Count_Offset,4));
      R.Initializer_Digest:=Read(Initializer_Digest_Offset,8);R.Resource_Digest:=Read(Resource_Digest_Offset,8);
      Path(Device_Path_Offset,R.Device_Path);Path(I2C_Path_Offset,R.I2C_Path);Path(GPIO_Path_Offset,R.GPIO_Path);
      Span(R.I2C_Source);Span(R.I2C_Vendor);Span(R.GPIO_Source);Span(R.GPIO_Vendor);
      At_Field:=Bad_Field(R);if At_Field/=0 then Fail(At_Field);return;end if;
      Item:=R;Result:=(Status=>Decoded,At_Byte=>0);
   end Decode;
end HID_Offer_Payload;
