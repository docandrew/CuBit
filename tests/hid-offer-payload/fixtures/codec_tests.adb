pragma Ada_2022;
with HID_Offer_Payload;with CuBit.HID_Offer_Exchange;with Interfaces;
with Ada.Command_Line;with Ada.Text_IO;with Ada.Streams;with Ada.Streams.Stream_IO;
procedure Codec_Tests is
   package P renames HID_Offer_Payload;package X renames CuBit.HID_Offer_Exchange;
   use type P.Byte,P.Half,P.Small_Word,P.Word,P.Request,P.Byte_Array,P.Decode_Status,P.PCI_Location;
   Checks:Natural:=0;
   type Offset_Array is array(Positive range <>) of P.Payload_Offset;
   Path_Offsets:constant Offset_Array:=[P.Device_Path_Offset,P.I2C_Path_Offset,P.GPIO_Path_Offset];
   procedure Check(OK:Boolean;Why:String) is
   begin Checks:=Checks+1;if not OK then raise Program_Error with Why&Checks'Image;end if;end;
   Wide:constant Boolean:=Ada.Command_Line.Argument(2)="wide";
   Partial:constant Boolean:=Ada.Command_Line.Argument(2)="partial";
   R,V:P.Request;Reply:P.Decode_Result;
   procedure Path(Item:in out P.Canonical_Path;A,B:String;C:String:="";D:String:="") is
   begin
      Item.Names(1):=A;Item.Names(2):=B;Item.Count:=2;
      if C'Length/=0 then Item.Names(3):=C;Item.Count:=3;end if;
      if D'Length/=0 then Item.Names(4):=D;Item.Count:=4;end if;
   end;
   procedure Span(Item:in out P.Owned_Span;Text:String) is
   begin Item.Length:=Text'Length;for I in Text'Range loop Item.Data(I-Text'First+1):=P.Byte(Character'Pos(Text(I)));end loop;end;
   procedure Put(Data:in out P.Byte_Array;Offset:Natural;Width:Positive;Value:P.Word) is
   begin for I in 0..Width-1 loop Data(Data'First+Offset+I):=P.Byte(Interfaces.Shift_Right(Value,8*I) mod 256);end loop;end;
   procedure Reject(Data:P.Byte_Array;Offset:P.Payload_Offset) is
   begin
      V:=R;P.Decode(Data,V,Reply);
      Check(Reply.Status=P.Malformed and Reply.At_Byte=Offset and V=P.Empty_Request,"malformed field and cleared output offset"&Offset'Image);
   end;
begin
   R.Epoch:=16#1_0000_0007#;R.Catalog_Revision:=17;R.Observation_Sequence:=29;
   R.PCI:=(Has_SEG=>True,Has_BBN=>not Partial,Has_ADR=>True,SEG=>0,BBN=>0,ADR=>16#15_0000#);
   R.Maximum_Hz:=400_000;R.Address:=16#15#;R.Descriptor_Register:=16#20#;R.Pin:=16#4C#;
   R.I2C_Revision:=2;R.I2C_Type_Revision:=1;R.IRQ_Polarity:=P.Active_Low;R.IRQ_Shared:=True;
   R.GPIO_UID_Kind:=P.Integer_UID;R.GPIO_HID:="INTC1057";R.Table:="DSDT";R.Table_ID:="LENOVO  ";
   R.Table_Digest:=16#1122_3344_5566_7788#;R.Initializer_Count:=6;
   R.Initializer_Digest:=16#9988_7766_5544_3322#;R.Resource_Digest:=16#FEDD_CCBB_AA99_8877#;
   Path(R.Device_Path,"_SB_","PC00","I2C1","TPD0");Path(R.I2C_Path,"_SB_","PC00","I2C1");Path(R.GPIO_Path,"_SB_","GPI0");
   Span(R.I2C_Source,"\_SB.PC00.I2C1"&P.NUL);Span(R.GPIO_Source,"\_SB.GPI0"&P.NUL);
   if Wide then
      R.Epoch:=P.Word'Last;R.Catalog_Revision:=P.Word'Last;R.Observation_Sequence:=P.Word'Last;
      R.PCI:=(Has_SEG=>True,Has_BBN=>True,Has_ADR=>True,SEG=>65_535,BBN=>255,ADR=>31*65_536+7);
      R.Maximum_Hz:=P.Small_Word'Last;R.Addressing:=P.Ten_Bit;R.Address:=1023;R.Descriptor_Register:=P.Half'Last;R.Pin:=P.Half'Last;
      R.I2C_Type_Revision:=P.Byte'Last;R.I2C_Source_Index:=P.Byte'Last;R.I2C_Specific_Flags:=P.Half'Last;R.I2C_Shared:=True;
      R.IRQ_Trigger:=P.Edge;R.IRQ_Polarity:=P.Active_Both;R.Wake_Capable:=True;
      R.Pin_Configuration:=P.Byte'Last;R.Drive:=P.Half'Last;R.Debounce:=P.Half'Last;
      R.GPIO_UID_Kind:=P.String_UID;R.GPIO_UID:=P.Word'Last;R.Table:="SSDT";R.Table_ID:="WIDECASE";
      R.Table_Digest:=P.Word'Last;R.Initializer_Count:=P.Small_Word'Last;R.Initializer_Digest:=P.Word'Last;R.Resource_Digest:=P.Word'Last;
      R.Device_Path:=(Count=>P.Max_Path_Segments,Names=>[others=>"ABCD"]);R.I2C_Path:=R.Device_Path;R.GPIO_Path:=R.Device_Path;
      R.I2C_Source:=(Length=>P.Max_Span_Bytes,Data=>[others=>0]);R.I2C_Vendor:=R.I2C_Source;R.GPIO_Source:=R.I2C_Source;R.GPIO_Vendor:=R.I2C_Source;
      for I in 1..P.Max_Span_Bytes loop R.I2C_Source.Data(I):=P.Byte(I mod 256);R.I2C_Vendor.Data(I):=16#55#;R.GPIO_Source.Data(I):=16#AA#;R.GPIO_Vendor.Data(I):=16#FF#;end loop;
   end if;
   Check(P.Valid(R),"golden request valid");
   declare
      N:constant Natural:=P.Encoded_Length(R);Data:P.Byte_Array(0..N-1);
      File:Ada.Streams.Stream_IO.File_Type;Raw:Ada.Streams.Stream_Element_Array(1..P.Max_Payload_Bytes);
      Last:Ada.Streams.Stream_Element_Offset;
   begin
      P.Encode(R,Data);Ada.Streams.Stream_IO.Open(File,Ada.Streams.Stream_IO.In_File,Ada.Command_Line.Argument(1));
      Ada.Streams.Stream_IO.Read(File,Raw,Last);Ada.Streams.Stream_IO.Close(File);
      Check(N=Natural(Last),"independent golden length");
      for I in Data'Range loop Check(Data(I)=P.Byte(Raw(Ada.Streams.Stream_Element_Offset(I+1))),"independent golden byte"&I'Image);end loop;
      P.Decode(Data,V,Reply);Check(Reply.Status=P.Decoded and V=R,"full-width round trip");
      Check(P.Fully_Resolved(V.PCI)=not Partial,"absent BBN differs from evaluated bus0");
      if not Partial then
         Check(P.Location(V.PCI)=(if Wide then P.PCI_Location'(Segment=>65_535,Bus=>255,Device=>31,Func=>7)
           else P.PCI_Location'(Segment=>0,Bus=>0,Device=>21,Func=>0)),"validated PCI conversion");
      end if;
      declare Shifted:P.Byte_Array(17..17+N-1);High:P.Byte_Array(Natural'Last-N+1..Natural'Last);begin
         P.Encode(R,Shifted);P.Decode(Shifted,V,Reply);Check(Reply.Status=P.Decoded and V=R,"nonzero bounds round trip");
         P.Encode(R,High);P.Decode(High,V,Reply);Check(Reply.Status=P.Decoded and V=R,"highest Natural bounds without intermediate overflow");
      end;
      for Length in 0..N-1 loop
         declare Prefix:P.Byte_Array(1..Length);begin
            for I in 1..Length loop Prefix(I):=Data(I-1);end loop;
            V:=R;P.Decode(Prefix,V,Reply);Check(Reply.Status=P.Malformed and V=P.Empty_Request,"truncated frame clears output");
         end;
      end loop;
      declare Extra:P.Byte_Array(0..N);begin Extra(0..N-1):=Data;Extra(N):=0;Reject(Extra,P.Length_Offset);Put(Extra,4,4,P.Word(N+1));Reject(Extra,P.Length_Offset);end;
      declare Changed:P.Byte_Array(Data'Range);begin
         Changed:=Data;Put(Changed,0,4,0);Reject(Changed,0);
         Changed:=Data;Put(Changed,4,4,0);Reject(Changed,4);
         Changed:=Data;Put(Changed,8,8,0);Reject(Changed,8);
         Changed:=Data;Put(Changed,24,8,0);Reject(Changed,24);
         Changed:=Data;Put(Changed,32,8,65_536);Reject(Changed,32);
         Changed:=Data;Changed(69):=7;Put(Changed,40,8,256);Reject(Changed,40);
         Changed:=Data;Put(Changed,48,8,8);Reject(Changed,48);
         Changed:=Data;Changed(69):=3;Reject(Changed,48);
         Changed:=Data;Put(Changed,56,4,0);Reject(Changed,56);
         Changed:=Data;Changed(75):=0;Put(Changed,60,2,128);Reject(Changed,60);
         Changed:=Data;Changed(75):=1;Put(Changed,60,2,1024);Reject(Changed,60);
         Changed:=Data;Changed(66):=0;Reject(Changed,66);
         Changed:=Data;Changed(69):=8;Reject(Changed,69);
         for Offset in 72..76 loop Changed:=Data;Changed(Offset):=2;Reject(Changed,Offset);end loop;
         Changed:=Data;Changed(72):=1;Reject(Changed,72);
         Changed:=Data;Changed(73):=1;Reject(Changed,73);
         Changed:=Data;Changed(77):=3;Reject(Changed,77);
         Changed:=Data;Changed(76):=0;Changed(77):=2;Reject(Changed,77);
         for Offset in 78..80 loop Changed:=Data;Changed(Offset):=2;Reject(Changed,Offset);end loop;
         Changed:=Data;Changed(80):=1;Reject(Changed,80);
         Changed:=Data;Changed(86):=3;Reject(Changed,86);
         for Offset in 87..89 loop
            Changed:=Data;Changed(Offset):=0;Reject(Changed,Offset);
            Changed:=Data;Changed(Offset):=33;Reject(Changed,Offset);
         end loop;
         for I in 0..3 loop Changed:=Data;Put(Changed,90+2*I,2,257);Reject(Changed,90+2*I);end loop;
         Changed:=Data;Changed(98):=1;Reject(Changed,98);
         Changed:=Data;Changed(99):=1;Reject(Changed,98);
         Changed:=Data;Changed(86):=0;Put(Changed,100,8,1);Reject(Changed,100);
         Changed:=Data;Changed(108):=0;Reject(Changed,108);
         Changed:=Data;Changed(110):=0;Changed(111):=Character'Pos('Z');Reject(Changed,108);
         Changed:=Data;Changed(116):=Character'Pos(' ');Reject(Changed,116);
         Changed:=Data;Changed(120):=0;Reject(Changed,120);
         Changed:=Data;Changed(140):=2;Reject(Changed,140);
         for Offset in 141..143 loop Changed:=Data;Changed(Offset):=1;Reject(Changed,Offset);end loop;
         for Offset in 160..167 loop Changed:=Data;Changed(Offset):=16#FF#;Reject(Changed,Offset);end loop;
         for Offset of Path_Offsets loop
            Changed:=Data;Changed(Offset):=Character'Pos('1');Reject(Changed,Offset);
         end loop;
         if not Wide then Changed:=Data;Changed(P.Device_Path_Offset+R.Device_Path.Count*4):=Character'Pos('A');Reject(Changed,P.Device_Path_Offset+R.Device_Path.Count*4);end if;
      end;
      for Mask in 0..7 loop
         declare Copy:P.Request:=R;Packed:P.Byte_Array(Data'Range);begin
            Copy.PCI:=(Has_SEG=>Mask mod 2=1,Has_BBN=>(Mask/2) mod 2=1,Has_ADR=>Mask/4=1,others=>0);
            Check(P.Valid(Copy),"partial PCI presence valid without default guesses");P.Encode(Copy,Packed);P.Decode(Packed,V,Reply);
            Check(Reply.Status=P.Decoded and V=Copy and P.Fully_Resolved(V.PCI)=(Mask=7),"exact PCI known mask");
         end;
      end loop;
      declare Copy:P.Request:=R;begin Copy.I2C_Vendor:=(Length=>0,Data=>[others=>0]);Copy.I2C_Vendor.Data(1):=1;Check(not P.Valid(Copy),"unencoded source tail rejected");end;
   end;
   for Value in 0..10 loop Check(X.Valid_Disposition(P.Word(Value))=(Value in 1..9),"reply enum domain");end loop;
   Check(not X.Valid_Disposition(P.Word'Last),"full-width unknown reply");
   Ada.Text_IO.Put_Line("HID offer codec checks"&Checks'Image);
end Codec_Tests;
