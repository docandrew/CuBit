package body Compositor_Clipboard_Protocol with SPARK_Mode is
   use type CuBit.Desktop_Protocol.Protocol_Flags;
   use type Interfaces.Unsigned_8;
   use type Interfaces.Unsigned_16;

   Field_Base : constant Word := 2 ** 20;
   Type_Shift : constant Word := 2 ** 40;
   Kind_Shift : constant Word := 2 ** 56;
   Type_Base  : constant Word := 2 ** 16;
   Want_CCL_Bit : constant Word := 1;
   Target_Bit   : constant Word := 2;

   function Pack (Shape : Layout; Kind : Sensitivity) return Word is
     (Word (Shape.Text_Length) + Word (Shape.CCL_Length) * Field_Base +
      Word (Shape.Type_Length) * Type_Shift +
      Word (Sensitivity'Enum_Rep (Kind)) * Kind_Shift);

   function Unpack (Value : Word) return Descriptor is
      Text : constant Word := Value mod Field_Base;
      CCL  : constant Word := (Value / Field_Base) mod Field_Base;
      Name : constant Word := (Value / Type_Shift) mod Type_Base;
      Kind : constant Word := Value / Kind_Shift;
      Shape : Layout;
   begin
      if Text > Maximum_Payload or else CCL > Maximum_Payload or else
        Name > Maximum_Type_Name or else Kind > 1
      then
         return (Valid => False);
      end if;
      Shape := (Type_Length => Type_Name_Length (Name),
                Text_Length => Payload_Length (Text),
                CCL_Length  => Payload_Length (CCL));
      if not Well_Formed (Shape) then
         return (Valid => False);
      end if;
      return (True, Shape, (if Kind = 1 then Sensitive else Ordinary));
   end Unpack;

   function Header (Wire : DP.Wire_Message; Label : Word; Length : Word)
     return Boolean is
     (Word (Wire.Label) = Label and then Word (Wire.Length) = Length and then
      Wire.Flags = 0 and then Wire.Reserved = 0);

   function Encode (Value : Offer_Request) return DP.Wire_Message is
     (Offer_Label, 4, 0, 0,
      [Word (Value.Surface), GR.Encode (Value.Window),
       Pack (Value.Shape, Value.Kind), 0]);

   function Decode_Offer (Wire : DP.Wire_Message) return Offer_Decoding is
   begin
      if not Header (Wire, Offer_Label, 4) or else Wire.Words (0) = 0 or else
        not GR.Valid_Wire (Wire.Words (1)) or else Wire.Words (3) /= 0
      then
         return (Accepted => False);
      end if;
      declare
         D : constant Descriptor := Unpack (Wire.Words (2));
      begin
         if not D.Valid then
            return (Accepted => False);
         end if;
         return (True, (Surface_Name (Wire.Words (0)),
                        GR.Decode (Wire.Words (1)), D.Shape, D.Kind));
      end;
   end Decode_Offer;

   function Status_Word (Status : DP.Status_Code) return Word is
     (Word (DP.Status_Code'Enum_Rep (Status)));

   --  Failure replies: one status word, the rest zero.
   function Failure (Wire : DP.Wire_Message; Label : Word)
     return DP.Status_Code
     with Post => Failure'Result /= DP.Success
   is
   begin
      if Header (Wire, Label, 1) and then Wire.Words (1) = 0 and then
        Wire.Words (2) = 0 and then Wire.Words (3) = 0
      then
         for Status in DP.Status_Code range DP.Denied .. DP.Resources_Exhausted
         loop
            if Wire.Words (0) = Status_Word (Status) then
               return Status;
            end if;
         end loop;
      end if;
      return DP.Invalid_Request;
   end Failure;

   function Encode (Value : Offer_Receipt) return DP.Wire_Message is
     (if Value.Status = DP.Success then
        (Offer_Label, 4, 0, 0, [0, Word (Value.Name), 0, 0])
      else (Offer_Label, 1, 0, 0, [Status_Word (Value.Status), 0, 0, 0]));

   function Decode_Offer_Receipt (Wire : DP.Wire_Message) return Offer_Receipt
   is
   begin
      if Header (Wire, Offer_Label, 4) and then Wire.Words (0) = 0 and then
        Wire.Words (1) /= 0 and then Wire.Words (2) = 0 and then
        Wire.Words (3) = 0
      then
         return (DP.Success, Entry_Name (Wire.Words (1)));
      end if;
      declare
         Refused : Offer_Receipt (Failure (Wire, Offer_Label));
      begin
         return Refused;
      end;
   end Decode_Offer_Receipt;

   function Encode (Value : Paste_Request) return DP.Wire_Message is
     (Paste_Label, 4, 0, 0,
      [Word (Value.Surface), GR.Encode (Value.Window),
       (if Value.Want_CCL then Want_CCL_Bit else 0) +
       (if Value.Target = Sensitive then Target_Bit else 0), 0]);

   function Decode_Paste (Wire : DP.Wire_Message) return Paste_Decoding is
   begin
      if not Header (Wire, Paste_Label, 4) or else Wire.Words (0) = 0 or else
        not GR.Valid_Wire (Wire.Words (1)) or else
        Wire.Words (2) > Want_CCL_Bit + Target_Bit or else Wire.Words (3) /= 0
      then
         return (Accepted => False);
      end if;
      return (True, (Surface_Name (Wire.Words (0)), GR.Decode (Wire.Words (1)),
                     Wire.Words (2) mod 2 = 1,
                     (if Wire.Words (2) / 2 = 1 then Sensitive else Ordinary)));
   end Decode_Paste;

   function Encode (Value : Paste_Receipt) return DP.Wire_Message is
     (if Value.Status = DP.Success then
        (Paste_Label, 4, 0, 0,
         [0, Word (Value.Name), Pack (Value.Shape, Value.Kind), 0])
      else (Paste_Label, 1, 0, 0, [Status_Word (Value.Status), 0, 0, 0]));

   function Decode_Paste_Receipt (Wire : DP.Wire_Message) return Paste_Receipt
   is
   begin
      if Header (Wire, Paste_Label, 4) and then Wire.Words (0) = 0 and then
        Wire.Words (1) /= 0 and then Wire.Words (3) = 0
      then
         declare
            D : constant Descriptor := Unpack (Wire.Words (2));
         begin
            if D.Valid then
               return (DP.Success, Entry_Name (Wire.Words (1)), D.Shape, D.Kind);
            end if;
         end;
      end if;
      declare
         Refused : Paste_Receipt (Failure (Wire, Paste_Label));
      begin
         return Refused;
      end;
   end Decode_Paste_Receipt;
end Compositor_Clipboard_Protocol;
