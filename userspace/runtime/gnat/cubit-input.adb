package body CuBit.Input with SPARK_Mode => On is

   DEVICE_MASK       : constant Unsigned_64 := 16#0000_0000_0000_00FF#;
   DELIVERY_MASK     : constant Unsigned_64 := 16#0000_0000_0000_FF00#;
   GENERATION_MASK   : constant Unsigned_64 := 16#FFFF_FFFF_0000_0000#;
   RESYNC_MASK       : constant Unsigned_64 := 16#0000_0000_0001_0000#;
   SUB_MILLISECOND_SHIFT : constant := 17;
   SUB_MILLISECOND_MASK : constant Unsigned_64 := 16#0000_0000_07FE_0000#;
   RESERVED_FLAG_MASK : constant Unsigned_64 := 16#0000_0000_F800_0000#;

   function Header_Sub_Millisecond (header : Unsigned_64) return Unsigned_64;
   function Header_Sub_Millisecond (header : Unsigned_64) return Unsigned_64 is
     (Shift_Right (header and SUB_MILLISECOND_MASK, SUB_MILLISECOND_SHIFT));

   function Encode (report : Source_Report) return CuBit.Messages.Message is
      header : Unsigned_64 :=
        Unsigned_64 (Device_Class'Enum_Rep (report.device)) or
        Shift_Left
          (Unsigned_64 (Delivery_Class'Enum_Rep (report.delivery)), 8) or
        Shift_Left (Unsigned_64 (report.generation), 32);
   begin
      if report.flags (RESYNCHRONIZE) then
         header := header or RESYNC_MASK;
      end if;

      return
        (tag => (label => OP_SOURCE_REPORT, length => 4,
                 protocol => 0, ipc => <>, reserved => 0),
         --  This field is deliberately ignored on send and kernel-stamped.
         authorityTag => 0,
         words => (0 => report.sequence,
                   1 => header,
                   2 => report.payload,
                   3 => report.snapshot));
   end Encode;

   function Encode_Stamped
     (report : Source_Report; Observed_Us : Unsigned_64) return CuBit.Messages.Message
   is
      msg : CuBit.Messages.Message := Encode (report);
   begin
      msg.words (1) := msg.words (1) or
        Shift_Left (Sub_Millisecond_Of (Observed_Us), SUB_MILLISECOND_SHIFT);
      return msg;
   end Encode_Stamped;

   function Source_Time_Us (msg : CuBit.Messages.Message) return Unsigned_64 is
      Whole : constant Unsigned_64 := Pointer_Time (msg.words (3));
      Part : constant Unsigned_64 := Header_Sub_Millisecond (msg.words (1));
   begin
      if Whole = Unsigned_64'Last or else Whole > Maximum_Stamped_Ms or else
        Part >= Microseconds_Per_Millisecond
      then
         return Unsigned_64'Last;
      end if;
      return Whole * Microseconds_Per_Millisecond + Part;
   end Source_Time_Us;

   function Is_Source_Report
     (msg : CuBit.Messages.Message; publisher : CuBit.Messages.Process_ID) return Boolean
   is
      use type CuBit.Messages.Process_ID;
      deviceRep   : constant Unsigned_64 := msg.words (1) and DEVICE_MASK;
      deliveryRep : constant Unsigned_64 :=
        Shift_Right (msg.words (1) and DELIVERY_MASK, 8);
   begin
      return msg.tag.label = OP_SOURCE_REPORT and then
        msg.tag.length = 4 and then
        --  Published through an input publication capability (its badge),
        --  by a process the kernel names.
        msg.authorityTag = Publisher_Tag and then
        publisher /= CuBit.Messages.No_Process and then
        msg.words (0) /= 0 and then
        (msg.words (1) and RESERVED_FLAG_MASK) = 0 and then
        Header_Sub_Millisecond (msg.words (1)) < Microseconds_Per_Millisecond and then
        (msg.words (1) and GENERATION_MASK) /= 0 and then
        deviceRep in
          Unsigned_64 (Device_Class'Enum_Rep (Device_Class'First)) ..
          Unsigned_64 (Device_Class'Enum_Rep (Device_Class'Last)) and then
        deliveryRep in
          Unsigned_64 (Delivery_Class'Enum_Rep (Delivery_Class'First)) ..
          Unsigned_64 (Delivery_Class'Enum_Rep (Delivery_Class'Last));
   end Is_Source_Report;

   procedure Decode
     (msg       : CuBit.Messages.Message;
      publisher : CuBit.Messages.Process_ID;
      report    : out Source_Report;
      valid     : out Boolean)
   is
      deviceRep   : Unsigned_64;
      deliveryRep : Unsigned_64;
   begin
      report := NULL_SOURCE_REPORT;
      valid := Is_Source_Report (msg, publisher);
      if not valid then
         return;
      end if;

      deviceRep := msg.words (1) and DEVICE_MASK;
      deliveryRep := Shift_Right (msg.words (1) and DELIVERY_MASK, 8);
      report.sourceIdentity := CuBit.Messages.To_Word (publisher);
      report.sequence := msg.words (0);
      report.generation := Source_Generation
        (Shift_Right (msg.words (1) and GENERATION_MASK, 32));
      --  Decode untrusted wire values explicitly. Avoiding unchecked enum
      --  conversion keeps malformed representations outside the Ada type and
      --  makes totality evident to both readers and the prover.
      case deviceRep is
         when 1 =>
            report.device := KEYBOARD;
         when 2 =>
            report.device := RELATIVE_POINTER;
         when 3 =>
            report.device := ABSOLUTE_POINTER;
         when others =>
            valid := False;
            return;
      end case;

      case deliveryRep is
         when 1 =>
            report.delivery := ORDERED_TRANSITION;
         when 2 =>
            report.delivery := ACCUMULABLE_DISPLACEMENT;
         when 3 =>
            report.delivery := REPLACEABLE_STATE;
         when others =>
            valid := False;
            return;
      end case;
      report.flags (RESYNCHRONIZE) :=
        (msg.words (1) and RESYNC_MASK) /= 0;
      report.payload := msg.words (2);
      report.snapshot := msg.words (3);
   end Decode;

end CuBit.Input;
