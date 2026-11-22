------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Net_Management with SPARK_Mode is

   function Operation_Of (Value : Unsigned_32) return Operation is
   begin
      for Op in Operation loop
         if Label (Op) = Value then
            return Op;
         end if;
         pragma Loop_Invariant (for all P in Operation'First .. Op => Label (P) /= Value);
      end loop;
      raise Program_Error;   --  unreachable: Is_Operation (Value)
   end Operation_Of;

   function Field (W : Unsigned_64; Shift : Natural; Mask : Unsigned_64) return Unsigned_64 is
     (Shift_Right (W, Shift) and Mask)
   with Pre => Shift < 64;

   --  The condition with code Value, if one has it.
   procedure Condition_Of
     (Value : Unsigned_64; C : out NL.Link_Condition; Found : out Boolean)
   with Post => (if Found then Unsigned_64 (NL.Link_Condition'Enum_Rep (C)) = Value);

   procedure Condition_Of
     (Value : Unsigned_64; C : out NL.Link_Condition; Found : out Boolean)
   is
   begin
      C := NL.Down;
      Found := False;
      for K in NL.Link_Condition loop
         if Unsigned_64 (NL.Link_Condition'Enum_Rep (K)) = Value then
            C := K;
            Found := True;
         end if;
         pragma Loop_Invariant
           (if Found then Unsigned_64 (NL.Link_Condition'Enum_Rep (C)) = Value);
      end loop;
   end Condition_Of;

   function Decode_Configuration (W : Words4) return Decoded_Configuration is
      Prefix_Value : constant Unsigned_64 := Field (W (2), Prefix_Shift, Byte_Field);
      Metric_Value : constant Unsigned_64 := Field (W (2), Metric_Shift, Half_Field);
   begin
      if not NL.Valid_Handle_Word (W (0)) or else W (1) > Word_Field or else
        W (3) > Word_Field or else Prefix_Value > Unsigned_64 (IPv4_Prefix'Last)
      then
         return (Valid => False);
      end if;
      declare
         H : constant NL.Interface_Handle := NL.Handle_Of (W (0));
         C : constant Configuration :=
           (Address => IPv4_Word (W (1)), Prefix => IPv4_Prefix (Prefix_Value),
            Gateway => IPv4_Word (W (3)), Metric => Route_Metric (Metric_Value));
      begin
         if Encode (H, C) /= W then
            return (Valid => False);
         end if;
         return (Valid => True, Handle => H, Item => C);
      end;
   end Decode_Configuration;

   function Decode_Summary (W : Words4) return Decoded_Summary is
      Identity_Value : constant Unsigned_64 := Field (W (0), 0, Byte_Field);
      Location_Value : constant Unsigned_64 := Shift_Right (W (0), NL.Location_Shift);
      Kind_Value     : constant Unsigned_64 := Field (W (1), Kind_Shift_W1, Byte_Field);
      Condition_Value : constant Unsigned_64 := Shift_Right (W (1), Condition_Shift_W1);
      Prefix_Value   : constant Unsigned_64 := Field (W (2), Prefix_Shift_W2, Byte_Field);
      Metric_Value   : constant Unsigned_64 := Shift_Right (W (2), Metric_Shift_W2);
      S : Interface_Summary;
      Found : Boolean;
   begin
      if Identity_Value = Unsigned_64 (NL.Identity_Kind'Enum_Rep (NL.PCI_Function))
        and then Location_Value <= Unsigned_64 (NL.PCI_Location'Last)
      then
         S.Identity := (Kind => NL.PCI_Function, Location => NL.PCI_Location (Location_Value));
      elsif Identity_Value = Unsigned_64 (NL.Identity_Kind'Enum_Rep (NL.Factory_Address)) then
         S.Identity := (Kind => NL.Factory_Address);
      else
         return (Valid => False);
      end if;
      if Kind_Value = Unsigned_64 (NL.Link_Kind'Enum_Rep (NL.Ethernet)) then
         S.Kind := NL.Ethernet;
      elsif Kind_Value = Unsigned_64 (NL.Link_Kind'Enum_Rep (NL.Wireless)) then
         S.Kind := NL.Wireless;
      else
         return (Valid => False);
      end if;
      Condition_Of (Condition_Value, S.Condition, Found);
      if not Found or else Prefix_Value > Unsigned_64 (IPv4_Prefix'Last)
        or else Metric_Value > Unsigned_64 (Route_Metric'Last)
      then
         return (Valid => False);
      end if;
      S.Address := NL.Address_Of (W (1) mod NL.Address_Limit);
      S.Settings := (Address => IPv4_Word (W (2) and Word_Field),
                     Prefix  => IPv4_Prefix (Prefix_Value),
                     Gateway => IPv4_Word (W (3) and Word_Field),
                     Metric  => Route_Metric (Metric_Value));
      S.Sequence := Unsigned_32 (Shift_Right (W (3), Sequence_Shift_W3));
      if Encode (S) /= W then
         return (Valid => False);
      end if;
      return (Valid => True, Item => S);
   end Decode_Summary;

   function Decode_Event (W : Words4) return Decoded_Event is
      Event_Value     : constant Unsigned_64 := Field (W (2), 0, Byte_Field);
      Condition_Value : constant Unsigned_64 := Field (W (2), Condition_Shift_Event, Byte_Field);
      Ups_Value       : constant Unsigned_64 := Field (W (2), Ups_Shift_Event, Half_Field);
      Reason_Value    : constant Unsigned_64 := Shift_Right (W (2), Reason_Shift_Event);
      E : Event_Report;
      Found : Boolean;
   begin
      if W (0) > Word_Field or else not NL.Valid_Handle_Word (W (1)) then
         return (Valid => False);
      end if;
      if Event_Value = Unsigned_64 (Interface_Event'Enum_Rep (Attached)) then
         E.Event := Attached;
      elsif Event_Value = Unsigned_64 (Interface_Event'Enum_Rep (Detached)) then
         E.Event := Detached;
      elsif Event_Value = Unsigned_64 (Interface_Event'Enum_Rep (Changed)) then
         E.Event := Changed;
      else
         return (Valid => False);
      end if;
      Condition_Of (Condition_Value, E.Condition, Found);
      if not Found or else Ups_Value > Unsigned_64 (Up_Count'Last) then
         return (Valid => False);
      end if;
      E.Ups := Up_Count (Ups_Value);
      Found := False;
      for R in NL.Down_Reason loop
         if Unsigned_64 (NL.Down_Reason'Enum_Rep (R)) = Reason_Value then
            E.Reason := R;
            Found := True;
         end if;
         pragma Loop_Invariant
           (if Found then Unsigned_64 (NL.Down_Reason'Enum_Rep (E.Reason)) = Reason_Value);
      end loop;
      if not Found then
         return (Valid => False);
      end if;
      E.Generation := NL.Generation (Shift_Right (W (3), Generation_Shift_Event));
      E.Sequence := Unsigned_32 (W (0));
      E.Handle := NL.Handle_Of (W (1));
      E.Latest := Unsigned_32 (W (3) and Word_Field);
      if Encode (E) /= W then
         return (Valid => False);
      end if;
      return (Valid => True, Item => E);
   end Decode_Event;

   function Decode_Route (W : Words2) return Decoded_Route is
      Prefix_Value : constant Unsigned_64 := Field (W (0), Prefix_Shift_W2, Byte_Field);
      Metric_Value : constant Unsigned_64 := Shift_Right (W (0), Metric_Shift_W2);
      R : Route_Entry;
   begin
      if Prefix_Value > Unsigned_64 (IPv4_Prefix'Last) or else
        Metric_Value > Unsigned_64 (Route_Metric'Last)
      then
         return (Valid => False);
      end if;
      R := (Destination => IPv4_Word (W (0) and Word_Field),
            Prefix      => IPv4_Prefix (Prefix_Value),
            Usable      => (W (0) and Usable_Bit) /= 0,
            Metric      => Route_Metric (Metric_Value),
            Gateway     => IPv4_Word (W (1) and Word_Field),
            Slot        => NL.Interface_Slot (Shift_Right (W (1), Sequence_Shift_W3)));
      if Encode (R) /= W then
         return (Valid => False);
      end if;
      return (Valid => True, Item => R);
   end Decode_Route;

end CuBit.Net_Management;
