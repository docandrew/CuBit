------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CCL.Canonical_Readers;
with CCL.Interfaces.Net_Interface;
with CCL.Setting_Codecs.Net_Interface;
with CuBit.Net_Address;
with CuBit.Settings.Codecs;

package body Net_Interface_Settings is
   package NI renames CCL.Interfaces.Net_Interface;
   package Codec renames CCL.Setting_Codecs.Net_Interface;
   package Addresses renames CuBit.Settings.Codecs.Address_Codec;
   use type Codec.Net_Interface_Choice;
   use type Codec.Gateway_Choice;
   use type Codec.Prefix_Length_Value;

   pragma Compile_Time_Error
     (NI.MAXIMUM_METRIC /= Long_Long_Integer (NM.Route_Metric'Last)
      or else Long_Long_Integer (Codec.Route_Metric_Value'Last) /= Long_Long_Integer (NM.Route_Metric'Last)
      or else Long_Long_Integer (Codec.Route_Metric_Value'First) /= Long_Long_Integer (NM.Route_Metric'First),
      "the CCL metric bounds must be the management contract's");

   Mapped_Prefix : constant := CuBit.Net_Address.Mapped_Prefix;
   --  The prefixes of a mapped IPv4 network: an IPv4 prefix length plus 96.
   subtype Mapped_Prefix_Length is Codec.Prefix_Length_Value
     range Mapped_Prefix .. Mapped_Prefix + Codec.Prefix_Length_Value (NM.IPv4_Prefix'Last);

   procedure Decode (Source : String; Item : out Value; Success : out Boolean) is
      Setting : Codec.Net_Interface_Value;
   begin
      Item := Default;
      Codec.Decode (Source, Setting, Success);
      if not Success then
         return;
      end if;
      case Setting.Choice is
         when Codec.Automatic =>
            Item := (Kind => Automatic);
         when Codec.Disabled =>
            Item := (Kind => Disabled);
         when Codec.Static =>
            declare
               Fields : Codec.Static_Interface_Value renames Setting.Static_Payload;
               Has_Gateway : constant Boolean := Fields.gateway.Choice = Codec.Via;
               Address, Gateway : CuBit.Net_Address.Address := CuBit.Net_Address.Unspecified;
               Address_OK : Boolean;
               Gateway_OK : Boolean := True;
            begin
               Addresses.Decode (CCL.Canonical_Readers.Image (Fields.address), Address, Address_OK);
               if Has_Gateway then
                  Addresses.Decode (CCL.Canonical_Readers.Image (Fields.gateway.Via_Payload), Gateway, Gateway_OK);
                  Gateway_OK := Gateway_OK and then CuBit.Net_Address.Is_Mapped (Gateway);
               end if;
               Success := Address_OK and then CuBit.Net_Address.Is_Mapped (Address) and then Gateway_OK
                 and then Fields.prefix in Mapped_Prefix_Length;
               if Success then
                  Item := (Kind => Static,
                           Settings => (Address => NM.Word_Of (Address),
                                        Prefix  => NM.IPv4_Prefix (Fields.prefix - Mapped_Prefix),
                                        Gateway => (if Has_Gateway then NM.Word_Of (Gateway) else NM.No_IPv4),
                                        Metric  => NM.Route_Metric (Fields.metric)));
               end if;
            end;
      end case;
   end Decode;

end Net_Interface_Settings;
