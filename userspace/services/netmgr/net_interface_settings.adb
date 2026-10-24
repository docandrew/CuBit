------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CCL.Typed_Settings;
with CCL.Setting_Kinds;
with CCL.Objects;
with CCL.Objects.Views;
with CCL.Interfaces.Net_Interface;
with CuBit.Net_Address;
with CuBit.Settings.Codecs;

package body Net_Interface_Settings is
   package Views renames CCL.Objects.Views;
   package TS renames CCL.Typed_Settings;
   package NI renames CCL.Interfaces.Net_Interface;
   package Addresses renames CuBit.Settings.Codecs.Address_Codec;

   --  The schema's member and field names (NI.TYPE_SOURCE).
   Automatic_Member : constant String := "Automatic";
   Static_Member    : constant String := "Static";
   Disabled_Member  : constant String := "Disabled";
   Address_Field    : constant String := "address";
   Prefix_Field     : constant String := "prefix";
   Gateway_Field    : constant String := "gateway";
   Via_Member       : constant String := "Via";
   Metric_Field     : constant String := "metric";

   pragma Compile_Time_Error
     (NI.MAXIMUM_METRIC /= Long_Long_Integer (NM.Route_Metric'Last),
      "the CCL metric bound must be the management contract's");

   --  In the program's data, not the heap: one heap block this size would
   --  open a whole medium-block arena for netmgr (CuAlloc commits 1 MiB).
   Work : Views.Snapshot;

   procedure Decode (Source : String; Item : out Value; Success : out Boolean) is
      Read : Boolean;
   begin
      Item := Default;
      Success := False;
      TS.Read (CCL.Setting_Kinds.Net_Interface_Setting, Source, Work, Read);
      if not Read then
         return;
      end if;
      declare
         Root : constant Views.Cursor := Views.Root (Work);
         Member : constant String := TS.Alternative_Name (Work, Root);
      begin
         if Member = Automatic_Member then
            Item := (Kind => Automatic);
            Success := True;
         elsif Member = Disabled_Member then
            Item := (Kind => Disabled);
            Success := True;
         elsif Member = Static_Member then
            declare
               Fields : constant Views.Cursor := Views.Payload (Work, Root);
               function Field_Text (Name : String) return String is
                 (Views.Text (Work, TS.Named (Work, Fields, Name)));
               function Field_Number (Name : String) return Integer_64 is
                 (CCL.Objects.Integer_Of (Views.Scalar (Work, TS.Named (Work, Fields, Name))));
               Address, Gateway : CuBit.Net_Address.Address := CuBit.Net_Address.Unspecified;
               Address_OK : Boolean;
               Gateway_OK : Boolean := True;
               Gateway_Choice : constant Views.Cursor := TS.Named (Work, Fields, Gateway_Field);
               Has_Gateway : constant Boolean := TS.Alternative_Name (Work, Gateway_Choice) = Via_Member;
               Prefix : constant Integer_64 := Field_Number (Prefix_Field);
               Metric : constant Integer_64 := Field_Number (Metric_Field);
               Mapped_Prefix : constant := CuBit.Net_Address.Mapped_Prefix;
            begin
               Addresses.Decode (Field_Text (Address_Field), Address, Address_OK);
               if Has_Gateway then
                  Addresses.Decode (Views.Text (Work, Views.Payload (Work, Gateway_Choice)),
                                    Gateway, Gateway_OK);
                  Gateway_OK := Gateway_OK and then CuBit.Net_Address.Is_Mapped (Gateway);
               end if;
               --  The schema bounds the prefix (0 .. 128) and the metric.
               if Address_OK and then CuBit.Net_Address.Is_Mapped (Address) and then Gateway_OK
                 and then Prefix in Mapped_Prefix .. Mapped_Prefix + Integer_64 (NM.IPv4_Prefix'Last)
                 and then Metric in 0 .. Integer_64 (NM.Route_Metric'Last)
               then
                  Item := (Kind => Static,
                           Settings => (Address => NM.Word_Of (Address),
                                        Prefix  => NM.IPv4_Prefix (Prefix - Mapped_Prefix),
                                        Gateway => (if Has_Gateway
                                                    then NM.Word_Of (Gateway) else NM.No_IPv4),
                                        Metric  => NM.Route_Metric (Metric)));
                  Success := True;
               end if;
            end;
         end if;
      end;
   end Decode;

end Net_Interface_Settings;
