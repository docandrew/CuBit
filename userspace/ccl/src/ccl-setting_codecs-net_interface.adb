--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-codecs (CAP-003); do not edit. Regenerate with
--  make -C kernel setting-kinds after changing the catalog or a type.
pragma Ada_2022;
with Interfaces;

package body CCL.Setting_Codecs.Net_Interface with SPARK_Mode is
   package R renames CCL.Canonical_Readers;

   procedure Read_Prefix_Length
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Prefix_Length_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
      Value : Interfaces.Integer_64;
   begin
      R.Read_Integer (Source, Position, OK, Interfaces.Integer_64 (Prefix_Length_Value'First), Interfaces.Integer_64 (Prefix_Length_Value'Last), Value);
      Item := Prefix_Length_Value (Value);
   end Read_Prefix_Length;

   procedure Read_Gateway
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Gateway_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      Item := (others => <>);
      if not OK then
         return;
      elsif R.Next_Is_Word (Source, Position, "Gateway.None") then
         R.Expect_Word (Source, Position, OK, "Gateway.None");
         Item.Choice := None;
      elsif R.Next_Is (Source, Position, "(Gateway.Via ") then
         R.Expect (Source, Position, OK, "(Gateway.Via ");
         R.Read_Text (Source, Position, OK, Item.Via_Payload);
         R.Expect (Source, Position, OK, ")");
         Item.Choice := Via;
      else
         OK := False;
      end if;
   end Read_Gateway;

   procedure Read_Route_Metric
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Route_Metric_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
      Value : Interfaces.Integer_64;
   begin
      R.Read_Integer (Source, Position, OK, Interfaces.Integer_64 (Route_Metric_Value'First), Interfaces.Integer_64 (Route_Metric_Value'Last), Value);
      Item := Route_Metric_Value (Value);
   end Read_Route_Metric;

   procedure Read_Static_Interface
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Static_Interface_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      R.Expect (Source, Position, OK, "(Static_Interface");
      R.Expect (Source, Position, OK, " address => ");
      R.Read_Text (Source, Position, OK, Item.address);
      R.Expect (Source, Position, OK, " prefix => ");
      Read_Prefix_Length (Source, Position, OK, Item.prefix);
      R.Expect (Source, Position, OK, " gateway => ");
      Read_Gateway (Source, Position, OK, Item.gateway);
      R.Expect (Source, Position, OK, " metric => ");
      Read_Route_Metric (Source, Position, OK, Item.metric);
      R.Expect (Source, Position, OK, ")");
   end Read_Static_Interface;

   procedure Read_Net_Interface
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Net_Interface_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      Item := (others => <>);
      if not OK then
         return;
      elsif R.Next_Is_Word (Source, Position, "Net_Interface.Automatic") then
         R.Expect_Word (Source, Position, OK, "Net_Interface.Automatic");
         Item.Choice := Automatic;
      elsif R.Next_Is (Source, Position, "(Net_Interface.Static ") then
         R.Expect (Source, Position, OK, "(Net_Interface.Static ");
         Read_Static_Interface (Source, Position, OK, Item.Static_Payload);
         R.Expect (Source, Position, OK, ")");
         Item.Choice := Static;
      elsif R.Next_Is_Word (Source, Position, "Net_Interface.Disabled") then
         R.Expect_Word (Source, Position, OK, "Net_Interface.Disabled");
         Item.Choice := Disabled;
      else
         OK := False;
      end if;
   end Read_Net_Interface;

   procedure Decode (Source : String; Item : out Net_Interface_Value; Success : out Boolean) is
   begin
      Item := (others => <>);
      Success := R.Readable (Source) and then Source'Length in 1 .. R.Maximum_Canonical;
      if not Success then
         return;
      end if;
      declare
         Position : Positive := Source'First;
      begin
         Read_Net_Interface (Source, Position, Success, Item);
         R.Finish (Source, Position, Success);
      end;
      if not Success then
         Item := (others => <>);
      end if;
   end Decode;
end CCL.Setting_Codecs.Net_Interface;

