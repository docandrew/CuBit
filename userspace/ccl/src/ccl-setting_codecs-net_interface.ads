--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-codecs (CAP-003); do not edit. Regenerate with
--  make -C kernel setting-kinds after changing the catalog or a type.
pragma Ada_2022;
with CuBit.Canonical_Readers;

--  The typed setting net.interface.<member>: a CCL Net_Interface (CCL.Interfaces.Net_Interface.TYPE_SOURCE),
--  as Ada types, and Decode, which reads its canonical source
--  (CCL.Typed_Settings renders it; CuBit.Canonical_Readers).
package CCL.Setting_Codecs.Net_Interface with SPARK_Mode, Pure is
   subtype Text is CuBit.Canonical_Readers.Text;
   type Prefix_Length_Value is range 0 .. 128;
   type Gateway_Choice is (None, Via);
   type Gateway_Value is record
      Choice : Gateway_Choice := Gateway_Choice'First;
      Via_Payload : Text := CuBit.Canonical_Readers.Empty_Text;
   end record;
   type Route_Metric_Value is range 0 .. 65535;
   type Static_Interface_Value is record
      address : Text := CuBit.Canonical_Readers.Empty_Text;
      prefix : Prefix_Length_Value := Prefix_Length_Value'First;
      gateway : Gateway_Value := (others => <>);
      metric : Route_Metric_Value := Route_Metric_Value'First;
   end record;
   type Net_Interface_Choice is (Automatic, Static, Disabled);
   type Net_Interface_Value is record
      Choice : Net_Interface_Choice := Net_Interface_Choice'First;
      Static_Payload : Static_Interface_Value := (others => <>);
   end record;

   --  Item is the value when Success: Source is exactly the canonical
   --  source of a Net_Interface. Otherwise Item is (others => <>).
   procedure Decode (Source : String; Item : out Net_Interface_Value; Success : out Boolean)
   with Global => null, Always_Terminates;
end CCL.Setting_Codecs.Net_Interface;

