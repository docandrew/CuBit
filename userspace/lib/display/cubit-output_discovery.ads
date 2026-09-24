pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Desktop_Protocol;

--  Read-only discovery on already authorized service endpoints. Catalog IDs
--  are scoped to that endpoint lifetime, not capabilities or durable names.
package CuBit.Output_Discovery with SPARK_Mode, Pure is
   package DP renames CuBit.Desktop_Protocol;
   use type DP.Wire_Message;
   type Provider is (Display_Broker, GPU_Backend);
   type Operation is (Get_Catalog, Get_Description);
   function Code (Service : Provider; Action : Operation) return Unsigned_32 is
     (case Service is
        when Display_Broker =>
          (case Action is when Get_Catalog => 16#090D#,
                          when Get_Description => 16#090E#),
        when GPU_Backend =>
          (case Action is when Get_Catalog => 16#0A08#,
                          when Get_Description => 16#0A09#));
   subtype Catalog_Revision is Unsigned_64 range 1 .. Unsigned_64'Last;
   --  Sixteen backend scanouts plus a distinct firmware output when selected.
   subtype Output_Count is Natural range 0 .. 17;
   subtype Output_Index is Output_Count range 1 .. Output_Count'Last;
   subtype Native_Output_Number is Natural range 0 .. 15;
   subtype Extent is Natural range 1 .. 65_535;
   type Output_Source is (Boot_Framebuffer, Virtio_GPU);
   for Output_Source use (Boot_Framebuffer => 1, Virtio_GPU => 2);
   type Output_Role is
     (Detected_Only, Backend_Ready, Selected_For_Desktop);
   for Output_Role use
     (Detected_Only => 0, Backend_Ready => 1, Selected_For_Desktop => 2);
   subtype Active_Role is Output_Role range
     Backend_Ready .. Selected_For_Desktop;
   type Description (Role : Output_Role := Detected_Only) is record
      Source : Output_Source := Boot_Framebuffer;
      Native_Number : Native_Output_Number := 0;
      Advertised_Width, Advertised_Height : Extent := 1;
      case Role is
         when Detected_Only => null;
         when Backend_Ready | Selected_For_Desktop =>
            Current_Width, Current_Height : Extent := 1;
      end case;
   end record;
   type Description_Array is array (Output_Index) of Description;
   type Catalog is record
      Count : Output_Count := 0;
      Items : Description_Array := [others => <>];
   end record;
   type Summary is record
      Revision : Catalog_Revision := 1;
      Count : Output_Count := 0;
   end record;
   type Query is record
      Revision : Catalog_Revision;
      Index : Output_Index;
   end record;
   type Query_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Query;
         when False => null;
      end case;
   end record;
   type Summary_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Summary;
         when False => null;
      end case;
   end record;
   type Description_Result is record
      Requested : Query;
      Item : Description;
   end record;
   type Description_Decoding (Valid : Boolean := False) is record
      case Valid is
         when True => Value : Description_Result;
         when False => null;
      end case;
   end record;
   function Catalog_Request (Service : Provider) return DP.Wire_Message is
     (Label => Code (Service, Get_Catalog), Length => 4, others => <>);
   function Valid_Catalog_Request
     (Service : Provider; Wire : DP.Wire_Message) return Boolean is
     (Wire = Catalog_Request (Service));
   function Decode_Query (Service : Provider; Wire : DP.Wire_Message)
      return Query_Decoding;
   function Encode_Query (Service : Provider; Item : Query)
      return DP.Wire_Message with
      Post => Decode_Query (Service, Encode_Query'Result) = (True, Item);
   function Decode_Summary (Service : Provider; Wire : DP.Wire_Message)
      return Summary_Decoding;
   function Encode_Summary (Service : Provider; Item : Summary)
      return DP.Wire_Message with
      Post => Decode_Summary (Service, Encode_Summary'Result) = (True, Item);
   function Encode_Description (Service : Provider; Item : Description_Result)
      return DP.Wire_Message;
   function Decode_Description (Service : Provider; Wire : DP.Wire_Message)
      return Description_Decoding;
   --  Invalid input/stale revision is a one-word typed DP status, never a
   --  partially successful descriptor. Revision changes require a new walk.
   function Respond
     (Service : Provider; Data : Catalog; Revision : Catalog_Revision;
      Request : DP.Wire_Message) return DP.Wire_Message with
      Post =>
        (if Decode_Query (Service, Request).Valid and then
            Decode_Query (Service, Request).Value.Revision /= Revision
         then Respond'Result =
           (Request.Label, 1, 0, 0,
            [DP.Status_Code'Enum_Rep (DP.Bad_State), 0, 0, 0]));
end CuBit.Output_Discovery;
