pragma Ada_2022;
with Interfaces;
with AML_Decode;
with AML_Names;
package AML_Container_Headers with SPARK_Mode, Pure is
   type Container_Kind is
     (Scope_Container, Device_Container, Processor_Container,
      Power_Resource_Container, Thermal_Zone_Container);
   type Processor_Identifier is range 0 .. 255;
   type Processor_Block_Address is new Interfaces.Unsigned_32;
   type Processor_Block_Length is range 0 .. 255;
   type Power_System_Level is range 0 .. 255;
   type Resource_Order is range 0 .. 65_535;
   type Attributes (Kind : Container_Kind := Scope_Container) is record
      case Kind is
         when Processor_Container =>
            Processor_ID : Processor_Identifier;
            Block_Address : Processor_Block_Address;
            Block_Length : Processor_Block_Length;
         when Power_Resource_Container =>
            System_Level : Power_System_Level;
            Order : Resource_Order;
         when others => null;
      end case;
   end record;
   type Truncation_Phase is (Opcode_Phase, Package_Phase, Name_Phase, Attribute_Phase);
   type Header_Status is (Accepted, Truncated, Bad_Package, Bad_Name, Unsupported);
   type Header_Result (Status : Header_Status := Truncated) is record
      case Status is
         when Accepted =>
            Metadata : Attributes;
            Name : AML_Names.Name_Result (AML_Names.Accepted);
            -- Relative counts from the opcode; Body_Offset may equal Extent.
            -- Only form an absolute body index when Body_Length is nonzero.
            Extent : Positive;
            Body_Offset : Positive;
            Body_Length : Natural;
         when Truncated => Phase : Truncation_Phase;
         when others => null;
      end case;
   end record;
   -- Data starts at the opcode and is already bounded by its enclosing package.
   -- Read_Package Truncated => Truncated; all other rejected package encodings
   -- => Bad_Package. Missing fixed tail/name bytes => Truncated. Malformed or
   -- excessive NameString => Bad_Name. Namespace resolution is not performed.
   -- Complete header syntax is checked before namespace resolution/insertion.
   -- NullName is syntactically accepted: non-Scope Count=0 rejection belongs to
   -- the owner; Scope root/current resolution likewise remains owner policy.
   function Read_Header (Data : AML_Decode.Bytes) return Header_Result with
     Post => (if Read_Header'Result.Status = Accepted then
       Read_Header'Result.Body_Offset <= Read_Header'Result.Extent
       and then Read_Header'Result.Extent <= Data'Length
       and then Read_Header'Result.Body_Length =
         Read_Header'Result.Extent - Read_Header'Result.Body_Offset);
end AML_Container_Headers;
