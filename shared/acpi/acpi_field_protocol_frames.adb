pragma Ada_2022;
with ACPI_NVS_Protocol;
with ACPI_PCI_Protocol;
with ACPI_Global_Lock_Protocol;
with ACPI_Locked_NVS_Protocol;
package body ACPI_Field_Protocol_Frames with SPARK_Mode is
   package N renames ACPI_NVS_Protocol;
   package C renames ACPI_PCI_Protocol;
   package G renames ACPI_Global_Lock_Protocol;
   package D renames ACPI_Locked_NVS_Protocol;
   use type D.Reply_Kind;
   function Owned_Label (Label : Interfaces.Unsigned_32) return Boolean is
     (Label in N.Header_Operation | N.Describe_Operation | N.Read_Operation
      or else G.Owned_Label (Label) or else D.Owned_Label (Label) or else C.Owned_Label (Label));
   function Request_Valid (Item : Packet) return Boolean is
   begin
      if C.Owned_Label (Item.Label) then return C.Decode_Request (Item).Valid;
      elsif G.Owned_Label (Item.Label) then return G.Decode_Request (Item).Valid;
      elsif D.Owned_Label (Item.Label) then return D.Decode_Request (Item).Valid;
      else return N.Decode_Request (Item).Valid;
      end if;
   end Request_Valid;
   function Reply_Valid (Request, Reply : Packet) return Boolean is
   begin
      if not Request_Valid (Request) then return False; end if;
      if C.Owned_Label (Request.Label) then
         declare Input : constant C.Request := C.Decode_Request (Request); begin
            return C.Decode_Reply (Reply, Input.Kind, Input.Width).Valid;
         end;
      elsif G.Owned_Label (Request.Label) then return G.Decode_Reply (Reply, G.Decode_Request (Request).Kind).Valid;
      elsif D.Owned_Label (Request.Label) then
         declare
            Input : constant D.Request := D.Decode_Request (Request);
            Output : constant D.Reply := D.Decode_Reply (Input.Kind, Reply, D.T.Qword_Access);
         begin
            if not Output.Valid then return False;
            elsif Output.Kind = D.Prepared then return D.Matches_Selection (Output, Input.ID, Input.Datum);
            elsif Output.Kind in D.Recorded | D.Cached then return D.Matches_Read (Output, Input.Descriptor, Input.Sequence);
            else return True;
            end if;
         end;
      else
         declare Input : constant N.Request := N.Decode_Request (Request); begin
            return N.Decode_Reply (Reply, Input.Kind, Input.Width).Valid;
         end;
      end if;
   end Reply_Valid;
end ACPI_Field_Protocol_Frames;
