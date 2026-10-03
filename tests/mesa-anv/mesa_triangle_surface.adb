with CCL_Manifest_Bindings;
with CuBit.Messages;
with CuBit.Desktop_Messages;
with CuBit.Desktop_Protocol;
package body Mesa_Triangle_Surface is
   package M renames CuBit.Messages;
   package D renames CuBit.Desktop_Protocol;
   use type D.Status_Code;
   use type D.Wire_Message;
   function Desktop_Slot return Unsigned_64 is
     (CCL_Manifest_Bindings.Slot_Desktop);
   function Send (Request : D.Wire_Message) return D.Wire_Message is
      Message : M.Message := CuBit.Desktop_Messages.From_Wire (Request);
   begin
      Message.tag := M.capCall (M.CapabilitySlot (Desktop_Slot), Message);
      return CuBit.Desktop_Messages.To_Wire (Message);
   end Send;
   function Create (Width, Height : Unsigned_32) return Unsigned_64 is
      Created : D.Creation_Result;
   begin
      if Width not in 1 .. 4096 or else Height not in 1 .. 4096 then return 0; end if;
      if D.Decode_Hello_Result (Send (D.Encode_Hello (D.Current_Revision))).Status /=
        D.Success then return 0; end if;
      Created := D.Decode_Creation_Result
        (Send (D.Encode_Create ((D.Pixel_Extent (Width), D.Pixel_Extent (Height), D.Window_Surface))));
      --  Desktop may enlarge the decorated window. The independent image
      --  attachment stays unscaled at the client origin; composition clips
      --  reads to its extent and fills the unused client area itself.
      if Created.Status /= D.Success then return 0; end if;
      return Unsigned_64 (Created.Surface);
   end Create;
   function Present (Surface : Unsigned_64; Width, Height : Unsigned_32) return Unsigned_32 is
   begin
      if Surface = 0 or else Width not in 1 .. 4096 or else Height not in 1 .. 4096 then return 1; end if;
      return (if Send (D.Encode_Present
        ((D.Live_Surface_Name (Surface), (0, 0, D.Pixel_Extent (Width), D.Pixel_Extent (Height))))) =
        D.Encode_Status (D.Present_Surface, D.Success) then 0 else 1);
   end Present;
   function Destroy (Surface : Unsigned_64) return Unsigned_32 is
   begin
      if Surface = 0 then return 1; end if;
      return (if Send (D.Encode_Destroy ((Surface => D.Live_Surface_Name (Surface)))) =
        D.Encode_Status (D.Destroy_Surface, D.Success) then 0 else 1);
   end Destroy;
end Mesa_Triangle_Surface;
