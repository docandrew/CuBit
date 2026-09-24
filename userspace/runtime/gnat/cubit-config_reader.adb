pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;

package body CuBit.Config_Reader is
   procedure Query
     (Op : CuBit.Config_Inspection.Operation; Key : String;
      Value : out CuBit.Config_Inspection.Text;
      Result : out CuBit.Config_Inspection.Status;
      Context : CuBit.Config_Inspection.Context_ID :=
        CuBit.Config_Inspection.Machine_Context)
   is
      use CuBit.Config_Inspection;
      Buffer : aliased String (1 .. 4096) := [others => Character'Val (0)]
        with Alignment => 4096;
      Reference : CuBit.Memory_Grants.Grant_Reference;
      Created, Retired : Boolean;
      Ignore : Unsigned_64;
      Msg : Message := NULL_MESSAGE;
   begin
      Value := (others => <>);
      Result := Invalid_Request;
      if Key'Length > 128 or else (Op = Read_Value and Key'Length = 0) then
         return;
      end if;
      Msg.tag := (label => Operation'Enum_Rep (Op), length => 4,
                  flags => 0, reserved => 0);
      Msg.words (3) := Unsigned_64 (Context);
      if Op /= Probe then
         CuBit.Memory_Grants.Create_Via_Capability
           (CAP_SLOT_CONFIG, Buffer'Address, 1, True, Reference, Created);
         if not Created then
            Result := Unavailable;
            return;
         end if;
         Buffer (1 .. Key'Length) := Key;
         Msg.words (0) := Reference.slot;
         Msg.words (1) := Reference.generation;
         Msg.words (2) := Unsigned_64 (Key'Length);
      end if;
      Msg.tag := capCall (CAP_SLOT_CONFIG, Msg);
      Result := Unavailable;
      for Candidate in Status loop
         if Msg.tag.label = Status'Enum_Rep (Candidate) then
            Result := Candidate;
         end if;
      end loop;
      if Result = OK and Op /= Probe then
         if Msg.tag.length /= 1 or else Msg.words (0) > Maximum_Text then
            Result := Invalid_Request;
         else
            Value.Length := Natural (Msg.words (0));
            Value.Data (1 .. Value.Length) := Buffer (1 .. Value.Length);
         end if;
      end if;
      if Op /= Probe then
         CuBit.Memory_Grants.Revoke (Reference, Retired);
         --  Accepted revocation alone is not permission to reuse stack pages.
         --  Config releases before replying, so this normally needs one query.
         --  Never unwind this frame while another process can still access it.
         while not CuBit.Memory_Grants.Retirement_Confirmed (Reference) loop
            Ignore := syscall (SYSCALL_SLEEP, 1);
            CuBit.Memory_Grants.Revoke (Reference, Retired);
         end loop;
         if not Retired then
            Result := Unavailable;
            Value := (others => <>);
         end if;
      end if;
   end Query;
end CuBit.Config_Reader;
