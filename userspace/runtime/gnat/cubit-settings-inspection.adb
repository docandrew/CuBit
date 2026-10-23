------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Config_Protocol;
with CuBit.Memory_Grants;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Settings.Transport;

package body CuBit.Settings.Inspection is
   package T renames CuBit.Settings.Transport;

   procedure Read (Through : Inspector; Key : String; Item : out Text; Result : out Outcome) is
      Status : T.Status;
   begin
      T.Get (Through.Service, Key, Item, Status);
      Result := (case Status is
                    when T.OK        => Present,
                    when T.Not_Found => Absent,
                    when T.Refused   => Denied,
                    when T.Failed    => Unavailable);
   end Read;

   procedure Write (Through : Inspector; Key : String; Item : String; Result : out Change_Outcome) is
      Status : T.Status;
   begin
      if Item'Length > Maximum_Text then
         Result := Too_Long;
         return;
      end if;
      T.Set (Through.Service, Key, Item, Status);
      Result := T.Changed (Status);
   end Write;

   procedure Delete (Through : Inspector; Key : String; Result : out Change_Outcome) is
      Status : T.Status;
   begin
      T.Delete (Through.Service, Key, Status);
      Result := T.Changed (Status);
   end Delete;

   procedure For_Each (Through : Inspector; Prefix : String; Result : out Outcome) is
      Names : Text;
      Count : Natural;
      Status : T.Status;
      First : Positive := 1;
   begin
      T.List (Through.Service, Prefix, Names, Count, Status);
      case Status is
         when T.OK        => Result := Present;
         when T.Not_Found => Result := Absent; return;
         when T.Refused   => Result := Denied; return;
         when T.Failed    => Result := Unavailable; return;
      end case;
      for I in 1 .. Names.Length loop
         if Names.Data (I) = ASCII.NUL then
            if I > First then
               Visit (Names.Data (First .. I - 1));
            end if;
            First := I + 1;
         end if;
      end loop;
   end For_Each;

   procedure Inspect
     (Through : Inspector; Op : CuBit.Config_Inspection.Operation; Key : String;
      Context : CuBit.Config_Inspection.Context_ID;
      Value : out CuBit.Config_Inspection.Text; Result : out CuBit.Config_Inspection.Status)
   is
      use CuBit.Config_Inspection;
      Page_Bytes : constant := 4096;
      --  Each call lends its own page: no pointer into a shared buffer.
      Buffer : aliased String (1 .. Page_Bytes) := [others => ASCII.NUL]
        with Alignment => Page_Bytes;
      Service : constant CuBit.Handles.Endpoint := CuBit.Handles.Services.Config.Handle (Through.Service);
      Reference : CuBit.Memory_Grants.Grant_Reference;
      Created, Retired : Boolean;
      Ignore : Unsigned_64;
      Msg : Message := NULL_MESSAGE;
   begin
      Value := (others => <>);
      Result := Invalid_Request;
      if Key'Length > CuBit.Config_Protocol.Maximum_Key or else (Op = Read_Value and Key'Length = 0) then
         return;
      end if;
      Msg.tag := (label => Operation'Enum_Rep (Op), length => 4,
                  protocol => 0, ipc => <>, reserved => 0);
      Msg.words (3) := Unsigned_64 (Context);
      if Op /= Probe then
         CuBit.Memory_Grants.Create_Via_Capability (Service, Buffer'Address, 1, True, Reference, Created);
         if not Created then
            Result := Unavailable;
            return;
         end if;
         Buffer (1 .. Key'Length) := Key;
         Msg.words (0) := Reference.slot;
         Msg.words (1) := Reference.generation;
         Msg.words (2) := Unsigned_64 (Key'Length);
      end if;
      Msg.tag := capCall (Service, Msg, CuBit.Messages.Wait_Forever);
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
   end Inspect;

end CuBit.Settings.Inspection;
