pragma Ada_2022;
package body ACPI_Region_Policy with SPARK_Mode is
   function Active (S : State) return Boolean is (S.Enabled);
   function Epoch (S : State) return Unsigned_64 is (S.Version);
   function Policy (S : State) return Configuration is (S.Config);
   function Busy (S : State) return Boolean is (S.In_Flight);
   function Receipt (S : State) return Unsigned_64 is (S.Sequence);
   procedure Install (S : in out State; C : Configuration; Accepted : out Boolean) is
   begin
      Accepted := False;
      if S.Enabled or else S.In_Flight or else not Valid (C) or else S.Version = Unsigned_64'Last then return; end if;
      S.Config := C;
      S.Version := S.Version + 1;
      S.Enabled := True;
      Accepted := True;
   end Install;
   procedure Revoke (S : in out State) is
   begin
      S.Enabled := False;
   end Revoke;
   function Resolve
     (S : State; Stamp, Token, Offset : Unsigned_64;
      Width : Access_Width; For_Write : Boolean) return Decision is
      Bytes : constant Unsigned_64 := Unsigned_64 (ACPI_FADT.Transactions.Octets (Width));
      Address : Unsigned_64;
   begin
      if not S.Enabled or else Stamp /= S.Config.Tag or else Token /= S.Version
        or else not S.Config.Widths (Width)
        or else (if For_Write then not S.Config.Writable else not S.Config.Readable)
      then return (Allowed => False); end if;
      if Offset >= S.Config.Length or else Bytes > S.Config.Length - Offset then
         return (Allowed => False);
      end if;
      Address := S.Config.Base + Offset;
      if not ACPI_FADT.Transactions.Aligned (Address, Width) then
         return (Allowed => False);
      end if;
      return (Allowed => True, Address => Address);
   end Resolve;
   procedure Begin_Access
     (S : in out State; Stamp, Token, Offset : Unsigned_64;
      Width : Access_Width; For_Write : Boolean;
      Result : out Decision; Ticket : out Unsigned_64) is
   begin
      Ticket := 0;
      Result := (Allowed => False);
      if S.In_Flight or else S.Sequence = Unsigned_64'Last then return; end if;
      Result := Resolve (S, Stamp, Token, Offset, Width, For_Write);
      if not Result.Allowed then return; end if;
      S.Sequence := S.Sequence + 1;
      S.In_Flight := True;
      Ticket := S.Sequence;
   end Begin_Access;
   procedure Finish_Access
     (S : in out State; Ticket : Unsigned_64; Accepted : out Boolean) is
   begin
      Accepted := S.In_Flight and then Ticket = S.Sequence;
      if Accepted then S.In_Flight := False; end if;
   end Finish_Access;
end ACPI_Region_Policy;
