------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  secrets.svc v0, the secret store (SEC-022): named secrets that programs
--  use through grants instead of environment variables, .env files, argv
--  or settings.
--
--  @description
--  v0 keeps values in memory only: nothing is written anywhere, so a
--  reboot forgets them, and encryption at rest (sealed under a TPM-held
--  key on the laptop) is a later slice, not done. A value enters through
--  Store (the console's non-echoing (secrets.set ...)) and is never read
--  back by the storer: only a grant holder can Use it.
--
--  A use grant names a secret and a holder: a registered service role, so
--  the kernel's service registry (Registered_Driver) says which process
--  holds it, and procmgr lets only the trusted startup plan's program of
--  that identity register the role. v0 takes its grants from its own
--  typed startup parameters (the trusted plan); Revoke_Use withdraws one
--  until the next boot. Every use is logged by name, never by value.
--
--  "Use without seeing" (sign, MAC, decrypt, add an auth header inside
--  secrets.svc) is the direction; v0's Use hands the holder the value in
--  its own page, which it must wipe, never log and never export.
--
--  Calls (all answered at once). The client's page holds the name (and
--  for Store the value after it); words = [grant slot, grant generation,
--  name length, value length (Store only)]:
--  - Store -> OK, or ERR [Refusal]. secrets.svc wipes the client's page.
--  - Forget -> OK (whether or not it was stored).
--  - Status -> OK [stored (0/1), holder role (0: none)].
--  - Use -> OK [value length], the value at offset 0 of the caller's page;
--    ERR Not_Granted unless the caller is the grant's registered holder.
--  - Revoke_Use -> OK: the grant is withdrawn (Use is refused).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.Secrets_Protocol with Pure, SPARK_Mode is

   --  Registered service identity and catalog service number.
   Service_Role : constant Unsigned_64 := 38;

   Store_Operation      : constant Unsigned_32 := 16#0D21#;
   Forget_Operation     : constant Unsigned_32 := 16#0D22#;
   Status_Operation     : constant Unsigned_32 := 16#0D23#;
   Use_Operation        : constant Unsigned_32 := 16#0D24#;
   Revoke_Use_Operation : constant Unsigned_32 := 16#0D25#;

   Reply_OK    : constant Unsigned_32 := 16#F000#;
   Reply_Error : constant Unsigned_32 := 16#F001#;

   type Refusal is
     (Malformed_Request, Bad_Name, Bad_Value, Not_Stored, Not_Granted, Full);
   for Refusal use
     (Malformed_Request => 1, Bad_Name => 2, Bad_Value => 3, Not_Stored => 4,
      Not_Granted => 5, Full => 6);

   function Name (Value : Refusal) return String is
     (case Value is
         when Malformed_Request => "malformed request",
         when Bad_Name => "a secret name is 1 .. 64 of a-z, 0-9 and -",
         when Bad_Value => "a value is 1 .. 256 printable ASCII characters",
         when Not_Stored => "no value is stored under that name",
         when Not_Granted => "this program holds no use grant for that secret",
         when Full => "the store holds as many secrets as it can");

   Maximum_Name_Bytes : constant := 64;
   --  v0 values: printable tokens (API keys, passphrases).
   Maximum_Value_Bytes : constant := 256;

   function Valid_Name (Name : String) return Boolean is
     (Name'Length in 1 .. Maximum_Name_Bytes and then
      (for all C of Name => C in 'a' .. 'z' | '0' .. '9' | '-'));
   function Valid_Value (Value : String) return Boolean is
     (Value'Length in 1 .. Maximum_Value_Bytes and then
      (for all C of Value => C in ' ' .. '~'));
end CuBit.Secrets_Protocol;
