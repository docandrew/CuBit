--  secrets.svc for secrets.* (CCL.Interfaces.Secrets), one body per
--  platform: native/ calls the service (CuBit.Secrets_Protocol); the Linux
--  preview's host/ has no store (Unavailable), a stand-in, not CuBit.
package CCL_Secrets is
   MAXIMUM_NAME : constant := 64;
   MAXIMUM_VALUE : constant := 256;

   function Valid_Name (Name : String) return Boolean is
     (Name'Length in 1 .. MAXIMUM_NAME and then
      (for all C of Name => C in 'a' .. 'z' | '0' .. '9' | '-'));

   type Store_Result is
     (Stored, Not_Granted, Unavailable, Bad_Name, Bad_Value, Full, Not_Requested);
   function Explain (R : Store_Result) return String is
     (case R is
        when Stored => "stored",
        when Not_Granted => "the program's manifest must request the secrets service (service ""secrets"" ""secrets"")",
        when Unavailable => "secrets.svc did not answer",
        when Bad_Name => "a secret name is 1 .. 64 of a-z, 0-9 and -",
        when Bad_Value => "a value is 1 .. 256 printable ASCII characters",
        when Full => "the store holds as many secrets as it can",
        when Not_Requested => "no secret entry was asked for");

   --  (secrets.set s) asks the console for the value: the next line typed
   --  is not echoed, not kept in history, and goes to secrets.svc
   --  (Store_Value), never to the program. Refused at once without the
   --  secrets service.
   procedure Request_Value (Name : String; Result : out Store_Result);
   function Value_Requested return Boolean;
   function Requested_Name return String;
   procedure Store_Value (Value : String; Result : out Store_Result);
   procedure Cancel_Value;

   type Call_Result is (Done, Not_Granted, Unavailable, Bad_Name);
   procedure Forget (Name : String; Result : out Call_Result);
   --  Holder: the service that may use it ("" for none).
   procedure Status
     (Name : String; Stored : out Boolean; Holder : out String; Holder_Length : out Natural;
      Result : out Call_Result);
   procedure Revoke_Use (Name : String; Result : out Call_Result);
end CCL_Secrets;
