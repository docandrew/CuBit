--  The Linux preview has no secret store: a Linux-hosted stand-in, not
--  CuBit. Every call says secrets.svc is unavailable; nothing is stored.
package body CCL_Secrets is
   procedure Request_Value (Name : String; Result : out Store_Result) is
      pragma Unreferenced (Name);
   begin
      Result := Unavailable;
   end Request_Value;
   function Value_Requested return Boolean is (False);
   function Requested_Name return String is ("");
   procedure Store_Value (Value : String; Result : out Store_Result) is
      pragma Unreferenced (Value);
   begin
      Result := Not_Requested;
   end Store_Value;
   procedure Cancel_Value is null;
   procedure Forget (Name : String; Result : out Call_Result) is
      pragma Unreferenced (Name);
   begin
      Result := Unavailable;
   end Forget;
   procedure Status
     (Name : String; Stored : out Boolean; Holder : out String; Holder_Length : out Natural;
      Result : out Call_Result)
   is
      pragma Unreferenced (Name);
   begin
      Stored := False;
      Holder := [others => ' '];
      Holder_Length := 0;
      Result := Unavailable;
   end Status;
   procedure Revoke_Use (Name : String; Result : out Call_Result) is
      pragma Unreferenced (Name);
   begin
      Result := Unavailable;
   end Revoke_Use;
end CCL_Secrets;
