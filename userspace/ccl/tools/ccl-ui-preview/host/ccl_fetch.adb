------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The Linux preview has no tls.svc: a Linux-hosted stand-in, not CuBit.
--  Fetches are refused as unavailable; nothing reaches any network.
------------------------------------------------------------------------------
package body CCL_Fetch is
   function Short (Text : String) return Short_Text is
      N : constant Natural := Natural'Min (Text'Length, MAXIMUM_TEXT);
      Result : Short_Text;
   begin
      Result.Data (1 .. N) := Text (Text'First .. Text'First + N - 1);
      Result.Length := N;
      return Result;
   end Short;

   procedure Start
     (Url, Root, Path, Name : String; Pinned : Boolean; Pin_Hex : String; Existing : Existing_Policy;
      Max_Bytes : Unsigned_64; Max_Redirects : Natural; Deadline_Ms : Positive; Now_Ms : Unsigned_64;
      Id : out Unsigned_64; Result : out Start_Result; Why : out Short_Text)
   is
      pragma Unreferenced (Url, Root, Path, Name, Pinned, Pin_Hex, Existing, Max_Bytes, Max_Redirects,
                           Deadline_Ms, Now_Ms);
   begin
      Id := 0;
      Result := Unavailable;
      Why := Short ("the Linux preview has no tls.svc");
   end Start;

   procedure Poll (Id : Unsigned_64; Item : out Report; Found : out Boolean) is
      pragma Unreferenced (Id);
   begin
      Item := (others => <>);
      Found := False;
   end Poll;

   procedure Cancel (Id : Unsigned_64) is null;
   procedure Forget (Id : Unsigned_64) is null;
   function Running return Boolean is (False);

   procedure Pump (Now_Ms : Unsigned_64; Next_Due : out Unsigned_64) is
      pragma Unreferenced (Now_Ms);
   begin
      Next_Due := Unsigned_64'Last;
   end Pump;
end CCL_Fetch;
