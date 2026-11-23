------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  net.fetch on CuBit: CCL_Fetch_Engine does the work.
------------------------------------------------------------------------------
with CCL_Fetch_Engine;

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
      Id : out Unsigned_64; Result : out Start_Result; Why : out Short_Text) renames CCL_Fetch_Engine.Start;
   procedure Poll (Id : Unsigned_64; Item : out Report; Found : out Boolean) renames CCL_Fetch_Engine.Poll;
   procedure Cancel (Id : Unsigned_64) renames CCL_Fetch_Engine.Cancel;
   procedure Forget (Id : Unsigned_64) renames CCL_Fetch_Engine.Forget;
   function Running return Boolean renames CCL_Fetch_Engine.Running;
   procedure Pump (Now_Ms : Unsigned_64; Next_Due : out Unsigned_64) renames CCL_Fetch_Engine.Pump;
end CCL_Fetch;
