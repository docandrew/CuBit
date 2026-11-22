------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What a server declares about each channel contract it offers (CAP-002
--  step 3, docs/capability-handles.md section 4): its limits and its
--  service class. The manifest's `channels` entry, the .cubit.channels
--  section (CuBit.Channel_Declarations), the generated Startup package and
--  CuBit.Channel_Servers all use these types.
------------------------------------------------------------------------------
pragma Ada_2022;

package CuBit.Channel_Limits with Pure, SPARK_Mode is

   --  The structural bound on channels per contract.
   Maximum_Total : constant := 65_536;
   subtype Limit_Count is Positive range 1 .. Maximum_Total;

   --  At_Most N, or Server_Limit: as many as the server can serve right
   --  now (spelled out, like Kernel_Limit for handles; never a default).
   type Limit_Kind is (At_Most, Server_Limit);
   for Limit_Kind use (At_Most => 1, Server_Limit => 2);

   type Channel_Limit (Kind : Limit_Kind := At_Most) is record
      case Kind is
         when At_Most      => Count : Limit_Count := 1;
         when Server_Limit => null;
      end case;
   end record;

   --  Whether Count channels are within Limit.
   function Within (Limit : Channel_Limit; Count : Natural) return Boolean is
     (case Limit.Kind is
         when At_Most      => Count <= Limit.Count,
         when Server_Limit => True);

   --  A channel's service class, highest first (user, 2026-10-17): Display's
   --  channel urgent, the compositor's interactive, applications normal.
   type Channel_Class is (Urgent, Interactive, Normal);
   for Channel_Class use (Urgent => 1, Interactive => 2, Normal => 3);

end CuBit.Channel_Limits;
