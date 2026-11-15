------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A metrics reader (MET-002): lists series and reads them from metrics.svc
--  through the program's metrics-observer endpoint (its manifest's
--  (service "metrics-observer" "metrics-observer"), granted by procmgr only
--  from the startup plan or with approval) within the read scopes the
--  manifest declares ((metrics "desktop.") or (metrics-all)). A series
--  outside them is refused (Out_Of_Scope) and metrics.svc logs it.
--
--  @description
--  Each call lends one page (CuBit.Metric_Query_Pages) and waits at most
--  Call_Wait_Ms for the answer: interactive, never for a render path.
--  Calls on one Reader are serialized by its owner.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Channels;
with CuBit.Handles;
with CuBit.Handles.Startup;
with CuBit.Metric_Protocol;
with CuBit.Metric_Query_Pages;

package CuBit.Metric_Queries is
   package Pages renames CuBit.Metric_Query_Pages;
   subtype Status is CuBit.Metric_Protocol.Status;

   Call_Wait_Ms : constant := 2_000;
   --  The startup entry a reader's endpoint is under (its manifest:
   --  (service "metrics-observer" "series")).
   Observer_Binding : constant String := "series";

   type Reader is limited private;
   function Bound_To (Target : CuBit.Handles.Startup.Optional_Endpoint) return Reader;
   function Available (Item : Reader) return Boolean;

   --  Which series: every one, one name exactly, or names under a prefix;
   --  Source empty for any source.
   type Name_Match is (Any_Name, Exact_Name, Name_Prefix);
   for Name_Match use (Any_Name => 0, Exact_Name => 1, Name_Prefix => 2);

   procedure List
     (Item : Reader; Match : Name_Match; Name, Source : String; Skip : Unsigned_64;
      Answer : out Pages.Page; Rows, Total : out Unsigned_64; Result : out Status);
   --  Step_Us 0: raw samples; otherwise windows of the coarsest tier no
   --  wider than Step_Us.
   procedure Read_Range
     (Item : Reader; Id, From_Us, To_Us, Skip, Step_Us : Unsigned_64;
      Answer : out Pages.Page; Rows, Total, Gaps, Resolution_Us : out Unsigned_64; Result : out Status);
   --  Id 0: every series the selector names, merged.
   procedure Read_Window
     (Item : Reader; Id : Unsigned_64; Match : Name_Match; Name, Source : String;
      From_Us, To_Us : Unsigned_64; Answer : out Pages.Page; Merged : out Unsigned_64; Result : out Status);

   --  A push subscription (CuBit.Metric_Updates): open Link, a consuming
   --  channel to metrics.svc, and subscribe it to the selector's series
   --  (within the read scopes, as every read). Result OK: samples arrive in
   --  Link from the store's next pass; read them with Channels.Take, no IPC.
   --  Otherwise Link is closed.
   Open_Wait_Ms : constant := 2_000;
   procedure Subscribe
     (Item : Reader; Match : Name_Match; Name, Source : String; Link : in out CuBit.Channels.Channel;
      Result : out Status);
private
   type Reader is limited record
      Slot : CuBit.Handles.Endpoint := CuBit.Handles.No_Endpoint;
   end record;
end CuBit.Metric_Queries;
