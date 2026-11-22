------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Config's data operations by key (docs/config-declarative-state.md),
--  private to CuBit.Settings: programs reach them only through their
--  generated, typed accessors (CAP-003).
--
--  @description
--  One page-aligned two-page buffer is lent to Config on first use (the
--  full 4096-byte value plus its key), through the program's Config
--  endpoint; values are copied out before the next call. Calls must be
--  serialized by the caller (one task per program).
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Handles.Services;

private package CuBit.Settings.Transport is

   subtype Endpoint is CuBit.Handles.Services.Config.Endpoint;
   type Status is (OK, Not_Found, Refused, Failed);

   --  A write's or delete's Status, as its callers see it.
   function Changed (Result : Status) return Change_Outcome is
     (case Result is
         when OK        => Done,
         when Not_Found => Not_Present,
         when Refused   => CuBit.Settings.Refused,
         when Failed    => No_Config);

   procedure Get (Through : Endpoint; Key : String; Value : out Text; Result : out Status);
   procedure Set (Through : Endpoint; Key : String; Value : String; Result : out Status);
   procedure Delete (Through : Endpoint; Key : String; Result : out Status);

   --  The keys under Prefix, NUL-separated, Count of them.
   procedure List (Through : Endpoint; Prefix : String; Names : out Text; Count : out Natural;
                   Result : out Status);

end CuBit.Settings.Transport;
