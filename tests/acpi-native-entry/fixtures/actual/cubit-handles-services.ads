------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The typed service endpoints (CAP-003), one per catalog client.
--
--  @description
--  The service catalog's (client X) column names the instance here that a
--  program's generated Startup function returns for that service
--  (userspace/ccl/catalogs/native-runtime-services.ccl).
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Handles.Typed_Endpoints;

package CuBit.Handles.Services with SPARK_Mode is
   --  logstore: log publishers (CuBit.Logging.Publisher, Announce).
   package Logstore is new CuBit.Handles.Typed_Endpoints;
   --  log-observer: log readers (CuBit.Logging.Reader).
   package Log_Observer is new CuBit.Handles.Typed_Endpoints;
   --  process-manager: launchers (CuBit.Launching.Launcher).
   package Process_Manager is new CuBit.Handles.Typed_Endpoints;
   --  config: settings (CuBit.Settings, through Startup.Settings).
   package Config is new CuBit.Handles.Typed_Endpoints;
end CuBit.Handles.Services;
