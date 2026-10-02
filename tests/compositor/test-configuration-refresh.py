"""Hosted actual Desktop configuration/refresh functions; mocked input delivery.

Run in Nix. This tests scene policy integration, not native IPC or scanout.
"""
from pathlib import Path
import subprocess
import tempfile

root = Path(__file__).resolve().parents[2]
source = (root / "userspace/services/desktop/main.adb").read_text()
start = source.index("   function currentPublicationConfiguration (")
end_marker = "   end refreshPublicationConfigurations;"
end = source.index(end_marker, start) + len(end_marker)
functions = source[start:end]

prefix = """
with Ada.Text_IO; with Interfaces; use Interfaces;
with CuBit.Desktop_Protocol.Publication;
with CuBit.Display_Geometry;
with Compositor_Density; with Compositor_Density_Selection;
with Compositor_Surface_State;
procedure Refresh_Tests is
   package DP renames CuBit.Desktop_Protocol;
   package Publication renames DP.Publication;
   package DG renames CuBit.Display_Geometry;
   package Surface_Policy is new Compositor_Surface_State;
   use type DP.Status_Code, Publication.Configuration, Surface_Policy.Phase;
   use type DP.Pixel_Extent;
   use type Surface_Policy.State, Publication.Configuration_Result;
   NO_PROCESS : constant := 0;
   SURFACE_FLAG_WINDOW : constant Unsigned_64 := 2;
   CLIENT_INSET_X : constant := 4;
   CLIENT_INSET_TOP : constant := 30;
   CLIENT_INSET_BOTTOM : constant := 4;
   type Surface is record
      used, publicationMode : Boolean := False;
      owner : Natural := 1;
      id : Unsigned_64 := 1;
      x, y : Natural := 100;
      w : Natural := 320;
      h : Natural := 234;
      flags : Unsigned_64 := SURFACE_FLAG_WINDOW;
      publicationPolicy : Surface_Policy.State;
      publicationConfiguration : Publication.Configuration_Result;
   end record;
   surfaces : array (1 .. 3) of Surface;
   type Output_Record is record
      Enabled : Boolean := True;
      Geometry : DG.Output;
   end record;
   presentations : array (0 .. 1) of Output_Record :=
     [(True, (Width => 1200, Height => 900, others => <>)),
      (True, (X => 1200, Width => 1200, Height => 900,
              Scale => (3, 2), others => <>))];
   primaryOutput : Natural := 0;
   nativeScene : Boolean := True;
   Notices : Natural := 0;
   procedure queueConfigure (ID, W, H : Unsigned_64) is
   begin
      pragma Assert (ID = 1 and W = 320 and H = 234);
      Notices := Notices + 1;
   end queueConfigure;
"""
suffix = """
   C : Publication.Configuration_Result;
   OK : Boolean;
begin
   surfaces (1).used := True; surfaces (1).publicationMode := True;
   C := currentPublicationConfiguration (surfaces (1));
   pragma Assert (C.Status = DP.Success and C.Value.Epoch = 1);
   pragma Assert (C.Value.Layout.Width = 312 and C.Value.Layout.Height = 200);
   pragma Assert (Notices = 0);
   Surface_Policy.Stage (surfaces (1).publicationPolicy, 1, 1, OK);
   pragma Assert (OK);
   Surface_Policy.Present (surfaces (1).publicationPolicy, 1, 1, 1, OK);
   pragma Assert (OK);
   -- No client request: moving the idle source must itself discover new DPI.
   surfaces (1).x := 1300;
   refreshPublicationConfigurations;
   C := surfaces (1).publicationConfiguration;
   pragma Assert (C.Value.Epoch = 2 and Notices = 1);
   pragma Assert (C.Value.Layout.Width = 468 and C.Value.Layout.Height = 300);
   Surface_Policy.Stage (surfaces (1).publicationPolicy, 2, 2, OK);
   pragma Assert (OK);
   -- A straddling source still chooses the denser output: no redundant wake.
   surfaces (1).x := 1100;
   for I in 1 .. 1000 loop refreshPublicationConfigurations; end loop;
   pragma Assert (Notices = 1);
   -- Output scale change, again with unchanged logical dimensions.
   presentations (1).Geometry.Scale := (2, 1);
   refreshPublicationConfigurations;
   C := surfaces (1).publicationConfiguration;
   pragma Assert (C.Value.Epoch = 3 and Notices = 2);
   pragma Assert (C.Value.Layout.Width = 624 and C.Value.Layout.Height = 400);
   pragma Assert (surfaces (1).publicationPolicy.Buffers (1).Status = Surface_Policy.Visible);
   pragma Assert (surfaces (1).publicationPolicy.Buffers (1).Epoch = 1);
   pragma Assert (surfaces (1).publicationPolicy.Buffers (2).Status = Surface_Policy.Retiring);
   pragma Assert (surfaces (1).publicationPolicy.Buffers (2).Epoch = 2);
   surfaces (1).x := 100;
   refreshPublicationConfigurations;
   pragma Assert (Notices = 3 and surfaces (1).publicationConfiguration.Value.Epoch = 4);
   -- Unused, unmanaged and internal surfaces are excluded from proactive work.
   surfaces (2).used := True;
   surfaces (3).used := True; surfaces (3).publicationMode := True;
   surfaces (3).owner := NO_PROCESS;
   refreshPublicationConfigurations;
   pragma Assert (Notices = 3);
   pragma Assert (surfaces (2).publicationPolicy.Requested = 0);
   pragma Assert (surfaces (3).publicationPolicy.Requested = 0);
   -- An inadmissible density must not retire anything or issue a false wake.
   declare
      Before : constant Surface_Policy.State := surfaces (1).publicationPolicy;
      Old_Config : constant Publication.Configuration_Result :=
        surfaces (1).publicationConfiguration;
   begin
      surfaces (1).x := 1200;
      surfaces (1).y := 0;
      presentations (1).Geometry.Scale := (16, 1);
      refreshPublicationConfigurations;
      C := currentPublicationConfiguration (surfaces (1));
      pragma Assert (C.Status = DP.Resources_Exhausted);
      pragma Assert (Notices = 3);
      pragma Assert (surfaces (1).publicationPolicy = Before);
      pragma Assert (surfaces (1).publicationConfiguration = Old_Config);
      presentations (1).Geometry.Scale := (3, 2);
      refreshPublicationConfigurations;
      pragma Assert (Notices = 4);
      pragma Assert (surfaces (1).publicationConfiguration.Value.Epoch = 5);
   end;
   -- Configuration admission also remains closed after lifetime shutdown.
   Surface_Policy.Close (surfaces (1).publicationPolicy);
   declare
      Before : constant Surface_Policy.State := surfaces (1).publicationPolicy;
   begin
      surfaces (1).x := 100;
      refreshPublicationConfigurations;
      pragma Assert (Notices = 4 and surfaces (1).publicationPolicy = Before);
   end;
   Ada.Text_IO.Put_Line ("CONFIGURATION-REFRESH: PASS idle move, scale, stable seam, retained ownership");
   Ada.Text_IO.Put_Line ("CONFIGURATION-REFRESH: PASS rejected density preserves state; recovery and closed lifetime");
end Refresh_Tests;
"""

with tempfile.TemporaryDirectory(prefix="cubit-configuration-refresh-") as tmp:
    out = Path(tmp)
    units = {
        "userspace/runtime/gnat": ["cubit.ads", "cubit-grant_references.ads",
            "cubit-desktop_protocol.ads", "cubit-desktop_protocol.adb",
            "cubit-desktop_protocol-publication.ads", "cubit-desktop_protocol-publication.adb"],
        "userspace/lib/display": ["cubit-display_geometry.ads", "cubit-display_geometry.adb"],
        "userspace/lib/compositor": [f"{name}.{ext}" for name in (
            "compositor_density", "compositor_density_selection", "compositor_surface_state")
            for ext in ("ads", "adb")],
    }
    for directory, names in units.items():
        for name in names:
            (out / name).write_bytes((root / directory / name).read_bytes())
    (out / "refresh.gpr").write_text('''project Refresh is
      for Source_Dirs use ("."); for Object_Dir use "obj";
      for Exec_Dir use "."; for Main use ("refresh_tests.adb");
      package Compiler is
        for Default_Switches ("Ada") use ("-gnat2022", "-gnata", "-gnato", "-O2");
      end Compiler;
    end Refresh;''')
    for negative in (False, True):
        body = functions
        if negative:
            # Missing notification was the original defect. Prove the oracle
            # rejects it rather than passing solely on configuration queries.
            needle = "queueConfigure (S.id, Unsigned_64 (S.w), Unsigned_64 (S.h));"
            assert body.count(needle) == 1
            body = body.replace(needle, "null;")
        (out / "refresh_tests.adb").write_text(prefix + body + suffix)
        subprocess.run(["gprbuild", "-q", "-p", "-P", str(out / "refresh.gpr")], check=True)
        result = subprocess.run([str(out / "refresh_tests")], capture_output=True, text=True)
        if negative:
            assert result.returncode != 0 and "ASSERTION_ERROR" in result.stderr, result
            print("CONFIGURATION-REFRESH: missing-notification negative control rejected")
        else:
            assert result.returncode == 0, result.stderr
            print(result.stdout, end="")
