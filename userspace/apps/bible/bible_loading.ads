--  The Bible app's translations, from the cubit-bible asset package.
with Bible_Asset_Files;
with Bible_Assets;
package Bible_Loading is new Bible_Asset_Files (Bible_Assets.Filesystem, Bible_Assets.Asset_Root);
