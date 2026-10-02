# Upload-search image verification

Read-only audit of the handed-off private image (2026-09-28).

Image: `.build-workspaces/intel-upload-search-5842h_bj/kernel/cubit_live_uefi.img`

SHA256: `c40e7f2c908e9cac1366b0aac22efcf0daaf410f629e9a83b55ae512c5e6b5e5`

Used xorriso to enumerate and extract `/apps/intel-gpu.drv` into a unique
temporary directory. The extracted file and the snapshot's freshly built
`userspace/services/intel-gpu/build/intel-gpu.drv` have identical SHA256:

`41886bf8cea8c5765faf357adbf166b0c47305abed87280a992260b67bdd1348`

Both diagnostic strings are present in the extracted binary:

- `intel-gpu: upload search `
- `intel-gpu: upload first nonzero index=`

This rules out omission of these strings or staging an older driver in this
specific image. It does not prove which image the NUC booted, that the relevant
branch executed, or that every diagnostic reached the viewer. The search
summary follows the firmware-mapping result on that execution path. The first
nonzero line is conditional on a nonzero count. This image uses enumeration
Image for the search outcome and may print an ordinal (3 means exhausted);
the later explicit-name correction is not included.

No image, boot media, or shared build output was modified by the audit.
