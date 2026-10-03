//! Read-only inspector of the active document's actual rustls handshake.
//! AWS-LC is already Servo's crypto provider; parsing here does not establish
//! trust or make a second connection. rustls remains the sole verifier.
// Narrow declarations matching aws-lc-sys 0.45.0's generated x86-64 ABI.
// Keep the exact Cargo version pin: these names intentionally fail to link
// on an unchecked provider upgrade. No all-bindings or second crypto library.
#[allow(non_snake_case)]
mod lc {
    pub use aws_lc_sys::{X509, X509_NAME, ASN1_TIME, NID_commonName};
    unsafe extern "C" {
        #[link_name = "aws_lc_0_45_0_X509_free"]
        pub fn X509_free(cert: *mut X509);
        #[link_name = "aws_lc_0_45_0_d2i_X509"]
        pub fn d2i_X509(out: *mut *mut X509, input: *mut *const u8, len: std::os::raw::c_long) -> *mut X509;
        #[link_name = "aws_lc_0_45_0_X509_get_subject_name"]
        pub fn X509_get_subject_name(cert: *const X509) -> *mut X509_NAME;
        #[link_name = "aws_lc_0_45_0_X509_get_issuer_name"]
        pub fn X509_get_issuer_name(cert: *const X509) -> *mut X509_NAME;
        #[link_name = "aws_lc_0_45_0_X509_NAME_get_text_by_NID"]
        pub fn X509_NAME_get_text_by_NID(name: *const X509_NAME, nid: i32, buffer: *mut i8, length: i32) -> i32;
        #[link_name = "aws_lc_0_45_0_X509_get0_notBefore"]
        pub fn X509_get0_notBefore(cert: *const X509) -> *const ASN1_TIME;
        #[link_name = "aws_lc_0_45_0_X509_get0_notAfter"]
        pub fn X509_get0_notAfter(cert: *const X509) -> *const ASN1_TIME;
        #[link_name = "aws_lc_0_45_0_ASN1_TIME_to_posix"]
        pub fn ASN1_TIME_to_posix(value: *const ASN1_TIME, seconds: *mut i64) -> i32;
    }
}
use servo::WebView;

struct Certificate(*mut lc::X509);
impl Drop for Certificate {
    fn drop(&mut self) { unsafe { lc::X509_free(self.0) }; }
}
fn name(cert: &Certificate, issuer: bool) -> String {
    let mut buffer = [0i8; 512];
    // X509 owns the borrowed name; the supplied output buffer is bounded.
    unsafe {
        let dn = if issuer { lc::X509_get_issuer_name(cert.0) }
                 else { lc::X509_get_subject_name(cert.0) };
        if dn.is_null() { return "Unavailable".into(); }
        let length = lc::X509_NAME_get_text_by_NID(dn, lc::NID_commonName,
            buffer.as_mut_ptr(), buffer.len() as i32);
        if length < 0 { return "No common name (identity may use subjectAltName)".into(); }
        let bytes: Vec<u8> = buffer[..(length as usize).min(buffer.len()-1)]
            .iter().map(|c| *c as u8).collect();
        String::from_utf8_lossy(&bytes).into_owned()
    }
}
fn date(cert: &Certificate, end: bool) -> String {
    let mut seconds = 0;
    unsafe {
        let value = if end { lc::X509_get0_notAfter(cert.0) } else { lc::X509_get0_notBefore(cert.0) };
        if value.is_null() || lc::ASN1_TIME_to_posix(value, &mut seconds) != 1 {
            return "Unavailable".into();
        }
    }
    match time::OffsetDateTime::from_unix_timestamp(seconds) {
        Ok(t) => format!("{:04}-{:02}-{:02} {:02}:{:02}:{:02} UTC",
            t.year(), t.month() as u8, t.day(), t.hour(), t.minute(), t.second()),
        Err(_) => "Unavailable".into(),
    }
}
fn line(out: &mut String, text: &str) {
    // Plain ASCII, no control characters from remote certificate names.
    let safe: String = text.chars().map(|c| if c.is_ascii() && !c.is_ascii_control() {c} else {'?'}).collect();
    if safe.is_empty() { out.push('\n'); }
    for part in safe.as_bytes().chunks(76) {
        out.push_str(std::str::from_utf8(part).unwrap()); out.push('\n');
    }
}
fn certificate(out: &mut String, der: &[u8], index: usize) {
    line(out, &format!("Certificate {}{}", index+1, if index==0 {" (site)"} else {" (issuer)"}));
    if der.is_empty() || der.len() > 65536 { line(out, "Certificate exceeds inspector limit."); return; }
    let mut cursor = der.as_ptr();
    let cert = Certificate(unsafe { lc::d2i_X509(std::ptr::null_mut(), &mut cursor, der.len() as _) });
    if cert.0.is_null() { line(out, "Certificate details could not be decoded."); return; }
    line(out, &format!("Subject: {}", name(&cert, false)));
    line(out, &format!("Issuer:  {}", name(&cert, true)));
    line(out, &format!("Valid from: {}", date(&cert, false)));
    line(out, &format!("Valid until: {}", date(&cert, true)));
    let hash = aws_lc_rs::digest::digest(&aws_lc_rs::digest::SHA256, der);
    let hex: String = hash.as_ref().iter().map(|b| format!("{b:02X}")).collect();
    line(out, "SHA-256 fingerprint:"); line(out, &hex); line(out, "");
}
pub fn report(view: &WebView, loading: bool) -> String {
    let mut out = String::new();
    let Some(url) = view.url() else { return "No document loaded.\n".into(); };
    line(&mut out, &format!("Site: {}", url.host_str().unwrap_or("Local document")));
    if loading { line(&mut out, "Navigation in progress; connection details pending."); return out; }
    if url.scheme() != "https" {
        line(&mut out, if url.scheme()=="http" {"HTTP: this document's connection is not encrypted."}
             else {"This document does not use a TLS connection."});
        return out;
    }
    let Some(info) = view.document_tls().filter(|i| {
        // Same-document fragment navigation must retain the original handshake.
        url::Url::parse(&i.url).ok().is_some_and(|mut original| {
            let mut current = url.clone(); original.set_fragment(None); current.set_fragment(None);
            original == current
        })
    }) else { line(&mut out, "No verified TLS details available for this document."); return out; };
    line(&mut out, "Main document connection (verified by rustls)");
    line(&mut out, if view.load_status() == servo::LoadStatus::Complete { "Page load: complete" }
        else { "Page load: subresources still loading" });
    line(&mut out, &format!("Protocol: {}   Application protocol: {}", info.protocol, if info.alpn.is_empty() { "not negotiated" } else { &info.alpn }));
    line(&mut out, &format!("Cipher: {}", info.cipher));
    line(&mut out, &format!("Server-supplied chain (up to 8): {} certificate(s)", info.certificates.len()));
    line(&mut out, "This describes the document connection, not every subresource."); line(&mut out, "");
    for (index, der) in info.certificates.iter().take(8).enumerate() { certificate(&mut out, der, index); }
    out.truncate(out.len().min(12288)); // ASCII; mirrors the native bounded buffer.
    out
}
